---
title: "Passwordless PostgreSQL access for workloads in Kubernetes: an experiment"
author: "Matthias Valvekens"
tags: [devops, k8s, postgres, oidc, security, tech]
published: 2025-04-21
license: "CC BY-SA 4.0 hybrid"
---

# Background


Managing secrets is hard to get right. Managing secrets for access to something as ubiquitous and mission-critical as a database server is _also_ hard.

The big cloud providers have an answer to this for their own managed services: they allow you to set up policy-based access to databases with credentials conjured up by the platform out of thin air. This largely removes the secrets management burden from developers and operators.

That's all very nice. But what if you're not running on a big cloud provider's infra? Is it possible to get similar benefits using only FOSS tooling?
I challenged myself to try and solve that problem for my homelab's K8S cluster and the PostgreSQL database server that I run alongside it. These are the results of that experiment, presented in "how-to" form with some discussion [at the end](#conclusions).


# Ingredients

To perform the setup that we're about to go through, you'll need the following:

 - A PostgreSQL server compiled with PAM support. This is available by default in many major Linux distros, but not all[^alpine-no-pam]. 
 - A Kubernetes cluster that supports OIDC for service accounts, and projected volumes. Unless your cluster is _very_ far behind, this shouldn't be an issue.
 - A PAM module that wires support for JWT/OIDC tokens into PAM. For this experiment, I'm using [this `pam_oidc` module open-sourced by pardot/Salesforce](https://github.com/salesforce/pam_oidc).
 - A working certificate management set-up. OIDC endpoints need to be exposed over HTTPS.

You will also need shell access to the underlying database server, and the ability to modify the arguments to the `kube-apiserver` component of your Kubernetes cluster. So trying this in a managed environment is probably off the table. But then again, that's not what this exercise is about.

For completeness' sake: my lab currently runs PostgreSQL 16 on Ubuntu 24 LTS. My Kubernetes distribution is k3s, v1.32 at the time of writing.

::: warning
I wasn't able to find a FOSS PAM module with the right capabilities that is still being maintained up to a standard that I would consider generally acceptable in a production environment.
Since I'm working in a low-stakes homelab setting, I can deal with the abandonware and/or patch it myself as needed, but [YMMV](#the-bad).
:::


[^alpine-no-pam]: Some more minimalist distros (e.g. Alpine Linux) don't currently package PostgreSQL with PAM support compiled in.


# Step one: Kubernetes cluster preparation


## Exposing the OIDC configuration to the outside

In order to use Kubernetes service account identities outside the cluster, we need to set up some glue that allows services running outside K8S to validate K8S service account tokens.


::: note
It's worth noting that no part of this section is in any way specific to PAM or Postgres.
If you're already using your K8S service account identities with any other system through OIDC (e.g. AWS IAM), you don't need to change anything. Feel free to skip ahead.
:::

If you're integrating with an external service that speaks OIDC[^token-review], all the necessary configuration is actually available from a standard URL published by your API server. If we take the API base URL as `k8s.example.com`, here's what that URL looks like:

```
https://k8s.example.com/.well-known/openid-configuration
```

If you hit that URL with `curl`, you'll see something like

```json
{
  "issuer": "https://k8s.example.com",
  "jwks_uri": "https://k8s.example.com/openid/v1/jwks",
  "response_types_supported": [
    "id_token"
  ],
  "subject_types_supported": [
    "public"
  ],
  "id_token_signing_alg_values_supported": [
    "RS256"
  ]
}
```


[^token-review]: I'm assuming OIDC for now. In principle, it's of course also possible to allow outside services to access the cluster's `TokenReview` API to achieve similar results, but that makes the integration very Kubernetes-specific.


While it's possible to proceed with these defaults, using the cluster's own API endpoint directly is usually ill-advised.
For one, if the counterparty that needs to validate your service account tokens doesn't live on your internal network, you'd have to expose your cluster's API endpoint to the public internet!

This is not usually an issue for database servers (which are what this post is about, after all), but if you also intend to use K8S service accounts to integrate with cloud providers like AWS or GCP, you might as well spend a few more minutes to get it right.

The idea is to set up a service in front of the K8S cluster's management API. The point of this service is to proxy calls to this configuration endpoint, and to `jwks_uri`.
We'll then have Kubernetes advertise the URL of this new service instead.

To summarise, we need to do three things:

 - Set up an external HTTP service that serves the right `/.well-known/openid-configuration` document, and that can retrieve JWKS from the cluster.
 - Make sure that service can be accessed over HTTPS[^public-trust].
 - Make sure the service can do OpenID discovery in your cluster.


[^public-trust]: Depending on your use case you may not _need_ to configure a certificate that is publicly trusted. Doing the setup with a private PKI involves some additional subtleties, though.


The HTTP service through which you proxy your OIDC discovery requests can live inside or outside your cluster. This is only one of several possible permutations, but I chose to put it inside for ease of configuration. You could, for example, deploy `nginx` with the following config:

```
server {
  listen 8000 default_server;
  listen [::]:8000 default_server;

  proxy_ssl_trusted_certificate /run/secrets/kubernetes.io/serviceaccount/ca.crt;
  proxy_ssl_verify       on;
  proxy_ssl_verify_depth 2;

  location = /.well-known/openid-configuration {
      proxy_pass https://kubernetes.default.svc.cluster.local;
  }

  location = /openid/v1/jwks {
      proxy_pass https://kubernetes.default.svc.cluster.local;
  }
}

```

I just put this behind my ingress controller to handle the public-facing TLS termination for me. I had already set up some certificate lifecycle management with `cert-manager`, so that avoided some additional configuration.

::: note
It's generally also possible to set up an ingress pointing directly at the `kubernetes` service in the default namespace, but that comes with its own share of configuration pitfalls. The above approach uses marginally more resources and is a bit on the verbose side, perhaps, but I find it easier to understand / troubleshoot.
:::

If you use the above setup, you'll also need a `ClusterRoleBinding` to allow unauthenticated API users to perform OIDC discovery:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: allow-unauthenticated-discovery
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: system:service-account-issuer-discovery
subjects:
- apiGroup: rbac.authorization.k8s.io
  kind: Group
  name: system:unauthenticated
```

## API server configuration

Let's say we exposed the above two endpoints as `https://k8s-oidc.example.com/.well-known/openid-configuration` and `https://k8s-oidc.example.com/openid/v1/jwks`. The final step is to make the Kubernetes API server aware of how the OIDC configuration is being advertised, by passing the following command-line arguments to it:

```
--service-account-issuer=https://k8s-oidc.example.com \
--service-account-jwks-uri=https://k8s-oidc.example.com/openid/v1/jwks
```

These settings need to be put in place on every server node; on agent-only nodes they're not meaningful.

::: note
The value of the `--service-account-issuer` flag corresponds to the `iss` claim in the ID tokens issued by Kubernetes. External validators will bootstrap OIDC from this URL by appending `/.well-known/openid-configuration` and retrieving the configuration from there.
:::

While not strictly necessary, it's a good idea to also set an explicit value for the `--api-audiences` flag. The whole point of this exercise is to be able to use K8S service account tokens for things other than interacting with the K8S platform itself, so basic token hygiene practices dictate that tokens should be scoped to a specific audience. You wouldn't want tokens generated for database access to also be accepted by the Kubernetes API, or vice versa.
If you don't specify anything in `--api-audiences`, Kubernetes will substitute the value of `--service-account-issuer` as a default in all tokens generated for Kubernetes API access.
This is fine as long as you take care to specify a different value for all _other_ service account token uses, but it's good to get in the habit of thinking about audiences when setting up anything involving token-based authentication.


# Step two: Setting up the database server

## Configuring PAM and user mapping

On the Postgres side, the goal is relatively simple. We want to be able to map Kubernetes service accounts to Postgres users. Workloads will authenticate by presenting a service account token issued by the Kubernetes control plane. All authorisation checks are done in Postgres in the usual way, i.e. deriving that from information in the token is not in scope.

I'll explain the configuration using the conventions of [the `pam_oidc` plugin I linked above](https://github.com/salesforce/pam_oidc).

First, move `pam_oidc.so` into the PAM module lib directory, which is typically `/usr/lib/security` or some variant thereof. This will allow PAM to find the module.
In order to actually use it, we need to define a PAM service referencing the module.

Create a file named `postgresql-k8s` in `/etc/pam.d` with the following contents:

```
auth    required        pam_oidc.so issuer=https://k8s-oidc.example.com aud=postgres [user_template={{.Subject|trimPrefix "system:serviceaccount:" }}]
account required        pam_permit.so
```

Let's walk through what each of these settings mean.

 - The first line configures the authentication mechanism through `pam_oidc`.

   - The `issuer` and `aud` parameters limit valid tokens to those with a matching `iss` and `aud` claim, respectively.
   - The `user_template` parameter defines the mapping from OIDC tokens to database users. This particular template will take the token's `sub` claim and strip out the `system:serviceaccount:` prefix, resulting in a username of the form `<namespace>:<account name>`.

 - The second line essentially tells PAM not to perform any account validity checks or authorisation. We'll leave that for the database server to handle.


## Configuring the PostgreSQL service

Locate your server's `pg_hba.conf` file[^hba]. All we need to add to turn on OIDC support in PostgreSQL is this:

```
host all all 192.0.2.0/24 pam pamservice=postgresql-k8s
host all all 2001:db8::/32 pam pamservice=postgresql-k8s
```

The subnets in this config snippets should be replaced by the IP range(s) from which your cluster will be connecting to the database.

You may also want to replace `host` with `hostssl` or `hostgssenc` here.


[^hba]: The exact location depends on how your distro packages `postgresql`.

::: note
As explained in [the PostgreSQL documentation about PAM authentication](https://www.postgresql.org/docs/current/auth-pam.html), PAM can only authenticate existing users.

Assigning a Kubernetes user any database rights requires first creating the user with `CREATE USER` and `GRANT`ing the relevant permissions.
:::


# Step three: Configuring the workloads


## Generating tokens for database access

Say you have a pod called `myesrvice-xyz` running in namespace `foo`, and that you want to use it with a service account called `myservice`.

With the configuration from the previous section, this would map to `foo:myservice` in Postgres---make sure that user exists before proceeding!

In order to access the database from within the pod, you'll need a token. You can have Kubernetes inject one for you and keep it fresh by mounting a _projected volume_ in the pod. Config-wise, here's what that looks like.

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: myservice
  namespace: foo
---
apiVersion: v1
kind: Pod
metadata:
   name: myservice-xyz
   namespace: foo
spec:
  containers:
  - image: alpine/psql
    name: main
    command: ["/bin/sh", "-c"]
    args:
      - >
          export PGPASSWORD=$(cat /var/run/secrets/postgres/token);
          export PGHOST=db.example.com
          export PGUSER=foo:myservice;
          psql -d testdb -c "select 'Hello world';"
    volumeMounts:
    - mountPath: /var/run/secrets/postgres
      name: db-access
      readOnly: true
  serviceAccountName: myservice
  volumes:
  - name: db-access
    projected:
      defaultMode: 256
      sources:
      - serviceAccountToken:
          path: token
          expirationSeconds: 600
          audience: postgres
```

This will make available a token at `/var/run/secrets/postgres/token`. Open a database connection with `foo:myservice` as the username and the token as the password, and you're in business!


::: warn
What you choose to set as the `audience` is up to you, but it needs to match the value configured in the PAM service on the Postgres server.
:::


## Making sure the app picks up the latest token


Short lived tokens for database access are all well and good, but how do you ensure that applications actually keep their access tokens up-to-date? Most Postgres-based applications out there don't provide an option to read (just) the password from a file on every new database connection.

Fortunately, PostgreSQL client libraries support reading credentials from `.pgpass` files, usually by passing in `PGPASSFILE` via the environment, or some variant thereof. At least in all cases that I'm aware of, this read happens on every connection attempt, so with a bit of elbow grease we can leverage that to get apps to use the latest token on every connection attempt.

Let's add a sidecar to that pod from before to show this strategy in action:

```yaml
apiVersion: v1
kind: Pod
metadata:
   name: myservice-xyz-pgpass
   namespace: foo
spec:
  initContainers:
  - name: pgpass-updater
    image: alpine
    command: ["/bin/sh", "-c"]
    args:
      - >
         while true; do
         echo "Refreshing .pgpass at $(date)...";
         echo "*:*:*:*:$(cat /var/run/secrets/db/token)" > /pgpass/.pgpass;
         chmod 600 pgpass/.pgpass;
         sleep 20;
         done;
    restartPolicy: Always
    volumeMounts:
    - mountPath: /var/run/secrets/db
      name: db-access
      readOnly: true
    - mountPath: /pgpass
      name: pgpass
  containers:
  - image: alpine/psql
    name: main
    command: ["/bin/sh", "-c"]
    args:
      - >
          export PGPASSFILE=/pgpass/.pgpass;
          export PGHOST=db.example.com
          export PGUSER=foo:myservice;
          psql -d testdb -c "select 'Hello world';"
    volumeMounts:
    - mountPath: /pgpass
      name: pgpass
      readOnly: true
  serviceAccountName: myservice
  volumes:
  - name: db-access
    projected:
      defaultMode: 256
      sources:
      - serviceAccountToken:
          path: token
          expirationSeconds: 600
          audience: postgres
  - name: pgpass
    emptyDir:
      sizeLimit: 1Mi
      medium: Memory
```

This `pgpass-updater` sidecar rewrites `/pgpass/.pgpass` every 20 seconds. Since tokens get renewed with a bit of headroom before they expire, this ensures that the service container always has a valid `.pgpass` file to work with.


::: warning
Postgres client libraries tend to be quite opinionated about the permissions on a `.pgpass` file. Typically, the requirement is that the permission bits can't be more lenient than `0600`.
Especially if you have services that run as a non-root user, you may have to play around with `securityContext` and `fsUser`/`fsGroup` to ensure that the `.pgpass` file is owned by the appropriate user and gets the appropriate permissions while still remaining writable by the sidecar container.
:::


# Conclusions {#conclusions}


## The good

I've been using the above approach in my homelab for slightly over a year or so. In my K8S cluster, all services that require database access authenticate using service account tokens. This goes for applications that I wrote/maintain myself, but also for "off-the-shelf" tools. For example, I have a Keycloak instance provisioned with the Keycloak operator connecting to Postgres using this method. No custom plugins or operator modifications required.


Here's what I like about it.

 - No long-lived secrets need to be shared between the database server and the cluster.
 - The `.pgpass` sidecar makes the rotating tokens completely transparent to most applications.
 - The number of "service dependencies" on the critical path is fairly small; no need to maintain an external secret manager, for example.
 - While the initial setup requires some effort, onboarding new applications after the fact is quite straightforward.
 - In general, this setup exceeded my stability expectations. Besides a handful of times where I screwed up by breaking DNS or messing with the wrong firewall rules, it's been performing quite reliably so far.

These properties make it a decent candidate for passwordless database access in a lab setting. There are, however, some significant drawbacks that would make me think twice before putting it in production.

## The bad {#the-bad}

 - The lack of a properly maintained & battle-tested OIDC module for PAM is a problem, especially since it's a cornerstone of this integration. In the real world, doing access control on a production database with an unmaintained auth plugin isn't exactly a great idea.
 - While it's great that PostgreSQL libraries support rotating credentials through `.pgpass`, the required sidecar to maintain the `.pgpass` file is a bit of a kludge.
 - It's not always straightforward to configure Postgres to authenticate different "populations" against the same databases via OIDC. Think workloads from different clusters, database access for human operators through SSO, and so on. The `pam_oidc` plugin only takes a single configuration per `pg_hba` entry, so essentially the only useful way to discriminate is the source IP address range. That's not always an option.

So, all things considered, the setup is neat once you get it going, but not quite ready for prime-time.


## What's next?

The problems highlighted above are not major in the sense that they all have fairly obvious technical solutions, but the momentum just doesn't seem to be there. Perhaps there's a better way?
Given the huge potential benefits associated with not having to manage long-lived secrets at all, looking further into democratising access control based on workload identities with off-the-shelf, open-source tools seems like a worthwhile endeavour.

I'm currently playing with the idea of going through the same exercise with [SPIFFE](https://spiffe.io/) as the framework. Comparing the end result of that with this bare-bones OIDC-based approach should be interesting.

I also have a number of non-Kubernetes workloads[^other-workloads] in my lab for which I currently don't have a good secrets management solution. Perhaps a SPIFFE-centric approach would kill two birds with one stone? To be investigated...

[^other-workloads]: We're talking some Proxmox VMs and one containerised applciation with networking needs that make k8s a poor fit.
