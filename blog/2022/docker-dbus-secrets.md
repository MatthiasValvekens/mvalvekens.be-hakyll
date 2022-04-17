---
title: "Using KeePassXC as a credential manager for sudoed Docker"
author: "Matthias Valvekens"
tags: [devops, docker, security, tech]
published: 2022-04-17
license: "CC BY-SA 4.0 hybrid"
---

# Background

As [noted in the official documentation][docker-sec], having access to the Docker daemon amounts to having passwordless root access to all files on the system.
That's why, even on my development machine, I don't put my personal account in the `docker` group.
As such, I have to use `sudo` every time I need to interface with the Docker daemon.

[docker-sec]: https://docs.docker.com/engine/security/#docker-daemon-attack-surface

However, this spawned a secondary problem when authenticating to remote docker registries.
With a `sudo`-based setup where only `root` can talk to the Docker daemon, `docker login` will (by default) store credentials in `/root/.docker/config.json` in plain text.
Docker dutifully emits a warning when it does that[^warning], cautioning the user to configure [a credential store][credstore] instead.
On my system, this turned out to be a bit more tricky than I expected.

[^warning]: Granted, it would've been better if Docker would warn the user _before_ showing the credential prompt, but I suppose a late warning is better than no warning at all.

[credstore]: https://docs.docker.com/engine/reference/commandline/login/#credentials-store


# Setting up KeePassXC as a credential store

Since I already use [KeePassXC](https://keepassxc.org/) as my password manager of choice, I was of course very keen to figure out a way to get Docker to use that as its credential backend.
Fortunately, KeePassXC integrates natively with [the freedesktop.org secrets API][secrets-api], also known as the "Secret Service" API. Essentially, this API defines a generic protocol for applications to talk to credential stores over a DBus session.
This is the protocol used by the Gnome keyring, among other applications.
Unfortunately, the [KeePassXC user guide][user-guide] currently doesn't contain any instructions on how to set up the secret service integration, but the process is quite straightforward:

- First, check the "Enable KeePassXC Freedesktop.org Secret Service integration" box in [Tools > Settings > Secret Service Integration]{style="font-weight: bold;"}. This enables the integration at the application level.

- Then, with your favourite password database open, go into [Database > Database Settings > Secret Service Integration]{style="font-weight: bold;"} and set up a folder to expose over the Secret Service API. You'll probably want to use a new, empty folder for that.

- After ensuring that there are no other keyring services running that interface with the Secret Service API, you should be all set. Perform a few tests using `secret-tool` (part of `libsecret` or `libsecret-tools` on most distros) with dummy credentials to make sure everything works as expected.

Here's an example command that you can use to check if the connection works:

```bash
secret-tool store --label='Test test' account cred-test
```

If all is well, you should see a credential with label "Test test" pop up in your KeePassXC Secret Service folder after putting in a password.

[secrets-api]: https://docs.docker.com/engine/reference/commandline/login/#credentials-store
[user-guide]: https://keepassxc.org/docs/KeePassXC_UserGuide.html


# Integration with Docker

That wasn't too hard, was it? What's even _better_ is that the Docker team [supplies a credential helper][cred-helpers] that implements the Secret Service protocol already.
Once you put `docker-credential-secretservice` on the path, the configuration in `$HOME/.docker/config.json` to set it up is as simple as this:

```json
{
  "credsStore": "secretservice"
}
```

[cred-helpers]: https://github.com/docker/docker-credential-helpers/releases

However, if we want this to work when interacting with the Docker container as `root`, we're not out of the woods yet. You'll notice that the following invocation throws an error:

```bash
sudo secret-tool store --label='Test test' account cred-test
```

Similarly, running `sudo docker login` won't work out of the box. This is because `root` doesn't know how to talk to your user's DBus session.
However, there's a trick that will allow us to get what we want.

First, check the value of the `DBUS_SESSION_BUS_ADDRESS` environment variable for your user. On my system, that's `unix:path=/run/user/<user ID>/bus`, which looks pretty stable across reboots.
Then, assuming your username is `myuser` and your user ID is `1000`, open up a text editor and write:

```bash
#!/bin/sh

export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus

sudo --preserve-env=DBUS_SESSION_BUS_ADDRESS -n -u myuser \
    /usr/local/bin/docker-credential-secretservice "$@"
```

Obviously, change the path to `docker-credential-secretservice` to whatever you need it to be.
Now save this file somewhere on the `$PATH` of the root user, with `docker-credential-secretservice-sudo` as the file name. Next, make sure `root` owns it, and set its permissions to `700`.

::: warning
Since this script will be invoked by `root`, it's very important to ensure that no-one else has write access to it.
Giving other users read or execute access is not necessarily problematic in itself, but since we don't need to, I went with `700` as opposed to something like `755`.
:::

This little script does the following.

 - It sets the value of the `DBUS_SESSION_BUS_ADDRESS` variable to your user's DBus socket.
 - It then invokes the "real" `docker-credential-secretservice` binary with the arguments passed to the script, as your user through `sudo`, while preserving the `DBUS_SESSION_BUS_ADDRESS` variable.

::: note
This assumes that `root` can impersonate anyone without a password, which is the default on all systems that I'm aware of.
:::

Finally, change the `credsStore` setting in `/root/.docker/config.json` as follows.

```json
{
  "credsStore": "secretservice-sudo"
}
```

This works because Docker looks for credential helpers by scanning the path for binaries named `docker-credential-XYZ`, where `XYZ` is the value of the `credsStore` setting.

Now you can use `docker login` over `sudo`, and push/pull containers to registries that require authentication without storing your credentials in plain text in `root`'s home folder!


::: note
While I only tested the scenario explained above, most of this explanation likely still applies if you're interfacing with the Docker daemon using a third, non-root account (e.g. when using rootless docker, or a service account in the `docker` group). The only tricky part is that you will additionally need to set up `sudo` so that said service account can run `docker-credential-secretservice` as your user.
:::
