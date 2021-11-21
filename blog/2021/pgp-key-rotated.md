---
title: "I rotated my primary PGP key"
author: "Matthias Valvekens"
tags: [digsig, pgp]
published: 2021-11-21
---


# Out with the old


I'd been using an RSA 2048 key since about 2016 for my PGP needs. This was its key fingerprint:

```
C474 1E2A CFD6 9911 13A2
A87B 4399 208B 3C51 1A9B 
```

I've been meaning to decommission it for a while now, but unfortunately that had to wait until I had time to upgrade my YubiKey.
The old key has been revoked.


# And in with the new

My new master keypair uses Ed25519 (EdDSA) and resides in my new YubiKey 5. Right now, it has two subkeys: one extra key for signing (also Ed25519; no certification capability), and one for encryption (Curve25519).

The primary key's fingerprint is this one:

```
9C41 44F3 5E74 2C88 A5D2
563C 15F4 2BEF A159 BA54
```

My [new key is available online](/static/misc/matthias-gpg.asc), signed with the old one. I've also uploaded [my old key signed with my new one](/static/misc/matthias-gpg-rsa-cross-sign-revoked.asc), together with a revocation certificate indicating the reason for revocation as "key superseded".
