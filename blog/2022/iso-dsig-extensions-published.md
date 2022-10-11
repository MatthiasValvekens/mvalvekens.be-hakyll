---
title: "ISO's new PDF signature extensions"
author: "Matthias Valvekens"
tags: [security, tech, pdf, pki]
published: 2022-10-11
license: "CC BY-SA 4.0"
---

---
link-bibliography: true
link-citations: true
references:
    - id: pdf2
      type: iso-iec-standard
      publisher: "ISO"
      number: 32000
      volume: 2
      title: "Document management — Portable document format — Part 2: PDF 2.0"
      citation-label: "ISO 32000-2"
      URL: "https://www.iso.org/standard/75839.html"
    - id: hash-extension
      type: iso-iec-standard
      publisher: "ISO"
      standard-type: "TS"
      number: 32001
      title: "Document management — Portable document format — Extensions to Hash Algorithm Support in ISO 32000-2 (PDF 2.0)"
      citation-label: "ISO/TS 32001"
      URL: "https://www.iso.org/standard/45874.html"
    - id: ecc-extension
      type: iso-iec-standard
      publisher: "ISO"
      standard-type: "TS"
      number: 32002
      title: "Document management — Portable document format — Extensions to Digital Signatures in ISO 32000-2 (PDF 2.0)"
      citation-label: "ISO/TS 32002"
      URL: "https://www.iso.org/standard/45875.html"
---


# Introduction

Two PDF 2.0 extensions that have been in the pipeline for a long time are now finally out there!

 - [@hash-extension] extends PDF to allow hash functions from the SHA-3 family to be used. This extension spec was published in September.
 - [@ecc-extension] clarifies ECDSA support in PDF and adds support for EdDSA (!) signatures. This one was published just yesterday.


Especially the addition of EdDSA is a pretty big deal, since [EdDSA has many advantages](https://ed25519.cr.yp.to/) over other popular signature algorithms.

So, how does this work? Read on.


# Why is an extension standard even necessary?


[@pdf2] explicitly enumerates all signature algorithms and digest functions (and combinations thereof) that are allowed in PDF 2.0. To cut a long story short: [@hash-extension] and [@ecc-extension] extend those lists. In fact, neither document proposes any significant changes to PDF-level structures. If so, why bother codifying them as ISO technical specifications?


There are a number of reasons, and they all have to do with interoperability. Here are some of the highlights.

 - In file formats where multiple algorithm choices are available, there needs to be a way to _announce_ the choices you made. [@hash-extension] and [@ecc-extension] tell you which object identifiers (OIDs) you should use when producing signatures with EdDSA or SHA-3 digest functions[^normref]. This allows recipients to detect systematically that, e.g., your Ed25519 signature is actually an Ed25519 signature instead of having to guess. Pretty crucial.
 - While [@pdf2] declares that ECDSA is allowed in PDF 2.0, it doesn't actually say which curves should be supported by all PDF 2.0 processors. [@ecc-extension] addresses that by explicitly including a list of common curves (and their OIDs) that should be recognised by all processors.
 - [@ecc-extension] also requires ECDSA public keys to specify their curves by name (i.e. OID). Passing curve parameters in-band is forbidden[^namedcurve]. The overwhelming majority of general-purpose ECDSA implementations already enforce this rule in some form or another.
 - Both extension documents also register themselves as PDF extensions, which allows implementers to explicitly declare the extensions they use in any given PDF document.


If you want to add support SHA-3 digests and EdDSA in your favourite piece of PDF software, _and_ you have access to cryptographic tooling that supports the relevant primitives, then all you have to do is write some bookkeeping code. In other words, you have no excuse[^fips], get to it already!


# Where can I try this out?

As it happens, [pyHanko](https://github.com/MatthiasValvekens/pyHanko) already implements the required changes. I've included a quick do-it-yourself demonstration below.

If you don't feel like going through the steps yourself, you can also [watch the demo as an asciicast](https://asciinema.org/a/527414).

<figure>
<script id="asciicast-527414" src="https://asciinema.org/a/527414.js" async></script>
<figcaption>
    EdDSA demo as an asciicast.
</figcaption>
</figure>


## Installation

Since [pyHanko is available through PyPI](https://pypi.org/project/pyHanko/), you can install it using `pip`[^aur]---optionally in a virtualenv.

```bash
pip install pyhanko>=0.15.0
```

Note that depending on settings, `pip` doesn't always install executables on the `PATH`. If so, prefix the commands below with `python -m`.


## Sample keys

Here is some sample key material[^genpkey] to play around with:

 - [An `ed25519` private key](/static/misc/eddsa-demo/signer.key.pem) (unencrypted/no passphrase)
 - [A certificate issued for said key](/static/misc/eddsa-demo/signer.cert.pem)
 - [A self-signed CA certificate for the issuer](/static/misc/eddsa-demo/ca.cert.pem)


## Creating and validating an Ed25519 signature

Once the key files are in place, grab any PDF file, and try creating a signature:

```bash
pyhanko sign addsig --field Signature \
    pemder --key signer.key.pem --cert signer.cert.pem \
    --chain ca.cert.pem --no-pass \
    input.pdf output.pdf
```

If all goes well, pyHanko should automatically figure out that it's dealing with an `ed25519` key, and proceed accordingly. You can validate the output as follows:

```bash
pyhanko sign validate --pretty-print --trust ca.cert.pem output.pdf
```

You should see this snippet in the output:

```
Integrity
---------
The signature is cryptographically sound.

The digest algorithm used was 'sha512'.
The signature mechanism used was 'ed25519'.
```

Moreover, if you open up the file in a PDF debugger such as [iText RUPS](https://github.com/itext/i7j-rups), you'll see that the [@ecc-extension] developer extension has been registered automatically as well.

![A file using ISO/TS 32002 opened in RUPS](/static/misc/eddsa-demo/rups.png)


# What's next?

Given that both of these extensions are very, very new, it'll probably take a while before you can expect broad support across the market. It's hard to say exactly how long that will take. On the one hand, several PDF software vendors participated in interoperability testing prior to publication, which bodes well for support in the PDF industry.

On the other hand, it'll probably take a lot longer before public CAs start routinely issuing document signing certificates for EdDSA keys.
Either way, they can't start doing that if they can't count on PDF implementations to support modern cryptography, so if you're a PDF developer, please do your part!


# Bibliography

::: {#refs}
:::

[^normref]: Technically, those requirements are simply "imported" from other standards (IETF RFCs and NIST publications) via normative references, but they're no less important.

[^namedcurve]: The astute reader will note that this is actually not a direct PDF file format rule or even a signature format rule, but actually a restriction on the _certificate_ used to identify the signer's public key. In practice, however, virtually all public CAs that issue ECDSA certificates for document signing already include this rule in their certificate profiles, since it appears in so many other standards. We included it in [@ecc-extension] to discourage insecure validation implementations.

[^fips]: Unless you are constrained by things like FIPS 140-2 compliance, in which case I feel sorry for you.

[^aur]: There's a [package for pyHanko in the AUR](https://aur.archlinux.org/packages/python-pyhanko) as well; the new version will probably get picked up in a week or so.

[^genpkey]: It's easy to generate test keys using the `openssl genpkey` command, provided that your flavour of `openssl` supports `ed25519`.
