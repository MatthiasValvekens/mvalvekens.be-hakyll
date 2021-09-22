---
title: "PDF As She Is Wrote #1: How are signers identified?"
tags: ["pdf-as-she-is-wrote", "tech", "pki", "digsig", "documentation"]
author: Matthias Valvekens
published: 2021-09-22
license: "CC BY-SA 4.0"
---

---
link-citations: true
references:
    - id: cms
      author: "Internet Engineering Task Force (IETF)"
      editor: "R. Housley"
      title: "RFC 5652: Cryptographic Message Syntax (CMS)"
      issued: 2009
      url: "https://tools.ietf.org/html/rfc5652.html"
      citation-label: "RFC 5652"
    - id: cades-rfc
      type: report
      author: "Internet Engineering Task Force (IETF)"
      editor: ["D. Pinkas", "N. Pope", "J. Ross"]
      number: "RFC 5126"
      title: "CMS Advanced Electronic Signatures (CAdES)"
      issued: 2008
      url: "https://tools.ietf.org/html/rfc5126.html"
      citation-label: "RFC 5126"
    - id: cades-etsi
      type: report
      author: ETSI
      number: "ETSI TS 101 733"
      title: "Electronic Signatures and Infrastructures (ESI); CMS Advanced Electronic Signatures (CAdES)"
      issued: 2013
      edition: V2.2.1
      citation-label: "ETSI TS 101 733"
    - id: pkix
      type: report
      author: "Internet Engineering Task Force (IETF)"
      editor: ["D. Cooper", "S. Santesson", "S. Farrell", "S. Boeyen", "R. Housley", "W. Polk"]
      title: "Internet X.509 Public Key Infrastructure Certificate and Certificate Revocation List (CRL) Profile"
      issued: 2008
      url: "https://tools.ietf.org/html/rfc5280.html"
      number: "RFC 5280"
      citation-label: "RFC 5280"
    - id: pkcs7
      type: report
      author: "Internet Engineering Task Force (IETF)"
      editor: "B. Kaliski"
      edition: "Version 1.5"
      issued: 1998
      title: "PKCS #7: Cryptographic Message Syntax"
      url: "https://tools.ietf.org/html/rfc2315.html"
      citation-label: "PKCS #7"
      number: "RFC 2315"
    - id: pdf17
      type: iso-iec-standard
      publisher: "ISO"
      number: 32000
      volume: 1
      title: "Document management — Portable document format — Part 1: PDF 1.7"
      citation-label: "ISO 32000-1"
    - id: pdf2
      type: iso-iec-standard
      publisher: "ISO"
      number: 32000
      volume: 2
      title: "Document management — Portable document format — Part 2: PDF 2.0"
      citation-label: "ISO 32000-2"
---


# Series introduction

This is the first installment in _PDF as She Is Wrote_ [^nameref]. With this post, I intend to start a series on the way the PDF standards (mainly ISO 32000-x) are implemented in the "real world".
The focus is on implementation behaviour, not the minutiae of what the standard says (or should say), although I'll occasionally comment on that as well. Given my area of interest, a significant portion of the content will relate to digital signing in some way.

There's no set publication schedule, I'll write things down as I think of them.


[^nameref]: For the uninitiated: The series title _PDF as She Is Wrote_ is a nod to [this magnificent piece of history](https://en.wikipedia.org/wiki/English_as_She_Is_Spoke).



# Signer identification in CMS


People who've worked with me will undoubtedly know that I'm a zealous advocate for the Cryptographic Message Syntax (<abbr>CMS</abbr>), defined in [@cms].
CMS is the spiritual successor to [@pkcs7], and is sometimes still referred to as such.
The `SignedData` type in CMS is particularly popular in all sorts of digital signing schemes, where it's used as a vehicle to encode a "raw" signature together with the relevant metadata necessary to understand it.
PDF is part of the CMS club too: virtually everyone uses CMS `SignedData` objects to encode signatures in PDF documents nowadays[^alternatives].

[^alternatives]: This wasn't always the case, but all non-CMS signature encodings were deprecated in ISO 32000-2.

But let's not get ahead of ourselves too much: we're still talking about CMS. The design philosophy of CMS (as interpreted by yours truly---don't put too much stock in that) is one of _maximal extensibility_: the CMS specification itself makes relatively few assumptions about what people put in. As far as `SignedData` is concerned, the specification only prescribes how to correctly encode `SignedData` objects, and sets out some basic validation rules.
The idea is that other standards can then profile CMS to obtain a subset that's more appropriate for their use cases.

One of the things that CMS does _not_ do (by default) is to pin the signature to a specific certificate.
This is actually a fairly common misconception. From a mathematical point of view, the signature validation procedure doesn't really care about _certificates_, it only needs to know the signer's _public key_.
There can be more than one certificate made out to the same public key, for any number of reasons.
The certificate is only relevant to verify the binding between a key and its owner, which is a different problem entirely. However, it's important to note that some _profiles_ of CMS _do_ pin the signature to a specific certificate[^cades-pinning], but that mechanism is beyond the scope of this post.

[^cades-pinning]: CAdES (see [@cades-rfc; @cades-etsi]) requires either the `ESS-signing-certificate` or the `ESS-signing-certificate-v2` attribute to be part of the signature's signed attributes, thus binding it to one particular signer's certificate.

So, how _does_ CMS identify signers? To answer that question, we need to take a look at some definitions from  [@cms].

```{.asn1 .number-lines}
SignedData ::= SEQUENCE {
       version CMSVersion,
       digestAlgorithms DigestAlgorithmIdentifiers,
       encapContentInfo EncapsulatedContentInfo,
       certificates [0] IMPLICIT CertificateSet OPTIONAL,
       crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
       signerInfos SignerInfos }

SignerInfo ::= SEQUENCE {
       version CMSVersion,
       sid SignerIdentifier,
       digestAlgorithm DigestAlgorithmIdentifier,
       signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
       signatureAlgorithm SignatureAlgorithmIdentifier,
       signature SignatureValue,
       unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL }

SignerIdentifier ::= CHOICE {
       issuerAndSerialNumber IssuerAndSerialNumber,
       subjectKeyIdentifier [0] SubjectKeyIdentifier }
```

The relevant definition is the one for the `SignerIdentifier` type. This is a union type between `IssuerAndSerialNumber` and `SubjectKeyIdentifier`.
Essentially, this is saying that we can either identify the signer by means of an `IssuerAndSerialNumber` value, or a `SubjectKeyIdentifier` value. We'll talk about what each of these mean in a minute, but the way they're used by the validator is more or less the same in either case: the validator searches its certificate store until it finds a certificate matching the identifier, and then tries to validate the signature against the public key found in that certificate[^theresmore].
The `certificates` entry would typically contain all signer's certificates (among other potentially relevant ones).


::: note
It's important to observe that the `sid` field is not actually part of the portion of the payload that's cryptographically signed. This allows for some flexibility, but is also potentially problematic in many cases. This is a large part of the _raison d'être_ for attributes like `ESS-signing-certificate`.
:::

[^theresmore]: Obviously, there's more to it than that, but that's a story for another time.


## The `IssuerAndSerialNumber` way

`IssuerAndSerialNumber` is defined like this:

```{.asn1 .number-lines}
IssuerAndSerialNumber ::= SEQUENCE {
       issuer Name,
       serialNumber CertificateSerialNumber }

CertificateSerialNumber ::= INTEGER
```

The meaning of `IssuerAndSerialNumber` as an identifier is very straightforward: it tells the validator who the issuer of the relevant certificate is, and also mentions the certificate's serial number. Since best practices dictate that a given CA should never issue more than one certificate with a given serial, this should uniquely identify the certificate.


## The `SubjectKeyIdentifier` way

If you take a look at [@cms], you'll notice that the definition of `SubjectKeyIdentifier` is simply this:

```{.asn1 .number-lines}
SubjectKeyIdentifier ::= OCTET STRING
```

This doesn't say all that much. In an X.509 context, this value is intended to be compared against the value of a certificate's subject key identifier (SKI) extension[^ski-optional].
It's wrong to expect this value to be generated by any particular algorithm, but they're generally derived from the public key by a hashing procedure. See [@pkix, § 4.2.1.2] for examples.

The advantages of this approach are twofold:

 - There's no expectation of global uniqueness for subject key identifiers, so it becomes possible to have a signature validate against more than one signer's certificate. Imagine a scenario where a signer doesn't know ahead of time what certificate will be used to verify their identity.

 - It (theoretically) enables the use of CMS-based signatures together with non-X.509 certificates, in particular in a context where there's no `Name` notion around.


[^ski-optional]: In the PKIX profile defined in [@pkix], the SKI extension is optional for end-entity (i.e. non-CA) certificates. Hence, this way of identifying signers isn't universally applicable.


# What about PDF?


## The reality

The CMS specification requires validators to implement support for both alternatives (see [@cms, § 5.3]). This requirement has been part of CMS since 2002, and since both [@pdf17] and [@pdf2] normatively cite CMS for signature generation, it would seem logical for PDF signature validators to support both alternatives.

However, that's not what we see in the wild: the vast majority of implementations in major PDF processors only support identifying the signer by issuer and serial number. If interoperability is a concern, you're therefore better off generating your signatures with an `IssuerAndSerialNumber` in the `sid` field.


## Some historical speculation

PDF had support for digital signatures long before it became an ISO standard, and in those times, [@pkcs7] (the predecessor to CMS) was more widely known. In [@pkcs7], the approach based on `IssuerAndSerialNumber` was the only available choice.

The current CMS definition still shows some hints of this history, as indicated by the fact that the `subjectKeyIdentifier` in the definition below has a context-specific tag of `0`, while the `issuerAndSerialNumber` field is universally tagged.

```{.asn1 .number-lines}
SignerIdentifier ::= CHOICE {
       issuerAndSerialNumber IssuerAndSerialNumber,
       subjectKeyIdentifier [0] SubjectKeyIdentifier }
```

This tagging choice ensures compatibility with [@pkcs7] in both directions, as long as the signer makes sure to identify themselves using the `IssuerAndSerialNumber` option.

::: tldr
If you care about interoperability with other PDF processors as a signer, stick to `IssuerAndSerialNumber` in your PDF signatures. If you're implementing a validator, support both `IssuerAndSerialNumber` and `SubjectKeyIdentifier`.
:::


# Bibliography
::: {#refs}
:::
