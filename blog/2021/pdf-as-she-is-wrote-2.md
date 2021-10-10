---
title: "PDF As She Is Wrote #2: The quirks of PDF public-key encryption"
part-title: "The quirks of PDF public-key encryption"
tags: ["pdf-as-she-is-wrote", "tech", "pki", "documentation", "encryption"]
author: Matthias Valvekens
series: pdf-as-she-is-wrote
series-part: 2
author-url: "https://mvalvekens.be/about.html"
published: 2021-10-10
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
      citation-label: "ISO 32000-1:2008"
    - id: pdf2
      type: iso-iec-standard
      publisher: "ISO"
      number: 32000
      volume: 2
      title: "Document management — Portable document format — Part 2: PDF 2.0"
      citation-label: "ISO 32000-2:2020"
    - id: pdf-insecurity-enc
      type: paper-conference
      author: ["J. Müller", "F. Ising", "V. Mladenov", "C. Mainka", "S. Schinzel", "J. Schwenk"]
      issued: 2019
      isbn: 9781450367479
      publisher: Association for Computing Machinery
      publisher-place: New York, NY, USA
      container-title: "Proceedings of the 2019 ACM SIGSAC Conference on Computer and Communications Security (CCS '19)"
      editor: ["Lorenzo Cavallaro", "Johannes Kinder", "XiaoFeng Wang", "Jonathan Katz"]
      title: "Practical Decryption ExFiltration: Breaking PDF Encryption"
      page: "15--29"
      citation-label: "MIMMSS19"
    - id: csor
      type: online
      author: "National Institute of Standards and Technology (NIST)"
      title: "Computer Security Objects Register (CSOR)"
      url: "https://csrc.nist.gov/projects/computer-security-objects-register"
      accessed: "2021-10-09"
      citation-label: CSOR
---


# PDF encryption in general

The [previous instalment in this series](pdf-as-she-is-wrote-1.html) had to do with digital signing in PDF. This post is about another, lesser known application of PKI in PDF: using PKI to encrypt PDF files.
The idea is that this allows you to encrypt a PDF file with key material from one or more X.509 certificates, so that only the designated recipients can decrypt the document using their respective private keys.

This is different from password-based encryption, which relies on a _shared_ secret (i.e. the password) between the encrypting and decrypting passwords. It's also much less widely implemented---this is probably at least partly due to the fact that the specification isn't particularly clear on the details.

Of course, PDF encryption has lots of well-documented, more fundamental issues (see e.g. [@pdf-insecurity-enc]) and while I've [spoken about those](fosdem-talk.html) in the past, that's not what this post is about. I'm going to assume that you have a reason to implement PDF's public key security handler, and that you only want to know how to do so in an interoperable manner.


# The specification {#sec:spec-overview}

First things first: other than the adoption/deprecation of certain algorithms and crypt filters (which applies to PDF encryption in general), in addition to some terminological tweaks, there were no substantial changes to the public-key security handler between [@pdf17] and [@pdf2]. The normative text is in clause 7.6.4.3 of [@pdf17], or equivalently, clause 7.6.5.3 of [@pdf2].

The rough _idea_ of the specified encryption scheme is the following.

 1. The encrypting party generates a 20-byte seed. This seed is used to derive the actual file encryption key; see below. Together with some other information, it is embedded into an envelope.

 2. The encrypting party chooses a suitable (symmetric!) envelope encryption algorithm[^envelope-choices], generates an envelope key[^cek-terminology] (length depending on the algorithm choice)

 3. After encrypting the envelope with the algorithm and key from the previous step, the envelope key is encrypted with the public key of each recipient.

 4. The encrypted envelope, all the encrypted copies of the envelope keys and recipient certificates are embedded into the PDF file.


The standard also tells us that we should use CMS[^legacy-cms] (see [@cms]) to encode this information.
Let's try to unpack what all of that means.

[^cek-terminology]: In [@cms], the envelope key is called a "content encryption key", but since it's not actually used to encrypt any _document_ content in PDF, I'll continue to call it an envelope key.

[^envelope-choices]: The full list of available choices is in the specification, but in [@pdf2], the only non-deprecated option is 256-bit AES-CBC.

[^legacy-cms]: [@pdf17] still uses the legacy term [@pkcs7] instead of "CMS". They're largely interchangeable for the purposes of this discussion, but understanding the relationship between the two is key to resolving some of the ambiguities in the current text.


# Implementation

## Which CMS structures?

The standard doesn't literally spell out which kind of CMS object it requires to store the keying material, but from context and phrases like "decrypt the enveloped data in the CMS object", it's pretty clear that the intended content type is `EnvelopedData`.
In [@cms, § 6.1], `EnvelopedData` is defined like this:

```{.asn1 .number-lines}
EnvelopedData ::= SEQUENCE {
    version CMSVersion,
    originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
    recipientInfos RecipientInfos,
    encryptedContentInfo EncryptedContentInfo,
    unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
```

This structure is wrapped into a CMS `ContentInfo` with the OID specified in [@cms, § 6.1], as usual.
Now, what do all of these fields mean? Let's go through them, starting with the untagged ones.

You'll note that [@cms] specifies a procedure to deduce the appropriate value of the `version` field. In our case, the answer is simple: the version number is virtually always `0` in practice, since all higher-version features are essentially off-limits, for reasons that should become clear soon.
The next untagged field is `recipientInfos`, which---as the name implies---will hold per-recipient key material for the intended recipients of the document. Its value type is a set of `RecipientInfo` objects, viz.

```{.asn1 .number-lines}
RecipientInfo ::= SET OF RecipientInfo

RecipientInfo ::= CHOICE {
    ktri KeyTransRecipientInfo,
    kari [1] KeyAgreeRecipientInfo,
    kekri [2] KEKRecipientInfo,
    pwri [3] PasswordRecipientinfo,
    ori [4] OtherRecipientInfo }
```

Hmmm, a sum type. That means that we have to figure out which of the recipient info types is the proper one---and it's not listed in the standard! We'll get to that.

The next field is `EncryptedContentInfo`. This is the proverbial "digital envelope" we encountered in [the overview](#sec:spec-overview). Here's the type definition:

```{.asn1 .number-lines}
EncryptedContentInfo ::= SEQUENCE {
    contentType ContentType,
    contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
    encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL }

EncryptedContent ::= OCTET STRING

ContentEncryptionAlgorithmIdentifier ::= AlgorithmIdentifier
```

This data structure identifies the type of content that was encrypted, together with the associated (symmetric) envelope encryption algorithm.
The actual encrypted envelope is stored in the `encryptedContent` field[^ec-optional].

Let's discuss how all of these structures play together in a bottom-up way, starting with the envelope.

[^ec-optional]: The `encryptedContent` field is marked as optional, but in the current context, it's always present.


## Constructing the envelope

The plain-text content of the encrypted envelope is straightforwardly specified in [@pdf17] and [@pdf2]. The content type is `id-data`, and the contents are

 - 20 bytes of key derivation seed, and
 - 4 bytes of permission bits[^pbits-caveat]

[^pbits-caveat]: There's one exception: the permission bits are absent when you're constructing an envelope to be used in a non-default crypt filter, but chances are that you don't need to worry about that.

I won't go into detail on what these permission bits mean, the standard does a decent job of explaining that. We'll focus on how to _process_ this data.

If you're the encrypting party, you should apply the padding scheme in [@cms, § 6.3] to pad the payload to a multiple of 16 bytes, and then apply 256-bit AES-CBC with a 32-byte envelope key generated using a strong random number generator. All of that should be available in your favourite cryptography library.

The relevant algorithm OID for 256-bit AES-CBC is `2.16.840.1.101.3.4.1.42`, as listed in [@csor].

The encrypted envelope content goes into the `encryptedContent` field, wrapped up in an `OCTET STRING` value.

::: warning
There's one notable difference in AES-CBC usage compared to string & stream encryption in PDF.
In CMS, the initialisation vector (<abbr>IV</abbr>) for CBC is _not_ stored as part of the encrypted content!

Instead, the IV must be supplied in as an `OCTET STRING` in the `parameters` field of the `ContentEncryptionAlgorithmIdentifier`.
:::

While all other symmetric algorithm choices that were listed in [@pdf17] are deprecated in [@pdf2], many are still used in the wild. Depending on your use case, you might still need to process them.


## Encrypting the envelope key

The construction of the envelope is pretty well-specified, but when we get to encrypting the envelope key for each recipient, things become murky: which encryption schemes are actually supported?

### Which `RecipientInfo` to choose?

The first issue we face is figuring out which `RecipientInfo` types can be used, since [@pdf2] is silent on that point.
Here, a brief foray into the history of the relevant standards can provide some insight.
The quickest way in is perhaps to look at how [@pkcs7] defines `EnvelopedData`:

```{.asn1 .number-lines}
EnvelopedData ::= SEQUENCE {
     version Version,
     recipientInfos RecipientInfos,
     encryptedContentInfo EncryptedContentInfo }

RecipientInfos ::= SET OF RecipientInfo

RecipientInfo ::= SEQUENCE {
    version Version,
    issuerAndSerialNumber IssuerAndSerialNumber,
    keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
    encryptedKey EncryptedKey }
```

At first sight, that looks pretty weird: in CMS, `RecipientInfo` is a choice type, while [@pkcs7] defines it in a way that looks suspiciously like `KeyTransRecipientInfo` in [@cms, § 6.2.1]!

```{.asn1 .number-lines}
KeyTransRecipientInfo ::= SEQUENCE {
    version CMSVersion,
    rid RecipientIdentifier,
    keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
    encryptedKey EncryptedKey }
```

This is the magic of backwards/forwards-compatible ASN.1 tagging at work: since the `ktri` choice in the [@cms] version of `RecipientInfo` is untagged, legacy [@pkcs7] `RecipientInfo` values will be automagically interpreted as `KeyTransRecipientInfo` by a compliant CMS reader. The converse is true as well: CMS encoders can produce messages that legacy [@pkcs7] processors can still read, provided that they don't use any of the new features in [@cms].
By the way, the distinction between `RecipientIdentifier` and `IssuerAndSerialNumber` is exactly the same as for signer identifiers in `SignedData`, [which we discussed before](pdf-as-she-is-wrote-1.html).


In hindsight, the fact that [@pdf2] does not unambiguously state which recipient info types can be used in PDF may be a simple editorial oversight.
In [@pkcs7] there's no choice, but that nuance probably got lost in the editing process when the references to [@pkcs7] were updated to [@cms].


### On key transport

So, from history and context, it looks like the intention of the PDF standard is that `KeyTransportRecipientInfo` be used when encrypting documents with the public-key security handler.
Let's back up a little to explain what that means, and what the ramifications are.

Suppose that Alice wants to send a confidential message to Bob, and that she knows Bob's public key. The simplest method by which she could try to accomplish that would be to somehow directly encrypt the entire message with Bob's public key, but [that's a terrible idea](https://soatok.blog/2021/01/20/please-stop-encrypting-with-rsa-directly/), and not how you're supposed to use asymmetric crypto.
The next simplest thing she could do would be to encrypt the message using an appropriate symmetric cipher with a random key, encrypt the generated key with Bob's public key, and send the entire package (encrypted message + encrypted key) to Bob.

This is, in a nutshell, the kind of key transport scheme that `KeyTransportRecipientInfo` represents.
It's inherently non-interactive and asymmetric: Alice can do the message key generation process all by herself, Bob does not contribute. Moreover, Alice doesn't even need to generate a public/private key pair of her own for this to work.
From an implementation simplicity point of view, this makes it a convenient choice for "traditionally one-directional" encryption use cases, such as encryption of file backups.

However, this simplicity is superficial: for one, by virtue of its non-interactivity, simple key transport (along with naive key agreement schemes) cannot achieve any kind of [forward secrecy](https://en.wikipedia.org/wiki/Forward_secrecy). Moreover, the scheme does not authenticate Alice's message to Bob in any way, which is also problematic in most use cases where encryption is at all relevant.
So Alice would have to sign her message in addition to encrypting it, and at that point, the advantages provided by this sort of scheme become rather small.

There are other kinds of key agreement schemes (such as Diffie--Hellman, or its ECC-based variants) where information from key pairs of both parties are used in the derivation of the shared secret key.
These still work largely non-interactively, but the keying material for those can't be encoded in a `KeyTransportRecipientInfo` value[^katri]. They also still require out-of-band authentication for public keys (e.g. via X.509 certificates).
Either way, these schemes aren't currently explicitly supported in the PDF standard[^ecdh].

Moreover, even if they were supported, they still wouldn't solve the forward secrecy problem. To achieve perfect forward secrecy, _both_ parties need to _actively_ contribute to the shared key generation process. This is very natural for parties sending messages over an active communication channel (e.g. TLS), but there's no real precedent for that sort of thing in PDF.

[^ecdh]: Interestingly enough, some [vendor-specific implementations do exist](https://www.pdflib.com/pdf-knowledge-base/pdf-20/whats-missing-in-pdf-20/).

[^katri]: Instead, you'd use `KeyAgreeRecipientInfo` (see [@cms, § 6.2.2]).


### The actual public-key encryption process

As it turns out, the only widely used public-key cryptosystem that can be used with `KeyTransportRecipientInfo` is RSA. All the other mainstream options effectively require `KeyAgreeRecipientInfo`, which is off the table in "old-school" [@pkcs7].

That leaves the padding scheme question: can you use RSAES-OAEP in an encrypted PDF, or are you effectively forced to use legacy PKCS#1 v1.5 padding?

Unfortunately---assuming you need to interoperate with other implementations---it's the latter: the specification is completely silent on this issue, but if you need your PDFs to be readable by off-the-shelf tools, PKCS#1 v1.5 is effectively the only thing that's supported in the wild (despite RSAES-OAEP having been around for over two decades!).


### Summary

If you're encrypting files using public-key cryptography, and interoperability with "generic" PDF processors is a requirement---i.e. you need to cater to the lowest common denominator of all PDF tools that support the "standard" public-key security handler---then here's what you need to do:

 - Stick to features that were available in [@pkcs7]: `KeyTransportRecipientInfo` for your `RecipientInfo` objects, and `IssuerAndSerialNumber` for the recipient identifiers.
 - You'll have to use RSA[^cert-constraint] with the legacy PKCS#1 v1.5 padding scheme to encrypt your envelope keys.
 - Encrypt the envelope itself with 256-bit AES in CBC mode---it's the least bad option among the ones available in the specification.

If your interoperability requirements aren't as strict, you might be able to substitute at least a couple of those for more modern/robust options. But then again, if interoperability is not a concern, PDF's "native" encryption provides little to no added value.


[^cert-constraint]: Note that this also implies that---with current standards in PDF-land---you can't asymmetrically encrypt PDFs for recipients using non-RSA key pairs.

## Key derivation

The procedure by which the file encryption key is derived from the 20-byte seed value is largely well-defined in the standard, but there are some sharp edges. These are relevant in case you need to be able to decrypt documents encrypted with legacy versions of the standard public-key security handler (_adbe.pkcs7.s3_ and _adbe.pkcs7.s4_), or if you're applying public-key encryption to non-default crypt filters (e.g. for embedded files).
See [this GitHub issue](https://github.com/pdf-association/pdf-issues/issues/25) for some useful background information.

# Conclusion

::: tldr
If you really, really have to support asymmetric cryptography using the public-key security handler in the PDF standard, you're effectively constrained to using RSA with PKCS#1 v1.5 padding to encrypt your envelope keys, and to the subset of CMS ([@cms]) that was already available in [@pkcs7].
:::


# Bibliography
::: {#refs}

:::
