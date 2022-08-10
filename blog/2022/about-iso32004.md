---
title: "ISO 32004: an overview"
author: "Matthias Valvekens"
tags: [security, tech, pdf]
published: 2022-07-24
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
      url: "https://datatracker.ietf.org/doc/html/rfc5652.html"
      citation-label: "RFC 5652"
    - id: pdf2
      type: iso-iec-standard
      publisher: "ISO"
      number: 32000
      volume: 2
      title: "Document management — Portable document format — Part 2: PDF 2.0"
      citation-label: "ISO 32000-2:2020"
    - id: pdfmac
      type: iso-iec-standard
      publisher: "ISO"
      standard-type: "DTS"
      number: 32004
      title: "Document management — Portable document format — Integrity protection in encrypted documents in PDF 2.0"
      citation-label: "ISO/DTS 32004"
---


# Introduction

For a few years now, the PDF industry has been working on a new mechanism to improve integrity protection in encrypted documents. [ISO/TC 171/SC 2](https://www.iso.org/committee/53674.html), the committee that manages the PDF standard, took ownership of that effort in the form of the [ISO 32004 project](https://www.iso.org/standard/45877.html).

A few months ago, that project moved into the DTS stage&mdash;the final step prior to publication! Now's a great time to go over which problems ISO 32004 sets out to solve, and perhaps more importantly, which problems it _doesn't_ solve.
Since I've been leading the project for the past year, I figured that I was in a good position to provide that write-up.

The goal of this post is not to explain what [@pdfmac] says exactly, nor to tell you how you should implement it at the technical level. Rather, I want to help other PDF developers understand the context of the document so they can make better use of it.


# Motivation


## Life in PDF-land without integrity protection

A few years ago, a team of security researches at the Ruhr University Bochum described [a number of attacks](https://pdf-insecurity.org/encryption/encryption.html) against PDF's encryption feature set.

Broadly, the attacks announced fall into two categories.

 - _data exfiltration_ tricks allowing an attacker to smuggle data out of an authorised user's working environment;
 - _content manipulation_ exploits that allow encrypted PDF files to be changed without knowledge of the encryption key, and without downstream users noticing.

The former category is more properly classified as exploiting the behaviour of specific viewers, and that's not what this post is about. However, the latter set of attacks really point to issues with the specification itself. Here are some of the highlights.

First up, we have cryptographic malleability issues.

 - As of [@pdf2], PDF supports RC4 (legacy) and AES-CBC for content encryption out of the box. Both of these schemes are unauthenticated, i.e. don't allow the decrypter to determine cryptographically whether a ciphertext has been tampered with. This is already problematic, but in the case of PDF, relatively difficult to exploit directly; it requires a little extra help.
 - Unfortunately, that extra help is provided by a little piece of ciphertext with a known plaintext required by [@pdf2]'s most recent security handler. That actually allows for _systematic_ injection of attacker-controlled content. If you're interested, give the [RUB team's explanation of CBC malleability](https://pdf-insecurity.org/encryption/cbc-malleability.html) a read.

Moreover, there are also fundamental ways in which the _design_ of the PDF standard's encryption features leaves documents wide open to manipulation, irrespective of any cryptographic issues.

 - In PDF, only streams and strings are encrypted. All other objects can be manipulated freely.
 - A PDF file's cross-reference data, which acts as a "roadmap" of sorts for the document, is never encrypted, and can be manipulated to do all sorts of crazy things.
 - Did I mention that you can also straight up _delete_ encrypted objects with impunity?
 - Even better: [@pdf2] allows "local" encryption overrides through the *Identity* crypt filter. This feature was intended to allow things like file metadata to remain unencrypted if desired, but actually allows entire streams, pages, images, etc. to be replaced with attacker-controlled ones as long as they declare themselves as unencrypted. Oops.

So, the upshot is this: given a legitimate encrypted PDF document, a sufficiently clever attacker can make the document say whatever they want without knowing the key. If an unsuspecting user then opens the document in their favourite viewer, they'll get a password prompt&mdash;as they probably expected. When the correct password is entered, the viewer will dutifully display the attacker-controlled content, and our poor user is none the wiser.

This is a problem, because in the mind of a business user&mdash;and the same probably goes for most tech people!&mdash;something being "password-protected" is pretty much synonymous with "kept under lock and key". People expect password-protected data to be _totally inaccessible and untouchable_ without knowledge of the password.
This expectation is being subverted by PDF's approach to encrypting documents.
Clearly, something had to be done...


## Authenticated encryption

The mismatch between users' (by no means unreasonable) expectations and reality is due to conflation of the following two security properties:

 * Data _confidentiality_ means that the data can't be _read_ without the key.
 * Data _authentication_ is necessary to ensure that data can't be _altered_ without knowing the key.

Historically, the authentication aspect was not a concern in the design of PDF's encryption features. That point of view is now very dated[^dated], but the benefit of hindsight doesn't really help us solve the issue at hand. So, what _can_ we do?

In modern cryptographic practice, the authentication problem is typically addressed by augmenting the ciphertext with a Message Authentication Code (<abbr>MAC</abbr>).
A MAC is a kind of keyed[^mac-key] digest, computed over the ciphertext. Tampering with the ciphertext invalidates the MAC, and to recompute the MAC, you need access to the key.
The idea is that the receiving party independently computes the MAC over the given ciphertext prior to decryption, and rejects the message if the result doesn't match the value supplied by the sender.

::: warning
It's crucial here that validating a MAC requires being able to _recreate_ it. Both operations require access to the _same_ key. There's no public/private key distinction. All participants in the process have exactly the same capabilities when it comes to producing and verifying MACs.
:::


There are many different ways to construct MACs. Here are some examples:

 - [**HMAC**](https://en.wikipedia.org/wiki/HMAC) is a family of MACs constructed from a hash algorithm (e.g. HMAC-SHA256). The cryptographic strength of the resulting MAC depends on that of the underlying hash function. HMAC is very widely used, and has also been adopted by [@pdfmac].
 - [**Poly1305**](https://en.wikipedia.org/wiki/Poly1305) is a MAC scheme that essentially works by considering the message as a polynomial over a finite field (the field of order 2<sup>130</sup> - 5, incidentally), and evaluating that polynomial at the key. It's commonly used in conjunction with ChaCha, including in TLS.
 - [**CBC-MAC**](https://en.wikipedia.org/wiki/CBC-MAC) is a (somewhat dated) MAC scheme that works by keeping the last block of a CBC encryption operation as a MAC. It used to be popular before HMAC gained widespread adoption, but is less common now.
 - [**GMAC**](https://en.wikipedia.org/wiki/Galois/Counter_Mode) is the MAC scheme that lies at the basis of the AES-GCM authenticated encryption algorithm.
 - [**KMAC**](https://en.wikipedia.org/wiki/SHA-3) is a more recent MAC scheme that one gets more or less "for free" from the fact that the design of SHA-3 (Keccak) is resistant against length extension attacks.


Most authenticated encryption schemes in common use&mdash;including AES-GCM, AES-CCM, ChaChaPoly1305 and many others&mdash;are constructed by combining an encryption primitive with a MAC function[^ocb].

So, having digested all that information, it seems that all we have to do is to apply a MAC to our encrypted data. Now, let's figure out how that's supposed to work in a PDF document.

::: info
Before we discuss how to make MACs work in PDF documents, a little detour might be in order...
This would be a good time to go read ["Intermezzo: MACs vs. signatures in PDF"][macs-digsig], the companion post to this piece that explains why the distinction between MACs and digital signatures is relevant.

It's not a strict prerequisite for understanding the rest of this article, but it's useful background info.
:::

[macs-digsig]: ./about-macs-vs-signatures.html


[^dated]: The technology and standards to include this kind of integrity protection have also been around for a long time. For example, the research around HMAC dates from the mid-90s, and the [IETF first standardised it](https://datatracker.ietf.org/doc/html/rfc2104) in '97. PDF 2.0, which included a major restructuring of the file encryption functionality in PDF, first saw the light of day _two decades later_.

[^mac-key]: The MAC key and the encryption key are usually derived from a common piece of secret data that is shared between the communicating parties.

[^ocb]: [AES-OCB](https://datatracker.ietf.org/doc/html/rfc7253) is a notable exception; here, the computation of the authentication tag is tightly integrated with the encryption process.



# Design philosophy {#design-philosophy}

## General design

The initial requirements of the PDF MAC project were more or less the following.

 - Find a way to protect entire (encrypted) PDF files using a MAC.
 - Leverage the shared secret behind the encryption (usually a password) to provide keying material[^keymat] for the MAC process.
 - Make sure the system is backwards compatible: files with MACs should remain readable to non-MAC-aware processors.
 - At the same time, there needs to be a degree of protection against attackers _removing_ MACs.

The MAC scheme standardised in [@pdfmac] integrates into PDF in much the same way as digital signatures.

 - There's a **ByteRange** to indicate the covered portion of the document.
 - The actual MAC token is stored in the "hole" left by the declared **ByteRange**.
 - Like signatures, MACs are embedded using CMS&mdash;although we use `AuthenticatedData` instead of `SignedData` for obvious reasons (see [@cms]).


![This is what an [@pdfmac] MAC looks like in context. The colour coding indicates the parts of the covered byte range.](/static/images/iso32004-structure.svg)


## Compatibility with digital signatures

This **ByteRange**-based approach works well enough, but there's a snag: any given revision of a PDF file can only have a single "complete" **ByteRange**! Early drafts of [@pdfmac] solved this problem by decreeing that MACs could only be used in unsigned documents.

Since MACs and digital signatures [serve very different purposes][macs-digsig], that incompatibility didn't sit well with me. Especially since there's an easy, backwards-compatible solution! PDF signatures use CMS `SignedData`, which supports attributes, so we could simply let the MAC token "hitch a ride" on the signature.
That way we only need a single **ByteRange** to make both the signature and the MAC work[^digsig-mac-combo]. The structure of the MAC token is otherwise pretty much the same as in the unsigned case.

While signatures in encrypted documents aren't a very common sight, we occasionally come across signed encrypted documents. By allowing MACs in any encrypted document, we can achieve (more or less) the same integrity guarantees for _all_ such documents.
This uniformity also benefits validation: a MAC checker with zero signature validation capabilities shouldn't have to make judgment calls about whether documents with a signature (and no MAC) are adequately protected.
In addition, the fix was simple enough that sacrificing compatibility wasn't worth it. After some discussion, we decided to put it in the spec.

::: note
For an example where the separation of concerns between MACs and signatures is even more clear: a document timestamp signature ordinarily has no authenticating value. Timestamping servers don't care about what they sign, and it's possible to add a timestamp to an encrypted document without knowing the key. In other words, there's no accountability _at all_.

Adding a [@pdfmac] MAC token as an (unsigned) attribute on the signature is a way to solve that problem.
:::

## Compatibility/security trade-off

In the PDF world, backwards compatibility is a big deal. When new functionality is considered for standardisation, one of the most important criteria involves evaluating how existing software would cope with the change.

This was no different for [@pdfmac]: a document with a MAC still needs to be understood by software that doesn't know how MACs work.
That, in itself, is a good thing.

The _converse_ problem is more tricky, though: how can a MAC-aware processor tell the difference between a "legacy" document without a MAC, and a document from which the MAC has been (maliciously) stripped?[^mac-incr-update]
Paranoid implementations could perhaps enforce MACs rigorously, but that might not be feasible for everyone.

To address this concern, [@pdfmac] defines an extra permission bit to indicate whether a MAC is expected to be present. Since the permission bits are already protected by a "pseudo-MAC" of sorts[^perms] in PDF 2.0, there's a degree of tamper-resistance built in.


[^keymat]: To be pedantic: the MAC key in itself actually isn't derived from the password or the file encryption key in [@pdfmac]. Rather, it's encrypted using a key derived from the file encryption key, following a common pattern used with CMS `AuthenticatedData` and `EncryptedData`.

[^digsig-mac-combo]: In a signed revision of a PDF document, the MAC token is actually computed over the digest of the byte range together with a digest of the signature. As such, it protects both the signature and the document content.

[^mac-incr-update]: The analogous problem for incremental updates by non-MAC-aware processors is left as an exercise to the reader. Alternatively, if you're not in the mood, feel free to [peek ahead](#coverage){title="Link to section on coverage checks"}.

[^perms]: The "pseudo-MAC" being a separate entry computed by encrypting the permission bits using AES-ECB (!). Perhaps ironically, this piece of known AES plaintext was a large part of why we needed [@pdfmac] in the first place...


# Pitfalls and judgment calls {#pitfalls}

## User passwords

PDF's standard security handler distinguishes between "user passwords" and "owner passwords".
It's somewhat common for people to apply encryption to a PDF document, but leave the user password empty, while still setting the owner password to something else. This is the digital equivalent of a "No Trespassing" sign on an unguarded fence. Sure, bona fide viewers will enforce permission bits if the owner password is not supplied, but nonetheless, _anyone can compute the file encryption key_ if the user password is left empty.

In other words, from a purely technical perspective, anyone can decrypt and modify the document content. This is no different in a scenario where MACs are used: if the user password is empty, anyone can validate, **but also regenerate the MAC**. In other words, **a MAC offers precisely zero protection in this situation**.

::: warning
Remember: a MAC that anyone can verify is a MAC that anyone can forge.
MACs are based on shared secrets. They're most useful if the relationship between the parties involved in a workflow is symmetric (i.e. everyone has the same access level).
:::

In fact, if I'd write some piece of software that required MACs for everything, I'd actively reject PDF files with empty user passwords.



## Coverage checks {#coverage}

A MAC should cover the entire document to which it is applied (other than the MAC container itself). As with digital signatures, coverage is indicated by the associated **ByteRange**. When receiving a document with a MAC, it's very important to check the **ByteRange**: if the covered region is too small, unauthorised changes could still lurk in the "unprotected" regions. Processors incrementally updating a document are also expected to update the MAC (including the coverage range).

This presents a problem: what to do when one receives a document with a "stale" MAC (i.e. a MAC that doesn't cover the full document anymore). That could be the result of a malicious edit, but also due to an authorised change by a tool that doesn't implement [@pdfmac].

Again, this one boils down to choosing compatibility vs. security, and there's no easy answer. Personally, I would write my implementation to reject such documents by default.


## On by default: yes or no?

Cryptographically, speaking, adding MACs to all[^pdf2-enc] encrypted PDF documents seems like the obvious thing to do. Ultimately, the security of the MAC process is protected by the same shared secret as the document encryption, so there's no need to involve external keying material (as would've been the case with signatures). In particular, the user doesn't necessarily need to do anything special to benefit from MACs.

That said, MACs do come with an I/O performance cost. As with signatures, the PDF data needs to be serialised to a place where the MAC can be inserted in-place later.
Depending on application architecture, that cost might be completely negligible, or prohibitive[^advanced-io].
I'd still recommend turning it on by default if you can&mdash;especially when updating files that already have MACs!


::: warning
If you're using MACs, don't forget to set the file permissions accordingly. The existing tamper protection on those is the only thing stopping an attacker from stripping the MAC and pretending the result is still a valid encrypted PDF 2.0 document.
:::



[^pdf2-enc]: Strictly speaking, all encrypted _PDF 2.0_ documents, since [@pdfmac] is an extension of [@pdf2].

[^advanced-io]: There are ways around that, though. In unsigned revisions, [@pdfmac] forces the MAC container to reside in (a subdictionary of) the document trailer, which means that it's somewhere near the end of the file. That can be leveraged to generate MACs very efficiently, even on large documents that don't fit into memory. Retrofitting that onto a legacy codebase is of course easier said than done.


# Conclusion

::: tldr
[@pdfmac] provides you with a MAC-based tool to protect your encrypted PDF files from malicious tampering. The MAC is bootstrapped from the same shared secret as the encryption uses.

Additionally, [@pdfmac] is fully backwards compatible and can be used in conjunction with all PDF 2.0 features, including digital signatures.

Things to keep in mind:

 - Think carefully about whether using MACs and/or digital signatures makes sense for your workflow. They're very different things.
 - Backwards compatibility comes with some security trade-offs. Keep those trade-offs in mind.
 - Validation for MACs is a lot more straightforward than for signatures.

Oh, and if you're keen to implement [@pdfmac] yourself once it's out, please give Annex B a proper read for some extra info on what to look out for when validating MACs in PDF documents.
:::


# Bibliography
::: {#refs}

:::
