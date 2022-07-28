---
title: "Intermezzo: MACs vs. signatures in PDF"
author: "Matthias Valvekens"
tags: [security, tech, pdf, digsig, pki]
published: 2022-07-24
license: "CC BY-SA 4.0"
---

---
link-citations: true
references:
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


::: info
This is an addendum to ["ISO 32004: an overview"](./about-iso32004.html) with some additional background information on how MACs differ from regular signatures, and what that implies for their usage in PDF.

It's best read in that context.
:::

# Why digital signatures are not a substitute for MACs

If you've dabbled in PDF-related development before, your first thought might be "Wait, why are we having this whole integrity protection conversation? Can't you just slap a digital signature on your encrypted files and call it a day?".

That's a very natural question to ask, and you certainly wouldn't be the first one to raise the point. That said, MACs and digital signatures really aren't interchangeable.
Some of the differences are intrinsic, others have more to do with how digital signatures are used in PDF documents and how PDF software processes them.


## Cryptographic differences

First, MACs and signatures are based on completely different cryptographic principles.

 - Digital signatures rely on asymmetric cryptography, with public-private key pairs. Creating a signature requires the private key, but the public key is sufficient for validation.
 - In the case of MACs, the sender and receiver use the same _shared_ secret to produce and to validate the MAC, respectively. From the cryptographic point of view, sender and receiver are indistinguishable.

When protecting an encrypted PDF document using a MAC, we want the MAC key and file encryption key to be computable from the password (+file data). With digital signatures, that's not possible: the key material is unrelated.
In fact, due to the way PDF encryption works, it's perfectly practical to add a signature form field to an encrypted document and sign it with any old key without even knowing the encryption key! Surely that doesn't qualify as "authenticated encryption"?

::: note
[@pdfmac] only deals with encrypted PDF documents, but generally speaking, there's actually nothing about MACs that makes them only applicable to encrypted data.
It's just that, when operating on encrypted PDFs, there's already a shared secret to work with, and that's convenient to bootstrap from.

There's no technological reason why one couldn't also define a password-based (?) integrity checking mechanism for _unencrypted_ PDFs. Whether that's a good idea is of course another question.
:::


## PKI is hard

Additionally, the asymmetric nature of digital signatures comes with a lot of added complexity:

 - The public key needs to be bound to a signer's identity, which requires the signer to be enrolled in some kind of public-key infrastructure (<abbr>PKI</abbr>) that the validator trusts.
 - Validation of certificates is nonstandard and murky by design. Validation results can depend on many external factors and change over time, due to certificates expiring, availability of validation information (or lack thereof), availability of secure timestamps, etc.
 - In most complex document signing workflows, signatures from multiple parties (signers, timestamping authorities, ...) are involved, often across multiple revisions of the same document. Some of these signatures can arguably even be non-authenticating[^dts]. The implications for signature validation are significant.

MACs are a lot simpler.

 - The secret is shared between all participating parties, thus obviating the need for binding the key to an identity before it can be trusted. The trust is established by the fact that all parties know the secret.
 - MAC validation is completely deterministic, again because there's no external PKI dependency.
 - MACs are especially well-suited to files with a large number of revisions. As long as all parties always check & recreate the MAC whenever they make a change[^mac-protocol], you only need to validate the outermost MAC. MACs don't distinguish between participants, which also serves as a huge simplification in this case.


[^dts]: Document timestamp signatures (see [@pdf2; 12.8.5]) would fall into that category. Those are a statement by a time stamping authority asserting that it witnessed a document at some point in time. That obviously has no bearing on the authenticity of the document's contents.

[^mac-protocol]: If this sounds like an unreasonable assumption, then I have bad news for you: regardless of which authentication scheme you choose, it's going to fall apart if you can't trust all (authorised!) parties involved to do their due diligence.


## PKI UX is even harder

It's easy to simply validate a MAC token when an encrypted document is opened, and throw up an error if there's a mismatch. On the other hand, with digital signatures, validation could fail for a lot of reasons[^mundane]. In addition, reasonable people can disagree on all sorts of policy questions related to signature validation. That complexity has several knock-on effects:

 - Presenting a signature validation result to the user involves a fair bit of nuance. The UI components used to convey that nuance are often (necessarily) pretty complicated. It's not a simple yes/no decision.
 - Announcement & validation of signatures are often postponed until after the document is open, and the user is already reading it. Chances are it's too late by then.
 - The vast majority of users don't understand how signature validation works, and will either panic when signature validation fails for a transient reason, or (more likely) have been conditioned to consider sporadic validation failures as "no big deal", and will accept basically anything.
 - Messing with system trust stores also opens up an attack vector that's just not an issue with MACs.

Given how long both file encryption and digital signatures have been around in the PDF ecosystem, it's safe to say that no vendor is going to adapt their viewer to perform signature validation _prior_ to displaying a document. Not that that'd change anything, though: users would still just dismiss all the warnings to see the file anyhow.

MACs do not require any trust assessments, and are therefore a lot more black-and-white.
When opening an encrypted document with a MAC, a MAC verification failure can simply[^not-that-simple] be treated as a decryption error.


[^mundane]: Those reasons can be very mundane, e.g. lack of network access.

[^not-that-simple]: In theory, that is... In practice, given that [@pdfmac] is not part of the "core" PDF spec, there are [compatibility issues to keep in mind](./about-iso32004.html#pitfalls).


# Signatures as just another kind of edit

In PDF-land, adding a signature to a document is a fairly invasive operation, and the various bits and pieces of metadata that come with it can impact how the signature should be understood in context.
Moreover, unlike most other digital signing applications, a digital signature in a PDF file can be visually part of the document content displayed on-screen!

In other words, signatures in PDF documents can have a significant semantic impact.
This, to me, is a strong argument in favour of considering signatures to be true _document modifications_ as opposed to mere _augmentations_.

Used properly, MAC tokens are semantically neutral, and therefore don't have any of those issues. What's more, [@pdfmac] MACs have the ability to also authenticate signature additions directly, just like any other edit.


# Different problems require different solutions

When working with encrypted documents, MACs are a simple solution to a simple problem. They allow you to answer the following question: "Did the last actor to modify this document know the encryption key?".

Digital signatures and PKI address something much more complicated and vague, ranging from questions like "Who authored/approved/countersigned this document?" over "When was this document known to exist?" to "On whose authority are all these assertions based?".

As such, it's not surprising that they require more baggage to process correctly.


-----

**Update** (2022-07-27)

Someone raised the following counterpoint to the above: what if one were to replace the MAC with a signature using a key pair for which the private key is known among all participants? The signature could simply use self-signed certificates (perhaps with some custom extensions). Wouldn't that amount to pretty much the same thing? If so, we don't have to adopt any new technologies.

I don't feel qualified to comment on whether one could provably recover the same (or roughly similar) security guarantees of a comparable MAC function with such a construction. It sounds plausible enough, so for the sake of the argument, let's assume that this "disguised MAC" idea is cryptographically sound.

Even so, I don't think the idea simplifies things in PDF applications. Here are a few reasons why:


 - You'd still need to find a way to derive the shared private key from the file encryption key (or save that shared key in an encrypted container of some kind).
 - "Regular" signature processors would get confused by these new signatures.
 - Signature algorithms are several orders of magnitude slower than MAC functions of a comparable bit strength (although probably not enough for it to matter in the big picture.
 - Either way, you wouldn't be able to repurpose anything but the most bare-bones parts of the validation logic. MAC validation ordinarily happens very early in the file opening process, so you'd have to rewire your application to deal with these special signatures before doing anything else. In the end, you'd just have to maintain two mostly separate digital signature validation modules.

Bottom line: MAC functions like HMAC are widely implemented and known to do very well in exactly this kind of use case, and insisting on using signatures would make things more confusing than they need to be.


-----


::: tldr
Oversimplifying slightly, I like to characterise the difference between MACs and signatures (in the PDF context) as follows.

 - Digital signatures are for **authenticating actors**, where it matters _precisely who_ did something.
 - MACs are an enforcement mechanism to restrict edits to **authorised actors**, i.e. those who know the key.

So, can MACs replace digital signatures? Absolutely not. But given the way signing is used in PDF, neither can digital signatures fully replace MACs. Both have their place.
:::


# Bibliography
::: {#refs}

:::
