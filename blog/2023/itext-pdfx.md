---
title: "Generating print-ready (PDF/X) output with iText 7"
author: "Matthias Valvekens"
tags: [pdf, itext, java]
published: 2023-01-15
license: "CC BY-SA 4.0 hybrid"
---


# Background

The impetus for this post is simple: I recently used an online printing service to place an order for paper cards to announce the birth of our newborn son. Since I planned to put together the card design using iText anyhow, I figured I might as well submit in PDF/X. After all, I would've been charged extra if the printing company had to fix my files!

Below, I'll try to explain how the process went.


::: warning
While I know a lot about PDF, I'm far from a prepress expert, so take my statements about PDF/X with a grain of salt.
:::


# About PDF/X

PDF/X is a subset (or rather, a collection of subsets) of PDF that is specifically geared for use in prepress workflows. The idea is that one should be able to print a file conforming to (some flavour of) PDF/X with minimal human intervention, in a consistent manner across different presses and/or printing platforms.

This is especially useful in situations where job submission is highly automated with little room for human support overhead, e.g. modest-volume jobs submitted through a public web portal, like my announcement card order.

Shouldn't this work with pretty much any PDF? Unfortunately not: PDF allows certain liberties that aren't compatible with this idea. Without going into too much detail, the various PDF/X subset standards outline

 - which features of PDF can be used in the given PDF/X profile, and
 - how to declare conformance with a particular PDF/X flavour.

In that way, it's a lot like PDF/A, but the focus is on prepress rather than archival use cases. For a nice overview of how PDF/X evolved over the years, make sure to give [this excellent article by Martin Bailey](https://www.pdfa.org/the-route-to-pdf-x-and-where-we-are-now-a-personal-history/) a read.


Also unlike PDF/A, iText does not come with "batteries included" PDF/X support[^pdfx-itext5]---but that's not actually as big an obstacle as it seems.

[^pdfx-itext5]: iText 5 used to offer built-in support for PDF/X-1a and PDF/X-3, but that feature wasn't carried over into iText 7.


Since the company that printed my cards did not advertise support for PDF 2.0, I went with PDF/X-4, which is based on PDF 1.6. As such, some bias towards X-4 is to be expected here.


# PDF/X restrictions

## Document content

Preparing a PDF/X document comes with certain restrictions. The details depend on exactly which flavour of PDF/X you need, but broadly speaking, the following apply.


::: warning
Obviously, this list is far from exhaustive, and it's definitely not a substitute for actually reading the standard. Also, while iText will attempt to detect problems when producing PDF/A output, no such auto-detection is available for PDF/X in iText 7.
:::

### Colour management

Like PDF/A documents, all PDF/X documents need an output intent describing the intended printing condition & colour profile. This output intent maps the characteristics of the intended output device's colour space into a standardised colour space. Often, the target printing condition will be a CMYK condition.

One also needs to be careful with PDF graphics objects:

 - The colouring information for all printed graphics needs to be compatible with the output intent. This means that, for example, you can't use RGB colours if the PDF/X output intent is a CMYK one. Using **DeviceGray** with a CMYK output intent is allowed, though.
 - In PDF/X-1a, only CMYK (+spot colours) is supported.
 - For those use cases that require it, it is also possible to use device-independent colours (from PDF/X-3 onwards). That's beyond the scope of this post.

In my use case, I was happy to stick with plain old device CMYK colours. I downloaded the correct colour profile from the printing company's website, loaded it into an **OutputIntent**, and made sure to set up my colours with `new DeviceCmyk(...)`.


::: note
Spot colours are also supported in all flavours of PDF/X, but those require special attention.
:::


### Bounding boxes

The various bounding boxes of your document's pages must fit inside one another in a reasonable way. This is the required order, from small to big.


 - The **TrimBox** (or, less commonly, **ArtBox**[^prefer-trimbox]) is the bounding box of the "final product", after cutting the printed paper down to size. This one must be present in all PDF/X documents.
 - The **BleedBox** is optional, and can extend beyond the **TrimBox** to allow graphics to extend all the way to the edge of the print medium.
 - The **CropBox** is optional, and delimits the area shown on the screen when the document is viewed using a PDF viewer.
 - The **MediaBox** is required and should encompass all page content.

Each box in this list must be contained within the next, if present.

It can be useful to make the **CropBox** larger than the bleed box, e.g. if you want to include instructions or other ancillary information on the page in a way that doesn't interfere with the print job itself. The printing company that I placed my order with used this trick to ship their template files.

All of these boxes can be configured in a very convenient way from within iText: the `PdfPage` class has `setMediaBox()`, `setBleedBox()`, `setCropBox()` and `setTrimBox()`.


::: note
If you're previewing your file(s) in Adobe Acrobat/Reader, it may be helpful to turn on "Show art, trim & bleed boxes" in the page display settings.
:::


[^prefer-trimbox]: A page in a PDF/X-4 document can't have both a **TrimBox** and an **ArtBox**. Either is allowed, but **TrimBox** is preferred. PDF/X-6 is a bit more lax in its requirements.

### Fonts

This one is pretty straightforward: to achieve consistent text rendering, all fonts must be embedded. Just make sure you explicitly load a font file whenever you render some text, and iText should embed it (or a subset) by default.

Some additional restrictions related to character encoding apply as well, depending on the type of font and the manner in which it is accessed from the document content.


### Transparency

This is a tricky one: transparency is a PDF 1.4 feature, so PDF/X-1a and PDF/X-3---being based on PDF 1.3---cannot make use of soft masks, shading, etc. Such effects must be flattened before incorporating them in a PDF 1.3 file.

From PDF/X-4 onwards, transparent effects can be used, subject to some restrictions and subtleties. Those relate mostly to the way blending is supposed to work, and effectively only come into play when advanced colour space usage is involved. If that's your situation, then chances are that you know a lot more about the topic than I do, so I won't bother trying to elaborate.


## Metadata requirements

PDF/X documents also need certain XMP metadata properties to be set. For PDF/X-4[^relaxed-xmpmm-x6], this is what you need (namespace URIs abbreviated to their common prefix form):

 - `pdfxid:GTS_PDFXVersion` must be `PDF/X-4`
 - `xmpMM:DocumentID`, `xmpMM:VersionID` and `xmpMM:RenditionClass` are mandatory to identify the document.
 - `xmp:CreateDate`, `xmp:ModifyDate` and `xmp:MetadataDate` must be supplied.
 - `pdf:Trapped` must be set. If you're not sure, set it to `False`.
 - `dc:title` is required.

 [^relaxed-xmpmm-x6]: PDF/X-6 is a little less opinionated on this topic, but it doesn't hurt to always supply these values.


# Sample code

Here's a subclass of iText's `PdfDocument` that you can use as a starting point to generate PDF/X output (used with iText 7.2.4).

```java
package be.mvalvekens.pdfxexample;

import com.itextpdf.kernel.pdf.PdfDate;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfName;
import com.itextpdf.kernel.pdf.PdfOutputIntent;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.xmp.XMPConst;
import com.itextpdf.kernel.xmp.XMPException;
import com.itextpdf.kernel.xmp.XMPMeta;

public class PdfXDocument extends PdfDocument {

    private final String documentVersion;
    private final String documentIdUri;

    public PdfXDocument(PdfWriter writer,
                        String title,
                        PdfOutputIntent oi,
                        String documentIdUri,
                        String documentVersion) {
        super(writer);
        oi.setOutputIntentSubtype(new PdfName("GTS_PDFX"));
        this.addOutputIntent(oi);
        this.getDocumentInfo().setTitle(title);
        this.getDocumentInfo().setTrapped(new PdfName("False"));
        this.documentIdUri = documentIdUri;
        this.documentVersion = documentVersion;
    }

    @Override
    protected void addCustomMetadataExtensions(XMPMeta meta) {
        try {
            String pdfxid = "http://www.npes.org/pdfx/ns/id/";
            meta.setProperty(pdfxid, "GTS_PDFXVersion", "PDF/X-4");
            meta.setProperty(
                    XMPConst.NS_XMP_MM, "RenditionClass", "default"
            );
            meta.setProperty(
                    XMPConst.NS_XMP_MM, "DocumentID", this.documentIdUri
            );
            meta.setProperty(
                    XMPConst.NS_XMP_MM, "VersionID", this.documentVersion
            );
            String pdfDate = this.getDocumentInfo()
                    .getMoreInfo(PdfName.ModDate.getValue());
            String date = PdfDate.getW3CDate(pdfDate);
            meta.setProperty(XMPConst.NS_XMP, "MetadataDate", date);
        } catch (XMPException e) {
            throw new RuntimeException(e);
        }
    }
}
```
