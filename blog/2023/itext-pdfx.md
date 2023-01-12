---
title: "Generating PDF/X output with iText Core"
author: "Matthias Valvekens"
tags: [pdf, itext, java]
published: 2023-01-12
license: "CC BY-SA 4.0 hybrid"
---


# Background

The impetus for this post is simple: I recently used an online printing service to place an order for paper cards to announce the birth of our newborn son. Since I planned to put together the card design using iText anyhow, I figured I might as well submit in PDF/X. After all, I would've been charged extra if the printing company had to fix my files!

Below, I'll try to explain how the process went.


::: warning
While I know a lot about PDF, I'm far from a prepress expert, so take my opinion here with a grain of salt.
:::


# About PDF/X

PDF/X is a subset (or rather, a collection of subsets) of PDF that is specifically geared for use in prepress workflows. The idea is that one should be able to print a file conforming to (some flavour of) PDF/X with minimal human intervention, in a consistent manner across different presses and/or printing platforms.

This is especially useful in situations where job submission is highly automated with little room for human support overhead, e.g. modest-volume jobs submitted through a public web portal, like my announcement card order.

Shouldn't this work with pretty much any PDF? Unfortunately not: PDF allows certain liberties that aren't compatible with this idea. Without going into too much detail, the various PDF/X subset standards outline

 - which features of PDF can be used in the given PDF/X profile, and
 - how to declare conformance with a particular PDF/X flavour.

In that way, it's a lot like PDF/A, but the focus is on prepress rather than archival use cases. For a nice overview of how PDF/X evolved over the years, make sure to give [this excellent article by Martin Bailey](https://www.pdfa.org/the-route-to-pdf-x-and-where-we-are-now-a-personal-history/) a read.


Also unlike PDF/A, iText does not come with "batteries included" PDF/X support---but that's not actually as big an obstacle as it seems.
