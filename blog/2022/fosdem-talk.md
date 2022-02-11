---
title: "FOSDEM 2022 talk: How (not) to make a mockery of trust"
author: "Matthias Valvekens"
tags: [talks, fosdem, pki]
keywords: [pdf]
published: 2022-02-11
---

# Summary

Last week, I gave a talk on testing & mocking PKI-based workflows from the relying party's point of view at [FOSDEM '22](https://fosdem.org/2022/),
in the [Testing & Automation devroom](https://fosdem.org/2022/schedule/track/testing_and_automation/).
The session was recorded, and is embedded below. Due to an unfortunate technical issue, the recording of the live Q&A session wasn't usable.

The presentation goes into some of the "whys" and "hows" behind my project [Certomancer](https://github.com/MatthiasValvekens/certomancer), and the
way I use it, both in my own projects and at [iText](https://itextpdf.com).

For further information, visit [the talk's info page](https://fosdem.org/2022/schedule/event/mockery_of_trust/).


# Video: How (not) to make a mockery of trust

```{#video .youtube ytid=3wFtQiU86EU}
{
  "@context": "https://schema.org",
  "@type": "VideoObject",
  "name": "How (not) to make a mockery of trust - FOSDEM 2022",
  "description": "The ever-continuing push for digitalisation has increased our reliance on trust services of various kinds, filling various needs relating to document signing, code signing, authorization tokens, and so forth. Many of these trust services rely on public-key infrastructure (PKI) and X.509 certificates. The sensitive nature of these tools makes them difficult to use in a testing environment. On the one hand, exposing access to production keys in your CI is obviously a terrible idea. But on the other hand, setting up and maintaining a fully functional \"mock\" PKI environment is also pretty tricky. What can you do about that?",
  "uploadDate": "2022-02-07",
  "duration": "PT12M41S",  
  "width": 560,
  "height": 315,
  "publisher": [
      {
        "@type": "Organization",
        "@id": "https://itextpdf.com",
        "name": "iTextPDF",
        "url": "https://itextpdf.com",
        "logo": {
            "@type": "ImageObject",
            "url": "https://itextpdf.com/sites/default/files/styles/max_325x325/public/itext_0.png",
            "width": "150",
            "height": "150"
        }
      },
      {
        "@type": "Organization",
        "@id": "https://fosdem.org#org",
        "name": "FOSDEM",
        "url": "https://fosdem.org"
      }
  ],
  "recordedAt": {   
    "@type": "Event",
    "@id": "https://fosdem.org/2022/schedule/event/mockery_of_trust/#talk",
    "url": "https://fosdem.org/2022/schedule/event/mockery_of_trust/",
    "name": "FOSDEM 2022 - How (not) to make a mockery of trust",
    "eventStatus": "https://schema.org/EventMovedOnline",
    "eventAttendanceMode": "https://schema.org/OnlineEventAttendanceMode",
    "description": "The ever-continuing push for digitalisation has increased our reliance on trust services of various kinds, filling various needs relating to document signing, code signing, authorization tokens, and so forth. Many of these trust services rely on public-key infrastructure (PKI) and X.509 certificates. The sensitive nature of these tools makes them difficult to use in a testing environment. On the one hand, exposing access to production keys in your CI is obviously a terrible idea. But on the other hand, setting up and maintaining a fully functional \"mock\" PKI environment is also pretty tricky. What can you do about that?",
    "location": {
      "@type": "VirtualLocation",
      "url": "https://chat.fosdem.org/#/room/#safety:fosdem.org"
    },
    "performer": {
        "@id": "https://mvalvekens.be/about.html#me",
        "@type": "Person",
        "name": "Matthias Valvekens"
    },
    "offers": {
        "@type": "Offer",
        "url": "https://live.fosdem.org/watch/dtesting",
        "price": "0",
        "priceCurrency": "EUR",
        "availability": "https://schema.org/InStock",
        "validFrom": "2022-02-05T16:00:00+01:00"
    },
    "organizer": { "@id": "https://fosdem.org#org" },
    "startDate": "2022-02-05T16:00:00+01:00",
    "endDate": "2022-02-06T16:15:00+01:00",
    "superEvent": {
        "@type": "Event",
        "@id": "https://fosdem.org/2022/#event",
        "name": "FOSDEM 2022",
        "description": "FOSDEM 2022 edition (virtual)",
        "url": "https://fosdem.org/2022/",
        "location": {
          "@type": "VirtualLocation",
          "url": "https://chat.fosdem.org/"
        },
        "offers": {
            "@type": "Offer",
            "url": "https://fosdem.org/2022/live/",
            "price": "0",
            "priceCurrency": "EUR",
            "availability": "https://schema.org/InStock",
            "validFrom": "2022-02-05T00:00:00+01:00"
        },
        "startDate": "2022-02-05T00:00:00+01:00",
        "endDate": "2022-02-06T23:59:59+01:00",
        "eventStatus": "https://schema.org/EventMovedOnline",
        "eventAttendanceMode": "https://schema.org/OnlineEventAttendanceMode",
        "organizer": { "@id": "https://fosdem.org#org" }
    }
  }
}
```
