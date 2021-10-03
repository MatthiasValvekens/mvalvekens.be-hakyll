---
title: "Certomancer"
project-url: "https://github.com/MatthiasValvekens/certomancer/"
---

```meta
{
    "@context": "https://schema.org",
    "@id": "https://mvalvekens.be/about.html#certomancer",
    "@type": ["SoftwareApplication", "SoftwareSourceCode"],
    "author": {
        "@type": "Person",
        "@id": "https://mvalvekens.be/about.html#me"
    },
    "name": "certomancer",
    "programmingLanguage": "Python",
    "sameAs": "https://github.com/MatthiasValvekens/certomancer/",
    "codeRepository": "https://github.com/MatthiasValvekens/certomancer/",
    "softwareHelp": [
        "https://github.com/MatthiasValvekens/certomancer/blob/master/docs/config.md",
        "https://github.com/MatthiasValvekens/certomancer/blob/master/docs/cli.md",
        "https://github.com/MatthiasValvekens/certomancer/blob/master/docs/plugins.md",
        "https://github.com/MatthiasValvekens/certomancer/blob/master/docs/deploy.md"
    ],
    "applicationCategory": ["CLI tool", "Library"],
    "license": "https://raw.githubusercontent.com/MatthiasValvekens/certomancer/master/LICENSE",
    "operatingSystem": ["Windows", "Linux", "macOS"]
}
```

# certomancer {.is-family-monospace}

Quickly construct, mock and deploy PKI test configurations using simple, declarative YAML config.
Includes integrated CRL, OCSP and time stamping service provisioning.
Certomancer provides a standalone WSGI application with minimal setup requirements, and also integrates with <span class="is-family-monospace">requests-mock</span>.
This tool is heavily used in pyHanko's test suite.
