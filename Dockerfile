# Setup from
# https://timwspence.github.io/blog/posts/2019-08-02-optimized-docker-builds-for-haskell-stack.html
FROM haskell:9.2.5-slim as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

# Libgmp is linked dynamically, so we have to be careful to consistently version things
RUN apt-get update && apt-get download libgmp10 \
        && mv libgmp*.deb libgmp.deb && rm -rf /var/lib/apt/lists/*

# The idea is that we only invalidate the dependency layer if the lock file changes
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only


FROM haskell:9.2.5-slim as build

RUN apt-get update && apt-get install -y zip && rm -rf /var/lib/apt/lists/*

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin


FROM debian:buster-slim

# Install the same libgmp version
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin/site /usr/bin/site

ENTRYPOINT ["/usr/bin/site"]

