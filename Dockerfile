FROM fpco/stack-build:lts-18.10 as build
RUN mkdir /build/
COPY . /build/
RUN cd /build && stack build --system-ghc

FROM ubuntu:latest
RUN mkdir -p /opt/workspace
COPY --from=build /build/.stack-work/install/*/*/*/bin/site /opt/site
WORKDIR /opt/workspace
CMD ["/opt/site", "build"]
