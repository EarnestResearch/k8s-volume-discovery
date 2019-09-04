# Stage 1: build dependencies
FROM fpco/stack-build:lts-14.3 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
# Stage 2: build application
FROM fpco/stack-build:lts-14.3 as build

COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Stage 3: run application
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb
RUN apt-get update && apt-get install -y ca-certificates

COPY --from=build /opt/build/bin .
ENTRYPOINT ["/opt/app/k8s-volume-discovery-exe"]