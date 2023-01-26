FROM fpco/stack-build-small:lts-20.7 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    locales \
    netbase \
    pkg-config && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src
WORKDIR /src

COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only
COPY src /src/src
COPY app /src/app
RUN stack install

FROM ubuntu:20.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/bluebook /bin/bluebook
CMD ["/bin/bluebook", "serve"]
