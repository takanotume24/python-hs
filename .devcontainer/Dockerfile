# syntax=docker/dockerfile:1.7

ARG BASE_IMAGE=mcr.microsoft.com/devcontainers/base:noble
FROM ${BASE_IMAGE}

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ARG DEBIAN_FRONTEND=noninteractive
ARG GHC_VERSION=latest
ARG CABAL_VERSION=latest
ARG HLS_VERSION=latest

ENV GHCUP_INSTALL_BASE_PREFIX=/usr/local
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=0
ENV PATH=/usr/local/.ghcup/bin:/home/vscode/.cabal/bin:${PATH}

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libnuma-dev \
    libtinfo-dev \
    xz-utils \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -fsSL https://get-ghcup.haskell.org | sh \
    && ghcup install ghc ${GHC_VERSION} \
    && ghcup set ghc ${GHC_VERSION} \
    && ghcup install cabal ${CABAL_VERSION} \
    && ghcup set cabal ${CABAL_VERSION} \
    && ghcup install hls ${HLS_VERSION} \
    && ghcup set hls ${HLS_VERSION} \
    && ghcup gc --cache

RUN mkdir -p /home/vscode/.cabal /home/vscode/.ghcup \
    && chown -R vscode:vscode /home/vscode/.cabal /home/vscode/.ghcup /usr/local/.ghcup

USER vscode
