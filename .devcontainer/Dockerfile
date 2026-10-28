# syntax=docker/dockerfile:1.7

ARG BASE_IMAGE=mcr.microsoft.com/devcontainers/base:noble
FROM ${BASE_IMAGE}

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ARG DEBIAN_FRONTEND=noninteractive
ARG GHC_VERSION=9.12.2
ARG CABAL_VERSION=3.14.2.0
ARG HLS_VERSION=2.10.0.0

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=0
ENV PATH=/home/vscode/.cabal/bin:/home/vscode/.ghcup/bin:${PATH}

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

USER vscode
WORKDIR /home/vscode

RUN curl --proto '=https' --tlsv1.2 -fsSL https://get-ghcup.haskell.org | sh \
    && ghcup install ghc ${GHC_VERSION} \
    && ghcup set ghc ${GHC_VERSION} \
    && ghcup install cabal ${CABAL_VERSION} \
    && ghcup set cabal ${CABAL_VERSION} \
    && ghcup install hls ${HLS_VERSION} \
    && ghcup set hls ${HLS_VERSION} \
    && ghcup gc --cache

# Install Haskell formatting and linting tools required by the quality gate.
RUN cabal update \
    && cabal install ormolu hlint \
    && cabal clean

RUN curl -fsSL https://opencode.ai/v2/install | bash
