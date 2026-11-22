# syntax=docker/dockerfile:1.7

ARG BASE_IMAGE=mcr.microsoft.com/devcontainers/base:noble
FROM ${BASE_IMAGE}

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ARG DEBIAN_FRONTEND=noninteractive

# Nix (the devcontainer "nix" feature) provides GHC, Cabal, HLS, HLint,
# Ormolu, Python, and the other build dependencies from flake.nix.
# Cover both the legacy ~/.nix-profile link and the XDG profile location.
ENV PATH=/home/vscode/.nix-profile/bin:/home/vscode/.local/state/nix/profiles/profile/bin:${PATH}

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

RUN curl -fsSL https://opencode.ai/v2/install | bash
