#!/usr/bin/env bash
set -euo pipefail

line='[[ "$TERM_PROGRAM" == "vscode" ]] && . "$(code --locate-shell-integration-path bash)"'
bashrc="$HOME/.bashrc"

if [[ ! -f "$bashrc" ]]; then
  touch "$bashrc"
fi

if ! grep -Fq "$line" "$bashrc"; then
  printf '\n%s\n' "$line" >> "$bashrc"
fi
