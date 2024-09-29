#!/usr/bin/env bash

if [ $# -eq 0 ]; then
  tmp_file="$(mktemp)"
  cat > "${tmp_file}"
  tmux load-buffer -w "${tmp_file}"
else
  tmux set-buffer -w "$*"
fi
