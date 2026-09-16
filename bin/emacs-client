#!/usr/bin/env bash

[[ -z "${EMACS_TTY_SERVER}" ]] && EMACS_TTY_SERVER="tty-server"

export EMACS_TTY_SERVER

declare -a editor_cmd
editor_cmd+=(/usr/bin/emacsclient --create-frame --tty --socket-name="${EMACS_TTY_SERVER}")

if ! pgrep -af '.*emacs.*'"--daemon=${EMACS_TTY_SERVER}"'.*' &>/dev/null; then
  (emacs --daemon="${EMACS_TTY_SERVER}" &> "/tmp/emacs-${EMACS_TTY_SERVER}-server.log") & disown
  # Give the server 125ms to start listening for connections.
  sleep 0.125
fi

EDITOR="'""${editor_cmd[*]}""'" "${editor_cmd[@]}" "$@"

