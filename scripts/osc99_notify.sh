#!/usr/bin/env bash

# osc99_notify.sh: Sends terminal notification using Kitty's OSC 99 protocol.
# Supports TMUX passthrough and targeted TTY writing.

TITLE="$1"
MESSAGE="$2"

if [[ -z "${TITLE}" ]]; then
  echo "Usage: $0 <title> [message]" >&2
  exit 1
fi

# If message is provided, combine them with a newline as required by Kitty OSC 99 spec
CONTENT="${TITLE}"
if [[ -n "${MESSAGE}" ]]; then
  CONTENT="${TITLE}\n${MESSAGE}"
fi

# Construct the OSC 99 escape sequence: ESC ] 99 ; i=jetski ; d=0 ; CONTENT BEL
# Inside tmux, we wrap the sequence with ESC P tmux ; and double the ESC character (\e -> \e\e)
if [[ -n "${TMUX}" ]]; then
  osc_seq=$(printf "\ePtmux;\e\e]99;i=jetski;d=0;%b\a\e\\\\" "${CONTENT}")
else
  osc_seq=$(printf "\e]99;i=jetski;d=0;%b\a" "${CONTENT}")
fi

# Find the target TTY of the parent/Jetski process to write directly to it
cli_tty=$(ps -p "${PPID}" -o tty= 2>/dev/null | awk '{print $1}')
if [[ -n "${cli_tty}" ]] && [[ "${cli_tty}" != "?" ]] && [[ -w "/dev/${cli_tty}" ]]; then
  printf "%s" "${osc_seq}" > "/dev/${cli_tty}"
else
  # Fallback to stderr
  printf "%s" "${osc_seq}" >&2
fi
