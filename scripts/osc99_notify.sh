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
  CONTENT="${TITLE}"$'\n'"${MESSAGE}"
fi

# Sanitize BEL (\a) from content to prevent premature sequence termination
CONTENT="${CONTENT//$'\a'/ }"

# Construct the OSC 99 escape sequence: ESC ] 99 ; i=jetski ; d=0 ; CONTENT BEL
# Inside tmux, we wrap the sequence with ESC P tmux ; and double the ESC character (\e -> \e\e)
# Use %s instead of %b to prevent printf from interpreting backslashes in user text
if [[ -n "${TMUX}" ]]; then
  osc_seq=$(printf "\ePtmux;\e\e]99;i=jetski;d=0;%s\a\e\\\\" "${CONTENT}")
else
  osc_seq=$(printf "\e]99;i=jetski;d=0;%s\a" "${CONTENT}")
fi

# Check for explicit TARGET_TTY or discover from PPID
target_tty="${TARGET_TTY}"
if [[ -n "${target_tty}" && "${target_tty}" != /* ]]; then
  target_tty="/dev/${target_tty}"
fi
if [[ -z "${target_tty}" ]]; then
  cli_tty=$(ps -p "${PPID}" -o tty= 2>/dev/null | awk '{print $1}')
  if [[ -n "${cli_tty}" && "${cli_tty}" != "?" ]]; then
    target_tty="/dev/${cli_tty#/dev/}"
  fi
fi

if [[ -n "${target_tty}" && -w "${target_tty}" ]]; then
  printf "%s" "${osc_seq}" > "${target_tty}"
else
  # Fallback to stderr
  printf "%s" "${osc_seq}" >&2
fi
