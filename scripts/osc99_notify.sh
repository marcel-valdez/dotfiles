#!/usr/bin/env bash

# osc99_notify.sh: Sends terminal notification using Kitty's OSC 99 protocol.
# Supports TMUX passthrough and targeted TTY writing.

TITLE="$1"
MESSAGE="$2"

if [[ -z "${TITLE}" ]]; then
  echo "Usage: $0 <title> [message]" >&2
  exit 1
fi

# Sanitize BEL (\a) and ESC (\e) from title and message to prevent premature sequence termination
SAFE_TITLE="${TITLE//$'\a'/ }"
SAFE_TITLE="${SAFE_TITLE//$'\e'/ }"
SAFE_MESSAGE="${MESSAGE//$'\a'/ }"
SAFE_MESSAGE="${SAFE_MESSAGE//$'\e'/ }"

# Construct the Kitty OSC 99 escape sequence using colon-separated key=value metadata:
#   - Title chunk: ESC ] 99 ; i=jetski:d=0:p=title ; <TITLE> BEL
#   - Body chunk:  ESC ] 99 ; i=jetski:d=1:p=body  ; <MESSAGE> BEL
# Inside tmux, wrap with ESC P tmux ; ... ESC \ and double internal ESC characters (\e -> \e\e).
if [[ -n "${SAFE_MESSAGE}" ]]; then
  if [[ -n "${TMUX}" ]]; then
    osc_seq=$(printf "\ePtmux;\e\e]99;i=jetski:d=0:p=title;%s\a\e\e]99;i=jetski:d=1:p=body;%s\a\e\\\\" "${SAFE_TITLE}" "${SAFE_MESSAGE}")
  else
    osc_seq=$(printf "\e]99;i=jetski:d=0:p=title;%s\a\e]99;i=jetski:d=1:p=body;%s\a" "${SAFE_TITLE}" "${SAFE_MESSAGE}")
  fi
else
  if [[ -n "${TMUX}" ]]; then
    osc_seq=$(printf "\ePtmux;\e\e]99;i=jetski:d=1:p=title;%s\a\e\\\\" "${SAFE_TITLE}")
  else
    osc_seq=$(printf "\e]99;i=jetski:d=1:p=title;%s\a" "${SAFE_TITLE}")
  fi
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
