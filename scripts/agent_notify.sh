#!/usr/bin/env bash

export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL:-}" ]] && export LOG_LEVEL=3
[[ -z "${LOG_FILE:-}" ]] && export LOG_FILE="/tmp/agent_notify.log"

source "${HOME}/lib/log_lib.sh"

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  log::debug "run $*"
  "$@"
}

populate_tmux_info

TITLE="$1"
MSG="$2"
FULL_MSG=$(cat<<EOF

${TITLE}
Tmux Session: ${TMUX_SESSION:-UNKNOWN} Window: ${TMUX_WINDOW:-UNKNOWN}
${MSG}
EOF
           )

# Attempt to use knock to notify
if [[ -e /google/bin/releases/knock/knock.sh ]]; then
  log::debug "source /google/bin/releases/knock/knock.sh"
  source /google/bin/releases/knock/knock.sh &>/dev/null
  dispatch knock "${FULL_MSG}"
else
  log::debug "Notifying on terminal."
  # Otherwise use local notifications on terminal & desktop.
  notified=
  if run type tmux-notify &>/dev/null; then
    dispatch tmux-notify "${title}" "${msg}"
    notified=1
  fi
  if [[ -n "${DISPLAY}" ]] && run type notify-send &>/dev/null; then
    dispatch notify-send "${title}" "${msg}"
    notified=1
  fi
  if [[ -z "${notified}" ]]; then
    log::error "neither tmux-notify nor notify-send where available to notify the user."
  fi
fi
