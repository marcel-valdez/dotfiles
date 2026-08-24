#!/usr/bin/env bash

export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL:-}" ]] && export LOG_LEVEL=2
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

function populate_tmux_info {
  local cli_tty=
  cli_tty=$(run ps -p "${PPID}" -o tty= | run awk '{print $1}')

  if [[ -n "${cli_tty}" ]] && [[ "${cli_tty}" != "?" ]]; then
    local full_tty="/dev/${cli_tty}"
    local tmux_info
    tmux_info=$(run tmux list-panes -a -F '#{pane_tty} #{session_name} #{window_name}' 2>/dev/null | run grep "^${full_tty} ")
    if [[ -n "${tmux_info}" ]]; then
      TMUX_SESSION=$(echo "${tmux_info}" | run awk '{print $2}')
      TMUX_WINDOW=$(echo "${tmux_info}" | run awk '{print $3}')
    fi
  fi
}

populate_tmux_info

TITLE="$1"
shift
BODY=$(cat<<EOF
Tmux Session: ${TMUX_SESSION:-UNKNOWN} Window: ${TMUX_WINDOW:-UNKNOWN}
$*
EOF
    )
FULL_MSG=$(cat<<EOF

${TITLE}
${BODY}
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
    dispatch tmux-notify "${TITLE}" "${BODY}"
    notified=1
  fi
  if [[ -n "${DISPLAY}" ]] && run type notify-send &>/dev/null; then
    dispatch notify-send "${TITLE}" "${BODY}"
    notified=1
  fi
  if [[ -z "${notified}" ]]; then
    log::error "neither tmux-notify nor notify-send where available to notify the user."
  fi
fi

# Send OSC 99 terminal notification (Kitty, etc.) if enabled
if [[ "${JETSKI_ENABLE_OSC99:-true}" == "true" ]]; then
  dispatch "${HOME}/scripts/osc99_notify.sh" "${TITLE}" "${BODY}"
fi
