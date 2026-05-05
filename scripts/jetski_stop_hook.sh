e#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=3
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"
source "${HOME}/lib/log_lib.sh"

function run {
  log::debug "run $*"
  "$@"
}

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

# The tracker file is the one from the first invocation.
TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_0.txt"
log::debug "TRACKER_FILE: ${TRACKER_FILE}"
NOTIFICATION_THRESHOLD_SECS=60
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# http://g3doc/devtools/jetski/g3doc/features/agent/agent-hooks.md
read -r -d '' PAYLOAD
log::debug "PAYLOAD: ${PAYLOAD}"
execution_num="$(echo "${PAYLOAD}" | run jq -r '.stopHookArgs.executionNum')"
log::debug "execution_num: ${execution_num}"
termination_reason="$(echo "${PAYLOAD}" | run jq -r '.stopHookArgs.terminationReason')"
log::debug "termination_reason: ${termination_reason}"
error="$(echo "${PAYLOAD}" | run jq -r '.stopHookArgs.error')"
log::debug "error: ${error}"

function populate_tmux_info {
  local cli_tty
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

log::info "Processing: $(echo "${PAYLOAD}" | run jq --monochrome-output)"

if [[ -f "${TRACKER_FILE}" ]]; then
  start_time="$(run cat "${TRACKER_FILE}")"
  end_time=$(run date +%s)
  elapsed=$((end_time-start_time))
  log::debug "elapsed: ${elapsed}"
  if [[ "${elapsed}" -ge "${NOTIFICATION_THRESHOLD_SECS}" ]]; then
    populate_tmux_info
    if [[ -n "${error}" ]]; then
      title="Jetski CLI: Error"
      msg=$(cat<<EOF

Jestki CLI Error
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Termination Reason: ${termination_reason}
Error: ${error}
EOF
         )
    else
      title="Jetski CLI: Response Ready"
      msg=$(cat<<EOF

Jestki CLI response ready.
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
EOF
       )
    fi

    # Attempt to use knock to notify
    if [[ -e /google/bin/releases/knock/knock.sh ]]; then
      source /google/bin/releases/knock/knock.sh &>/dev/null
      dispatch knock "${msg}"
    else
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
  fi
fi

if [[ -n "${notified}" ]]; then
  echo '{"decision": "", "reason": "Notified user of failure."}'
else
  echo '{"decision": "", "reason": ""}'
fi
