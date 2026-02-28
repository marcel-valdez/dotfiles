#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/gemini_after_agent_hook.log"
source "${HOME}/lib/log_lib.sh"

function run {
  log::debug "run $*"
  "$@"
}

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

TRACKER_FILE="/tmp/gemini_req_${PPID}.txt"
NOTIFICATION_THRESHOLD_SECS=60
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# https://geminicli.com/docs/hooks/reference/#afteragent
read -r -d '' PAYLOAD
log::debug "PAYLOAD: ${PAYLOAD}"
event_name="$(echo "${PAYLOAD}" | run jq -r '.hook_event_name')"
log::debug "event_name: ${event_name}"
prompt="$(echo "${PAYLOAD}" | run jq -r '.prompt')"
log::debug "prompt: ${prompt}"
prompt_response="$(echo "${PAYLOAD}" | run jq -r '.prompt_response')"
log::debug "prompt_response: ${prompt_response}"

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
if [[ "${event_name}" == "AfterAgent" ]]; then
  if [[ -f "${TRACKER_FILE}" ]]; then
    start_time="$(run cat "${TRACKER_FILE}")"

    end_time=$(run date +%s)
    elapsed=$((end_time-start_time))
    log::debug "elapsed: ${elapsed}"
    if [[ "${elapsed}" -ge "${NOTIFICATION_THRESHOLD_SECS}" ]]; then
      title="Gemini CLI"
      populate_tmux_info
      msg=$(cat<<EOF

Gemini CLI response ready.
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Prompt: ${prompt:0:50}...
Response: ${prompt_response:0:50}...
EOF
         )

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
elif [[ "${event_name}" == "Notification" ]]; then
  run sleep 0
fi

echo '{"decision": "allow"}'
