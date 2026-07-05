#!/usr/bin/env bash

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

NOTIFICATION_THRESHOLD_SECS=60
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# https://geminicli.com/docs/hooks/reference/#afteragent
read -r -d '' PAYLOAD
log::debug "PAYLOAD: ${PAYLOAD}"
# Example:
#{
#  "artifactDirectoryPath": "/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#  "conversationId": "f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#  "executionId": "992720db-22af-44df-a982-192a45415ad4",
#  "initialNumSteps": 54,
#  "invocationNum": 25,
#  "modelName": "auto",
#  "transcriptPath": "/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/f13b6cc2-f2a7-4aac-8590-aa1a8db311b6/.system_generated/logs/transcript_full.jsonl",
#  "workspacePaths": [
#    "/google/src/cloud/marcelvaldez/avid_tdp_datastore_monitoring"
#  ]
#}
conversation_id="$(echo "${PAYLOAD}" | run jq -r '.conversationId')"
log::debug "conversation_id: ${conversation_id}"
execution_id="$(echo "${PAYLOAD}" | run jq -r '.executionId')"
log::debug "execution_id: ${execution_id}"
invocation_num="$(echo "${PAYLOAD}" | run jq .invocationNum)"
log::debug "invocation_num: ${invocation_num}"
initial_num_steps="$(echo "${PAYLOAD}" | run jq .initialNumSteps)"
log::debug "initial_num_steps: ${initial_num_steps}"
workspace_path="$(echo "${PAYLOAD}" | run jq -r '.workspacePaths[0]')"
log::debug "workspace_path: ${workspace_path}"
workspace_dir="$(basename "${workspace_path}")"
EXECUTION_TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${execution_id}.txt"
log::debug "EXECUTION_TRACKER_FILE: ${EXECUTION_TRACKER_FILE}"
INVOCATION_TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${execution_id}_${invocation_num}.txt"
log::debug "INVOCATION_TRACKER_FILE: ${INVOCATION_TRACKER_FILE}"

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

if [[ -f "${INVOCATION_TRACKER_FILE}" ]]; then
  start_time="$(run cat "${INVOCATION_TRACKER_FILE}")"
  end_time=$(run date +%s)
  elapsed=$((end_time-start_time))
  log::debug "elapsed: ${elapsed}"
  if [[ "${elapsed}" -ge "${NOTIFICATION_THRESHOLD_SECS}" ]]; then
    title="Jetski CLI: Tool Invocation Done"
    populate_tmux_info
    msg=$(cat<<EOF

Jestki CLI Tool Invocation on ${workspace_dir} done
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
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

echo '{"injectSteps": [], "terminationBehavior": ""}'
