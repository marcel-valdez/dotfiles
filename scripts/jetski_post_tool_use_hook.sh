#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
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

NOTIFICATION_THRESHOLD_SECS=300
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# https://g3doc.corp.google.com/devtools/jetski/g3doc/features/agent/agent-hooks.md
#  {"toolHookArgs":{"stepIdx":11}}
read -r -d '' PAYLOAD
log::info "PAYLOAD: $(echo "${PAYLOAD}" | run jq --monochrome-output)"
# Example:
# {
#   "artifactDirectoryPath":"/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#   "conversationId":"f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#   "error":"",
#   "executionId":"992720db-22af-44df-a982-192a45415ad4",
#   "modelName":"auto",
#   "stepIdx":56,
#   "toolCall":{
#     "args":{
#       "Message":"<message contents>",
#       "Recipient":"fbb2007e-2f15-4db3-a828-0ef66400b785",
#       "toolAction":"Sending review report to parent agent",
#       "toolSummary":"Send code review report to parent"
#     },
#     "name":"send_message"
#   },
#   "transcriptPath":"/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/f13b6cc2-f2a7-4aac-8590-aa1a8db311b6/.system_generated/logs/transcript_full.jsonl",
#   "workspacePaths":["/google/src/cloud/marcelvaldez/avid_tdp_datastore_monitoring"]
# }

conversation_id="$(echo "${PAYLOAD}" | run jq -r '.conversationId')"
log::info "conversation_id: ${conversation_id}"
execution_id="$(echo "${PAYLOAD}" | run jq -r '.executionId')"
log::info "execution_id: ${execution_id}"
step_idx="$(echo "${PAYLOAD}" | run jq .stepIdx)"
log::info "step_idx: ${step_idx}"
workspace_path="$(echo "${PAYLOAD}" | run jq -r '.workspacePaths[0]')"
log::info "workspace_path: ${workspace_path}"
workspace_dir="$(basename "${workspace_path}")"
tool_name="$(echo "${PAYLOAD}" | run jq -r '.toolCall.name')"
log::info "tool_name: ${tool_name}"
tool_args="$(echo "${PAYLOAD}" | run jq -r '.toolCall.args')"
log::info "tool_args: ${tool_args}"
tool_summary="$(echo "${PAYLOAD}" | run jq -r '.toolCall.args.toolSummary')"
log::info "tool_summary: ${tool_summary}"
tool_action="$(echo "${PAYLOAD}" | run jq -r '.toolCall.args.toolAction')"
log::info "tool_action: ${tool_action}"

TOOL_TRACKER_FILE="/tmp/jetski_tool_use_req_${PPID}_${execution_id}_${step_idx}.txt"
log::debug "TOOL_TRACKER_FILE: ${TOOL_TRACKER_FILE}"

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

if [[ -f "${TOOL_TRACKER_FILE}" ]]; then
  start_time="$(run cat "${TOOL_TRACKER_FILE}")"
  end_time=$(run date +%s)
  elapsed=$((end_time-start_time))
  log::debug "elapsed: ${elapsed}"
  if [[ "${elapsed}" -ge "${NOTIFICATION_THRESHOLD_SECS}" ]]; then
    body="Args: ${tool_args}"
    if [[ -n "${tool_summary}" ]]; then
      body="Summary: ${tool_summary}"
    fi
    title="Jetski CLI: Tool ${tool_name} Done"
    populate_tmux_info
    msg=$(cat<<EOF

Jestki CLI Tool ${tool_name} on ${workspace_dir} Done.
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
${body}
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

    # Send OSC 99 terminal notification (Kitty, etc.) if enabled
    if [[ "${JETSKI_ENABLE_OSC99:-true}" == "true" ]]; then
      dispatch "${HOME}/scripts/osc99_notify.sh" "${title}" "${msg}"
    fi
  fi
fi

echo '{ "allowTool": true }'
