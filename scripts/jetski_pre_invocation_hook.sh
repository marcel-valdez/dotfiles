#!/usr/bin/env bash
# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"
source "${HOME}/lib/log_lib.sh"

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  log::debug "run $*"
  "$@"
}

read -r -d '' PAYLOAD
log::info "PAYLOAD: $(echo "${PAYLOAD}" | run jq --monochrome-output)"
# Example:
#{
#  "artifactDirectoryPath": "/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/fbb2007e-2f15-4db3-a828-0ef66400b785",
#  "conversationId": "fbb2007e-2f15-4db3-a828-0ef66400b785",
#  "executionId": "4c52e06a-7e22-4d51-90d6-8264e08e2c30",
#  "initialNumSteps": 97,
#  "invocationNum": 0,
#  "modelName": "auto",
#  "transcriptPath": "/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/fbb2007e-2f15-4db3-a828-0ef66400b785/.system_generated/logs/transcript_full.jsonl",
#  "workspacePaths": [
#    "/google/src/cloud/marcelvaldez/avid_tdp_datastore_monitoring"
#  ]
#}
conversation_id="$(echo "${PAYLOAD}" | run jq -r '.conversationId')"
log::info "conversation_id: ${conversation_id}"
execution_id="$(echo "${PAYLOAD}" | run jq -r '.executionId')"
log::info "execution_id: ${execution_id}"
invocation_num="$(echo "${PAYLOAD}" | run jq .invocationNum)"
log::info "invocation_num: ${invocation_num}"
initial_num_steps="$(echo "${PAYLOAD}" | run jq .initialNumSteps)"
log::info "initial_num_steps: ${initial_num_steps}"
workspace_path="$(echo "${PAYLOAD}" | run jq -r '.workspacePaths[0]')"
log::info "workspace_path: ${workspace_path}"

EXECUTION_TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${execution_id}.txt"
log::debug "EXECUTION_TRACKER_FILE: ${EXECUTION_TRACKER_FILE}"
if ! [[ -f "${EXECUTION_TRACKER_FILE}" ]]; then
  run date +%s > "${EXECUTION_TRACKER_FILE}"
fi

# Track the main conversation ID for this CLI PID
MAIN_CONV_FILE="/tmp/jetski_cli_main_${PPID}.txt"
if [[ -f "${HOME}/.gemini/jetski/annotations/${conversation_id}.pbtxt" ]] || ! [[ -f "${MAIN_CONV_FILE}" ]]; then
  echo "${conversation_id}" > "${MAIN_CONV_FILE}"
fi

INVOCATION_TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${execution_id}_${invocation_num}.txt"
log::debug "INVOCATION_TRACKER_FILE: ${INVOCATION_TRACKER_FILE}"
if ! [[ -f "${INVOCATION_TRACKER_FILE}" ]]; then
  run date +%s > "${INVOCATION_TRACKER_FILE}"
fi

echo '{"injectSteps": []}'
