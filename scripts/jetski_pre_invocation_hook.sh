#!/usr/bin/env bash
# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=3
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
# {
#   "invocationHookArgs": {
#     "invocationNum": 1,
#     "initialNumSteps": 28
#   }
# }
invocation_num="$(echo "${PAYLOAD}" | run jq .invocationHookArgs.invocationNum)"
log::debug "invocation_num: ${invocation_num}"
initial_num_steps="$(echo "${PAYLOAD}" | run jq .invocationHookArgs.initialNumSteps)"
log::debug "initial_num_steps: ${initial_num_steps}"
TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${invocation_num}.txt"
log::debug "TRACKER_FILE: ${TRACKER_FILE}"

run date +%s > "${TRACKER_FILE}"

echo '{"injectSteps": []}'
