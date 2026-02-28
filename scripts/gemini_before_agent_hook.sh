#!/usr/bin/env bash
# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/gemini_before_agent_hook.log"
source "${HOME}/scripts/log_lib.sh"

function dispatch {
  debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  debug "run $*"
  "$@"
}

read -r -d '' PAYLOAD
info "PAYLOAD: $(echo "${PAYLOAD}" | run jq --monochrome-output)"
TRACKER_FILE="/tmp/gemini_req_${PPID}.txt"

run date +%s > "${TRACKER_FILE}"

echo '{"decision": "allow"}'
