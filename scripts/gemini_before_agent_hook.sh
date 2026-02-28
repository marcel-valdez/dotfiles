#!/usr/bin/env bash
# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/gemini_before_agent_hook.log"
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
TRACKER_FILE="/tmp/gemini_req_${PPID}.txt"

run date +%s > "${TRACKER_FILE}"

echo '{"decision": "allow"}'
