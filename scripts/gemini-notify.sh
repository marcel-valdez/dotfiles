#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/gemini_notify_hook.log"
source "${HOME}/lib/log_lib.sh"

function dispatch {
  log::debug "dispatch $*"
  "$@" &>/dev/null & disown
}

function run {
  log::debug "run $*"
  "$@"
}

# Read the incoming JSON context from Gemini CLI
read -r -d '' PAYLOAD
log::info "Processing: $(echo "${PAYLOAD}" | run jq --monochrome-output)"

NOTIFICATION_TYPE="$(echo "${PAYLOAD}" | run jq -r '.notification_type')"
if ! [[ "${NOTIFICATION_TYPE}" == "ToolPermission" ]]; then
  echo "{}"
  exit 0
fi

TOOL_NAME="$(echo "${PAYLOAD}" | run jq -r '.details.command')"
MSG="$(echo "${PAYLOAD}" | run jq -r '.message')"
CWD="$(echo "${PAYLOAD}" | run jq -r '.cwd')"
WORKSPACE="$(run basename "${CWD}")"

if echo "${CWD}" | run grep "/google3" &>/dev/null; then
  GOOGLE3_DIR="$(echo "${CWD}" | run grep -o ".*/google3")"
  WORKSPACE_DIR="$(run dirname "${GOOGLE3_DIR}")"
  WORKSPACE="$(run basename "${WORKSPACE_DIR}")"
fi

# Trigger Knock with the custom message
# Ensure knock.sh is sourced in your shell or the environment where this script runs.
if [[ -z "${MESSAGE}" ]]; then
  MSG="Gemini CLI needs approval for '${TOOL_NAME}'"
fi

if [[ -e /google/bin/releases/knock/knock.sh ]]; then
  # If we don't have access to knock due to missing gcert, skip.
  source /google/bin/releases/knock/knock.sh
  if [[ -n "${GOOGLE3_DIR}" ]]; then
    dispatch knock "[CitC Workspace $WORKSPACE] ${MSG}"
  else
    dispatch knock "[Local ${WORKSPACE}] ${MSG}"
  fi
else
  TITLE="Gemini CLI"
  # Otherwise use local notifications on terminal & desktop.
  notified=
  if run type tmux-notify &>/dev/null; then
    dispatch tmux-notify "${TITLE}" "${MSG}"
    notified=1
  fi
  if [[ -n "${DISPLAY}" ]] && run type notify-send &>/dev/null; then
    dispatch notify-send "${TITLE}" "${MSG}"
    notified=1
  fi
  if [[ -z "${notified}" ]]; then
    log::error "neither tmux-notify nor notify-send where available to notify the user."
  fi
fi

# Return an empty JSON object so Gemini CLI can proceed
echo "{}"
