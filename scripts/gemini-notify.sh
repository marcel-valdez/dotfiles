#!/usr/bin/env bash

# Log settings
export LOG_SCRIPT_NAME=
LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/gemini_after_agent_hook.log"
source "${HOME}/scripts/log_lib.sh"

function dispatch {
  debug "dispatch $*"
  "$@" &>/dev/null & disown
}

# Read the incoming JSON context from Gemini CLI
read -r -d '' PAYLOAD


NOTIFICATION_TYPE="$(echo "${PAYLOAD}" | jq -r '.notification_type')"
if ! [[ "${NOTIFICATION_TYPE}" == "ToolPermission" ]]; then
  echo "{}"
  exit 0
fi


TOOL_NAME="$(echo "${PAYLOAD}" | jq -r '.details.toolName')"
CWD="$(echo "${PAYLOAD}" | jq -r '.cwd')"
WORKSPACE="$(basename "${CWD}")"

if echo "${CWD}" | grep "/google3" &>/dev/null; then
  GOOGLE3_DIR="$(echo "${CWD}" | grep -o ".*/google3")"
  WORKSPACE_DIR="$(dirname "${GOOGLE3_DIR}")"
  WORKSPACE="$(basename "${WORKSPACE_DIR}")"
fi

PARSED_DATA="${TOOL_NAME}|${WORKSPACE}"

# If PARSED_DATA is empty, exit gracefully
if [ -z "${PARSED_DATA}" ]; then
  echo "{}"
  exit 0
fi

# Trigger Knock with the custom message
# Ensure knock.sh is sourced in your shell or the environment where this script runs.
MSG="Gemini CLI needs approval for '${TOOL_NAME}'"
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
  if type tmux-notify &>/dev/null; then
    dispatch tmux-notify "${TITLE}" "${MSG}"
    notified=1
  fi
  if [[ -n "${DISPLAY}" ]] && type notify-send &>/dev/null; then
    dispatch notify-send "${TITLE}" "${MSG}"
    notified=1
  fi
  if [[ -z "${notified}" ]]; then
    error "neither tmux-notify nor notify-send where available to notify the user."
  fi
fi

# Return an empty JSON object so Gemini CLI can proceed
echo "{}"
