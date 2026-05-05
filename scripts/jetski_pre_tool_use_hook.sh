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

# Triggers it ASAP
NOTIFICATION_THRESHOLD_SECS=0
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# https://g3doc.corp.google.com/devtools/jetski/g3doc/features/agent/agent-hooks.md
read -r -d '' PAYLOAD
log::debug "PAYLOAD: ${PAYLOAD}"
tool_name="$(echo "${PAYLOAD}" | run jq -r '.preToolHookArgs.toolCall.name')"
log::debug "tool_name: ${tool_name}"
tool_args="$(echo "${PAYLOAD}" | run jq -r '.preToolHookArgs.toolCall.args')"
log::debug "tool_args: ${tool_args}"
step_idx="$(echo "${PAYLOAD}" | run jq -r '.preToolHookArgs.stepIdx')"
log::debug "step_idx: ${step_idx}"

TRACKER_FILE="/tmp/jetski_tool_use_req_${PPID}_${step_idx}.txt"
log::debug "TRACKER_FILE: ${TRACKER_FILE}"
TOOL_NAME_FILE="/tmp/jetski_tool_use_req_${PPID}_${step_idx}_tool_name.txt"
log::debug "TOOL_NAME_FILE: ${TOOL_NAME_FILE}"
TOOL_ARGS_FILE="/tmp/jetski_tool_use_req_${PPID}_${step_idx}_tool_args.txt"
log::debug "TOOL_ARGS_FILE: ${TOOL_ARGS_FILE}"
run date +%s > "${TRACKER_FILE}"
echo "${tool_name}" > "${TOOL_NAME_FILE}"
echo "${tool_args}" > "${TOOL_ARGS_FILE}"

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

# Example input:
#{
#  "preToolHookArgs": {
#    "toolCall": {
#      "name": "ask_question",
#      "args": {
#        "questions": [
#          {
#            "is_multi_select": false,
#            "options": [
#              "Edit hooks.json to change the hook command to wait 61 seconds and then trigger it.",
#              "Run a tool (e.g., run_command with sleep 61) to trigger the hook."
#            ],
#            "question": "You asked to trigger the hook again, but have the command be a shell command to wait 61 seconds. How would you like me to proceed?"
#          }
#        ]
#      }
#    },
#    "stepIdx": 51
#  }
#}

APPROVAL_TOOLS_REGX='.*(ask_question).*'
function is_approval_tool {
  echo "$@" | grep -E "${APPROVAL_TOOLS_REGX}" &>/dev/null
}

function get_approval_prompt {
  local question="$(echo "${PAYLOAD}" | run jq -r .preToolHookArgs.toolCall.args.questions[0].question)"
  log::debug "${question}"
  echo "${question}"
}

if [[ -f "${TRACKER_FILE}" ]] && is_approval_tool "${tool_name}"; then
  approval_prompt="$(get_approval_prompt)"
  start_time="$(run cat "${TRACKER_FILE}")"
  end_time=$(run date +%s)
  elapsed=$((end_time-start_time))
  log::debug "elapsed: ${elapsed}"
  if [[ "${elapsed}" -ge "${NOTIFICATION_THRESHOLD_SECS}" ]]; then
    title="Jetski CLI: Start Tool Use"
    populate_tmux_info
    msg=$(cat<<EOF

Jestki CLI Start Tool Use
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
${approval_prompt}
Tool: ${tool_name}
Args: ${tool_args}
Step Idx: ${step_idx}
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

echo '{"allowTool": true}'
