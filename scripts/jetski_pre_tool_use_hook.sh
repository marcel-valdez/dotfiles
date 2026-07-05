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

# Threshold of time elapsed since the *prompt* began execution to decide if a
# notification should be sent to the user.
NOTIFICATION_THRESHOLD_SECS=10
TMUX_SESSION="Unknown"
TMUX_WINDOW="Unknown"
# https://g3doc.corp.google.com/devtools/jetski/g3doc/features/agent/agent-hooks.md
read -r -d '' PAYLOAD
log::debug "PAYLOAD: $(echo "${PAYLOAD}" | run jq --monochrome-output)"
# Example:
# {
#   "artifactDirectoryPath":"/usr/local/google/home/marcelvaldez/.gemini/jetski/brain/f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#   "conversationId":"f13b6cc2-f2a7-4aac-8590-aa1a8db311b6",
#   "executionId":"992720db-22af-44df-a982-192a45415ad4",
#   "modelName":"auto",
#   "stepIdx":56,
#   "toolCall":{
#     "args":{
#       "Message":"<message contents>",
#       "Recipient":"fbb2007e-2f15-4db3-a828-0ef66400b785"
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
tool_name="$(echo "${PAYLOAD}" | run jq -r '.toolCall.name')"
log::info "tool_name: ${tool_name}"
tool_args="$(echo "${PAYLOAD}" | run jq -r '.toolCall.args')"
log::info "tool_args: ${tool_args}"

EXECUTION_TRACKER_FILE="/tmp/jetski_invocation_req_${PPID}_${execution_id}.txt"
log::debug "EXECUTION_TRACKER_FILE: ${EXECUTION_TRACKER_FILE}"
TOOL_TRACKER_FILE="/tmp/jetski_tool_use_req_${PPID}_${execution_id}_${step_idx}.txt"
log::debug "TOOL_TRACKER_FILE: ${TOOL_TRACKER_FILE}"
run date +%s > "${TOOL_TRACKER_FILE}"

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

# Example input for question:
#  {
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

APPROVAL_TOOLS_REGX='.*(ask_question).*'
function is_approval_tool {
  echo "$@" | grep -E "${APPROVAL_TOOLS_REGX}" &>/dev/null
}

function get_approval_prompt {
  local question=
  question="$(echo "${PAYLOAD}" | run jq -r .toolCall.args.questions[0].question)"
  log::debug "question: ${question}"
  echo "${question}"
}


if is_approval_tool "${tool_name}"; then
  approval_prompt="$(get_approval_prompt)"
  # Force notification if we can't measure elapsed time
  elapsed=$((NOTIFICATION_THRESHOLD_SECS+1))
  if [[ -f "${EXECUTION_TRACKER_FILE}" ]]; then
      start_time="$(run cat "${EXECUTION_TRACKER_FILE}")"
      end_time=$(run date +%s)
      elapsed=$((end_time-start_time))
  fi
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
