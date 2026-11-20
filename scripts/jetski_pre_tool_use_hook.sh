#!/usr/bin/env bash
# jetski_pre_tool_use_hook.sh: PreToolUse hook for Jetski agents.
# Dispatches immediate 0s notifications for interactive approval tools.

export LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"
source "${HOME}/scripts/jetski_hook_utils.sh"

read -r -d '' PAYLOAD
log::info "PAYLOAD: $(printf '%s' "${PAYLOAD}" | run jq -c . 2>/dev/null)"

{
  read -r conversation_id
  read -r execution_id
  read -r step_idx
  read -r workspace_path
  read -r tool_name
} < <(
  printf '%s' "${PAYLOAD}" | run jq -r '
    (.conversationId // ""),
    (.executionId // ""),
    ((.stepIdx // 0) | tostring),
    (.workspacePaths[0] // ""),
    (.toolCall.name // "")
  ' 2>/dev/null
)
workspace_dir="$(basename "${workspace_path}")"

log::info "tool_name: ${tool_name} step_idx: ${step_idx}"

if [[ "${tool_name}" =~ (^|:)(ask_question|ask_permission|ask_custom_permission)$ ]]; then
  approval_prompt=$(extract_approval_prompt "${PAYLOAD}")
  populate_tmux_info "${JETSKI_PPID:-${PPID}}"
  title="Jetski Approval Required"
  msg=$(cat <<EOF

Jetski Approval Required on ${workspace_dir}
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
${approval_prompt}
Tool: ${tool_name}
Step Idx: ${step_idx}
EOF
)
  dispatch_notification "${title}" "${msg}" "critical"
fi

echo '{"decision": "allow"}'
exit 0
