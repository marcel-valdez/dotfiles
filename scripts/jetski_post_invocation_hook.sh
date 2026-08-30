#!/usr/bin/env bash
# jetski_post_invocation_hook.sh: PostInvocation hook for Jetski agents.
# Monitors long-running invocation turns (>=300s) and emits strict output contract.

export LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"
source "${HOME}/scripts/jetski_hook_utils.sh"

read -r -d '' PAYLOAD
log::info "PAYLOAD: $(printf '%s' "${PAYLOAD}" | run jq -c . 2>/dev/null)"

{
  read -r conversation_id
  read -r execution_id
  read -r invocation_num
  read -r workspace_path
} < <(
  printf '%s' "${PAYLOAD}" | run jq -r '
    (.conversationId // ""),
    (.executionId // ""),
    ((.invocationNum // 0) | tostring),
    (.workspacePaths[0] // "")
  ' 2>/dev/null
)
workspace_dir="$(basename "${workspace_path}")"
state_dir=$(get_session_state_dir "${JETSKI_PPID:-${PPID}}")
now=$(get_now_seconds)

step_tracker="${state_dir}/invocation_step_${execution_id}_${invocation_num}.txt"
start_time=$(read_numeric_state "${step_tracker}")

if [[ -n "${start_time}" && "${start_time}" =~ ^[0-9]+$ ]]; then
  elapsed=$((now - start_time))
  log::debug "invocation step elapsed: ${elapsed}"
  if [[ "${elapsed}" -ge "${JETSKI_TOOL_NOTIFY_THRESHOLD_SECS}" ]]; then
    populate_tmux_info "${JETSKI_PPID:-${PPID}}"
    title="Jetski Long Invocation"
    msg=$(cat <<EOF

Jetski CLI Tool Invocation on ${workspace_dir} done
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Elapsed: ${elapsed}s
EOF
)
    dispatch_notification "${title}" "${msg}" "normal"
  fi
fi

echo '{"injectSteps": [], "terminationBehavior": ""}'
exit 0
