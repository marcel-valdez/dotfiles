#!/usr/bin/env bash
# jetski_pre_invocation_hook.sh: PreInvocation hook for Jetski agents.
# Initializes request origin start times and per-turn trackers.

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
  read -r transcript_path
} < <(
  printf '%s' "${PAYLOAD}" | run jq -r '
    (.conversationId // ""),
    (.executionId // ""),
    ((.invocationNum // 0) | tostring),
    (.workspacePaths[0] // ""),
    (.transcriptPath // "")
  ' 2>/dev/null
)

prompt_hash=$(printf '%s' "${PAYLOAD}" | run jq -r '(.lastUserInput // "")' 2>/dev/null | md5sum | awk '{print $1}')
last_user_input_len=$(printf '%s' "${PAYLOAD}" | run jq -r '(.lastUserInput // "") | length' 2>/dev/null)
[[ -z "${last_user_input_len}" ]] && last_user_input_len=0

log::info "conversation_id: ${conversation_id} execution_id: ${execution_id} invocation_num: ${invocation_num} last_user_input_len: ${last_user_input_len}"

state_dir=$(get_session_state_dir "${JETSKI_PPID:-${PPID}}")
now=$(get_now_seconds)

write_atomic_state "${state_dir}/invocation_req_${execution_id}.txt" "${now}"
write_atomic_state "${state_dir}/invocation_step_${execution_id}_${invocation_num}.txt" "${now}"

if [[ ! -f "${state_dir}/cli_main.txt" ]] || { [[ "${last_user_input_len}" -gt 0 ]] && [[ ! "${transcript_path}" =~ /subagents?/ ]]; }; then
  init_cli_main_session "${conversation_id}" "${transcript_path}" "${JETSKI_PPID:-${PPID}}"
fi

if is_main_conversation "${conversation_id}" "${transcript_path}" "${JETSKI_PPID:-${PPID}}"; then
  req_tracker="${state_dir}/request_start_${conversation_id}.txt"
  read -r stored_time stored_hash < <(read_request_origin_state "${req_tracker}")
  if [[ "${last_user_input_len}" -gt 0 ]] && { [[ -z "${stored_hash}" ]] || [[ "${stored_hash}" != "${prompt_hash}" ]] || [[ ! -f "${req_tracker}" ]]; }; then
    write_atomic_state "${req_tracker}" "${now} ${prompt_hash}"
  fi
fi

echo '{"injectSteps": []}'
exit 0
