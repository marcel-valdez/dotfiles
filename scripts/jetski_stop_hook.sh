#!/usr/bin/env bash
# jetski_stop_hook.sh: Stop hook for Jetski agents.
# Implements dual-tier notification protocol for intermediate vs. final completion,
# routes subagent lifecycles, and executes hermetic session cleanup.

export LOG_SCRIPT_NAME="$(basename "$0")"
[[ -z "${LOG_LEVEL}" ]] && export LOG_LEVEL=2
[[ -z "${LOG_FILE}" ]] && export LOG_FILE="/tmp/jetski_hook.log"
source "${HOME}/scripts/jetski_hook_utils.sh"

read -r -d '' PAYLOAD
log::info "PAYLOAD: $(printf '%s' "${PAYLOAD}" | run jq -c . 2>/dev/null)"

{
  read -r conversation_id
  read -r execution_id
  read -r execution_num
  read -r termination_reason
  read -r error
  read -r fully_idle
  read -r workspace_path
  read -r transcript_path
} < <(
  printf '%s' "${PAYLOAD}" | run jq -r '
    (.conversationId // ""),
    (.executionId // ""),
    ((.executionNum // "") | tostring),
    (.terminationReason // ""),
    (.error // ""),
    ((.fullyIdle // false) | tostring),
    (.workspacePaths[0] // ""),
    (.transcriptPath // "")
  ' 2>/dev/null
)

workspace_dir="$(basename "${workspace_path}")"
state_dir=$(get_session_state_dir "${JETSKI_PPID:-${PPID}}")
now=$(get_now_seconds)

log::info "Stop hook: cid=${conversation_id} exec=${execution_id} fully_idle=${fully_idle} error=${error}"

# Route Subagent vs Main Agent
if is_subagent_conversation "${conversation_id}" "${transcript_path}" "${JETSKI_PPID:-${PPID}}"; then
  log::info "Handling stop event for subagent: ${conversation_id}"
  if [[ -n "${error}" ]]; then
    populate_tmux_info "${JETSKI_PPID:-${PPID}}"
    title="Jetski Subagent Error"
    msg=$(cat <<EOF

Jetski Subagent Error on ${workspace_dir}
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Termination Reason: ${termination_reason}
Error: ${error}
EOF
)
    dispatch_notification "${title}" "${msg}" "critical"
  elif [[ -n "${TMUX:-}" ]]; then
    populate_tmux_info "${JETSKI_PPID:-${PPID}}"
    target="${TARGET_PANE:-${TMUX_SESSION}}"
    status_msg=""
    if [[ "${target}" != "Unknown" && -n "${target}" ]]; then
      status_msg="Jetski [${workspace_dir}]: Sub-agent turn finished (${TMUX_WINDOW})"
      dispatch tmux display-message -t "${target}" -d 1000 "${status_msg}"
    else
      status_msg="Jetski [${workspace_dir}]: Sub-agent turn finished"
      dispatch tmux display-message -d 1000 "${status_msg}"
    fi
    if [[ -n "${JETSKI_TEST_TMUX_LOG:-}" ]]; then
      printf 'TMUX_MSG\t%s\t%s\n' "${target}" "${status_msg}" >> "${JETSKI_TEST_TMUX_LOG}"
    fi
  fi

  # Purge turn tracker files for this subagent execution
  rm -f "${state_dir}/invocation_req_${execution_id}.txt" "${state_dir}/invocation_step_${execution_id}_"*.txt
  echo '{"decision": "", "reason": ""}'
  exit 0
fi

# Main Agent: Intermediate yield vs Final completion
if [[ "${fully_idle}" != "true" && -z "${error}" ]]; then
  log::info "Main agent intermediate yield (fully_idle=${fully_idle})"
  if [[ -n "${TMUX:-}" ]]; then
    populate_tmux_info "${JETSKI_PPID:-${PPID}}"
    target="${TARGET_PANE:-${TMUX_SESSION}}"
    status_msg=""
    if [[ "${target}" != "Unknown" && -n "${target}" ]]; then
      status_msg="Jetski [${workspace_dir}]: Step completed, continuing in background (${TMUX_WINDOW})"
      dispatch tmux display-message -t "${target}" -d 1000 "${status_msg}"
    else
      status_msg="Jetski [${workspace_dir}]: Step completed, continuing in background"
      dispatch tmux display-message -d 1000 "${status_msg}"
    fi
    if [[ -n "${JETSKI_TEST_TMUX_LOG:-}" ]]; then
      printf 'TMUX_MSG\t%s\t%s\n' "${target}" "${status_msg}" >> "${JETSKI_TEST_TMUX_LOG}"
    fi
  fi
  # Purge intermediate turn trackers, but preserve request_start_${conversation_id}.txt
  rm -f "${state_dir}/invocation_req_${execution_id}.txt" "${state_dir}/invocation_step_${execution_id}_"*.txt
  echo '{"decision": "", "reason": ""}'
  exit 0
fi

# Main Agent: Final Completion or Fatal Error
start_time=""
read -r req_start_time _ < <(read_request_origin_state "${state_dir}/request_start_${conversation_id}.txt")
if [[ -n "${req_start_time}" && "${req_start_time}" =~ ^[0-9]+$ ]]; then
  start_time="${req_start_time}"
else
  exec_start_time=$(read_numeric_state "${state_dir}/invocation_req_${execution_id}.txt")
  if [[ -n "${exec_start_time}" && "${exec_start_time}" =~ ^[0-9]+$ ]]; then
    start_time="${exec_start_time}"
  fi
fi

if [[ -n "${start_time}" && "${start_time}" =~ ^[0-9]+$ ]]; then
  total_elapsed=$((now - start_time))
else
  total_elapsed=0
fi
log::debug "total_elapsed: ${total_elapsed}"

if [[ "${total_elapsed}" -ge "${JETSKI_STOP_NOTIFY_THRESHOLD_SECS}" ]] || [[ -n "${error}" ]]; then
  populate_tmux_info "${JETSKI_PPID:-${PPID}}"
  if [[ -n "${error}" ]]; then
    title="Jetski CLI: Error"
    msg=$(cat <<EOF

Jetski CLI Error on ${workspace_dir}
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Termination Reason: ${termination_reason}
Error: ${error}
EOF
)
    dispatch_notification "${title}" "${msg}" "critical"
  else
    title="Jetski CLI: Response Ready"
    msg=$(cat <<EOF

Jetski CLI response ready on ${workspace_dir}
Tmux Session: ${TMUX_SESSION} Window: ${TMUX_WINDOW}
Elapsed: ${total_elapsed}s
EOF
)
    dispatch_notification "${title}" "${msg}" "normal"
  fi
fi

# Scoped Session Cleanup
rm -f "${state_dir}/invocation_req_"*.txt \
      "${state_dir}/invocation_step_"*.txt \
      "${state_dir}/request_start_${conversation_id}.txt"

# Age-gated background dead PID pruning
prune_dead_sessions & disown

echo '{"decision": "", "reason": ""}'
exit 0
