#!/usr/bin/env bash
# test_jetski_hooks.sh: Comprehensive unit test suite for Jetski hooks.
# Covers TC-1 through TC-19 with deterministic mock time and session isolation.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export HOME="${HOME:-/usr/local/google/home/marcelvaldez}"
export LOG_LEVEL=2
export LOG_TO_STDERR=""
export DEBUG=""
export LOG_FILE="/tmp/jetski_test_run_$$.log"
export JETSKI_TEST_NOTIFY_LOG="/tmp/jetski_test_notify_$$.log"
export JETSKI_TEST_TMUX_LOG="/tmp/jetski_test_tmux_$$.log"
export JETSKI_ENABLE_OSC99="false" # Disable OSC99 dispatch to avoid tty noise during tests

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

function cleanup_test_artifacts {
  rm -f "${LOG_FILE}" "${JETSKI_TEST_NOTIFY_LOG}" "${JETSKI_TEST_TMUX_LOG}" /tmp/mock_target_tty_$$* /tmp/jetski_test_*
  rm -rf /tmp/jetski_${UID}_900* /tmp/jetski_${UID}_99999* /tmp/mock_brain_$$ /tmp/jetski_${UID}_active_*
}
trap cleanup_test_artifacts EXIT

function pass {
  local name="$1"
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
  PASSED_TESTS=$((PASSED_TESTS + 1))
  printf "  [PASS] %s\n" "${name}"
}

function fail {
  local name="$1"
  local reason="${2:-}"
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
  FAILED_TESTS=$((FAILED_TESTS + 1))
  printf "  [FAIL] %s: %s\n" "${name}" "${reason}"
}

function assert_eq {
  local expected="$1"
  local actual="$2"
  local test_name="$3"
  if [[ "${expected}" == "${actual}" ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "expected '${expected}', got '${actual}'"
  fi
}

function assert_file_exists {
  local file="$1"
  local test_name="$2"
  if [[ -f "${file}" ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "file does not exist: ${file}"
  fi
}

function assert_file_not_exists {
  local file="$1"
  local test_name="$2"
  if [[ ! -f "${file}" ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "file unexpectedly exists: ${file}"
  fi
}

function assert_dir_exists {
  local dir="$1"
  local test_name="$2"
  if [[ -d "${dir}" ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "directory does not exist: ${dir}"
  fi
}

function assert_dir_not_exists {
  local dir="$1"
  local test_name="$2"
  if [[ ! -d "${dir}" ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "directory unexpectedly exists: ${dir}"
  fi
}

function assert_contains {
  local needle="$1"
  local haystack="$2"
  local test_name="$3"
  if [[ "${haystack}" == *"${needle}"* ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "'${haystack}' does not contain '${needle}'"
  fi
}

function assert_not_contains {
  local needle="$1"
  local haystack="$2"
  local test_name="$3"
  if [[ "${haystack}" != *"${needle}"* ]]; then
    pass "${test_name}"
  else
    fail "${test_name}" "'${haystack}' unexpectedly contains '${needle}'"
  fi
}

function reset_logs {
  : > "${JETSKI_TEST_NOTIFY_LOG}"
  : > "${JETSKI_TEST_TMUX_LOG}"
}

function get_notify_log {
  if [[ -f "${JETSKI_TEST_NOTIFY_LOG}" ]]; then
    cat "${JETSKI_TEST_NOTIFY_LOG}"
  else
    echo ""
  fi
}

function get_tmux_log {
  if [[ -f "${JETSKI_TEST_TMUX_LOG}" ]]; then
    cat "${JETSKI_TEST_TMUX_LOG}"
  else
    echo ""
  fi
}

# Helper to invoke hooks in a subshell with a specific JETSKI_PPID and mock time
function invoke_pre_invocation {
  local ppid="$1"
  local mock_now="$2"
  local payload="$3"
  (
    export JETSKI_PPID="${ppid}"
    export JETSKI_MOCK_NOW="${mock_now}"
    printf '%s' "${payload}" | bash "${SCRIPT_DIR}/jetski_pre_invocation_hook.sh"
  )
}

function invoke_pre_tool_use {
  local ppid="$1"
  local mock_now="$2"
  local payload="$3"
  (
    export JETSKI_PPID="${ppid}"
    export JETSKI_MOCK_NOW="${mock_now}"
    printf '%s' "${payload}" | bash "${SCRIPT_DIR}/jetski_pre_tool_use_hook.sh"
  )
}

function invoke_post_invocation {
  local ppid="$1"
  local mock_now="$2"
  local payload="$3"
  (
    export JETSKI_PPID="${ppid}"
    export JETSKI_MOCK_NOW="${mock_now}"
    printf '%s' "${payload}" | bash "${SCRIPT_DIR}/jetski_post_invocation_hook.sh"
  )
}

function invoke_stop {
  local ppid="$1"
  local mock_now="$2"
  local payload="$3"
  (
    export JETSKI_PPID="${ppid}"
    export JETSKI_MOCK_NOW="${mock_now}"
    printf '%s' "${payload}" | bash "${SCRIPT_DIR}/jetski_stop_hook.sh"
  )
}

printf "========================================\n"
printf "Running Jetski Hooks Unit Test Suite\n"
printf "========================================\n\n"

# ---------------------------------------------------------
# TC-1: Fast Synchronous Turn (< 60s)
# ---------------------------------------------------------
printf "Test Case 1: Fast Synchronous Turn (< 60s)\n"
reset_logs
PPID_1=90001
CID_1="cid-tc1"
EXEC_1="exec-tc1"
PAYLOAD_PRE_1=$(jq -n --arg cid "${CID_1}" --arg exec "${EXEC_1}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 0,
  lastUserInput: "quick request",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')
PAYLOAD_STOP_1=$(jq -n --arg cid "${CID_1}" --arg exec "${EXEC_1}" '{
  conversationId: $cid,
  executionId: $exec,
  executionNum: 0,
  fullyIdle: true,
  error: "",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')

OUT_PRE_1=$(invoke_pre_invocation "${PPID_1}" 1000 "${PAYLOAD_PRE_1}")
assert_eq '{"injectSteps": []}' "${OUT_PRE_1}" "TC-1: PreInvocation stdout contract"

STATE_DIR_1="/tmp/jetski_${UID}_${PPID_1}"
assert_file_exists "${STATE_DIR_1}/request_start_${CID_1}.txt" "TC-1: request_start exists before stop"
assert_file_exists "${STATE_DIR_1}/invocation_req_${EXEC_1}.txt" "TC-1: turn tracker exists before stop"

OUT_STOP_1=$(invoke_stop "${PPID_1}" 1010 "${PAYLOAD_STOP_1}") # Elapsed = 10s < 60s
assert_eq '{"decision": "", "reason": ""}' "${OUT_STOP_1}" "TC-1: StopHook stdout contract"

NOTIFY_1=$(get_notify_log)
assert_eq "" "${NOTIFY_1}" "TC-1: No notification triggered for fast turn"
assert_file_not_exists "${STATE_DIR_1}/request_start_${CID_1}.txt" "TC-1: request_start purged"
assert_file_not_exists "${STATE_DIR_1}/invocation_req_${EXEC_1}.txt" "TC-1: invocation_req purged"

# ---------------------------------------------------------
# TC-2: Long Synchronous Turn (>= 60s)
# ---------------------------------------------------------
printf "\nTest Case 2: Long Synchronous Turn (>= 60s)\n"
reset_logs
PPID_2=90002
CID_2="cid-tc2"
EXEC_2="exec-tc2"
PAYLOAD_PRE_2=$(jq -n --arg cid "${CID_2}" --arg exec "${EXEC_2}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 0,
  lastUserInput: "long running build",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')
PAYLOAD_STOP_2=$(jq -n --arg cid "${CID_2}" --arg exec "${EXEC_2}" '{
  conversationId: $cid,
  executionId: $exec,
  executionNum: 0,
  fullyIdle: true,
  error: "",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')

invoke_pre_invocation "${PPID_2}" 1000 "${PAYLOAD_PRE_2}" >/dev/null
invoke_stop "${PPID_2}" 1070 "${PAYLOAD_STOP_2}" >/dev/null # Elapsed = 70s >= 60s

NOTIFY_2=$(get_notify_log)
assert_contains "Jetski CLI: Response Ready" "${NOTIFY_2}" "TC-2: Response ready notification triggered"
assert_contains "Elapsed: 70s" "${NOTIFY_2}" "TC-2: Total elapsed correctly calculated"
assert_file_not_exists "/tmp/jetski_${UID}_${PPID_2}/request_start_${CID_2}.txt" "TC-2: Session trackers purged"

# ---------------------------------------------------------
# TC-3: Single Async Task / Timer (>= 60s)
# ---------------------------------------------------------
printf "\nTest Case 3: Single Async Task / Timer (>= 60s)\n"
reset_logs
PPID_3=90003
CID_3="cid-tc3"
EXEC_3_1="exec-tc3-turn1"
EXEC_3_2="exec-tc3-turn2"

# Turn 1: User prompt schedules async task, ends with fullyIdle: false
PAYLOAD_PRE_3_1=$(jq -n --arg cid "${CID_3}" --arg exec "${EXEC_3_1}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 0,
  lastUserInput: "schedule timer for 65s",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')
PAYLOAD_STOP_3_1=$(jq -n --arg cid "${CID_3}" --arg exec "${EXEC_3_1}" '{
  conversationId: $cid,
  executionId: $exec,
  executionNum: 0,
  fullyIdle: false,
  error: "",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')

invoke_pre_invocation "${PPID_3}" 1000 "${PAYLOAD_PRE_3_1}" >/dev/null
invoke_stop "${PPID_3}" 1002 "${PAYLOAD_STOP_3_1}" >/dev/null

assert_eq "" "$(get_notify_log)" "TC-3 Turn 1: No popup notification on intermediate yield"
STATE_DIR_3="/tmp/jetski_${UID}_${PPID_3}"
assert_file_exists "${STATE_DIR_3}/request_start_${CID_3}.txt" "TC-3: T0 preserved across intermediate turn"
assert_file_not_exists "${STATE_DIR_3}/invocation_req_${EXEC_3_1}.txt" "TC-3: Turn 1 tracker purged"

# Turn 2: Reactive wakeup 65s later with empty lastUserInput, ends with fullyIdle: true
PAYLOAD_PRE_3_2=$(jq -n --arg cid "${CID_3}" --arg exec "${EXEC_3_2}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 0,
  lastUserInput: "",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')
PAYLOAD_STOP_3_2=$(jq -n --arg cid "${CID_3}" --arg exec "${EXEC_3_2}" '{
  conversationId: $cid,
  executionId: $exec,
  executionNum: 0,
  fullyIdle: true,
  error: "",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')

invoke_pre_invocation "${PPID_3}" 1065 "${PAYLOAD_PRE_3_2}" >/dev/null
invoke_stop "${PPID_3}" 1067 "${PAYLOAD_STOP_3_2}" >/dev/null # Total elapsed = 1067 - 1000 = 67s

NOTIFY_3=$(get_notify_log)
assert_contains "Jetski CLI: Response Ready" "${NOTIFY_3}" "TC-3 Turn 2: Full notification triggered on final completion"
assert_contains "Elapsed: 67s" "${NOTIFY_3}" "TC-3: Elapsed accurately includes async wait duration"
assert_file_not_exists "${STATE_DIR_3}/request_start_${CID_3}.txt" "TC-3: request_start purged after final completion"

# ---------------------------------------------------------
# TC-4: Chained Sequential Async Tasks (3 x 65s)
# ---------------------------------------------------------
printf "\nTest Case 4: Chained Sequential Async Tasks (3 x 65s)\n"
reset_logs
PPID_4=90004
CID_4="cid-tc4"
STATE_DIR_4="/tmp/jetski_${UID}_${PPID_4}"

# Turn 1 (0s -> 2s, dispatches Task 1)
invoke_pre_invocation "${PPID_4}" 1000 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-1", invocationNum: 0, lastUserInput: "chain 3 tasks", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_4}" 1002 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-1", fullyIdle: false, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

# Turn 2 (65s -> 67s, ingests Task 1, dispatches Task 2)
invoke_pre_invocation "${PPID_4}" 1065 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-2", invocationNum: 0, lastUserInput: "", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_4}" 1067 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-2", fullyIdle: false, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

# Turn 3 (130s -> 132s, ingests Task 2, dispatches Task 3)
invoke_pre_invocation "${PPID_4}" 1130 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-3", invocationNum: 0, lastUserInput: "", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_4}" 1132 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-3", fullyIdle: false, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

assert_eq "" "$(get_notify_log)" "TC-4: Zero popups during intermediate turns 1-3"

# Turn 4 (195s -> 198s, ingests Task 3, delivers FINAL response)
invoke_pre_invocation "${PPID_4}" 1195 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-4", invocationNum: 0, lastUserInput: "", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_4}" 1198 "$(jq -n --arg cid "${CID_4}" '{conversationId: $cid, executionId: "exec-4-4", fullyIdle: true, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

NOTIFY_4=$(get_notify_log)
assert_contains "Jetski CLI: Response Ready" "${NOTIFY_4}" "TC-4: Final response triggered notification"
assert_contains "Elapsed: 198s" "${NOTIFY_4}" "TC-4: Total elapsed reflects all 3 chained async tasks (198s)"
assert_file_not_exists "${STATE_DIR_4}/request_start_${CID_4}.txt" "TC-4: Cleaned up request origin"

# ---------------------------------------------------------
# TC-5: Multi-Turn Subagent Chain (>= 60s)
# ---------------------------------------------------------
printf "\nTest Case 5: Multi-Turn Subagent Chain (>= 60s)\n"
reset_logs
PPID_5=90005
MAIN_CID_5="main-cid-5"
SUB_CID_5="sub-cid-5"
STATE_DIR_5="/tmp/jetski_${UID}_${PPID_5}"

# Step 0: Ensure a subagent running before any main agent prompt does NOT create cli_main.txt
SUB_TRANSCRIPT_EARLY="/tmp/brain/${MAIN_CID_5}/subagents/sub-early/transcript.jsonl"
invoke_pre_invocation "${PPID_5}" 990 "$(jq -n --arg cid "sub-early" --arg tp "${SUB_TRANSCRIPT_EARLY}" '{conversationId: $cid, executionId: "sub-early-exec", invocationNum: 0, lastUserInput: "early subagent task", workspacePaths: ["/tmp/workspace"], transcriptPath: $tp}')" >/dev/null
assert_file_not_exists "${STATE_DIR_5}/cli_main.txt" "TC-5: Early subagent did not create cli_main.txt"

# Main agent starts at T=1000
invoke_pre_invocation "${PPID_5}" 1000 "$(jq -n --arg cid "${MAIN_CID_5}" '{conversationId: $cid, executionId: "main-exec-1", invocationNum: 0, lastUserInput: "spawn subagent", workspacePaths: ["/tmp/workspace"], transcriptPath: "/tmp/transcript.jsonl"}')" >/dev/null

# Subagent runs multiple turns
SUB_TRANSCRIPT="/tmp/brain/${MAIN_CID_5}/subagents/${SUB_CID_5}/transcript.jsonl"
invoke_pre_invocation "${PPID_5}" 1005 "$(jq -n --arg cid "${SUB_CID_5}" --arg tp "${SUB_TRANSCRIPT}" '{conversationId: $cid, executionId: "sub-exec-1", invocationNum: 0, lastUserInput: "subagent task", workspacePaths: ["/tmp/workspace"], transcriptPath: $tp}')" >/dev/null

# Verify cli_main.txt was NOT overwritten by subagent
MAIN_TRACKED=$(cat "${STATE_DIR_5}/cli_main.txt")
assert_eq "${MAIN_CID_5}" "${MAIN_TRACKED}" "TC-5: Subagent did not overwrite cli_main.txt"

# Subagent finishes turn after running for 95 seconds (T=1005 to T=1100, elapsed = 95s >= 60s)
invoke_stop "${PPID_5}" 1100 "$(jq -n --arg cid "${SUB_CID_5}" --arg tp "${SUB_TRANSCRIPT}" '{conversationId: $cid, executionId: "sub-exec-1", fullyIdle: true, workspacePaths: ["/tmp/workspace"], transcriptPath: $tp}')" >/dev/null
assert_eq "" "$(get_notify_log)" "TC-5: Long-running subagent (95s >= 60s) stop did not trigger full notification popup"

# Main agent request start tracker is still preserved
assert_file_exists "${STATE_DIR_5}/request_start_${MAIN_CID_5}.txt" "TC-5: Main agent request origin tracker preserved during subagent execution"

# Main agent finishes at T=1120 (Elapsed = 120s)
invoke_stop "${PPID_5}" 1120 "$(jq -n --arg cid "${MAIN_CID_5}" '{conversationId: $cid, executionId: "main-exec-1", fullyIdle: true, workspacePaths: ["/tmp/workspace"], transcriptPath: "/tmp/transcript.jsonl"}')" >/dev/null

NOTIFY_5=$(get_notify_log)
assert_contains "Jetski CLI: Response Ready" "${NOTIFY_5}" "TC-5: Main agent triggered notification on completion"
assert_contains "Elapsed: 120s" "${NOTIFY_5}" "TC-5: Main agent duration measured correctly"

# ---------------------------------------------------------
# TC-6: Interactive Approval After Async Wakeup
# ---------------------------------------------------------
printf "\nTest Case 6: Interactive Approval After Async Wakeup\n"
reset_logs
PPID_6=90006
CID_6="cid-tc6"

PAYLOAD_TOOL_6=$(jq -n --arg cid "${CID_6}" '{
  conversationId: $cid,
  executionId: "exec-6",
  stepIdx: 12,
  workspacePaths: ["/google/src/cloud/user/ws"],
  toolCall: {
    name: "default_api:ask_question",
    args: {
      questions: [
        {question: "Should I proceed with option A?"},
        {question: "Or option B?"}
      ]
    }
  }
}')

OUT_TOOL_6=$(invoke_pre_tool_use "${PPID_6}" 1065 "${PAYLOAD_TOOL_6}")
assert_eq '{"decision": "allow"}' "${OUT_TOOL_6}" "TC-6: PreToolUse stdout contract"

NOTIFY_6=$(get_notify_log)
assert_contains "Jetski Approval Required" "${NOTIFY_6}" "TC-6: Immediate 0s approval notification triggered"
assert_contains "Should I proceed with option A? | Or option B?" "${NOTIFY_6}" "TC-6: Polymorphic questions joined correctly"

# ---------------------------------------------------------
# TC-7: Repeated Identical User Prompts
# ---------------------------------------------------------
printf "\nTest Case 7: Repeated Identical User Prompts\n"
reset_logs
PPID_7=90007
CID_7="cid-tc7"

# Turn 1: "continue" at 1000, finishes at 1005
invoke_pre_invocation "${PPID_7}" 1000 "$(jq -n --arg cid "${CID_7}" '{conversationId: $cid, executionId: "exec-7-1", invocationNum: 0, lastUserInput: "continue", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_7}" 1005 "$(jq -n --arg cid "${CID_7}" '{conversationId: $cid, executionId: "exec-7-1", fullyIdle: true, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

assert_eq "" "$(get_notify_log)" "TC-7 Turn 1: No notification for 5s turn"

# Turn 2: "continue" again at 1010, finishes at 1015 (5s elapsed)
invoke_pre_invocation "${PPID_7}" 1010 "$(jq -n --arg cid "${CID_7}" '{conversationId: $cid, executionId: "exec-7-2", invocationNum: 0, lastUserInput: "continue", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
invoke_stop "${PPID_7}" 1015 "$(jq -n --arg cid "${CID_7}" '{conversationId: $cid, executionId: "exec-7-2", fullyIdle: true, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

assert_eq "" "$(get_notify_log)" "TC-7 Turn 2: Second turn did not falsely accumulate previous turn duration"

# ---------------------------------------------------------
# TC-8: Reactive Wakeup with Null/Empty lastUserInput
# ---------------------------------------------------------
printf "\nTest Case 8: Reactive Wakeup with Null/Empty lastUserInput\n"
reset_logs
PPID_8=90008
CID_8="cid-tc8"
STATE_DIR_8="/tmp/jetski_${UID}_${PPID_8}"

# Turn 1: User prompt at T=1000
invoke_pre_invocation "${PPID_8}" 1000 "$(jq -n --arg cid "${CID_8}" '{conversationId: $cid, executionId: "exec-8-1", invocationNum: 0, lastUserInput: "start timer", workspacePaths: ["/tmp/workspace"]}')" >/dev/null

# Reactive wakeup at T=1060 with null lastUserInput
invoke_pre_invocation "${PPID_8}" 1060 "$(jq -n --arg cid "${CID_8}" '{conversationId: $cid, executionId: "exec-8-2", invocationNum: 0, lastUserInput: null, workspacePaths: ["/tmp/workspace"]}')" >/dev/null

REQ_TRACKER_8="${STATE_DIR_8}/request_start_${CID_8}.txt"
assert_file_exists "${REQ_TRACKER_8}" "TC-8: request_start file exists"
ST_8=$(awk '{print $1}' "${REQ_TRACKER_8}")
assert_eq "1000" "${ST_8}" "TC-8: T0 preserved as 1000 despite null lastUserInput"

# ---------------------------------------------------------
# TC-9: Corrupted/Empty Tracker Recovery
# ---------------------------------------------------------
printf "\nTest Case 9: Corrupted/Empty Tracker Recovery\n"
reset_logs
PPID_9=90009
CID_9="cid-tc9"
EXEC_9="exec-tc9"
STATE_DIR_9="/tmp/jetski_${UID}_${PPID_9}"
mkdir -p "${STATE_DIR_9}"

# Inject corrupted files
echo "CORRUPTED_NOT_A_NUMBER abc123" > "${STATE_DIR_9}/request_start_${CID_9}.txt"
: > "${STATE_DIR_9}/invocation_req_${EXEC_9}.txt"

PAYLOAD_STOP_9=$(jq -n --arg cid "${CID_9}" --arg exec "${EXEC_9}" '{
  conversationId: $cid,
  executionId: $exec,
  fullyIdle: true,
  error: "",
  workspacePaths: ["/tmp/workspace"]
}')

OUT_STOP_9=$(invoke_stop "${PPID_9}" 1010 "${PAYLOAD_STOP_9}")
assert_eq '{"decision": "", "reason": ""}' "${OUT_STOP_9}" "TC-9: StopHook handles corrupted tracker gracefully without bash syntax error"

# ---------------------------------------------------------
# TC-10: Long Invocation Turn Alert (>= 300s)
# ---------------------------------------------------------
printf "\nTest Case 10: Long Invocation Turn Alert (>= 300s)\n"
reset_logs
PPID_10=90010
CID_10="cid-tc10"
EXEC_10="exec-tc10"

PAYLOAD_PRE_10=$(jq -n --arg cid "${CID_10}" --arg exec "${EXEC_10}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 4,
  lastUserInput: "heavy compilation",
  workspacePaths: ["/tmp/workspace"]
}')
PAYLOAD_POST_10=$(jq -n --arg cid "${CID_10}" --arg exec "${EXEC_10}" '{
  conversationId: $cid,
  executionId: $exec,
  invocationNum: 4,
  workspacePaths: ["/tmp/workspace"]
}')

invoke_pre_invocation "${PPID_10}" 1000 "${PAYLOAD_PRE_10}" >/dev/null
OUT_POST_10=$(invoke_post_invocation "${PPID_10}" 1350 "${PAYLOAD_POST_10}") # Elapsed = 350s >= 300s
assert_eq '{"injectSteps": [], "terminationBehavior": ""}' "${OUT_POST_10}" "TC-10: PostInvocation stdout contract"

NOTIFY_10=$(get_notify_log)
assert_contains "Jetski Long Invocation" "${NOTIFY_10}" "TC-10: Long invocation notification triggered"
assert_contains "Elapsed: 350s" "${NOTIFY_10}" "TC-10: Elapsed duration in long invocation message"

# ---------------------------------------------------------
# TC-11: Subagent Error Termination
# ---------------------------------------------------------
printf "\nTest Case 11: Subagent Error Termination\n"
reset_logs
PPID_11=90011
CID_11="subagent-error-cid"
SUB_TRANSCRIPT_11="/tmp/brain/main/subagents/${CID_11}/transcript.jsonl"

PAYLOAD_STOP_11=$(jq -n --arg cid "${CID_11}" --arg tp "${SUB_TRANSCRIPT_11}" '{
  conversationId: $cid,
  executionId: "exec-11",
  fullyIdle: true,
  error: "Subagent worker process killed by OOM",
  terminationReason: "ERROR",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: $tp
}')

OUT_STOP_11=$(invoke_stop "${PPID_11}" 1010 "${PAYLOAD_STOP_11}")
assert_eq '{"decision": "", "reason": ""}' "${OUT_STOP_11}" "TC-11: Subagent error stop stdout contract"

NOTIFY_11=$(get_notify_log)
assert_contains "Jetski Subagent Error" "${NOTIFY_11}" "TC-11: Subagent error notification dispatched"
assert_contains "critical" "${NOTIFY_11}" "TC-11: Urgency set to critical"

# ---------------------------------------------------------
# TC-12: Main Agent Error Termination
# ---------------------------------------------------------
printf "\nTest Case 12: Main Agent Error Termination\n"
reset_logs
PPID_12=90012
CID_12="cid-tc12"

PAYLOAD_PRE_12=$(jq -n --arg cid "${CID_12}" '{
  conversationId: $cid,
  executionId: "exec-12",
  invocationNum: 0,
  lastUserInput: "run query",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')
PAYLOAD_STOP_12=$(jq -n --arg cid "${CID_12}" '{
  conversationId: $cid,
  executionId: "exec-12",
  fullyIdle: true,
  error: "Backend RPC DEADLINE_EXCEEDED",
  terminationReason: "ERROR",
  workspacePaths: ["/tmp/workspace"],
  transcriptPath: "/tmp/transcript.jsonl"
}')

invoke_pre_invocation "${PPID_12}" 1000 "${PAYLOAD_PRE_12}" >/dev/null
invoke_stop "${PPID_12}" 1005 "${PAYLOAD_STOP_12}" >/dev/null # 5s elapsed < 60s, but error != ""

NOTIFY_12=$(get_notify_log)
assert_contains "Jetski CLI: Error" "${NOTIFY_12}" "TC-12: Immediate error notification dispatched"
assert_contains "DEADLINE_EXCEEDED" "${NOTIFY_12}" "TC-12: Error details included"
assert_file_not_exists "/tmp/jetski_${UID}_${PPID_12}/request_start_${CID_12}.txt" "TC-12: Cleaned up on error termination"

# ---------------------------------------------------------
# TC-13: Subagent Approval Tool Call
# ---------------------------------------------------------
printf "\nTest Case 13: Subagent Approval Tool Call\n"
reset_logs
PPID_13=90013
CID_13="subagent-approval-cid"

PAYLOAD_TOOL_13=$(jq -n --arg cid "${CID_13}" '{
  conversationId: $cid,
  executionId: "exec-13",
  stepIdx: 5,
  workspacePaths: ["/tmp/workspace"],
  toolCall: {
    name: "ask_permission",
    args: {
      Action: "write_file",
      Target: "/etc/hosts",
      Reason: "modify network config"
    }
  }
}')

OUT_TOOL_13=$(invoke_pre_tool_use "${PPID_13}" 1000 "${PAYLOAD_TOOL_13}")
assert_eq '{"decision": "allow"}' "${OUT_TOOL_13}" "TC-13: PreToolUse stdout contract"

NOTIFY_13=$(get_notify_log)
assert_contains "Jetski Approval Required" "${NOTIFY_13}" "TC-13: Subagent approval alerts immediately"
assert_contains "[Permission: write_file /etc/hosts] modify network config" "${NOTIFY_13}" "TC-13: Formatted permission string"

# Also test ask_custom_permission
reset_logs
PAYLOAD_TOOL_13_CUSTOM=$(jq -n --arg cid "${CID_13}" '{
  conversationId: $cid,
  executionId: "exec-13b",
  stepIdx: 6,
  workspacePaths: ["/tmp/workspace"],
  toolCall: {
    name: "default_api:ask_custom_permission",
    args: {
      Grant: "read_file /var/log"
    }
  }
}')
invoke_pre_tool_use "${PPID_13}" 1001 "${PAYLOAD_TOOL_13_CUSTOM}" >/dev/null
NOTIFY_13_CUSTOM=$(get_notify_log)
assert_contains "[Custom Permission: read_file /var/log]" "${NOTIFY_13_CUSTOM}" "TC-13: Formatted custom permission string"

# ---------------------------------------------------------
# TC-14: Terminal OSC 99 Bell via Background Dispatch
# ---------------------------------------------------------
printf "\nTest Case 14: Terminal OSC 99 Bell via Background Dispatch\n"
MOCK_TTY="/tmp/mock_target_tty_$$"
: > "${MOCK_TTY}"

TARGET_TTY="${MOCK_TTY}" bash "${SCRIPT_DIR}/osc99_notify.sh" "Test OSC Title" "Test OSC Message"

OSC_OUT=$(cat "${MOCK_TTY}")
assert_contains "Test OSC Title" "${OSC_OUT}" "TC-14: OSC99 contains title"
assert_contains "Test OSC Message" "${OSC_OUT}" "TC-14: OSC99 contains message"
assert_contains "99;i=jetski" "${OSC_OUT}" "TC-14: OSC99 escape sequence structure"

# ---------------------------------------------------------
# TC-15: Concurrent Multi-Session Isolation
# ---------------------------------------------------------
printf "\nTest Case 15: Concurrent Multi-Session Isolation\n"
reset_logs
PPID_15_A=90015
PPID_15_B=90016
CID_15_A="cid-15a"
CID_15_B="cid-15b"

invoke_pre_invocation "${PPID_15_A}" 1000 "$(jq -n --arg cid "${CID_15_A}" '{conversationId: $cid, executionId: "exec-15a", invocationNum: 0, lastUserInput: "session A prompt", workspacePaths: ["/tmp/workspaceA"]}')" >/dev/null
invoke_pre_invocation "${PPID_15_B}" 2000 "$(jq -n --arg cid "${CID_15_B}" '{conversationId: $cid, executionId: "exec-15b", invocationNum: 0, lastUserInput: "session B prompt", workspacePaths: ["/tmp/workspaceB"]}')" >/dev/null

STATE_DIR_A="/tmp/jetski_${UID}_${PPID_15_A}"
STATE_DIR_B="/tmp/jetski_${UID}_${PPID_15_B}"

assert_file_exists "${STATE_DIR_A}/request_start_${CID_15_A}.txt" "TC-15: Session A tracker written to session A dir"
assert_file_exists "${STATE_DIR_B}/request_start_${CID_15_B}.txt" "TC-15: Session B tracker written to session B dir"

ST_A=$(awk '{print $1}' "${STATE_DIR_A}/request_start_${CID_15_A}.txt")
ST_B=$(awk '{print $1}' "${STATE_DIR_B}/request_start_${CID_15_B}.txt")
assert_eq "1000" "${ST_A}" "TC-15: Session A timestamp is 1000"
assert_eq "2000" "${ST_B}" "TC-15: Session B timestamp is 2000"

invoke_stop "${PPID_15_A}" 1070 "$(jq -n --arg cid "${CID_15_A}" '{conversationId: $cid, executionId: "exec-15a", fullyIdle: true, workspacePaths: ["/tmp/workspaceA"]}')" >/dev/null
assert_file_not_exists "${STATE_DIR_A}/request_start_${CID_15_A}.txt" "TC-15: Session A cleaned up"
assert_file_exists "${STATE_DIR_B}/request_start_${CID_15_B}.txt" "TC-15: Session B remains untouched"

# Clean up B
invoke_stop "${PPID_15_B}" 2010 "$(jq -n --arg cid "${CID_15_B}" '{conversationId: $cid, executionId: "exec-15b", fullyIdle: true, workspacePaths: ["/tmp/workspaceB"]}')" >/dev/null

# ---------------------------------------------------------
# TC-16: Mid-Task User Abort (Ctrl+C)
# ---------------------------------------------------------
printf "\nTest Case 16: Mid-Task User Abort (Ctrl+C)\n"
reset_logs
PPID_16=90016
CID_16="cid-tc16"
STATE_DIR_16="/tmp/jetski_${UID}_${PPID_16}"

# Turn 1: User enters prompt at T=1000
invoke_pre_invocation "${PPID_16}" 1000 "$(jq -n --arg cid "${CID_16}" '{conversationId: $cid, executionId: "exec-16-1", invocationNum: 0, lastUserInput: "task 1", workspacePaths: ["/tmp/workspace"]}')" >/dev/null

# User aborts mid-turn (no Stop hook runs)
# User enters new prompt at T=1030
invoke_pre_invocation "${PPID_16}" 1030 "$(jq -n --arg cid "${CID_16}" '{conversationId: $cid, executionId: "exec-16-2", invocationNum: 0, lastUserInput: "task 2 new after abort", workspacePaths: ["/tmp/workspace"]}')" >/dev/null

ST_16=$(awk '{print $1}' "${STATE_DIR_16}/request_start_${CID_16}.txt")
assert_eq "1030" "${ST_16}" "TC-16: T0 updated cleanly to 1030 for new prompt after abort"

# ---------------------------------------------------------
# TC-17: In-Process Conversation Switch
# ---------------------------------------------------------
printf "\nTest Case 17: In-Process Conversation Switch\n"
reset_logs
PPID_17=90017
CID_17_A="cid-17a"
CID_17_B="cid-17b"
STATE_DIR_17="/tmp/jetski_${UID}_${PPID_17}"

# Session starts with conversation A
invoke_pre_invocation "${PPID_17}" 1000 "$(jq -n --arg cid "${CID_17_A}" '{conversationId: $cid, executionId: "exec-17a", invocationNum: 0, lastUserInput: "conv A prompt", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
assert_eq "${CID_17_A}" "$(cat "${STATE_DIR_17}/cli_main.txt")" "TC-17: cli_main initialized to conv A"

# Switch conversation in same process to conversation B with user input
invoke_pre_invocation "${PPID_17}" 1010 "$(jq -n --arg cid "${CID_17_B}" '{conversationId: $cid, executionId: "exec-17b", invocationNum: 0, lastUserInput: "conv B prompt", workspacePaths: ["/tmp/workspace"]}')" >/dev/null
assert_eq "${CID_17_B}" "$(cat "${STATE_DIR_17}/cli_main.txt")" "TC-17: cli_main updated to conv B"

# ---------------------------------------------------------
# TC-18: Log Integrity Verification
# ---------------------------------------------------------
printf "\nTest Case 18: Log Integrity Verification\n"
assert_file_exists "${LOG_FILE}" "TC-18: Log file exists"
LOG_SIZE=$(wc -l < "${LOG_FILE}")
if [[ "${LOG_SIZE}" -gt 10 ]]; then
  pass "TC-18: Log file contains entries (${LOG_SIZE} lines) and was never deleted"
else
  fail "TC-18" "Log file has too few lines: ${LOG_SIZE}"
fi

# ---------------------------------------------------------
# TC-19: Dead Process Pruning
# ---------------------------------------------------------
printf "\nTest Case 19: Dead Process Pruning\n"
DEAD_DIR="/tmp/jetski_${UID}_999998"
mkdir -p "${DEAD_DIR}"
touch -d "2 days ago" "${DEAD_DIR}"

ACTIVE_PID=$$
ACTIVE_DIR="/tmp/jetski_${UID}_${ACTIVE_PID}"
mkdir -p "${ACTIVE_DIR}"

# Create a stale lock older than 1 hour
STALE_LOCK="/tmp/jetski_${UID}_prune.lock"
mkdir -p "${STALE_LOCK}"
touch -d "2 hours ago" "${STALE_LOCK}"

(
  export LOG_TO_STDERR=""
  source "${SCRIPT_DIR}/jetski_hook_utils.sh"
  prune_dead_sessions 86400
)

assert_dir_not_exists "${DEAD_DIR}" "TC-19: Dead session directory older than 24h pruned"
assert_dir_exists "${ACTIVE_DIR}" "TC-19: Active session directory preserved"
assert_dir_not_exists "${STALE_LOCK}" "TC-19: Stale prune lock directory recovered and cleaned up"
rm -rf "${ACTIVE_DIR}"

# ---------------------------------------------------------
# Test Results Summary
# ---------------------------------------------------------
printf "\n========================================\n"
printf "Test Summary: %d passed, %d failed (Total: %d)\n" "${PASSED_TESTS}" "${FAILED_TESTS}" "${TOTAL_TESTS}"
printf "========================================\n"

if [[ "${FAILED_TESTS}" -eq 0 ]]; then
  exit 0
else
  exit 1
fi
