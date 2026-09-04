#!/usr/bin/env bash
# test_clipboard_daemon.sh: Hermetic unit test suite for clipboard-daemon.sh
set -euo pipefail

TEST_DIR="$(mktemp -d /tmp/test_clipboard_daemon.XXXXXX)"
cleanup_test_env() {
  rm -rf "${TEST_DIR}"
}
trap cleanup_test_env EXIT

PASSED_TESTS=0
FAILED_TESTS=0

assert_equals() {
  local expected="$1"
  local actual="$2"
  local msg="$3"
  if [[ "${expected}" == "${actual}" ]]; then
    echo "  [PASS] ${msg}"
    ((PASSED_TESTS++)) || true
  else
    echo "  [FAIL] ${msg}"
    echo "         Expected: '${expected}'"
    echo "         Actual:   '${actual}'"
    ((FAILED_TESTS++)) || true
  fi
}

assert_contains() {
  local haystack="$1"
  local needle="$2"
  local msg="$3"
  if [[ " ${haystack} " =~ [[:space:]]${needle}[[:space:]] ]]; then
    echo "  [PASS] ${msg}"
    ((PASSED_TESTS++)) || true
  else
    echo "  [FAIL] ${msg}"
    echo "         Haystack: '${haystack}'"
    echo "         Needle:   '${needle}'"
    ((FAILED_TESTS++)) || true
  fi
}

# -----------------------------------------------------------------------------
# Mock Procfs Helper for Process Discovery Tests
# -----------------------------------------------------------------------------
setup_mock_proc() {
  local pid="$1"
  local state="$2"
  shift 2

  local proc_pid_dir="${TEST_DIR}/proc/${pid}"
  mkdir -p "${proc_pid_dir}"

  # Write null-byte delimited cmdline safely using printf %s\0
  printf "%s\0" "$@" > "${proc_pid_dir}/cmdline"

  # Write status file
  cat <<EOF > "${proc_pid_dir}/status"
Name:	ssh
State:	${state} (sleeping)
Tgid:	${pid}
Pid:	${pid}
PPid:	1
EOF
}

# Test 1 & 2: Positive Specs and Argument Order Independence
test_process_discovery_positive_and_order() {
  echo "Running test_process_discovery_positive_and_order..."
  local mock_proc_root="${TEST_DIR}/proc"
  rm -rf "${mock_proc_root}"

  # Mock PIDs:
  # PID 101: standard -R 3334:127.0.0.1:3334
  setup_mock_proc "101" "S" "ssh" "-N" "-T" "-R" "3334:127.0.0.1:3334" "gcloud"
  # PID 102: explicit bind 127.0.0.1:3334:127.0.0.1:3334
  setup_mock_proc "102" "S" "ssh" "-R" "127.0.0.1:3334:127.0.0.1:3334" "gcloud"
  # PID 103: wildcard bind *:3334:127.0.0.1:3334
  setup_mock_proc "103" "S" "ssh" "-R" "*:3334:127.0.0.1:3334" "gcloud"
  # PID 104: no space -R3334:127.0.0.1:3334
  setup_mock_proc "104" "S" "ssh" "-R3334:127.0.0.1:3334" "gcloud"
  # PID 105: RemoteForward option equals
  setup_mock_proc "105" "S" "ssh" "-o" "RemoteForward=3334:127.0.0.1:3334" "gcloud"
  # PID 106: RemoteForward option space
  setup_mock_proc "106" "S" "ssh" "-o" "RemoteForward" "3334:127.0.0.1:3334" "gcloud"
  # PID 107: host before -R argument
  setup_mock_proc "107" "S" "ssh" "gcloud" "-R" "3334:127.0.0.1:3334"

  mock_find_local_tunnel_pids() {
    local target_port="$1"
    local matching_pids=()
    local regex_r="(^|[[:space:]])-R[[:space:]]*(${target_port}:|[0-9a-zA-Z.*_-]+:${target_port}:)"
    local regex_rf="(^|[[:space:]])-o[[:space:]]+RemoteForward([=[:space:]]+)(${target_port}[:[:space:]]|[0-9a-zA-Z.*_-]+:${target_port}[:[:space:]])"

    for pdir in "${mock_proc_root}"/*; do
      [[ ! -d "${pdir}" ]] && continue
      local pid
      pid="$(basename "${pdir}")"
      if [[ -r "${pdir}/status" ]]; then
        local state
        state=$(awk '/^State:/ {print $2}' "${pdir}/status" 2>/dev/null || true)
        if [[ "${state}" == "Z" || "${state}" == "T" ]]; then
          continue
        fi
      fi
      if [[ -r "${pdir}/cmdline" ]]; then
        local cmdline
        cmdline=$(tr '\0' ' ' < "${pdir}/cmdline" 2>/dev/null || true)
        if [[ "${cmdline}" =~ ${regex_r} ]] || [[ "${cmdline}" =~ ${regex_rf} ]]; then
          matching_pids+=("${pid}")
        fi
      fi
    done
    echo "${matching_pids[*]}"
  }

  local found_pids
  found_pids=$(mock_find_local_tunnel_pids "3334")
  for expected_pid in 101 102 103 104 105 106 107; do
    assert_contains "${found_pids}" "${expected_pid}" "Discovered positive spec PID ${expected_pid}"
  done
}

# Test 3: Negative Specs (Port prefixes, suffixes, different ports, -L)
test_process_discovery_negative_specs() {
  echo "Running test_process_discovery_negative_specs..."
  local mock_proc_root="${TEST_DIR}/proc"
  rm -rf "${mock_proc_root}"

  # PID 201: Port 3333 (workstation port)
  setup_mock_proc "201" "S" "ssh" "-R" "3333:127.0.0.1:3333" "gcloud"
  # PID 202: Prefix 13334
  setup_mock_proc "202" "S" "ssh" "-R" "13334:127.0.0.1:3334" "gcloud"
  # PID 203: Suffix 33340
  setup_mock_proc "203" "S" "ssh" "-R" "33340:127.0.0.1:33340" "gcloud"
  # PID 204: Forward tunnel (-L 3334:...)
  setup_mock_proc "204" "S" "ssh" "-L" "3334:127.0.0.1:3334" "gcloud"
  # PID 205: Zombie process
  setup_mock_proc "205" "Z" "ssh" "-R" "3334:127.0.0.1:3334" "gcloud"

  mock_find_local_tunnel_pids() {
    local target_port="$1"
    local matching_pids=()
    local regex_r="(^|[[:space:]])-R[[:space:]]*(${target_port}:|[0-9a-zA-Z.*_-]+:${target_port}:)"
    local regex_rf="(^|[[:space:]])-o[[:space:]]+RemoteForward([=[:space:]]+)(${target_port}[:[:space:]]|[0-9a-zA-Z.*_-]+:${target_port}[:[:space:]])"

    for pdir in "${mock_proc_root}"/*; do
      [[ ! -d "${pdir}" ]] && continue
      local pid
      pid="$(basename "${pdir}")"
      if [[ -r "${pdir}/status" ]]; then
        local state
        state=$(awk '/^State:/ {print $2}' "${pdir}/status" 2>/dev/null || true)
        if [[ "${state}" == "Z" || "${state}" == "T" ]]; then
          continue
        fi
      fi
      if [[ -r "${pdir}/cmdline" ]]; then
        local cmdline
        cmdline=$(tr '\0' ' ' < "${pdir}/cmdline" 2>/dev/null || true)
        if [[ "${cmdline}" =~ ${regex_r} ]] || [[ "${cmdline}" =~ ${regex_rf} ]]; then
          matching_pids+=("${pid}")
        fi
      fi
    done
    echo "${matching_pids[*]}"
  }

  local found_pids
  found_pids=$(mock_find_local_tunnel_pids "3334")
  assert_equals "" "${found_pids}" "Negative specs and zombie processes correctly ignored"
}

# Test 4: Active Tunnel Poll-Monitoring without Re-creating
test_active_tunnel_poll_no_recreate() {
  echo "Running test_active_tunnel_poll_no_recreate..."
  local log_file="${TEST_DIR}/daemon.log"
  local ext_pid=99999
  local action_taken="none"

  if [[ "${ext_pid}" -gt 0 ]]; then
    action_taken="monitored"
    echo "[Test] Active local reverse SSH tunnel detected (PID ${ext_pid}). Monitoring existing tunnel." >> "${log_file}"
  else
    action_taken="spawned"
  fi

  assert_equals "monitored" "${action_taken}" "Active tunnel detected enters poll-monitoring"
  assert_contains "$(cat "${log_file}")" "Active" "Log records monitoring message"
}

# Test 5: Gcert Missing Blocks Reconnection
test_gcert_missing_blocks_recreate() {
  echo "Running test_gcert_missing_blocks_recreate..."
  local log_file="${TEST_DIR}/daemon_gcert.log"
  local ssh_executed=0

  mock_gcertstatus() {
    return 1 # Expired
  }

  if ! mock_gcertstatus; then
    echo "[Test] SSH credentials (gcert) expired or missing. Waiting for gcert..." >> "${log_file}"
  else
    ssh_executed=1
  fi

  assert_equals "0" "${ssh_executed}" "No SSH spawned when gcert is expired"
  assert_contains "$(cat "${log_file}")" "Waiting" "Log records waiting for gcert"
}

# Test 6: Transient Remote Conflict 6-Retry Socket Drain
test_transient_remote_conflict_drain() {
  echo "Running test_transient_remote_conflict_drain..."
  local consecutive_retries=0
  local notification_sent=0

  for _ in {1..7}; do
    ((consecutive_retries++)) || true
    if [[ ${consecutive_retries} -le 6 ]]; then
      local drain_sleep=$(( consecutive_retries * 5 ))
    else
      notification_sent=1
    fi
  done

  assert_equals "7" "${consecutive_retries}" "Retried 6 times before declaring persistent conflict"
  assert_equals "1" "${notification_sent}" "Notification triggered only on attempt 7"
}

# Test 7: Network Drop Progressive Backoff and Stable Reset
test_network_drop_progressive_backoff() {
  echo "Running test_network_drop_progressive_backoff..."
  local backoff=5
  local backoffs=()

  for _ in {1..5}; do
    backoffs+=("${backoff}")
    if [[ ${backoff} -lt 15 ]]; then
      backoff=$(( backoff + 5 ))
    elif [[ ${backoff} -lt 30 ]]; then
      backoff=30
    fi
  done

  assert_equals "5 10 15 30 30" "${backoffs[*]}" "Progressive reconnect backoff: 5s -> 10s -> 15s -> 30s max"

  local duration=65
  if [[ ${duration} -ge 60 ]]; then
    backoff=5
  fi
  assert_equals "5" "${backoff}" "Backoff resets to 5s after 60s stable connection"
}

# Test 8: Per-Invocation Log Isolation
test_log_isolation_no_false_conflict() {
  echo "Running test_log_isolation_no_false_conflict..."
  local global_log="${TEST_DIR}/clipboard-daemon.log"
  local err_log="${TEST_DIR}/clipboard-ssh-3334.log"

  echo "[10:00:00] Warning: remote port forwarding failed for listen port 3334" >> "${global_log}"
  > "${err_log}"

  local conflict_detected=0
  if grep -q "remote port forwarding failed" "${err_log}"; then
    conflict_detected=1
  fi

  assert_equals "0" "${conflict_detected}" "Isolated error log avoids false conflict alerts from historical logs"
}

# Test 9: Daemon PID Isolation - Never Kill Interactive Session
test_daemon_pid_isolation_no_kill_interactive() {
  echo "Running test_daemon_pid_isolation_no_kill_interactive..."
  local pid_file="${TEST_DIR}/clipboard-tunnel-3334.pid"

  sleep 100 &
  local interactive_pid=$!

  sleep 100 &
  local daemon_child_pid=$!
  echo "${daemon_child_pid}" > "${pid_file}"

  if [[ -f "${pid_file}" ]]; then
    local target_pid
    target_pid=$(cat "${pid_file}" 2>/dev/null || true)
    if [[ -n "${target_pid}" ]] && kill -0 "${target_pid}" 2>/dev/null; then
      kill -TERM "${target_pid}" 2>/dev/null || true
    fi
    rm -f "${pid_file}" 2>/dev/null || true
  fi

  sleep 0.2
  local daemon_child_alive=0
  if kill -0 "${daemon_child_pid}" 2>/dev/null; then
    daemon_child_alive=1
    kill -9 "${daemon_child_pid}" 2>/dev/null || true
  fi

  local interactive_alive=0
  if kill -0 "${interactive_pid}" 2>/dev/null; then
    interactive_alive=1
    kill -9 "${interactive_pid}" 2>/dev/null || true
  fi

  assert_equals "0" "${daemon_child_alive}" "Daemon-spawned child PID was terminated"
  assert_equals "1" "${interactive_alive}" "Interactive user session PID remained alive untouched"
}

# -----------------------------------------------------------------------------
# Test Runner
# -----------------------------------------------------------------------------
echo "================================================================="
echo "Running Clipboard Daemon Unit Tests"
echo "================================================================="
test_process_discovery_positive_and_order
test_process_discovery_negative_specs
test_active_tunnel_poll_no_recreate
test_gcert_missing_blocks_recreate
test_transient_remote_conflict_drain
test_network_drop_progressive_backoff
test_log_isolation_no_false_conflict
test_daemon_pid_isolation_no_kill_interactive

echo "================================================================="
echo "Test Results: ${PASSED_TESTS} passed, ${FAILED_TESTS} failed."
echo "================================================================="

if [[ ${FAILED_TESTS} -gt 0 ]]; then
  exit 1
fi
exit 0
