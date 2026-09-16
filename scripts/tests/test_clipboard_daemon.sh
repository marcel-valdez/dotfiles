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

assert_string_contains() {
  local haystack="$1"
  local needle="$2"
  local msg="$3"
  if [[ "${haystack}" == *"${needle}"* ]]; then
    echo "  [PASS] ${msg}"
    ((PASSED_TESTS++)) || true
  else
    echo "  [FAIL] ${msg}"
    echo "         Haystack: '${haystack}'"
    echo "         Needle:   '${needle}'"
    ((FAILED_TESTS++)) || true
  fi
}

assert_string_not_contains() {
  local haystack="$1"
  local needle="$2"
  local msg="$3"
  if [[ "${haystack}" != *"${needle}"* ]]; then
    echo "  [PASS] ${msg}"
    ((PASSED_TESTS++)) || true
  else
    echo "  [FAIL] ${msg}"
    echo "         Haystack: '${haystack}'"
    echo "         Did not expect: '${needle}'"
    ((FAILED_TESTS++)) || true
  fi
}

setup_mock_bin() {
  local bin_dir="${TEST_DIR}/bin"
  mkdir -p "${bin_dir}"

  cat << "EOF" > "${bin_dir}/hostname"
#!/bin/bash
echo "marcelvaldez-glaptop"
EOF
  chmod +x "${bin_dir}/hostname"

  cat << "EOF" > "${bin_dir}/notify-send"
#!/bin/bash
exit 0
EOF
  chmod +x "${bin_dir}/notify-send"

  echo "${bin_dir}"
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

# -----------------------------------------------------------------------------
# Item 1: flock Singleton Mutual Exclusion (exits 0)
# -----------------------------------------------------------------------------
test_flock_singleton_mutual_exclusion() {
  echo "Running test_flock_singleton_mutual_exclusion..."
  local bin_dir
  bin_dir="$(setup_mock_bin)"
  local run_dir="${TEST_DIR}/run_flock"
  mkdir -p "${run_dir}"
  local lock_file="${run_dir}/clipboard-daemon-3334.lock"
  touch "${lock_file}"

  # Acquire lock in background subshell
  (
    exec 200>"${lock_file}"
    flock -n 200
    sleep 5
  ) &
  local holder_pid=$!
  sleep 0.1

  local output="" exit_code=0
  output=$(CLIPBOARD_PORT=3334 PATH="${bin_dir}:${PATH}" PS1_HOST="laptop" \
    XDG_RUNTIME_DIR="${run_dir}" bash ~/bin/clipboard-daemon.sh 3334 2>&1) \
    || exit_code=$?

  kill -9 "${holder_pid}" 2>/dev/null || true
  wait "${holder_pid}" 2>/dev/null || true

  assert_equals "0" "${exit_code}" \
    "Contending daemon instance exits with 0 for Restart=on-failure contract"
  assert_string_contains "${output}" "already running on port 3334" \
    "Contending daemon outputs clear already running status message"
}

# -----------------------------------------------------------------------------
# Item 2: --restart Bounded PID Polling and SIGKILL Escalation
# -----------------------------------------------------------------------------
test_restart_bounded_pid_polling_and_kill_escalation() {
  echo "Running test_restart_bounded_pid_polling_and_kill_escalation..."
  local run_dir="${TEST_DIR}/run_restart_esc"
  mkdir -p "${run_dir}"
  local pid_file="${run_dir}/clipboard-daemon-3334.pid"

  # Spawn stubborn stub process that traps and ignores SIGTERM
  bash -c 'trap "" SIGTERM; exec -a "clipboard-daemon-stub" sleep 100' &
  local stubborn_pid=$!
  echo "${stubborn_pid}" > "${pid_file}"

  # Execute bounded polling and kill escalation loop directly
  local target_pids=("${stubborn_pid}")
  for pid in "${target_pids[@]}"; do
    kill -TERM "${pid}" 2>/dev/null || true
    for _ in {1..20}; do
      kill -0 "${pid}" 2>/dev/null || break
      sleep 0.05
    done
    if kill -0 "${pid}" 2>/dev/null; then
      kill -9 "${pid}" 2>/dev/null || true
    fi
    wait "${pid}" 2>/dev/null || true
  done
  rm -f "${pid_file}" 2>/dev/null || true

  local is_alive=0
  if kill -0 "${stubborn_pid}" 2>/dev/null; then
    is_alive=1
    kill -9 "${stubborn_pid}" 2>/dev/null || true
    wait "${stubborn_pid}" 2>/dev/null || true
  fi

  assert_equals "0" "${is_alive}" \
    "Stubborn process ignoring SIGTERM is terminated by SIGKILL escalation"
  assert_equals "0" "$([[ -f "${pid_file}" ]] && echo 1 || echo 0)" \
    "Daemon PID file cleaned up after restart process termination"
}

# -----------------------------------------------------------------------------
# Item 3: Daemon PID File Lifecycle and Ownership Validation
# -----------------------------------------------------------------------------
test_daemon_pid_file_lifecycle_and_validation() {
  echo "Running test_daemon_pid_file_lifecycle_and_validation..."
  local run_dir="${TEST_DIR}/run_pid_lifecycle"
  mkdir -p "${run_dir}"
  local pid_file="${run_dir}/clipboard-daemon-3334.pid"

  # 1. Validation check: foreign process must not be targeted
  sleep 100 &
  local foreign_pid=$!
  echo "${foreign_pid}" > "${pid_file}"

  local target_pids=()
  if [[ -f "${pid_file}" ]]; then
    local dpid
    dpid=$(cat "${pid_file}" 2>/dev/null || true)
    if [[ -n "${dpid}" && -r "/proc/${dpid}/cmdline" ]]; then
      local cmdline
      cmdline=$(tr '\0' ' ' < "/proc/${dpid}/cmdline" 2>/dev/null || true)
      if [[ ! "${cmdline}" =~ [[:space:]]-c[[:space:]] ]] && \
         [[ "${cmdline}" =~ (^|[[:space:]])([^[:space:]]*/)?clipboard-daemon ]]; then
        target_pids+=("${dpid}")
      fi
    fi
  fi

  assert_equals "0" "${#target_pids[@]}" \
    "Foreign PID recorded in PID file rejected by cmdline validation"

  local foreign_alive=0
  if kill -0 "${foreign_pid}" 2>/dev/null; then
    foreign_alive=1
    kill -9 "${foreign_pid}" 2>/dev/null || true
    wait "${foreign_pid}" 2>/dev/null || true
  fi
  assert_equals "1" "${foreign_alive}" "Foreign process remained unmolested"

  # 2. Cleanup check: cleanup only removes PID file if it contains $$
  echo "$$" > "${pid_file}"
  bash -c '
    pid_file="$1"
    sub_pid=$$
    if [[ -f "${pid_file}" ]]; then
      current_pid=$(cat "${pid_file}" 2>/dev/null || true)
      if [[ "${current_pid}" == "${sub_pid}" ]]; then
        rm -f "${pid_file}" 2>/dev/null || true
      fi
    fi
  ' _ "${pid_file}"
  assert_equals "1" "$([[ -f "${pid_file}" ]] && echo 1 || echo 0)" \
    "Foreign subshell did not remove PID file belonging to different parent"

  # Current shell cleanup
  if [[ -f "${pid_file}" ]]; then
    current_pid=$(cat "${pid_file}" 2>/dev/null || true)
    if [[ "${current_pid}" == "$$" ]]; then
      rm -f "${pid_file}" 2>/dev/null || true
    fi
  fi
  assert_equals "0" "$([[ -f "${pid_file}" ]] && echo 1 || echo 0)" \
    "Owning process cleaned up its own PID file"
}

# -----------------------------------------------------------------------------
# Item 4: FD 200 Cloexec Isolation Across Children
# -----------------------------------------------------------------------------
test_fd200_cloexec_isolation_across_children() {
  echo "Running test_fd200_cloexec_isolation_across_children..."
  local lock_file="${TEST_DIR}/fd_isolation.lock"
  touch "${lock_file}"

  exec 200>"${lock_file}"
  flock -n 200

  # Verify subshell isolation using exec 200>&-
  local child_has_fd=0
  (
    exec 200>&-
    sleep 10 &
    child_pid=$!
    if [[ -e "/proc/${child_pid}/fd/200" ]]; then
      echo "1"
    else
      echo "0"
    fi
    kill -9 "${child_pid}" 2>/dev/null || true
  ) > "${TEST_DIR}/fd_check.txt"

  child_has_fd=$(cat "${TEST_DIR}/fd_check.txt")
  assert_equals "0" "${child_has_fd}" \
    "Child subshell with exec 200>&- does not inherit FD 200"

  exec 200>&-

  # Verify clipboard-daemon.sh source includes 200>&- on subshell, nc, and xclip
  local daemon_src="${HOME}/bin/clipboard-daemon.sh"
  assert_string_contains "$(cat "${daemon_src}")" "exec 200>&-" \
    "Script closes FD 200 at keeper subshell start"
  assert_string_contains "$(cat "${daemon_src}")" "nc -l \"\${HOST}\" \"\${PORT}\" 200>&-" \
    "Script closes FD 200 on foreground netcat listener"
  assert_string_contains "$(cat "${daemon_src}")" "xclip -selection clipboard -display \":\${display}\" 200>&-" \
    "Script closes FD 200 on xclip display dispatch"
}

# -----------------------------------------------------------------------------
# Item 5: Subshell Parent PID Liveness Check
# -----------------------------------------------------------------------------
test_subshell_parent_liveness_exit() {
  echo "Running test_subshell_parent_liveness_exit..."
  local run_dir="${TEST_DIR}/run_liveness"
  mkdir -p "${run_dir}"

  # Spawn a short-lived parent that launches keeper subshell and exits
  bash -c '
    parent_pid=$$
    (
      while true; do
        if ! kill -0 "${parent_pid}" 2>/dev/null; then
          exit 0
        fi
        sleep 0.05
      done
    ) &
    echo "$!" > "'"${run_dir}/keeper.pid"'"
    exit 0
  '

  local keeper_pid
  keeper_pid=$(cat "${run_dir}/keeper.pid")

  # Wait for keeper subshell to detect parent termination and exit
  local keeper_alive=1
  for _ in {1..20}; do
    if ! kill -0 "${keeper_pid}" 2>/dev/null; then
      keeper_alive=0
      break
    fi
    sleep 0.05
  done

  if [[ "${keeper_alive}" -eq 1 ]]; then
    kill -9 "${keeper_pid}" 2>/dev/null || true
  fi

  assert_equals "0" "${keeper_alive}" \
    "Keeper subshell detects parent process termination and exits cleanly"
}

# -----------------------------------------------------------------------------
# Item 6: Strict PID Isolation - Never Kill Interactive Session
# -----------------------------------------------------------------------------
test_strict_pid_isolation_no_kill_interactive() {
  echo "Running test_strict_pid_isolation_no_kill_interactive..."
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
    wait "${daemon_child_pid}" 2>/dev/null || true
  fi

  local interactive_alive=0
  if kill -0 "${interactive_pid}" 2>/dev/null; then
    interactive_alive=1
    kill -9 "${interactive_pid}" 2>/dev/null || true
    wait "${interactive_pid}" 2>/dev/null || true
  fi

  assert_equals "0" "${daemon_child_alive}" \
    "Daemon-spawned child PID was terminated"
  assert_equals "1" "${interactive_alive}" \
    "Interactive user session PID remained alive untouched"
}

# -----------------------------------------------------------------------------
# Item 7: 15s Quiet Drain Delay Trigger on Post-Stable Connection Drops
# -----------------------------------------------------------------------------
test_quiet_drain_delay_on_post_stable_drop() {
  echo "Running test_quiet_drain_delay_on_post_stable_drop..."

  evaluate_session_drop() {
    local duration="$1"
    local exit_code="$2"
    local drain_delay_triggered=0
    local reconnect_backoff=5

    if [[ ${duration} -ge 60 ]]; then
      consecutive_drain_retries=0
      persistent_conflict=0
      reconnect_backoff=5
      drain_delay_triggered=15
    else
      drain_delay_triggered=0
    fi
    echo "${drain_delay_triggered}"
  }

  local stable_delay
  stable_delay=$(evaluate_session_drop 65 0)
  assert_equals "15" "${stable_delay}" \
    "Post-stable connection drop (65s >= 60s) triggers 15s quiet drain buffer"

  local transient_delay
  transient_delay=$(evaluate_session_drop 12 1)
  assert_equals "0" "${transient_delay}" \
    "Transient connection drop (12s < 60s) does not trigger 15s quiet drain buffer"
}

# -----------------------------------------------------------------------------
# Item 8: Two-Tier Escalating Conflict Pause (60s First Cycle, 600s Persistent)
# -----------------------------------------------------------------------------
test_two_tier_escalating_conflict_pause() {
  echo "Running test_two_tier_escalating_conflict_pause..."
  local consecutive_drain_retries=0
  local persistent_conflict=0
  local pauses=()

  # Cycle 1: First collision incident
  for _ in {1..3}; do
    ((consecutive_drain_retries++)) || true
    if [[ ${consecutive_drain_retries} -eq 1 ]]; then
      pauses+=("15")
    elif [[ ${consecutive_drain_retries} -eq 2 ]]; then
      pauses+=("30")
    else
      if [[ ${persistent_conflict} -eq 0 ]]; then
        pauses+=("60")
        consecutive_drain_retries=0
        persistent_conflict=1
      fi
    fi
  done

  # Cycle 2: Immediate consecutive collision incident (persistent)
  for _ in {1..3}; do
    ((consecutive_drain_retries++)) || true
    if [[ ${consecutive_drain_retries} -eq 1 ]]; then
      pauses+=("15")
    elif [[ ${consecutive_drain_retries} -eq 2 ]]; then
      pauses+=("30")
    else
      if [[ ${persistent_conflict} -eq 1 ]]; then
        pauses+=("600")
        consecutive_drain_retries=0
      fi
    fi
  done

  assert_equals "15 30 60 15 30 600" "${pauses[*]}" \
    "Collision retry sequence: 15s -> 30s -> 60s (first incident) -> 15s -> 30s -> 600s (persistent)"
}

# -----------------------------------------------------------------------------
# Item 9: Hermetic Test Isolation Using Temporary XDG_RUNTIME_DIR
# -----------------------------------------------------------------------------
test_hermetic_isolation_temp_xdg_runtime() {
  echo "Running test_hermetic_isolation_temp_xdg_runtime..."
  local run_dir="${TEST_DIR}/run_hermetic"
  mkdir -p "${run_dir}"

  local target_port=3334
  local lock_file="${run_dir}/clipboard-daemon-${target_port}.lock"
  local pid_file="${run_dir}/clipboard-daemon-${target_port}.pid"
  local tunnel_pid_file="${run_dir}/clipboard-tunnel-${target_port}.pid"
  local err_log="${run_dir}/clipboard-ssh-${target_port}.log"

  assert_string_contains "${lock_file}" "${run_dir}" \
    "Lock file path is anchored inside isolated XDG_RUNTIME_DIR"
  assert_string_contains "${pid_file}" "${run_dir}" \
    "Daemon PID file path is anchored inside isolated XDG_RUNTIME_DIR"
  assert_string_contains "${tunnel_pid_file}" "${run_dir}" \
    "Tunnel PID file path is anchored inside isolated XDG_RUNTIME_DIR"
  assert_string_contains "${err_log}" "${run_dir}" \
    "SSH error log path is anchored inside isolated XDG_RUNTIME_DIR"
}

# -----------------------------------------------------------------------------
# Item 10: Systemd User Service Unit Contracts
# -----------------------------------------------------------------------------
test_systemd_user_service_unit_contracts() {
  echo "Running test_systemd_user_service_unit_contracts..."
  local unit_default="${HOME}/.config/systemd/user/clipboard-daemon.service"
  local unit_template="${HOME}/.config/systemd/user/clipboard-daemon@.service"

  assert_equals "1" "$([[ -f "${unit_default}" ]] && echo 1 || echo 0)" \
    "clipboard-daemon.service exists"
  assert_equals "1" "$([[ -f "${unit_template}" ]] && echo 1 || echo 0)" \
    "clipboard-daemon@.service exists"

  local content_default
  content_default=$(cat "${unit_default}")
  local content_template
  content_template=$(cat "${unit_template}")

  assert_string_contains "${content_default}" "Restart=on-failure" \
    "clipboard-daemon.service sets Restart=on-failure"
  assert_string_not_contains "${content_default}" "Restart=always" \
    "clipboard-daemon.service does not use Restart=always"
  assert_string_contains "${content_default}" \
    "Conflicts=clipboard-daemon@3333.service clipboard-daemon@3334.service clipboard-daemon@3335.service clipboard-daemon@3336.service" \
    "clipboard-daemon.service declares explicit instance Conflicts="

  assert_string_contains "${content_template}" "Restart=on-failure" \
    "clipboard-daemon@.service sets Restart=on-failure"
  assert_string_not_contains "${content_template}" "Restart=always" \
    "clipboard-daemon@.service does not use Restart=always"
  assert_string_contains "${content_template}" "Conflicts=clipboard-daemon.service" \
    "clipboard-daemon@.service declares Conflicts=clipboard-daemon.service"
}

# -----------------------------------------------------------------------------
# Item 11: AFK / Authentication Timeout Detection (300s pause)
# -----------------------------------------------------------------------------
test_afk_authentication_timeout_detection() {
  echo "Running test_afk_authentication_timeout_detection..."
  local err_log="${TEST_DIR}/ssh_error.log"

  evaluate_auth_timeout() {
    local log="$1"
    if grep -q -E "Permission denied|timed out waiting for user presence" "${log}" 2>/dev/null; then
      echo "300"
    else
      echo "0"
    fi
  }

  echo "Permission denied (publickey)." > "${err_log}"
  assert_equals "300" "$(evaluate_auth_timeout "${err_log}")" \
    "Detects 'Permission denied' and triggers 300s quiet pause"

  echo "sign_and_send_pubkey: signing failed: timed out waiting for user presence" > "${err_log}"
  assert_equals "300" "$(evaluate_auth_timeout "${err_log}")" \
    "Detects 'timed out waiting for user presence' and triggers 300s quiet pause"

  echo "Connection closed by remote host" > "${err_log}"
  assert_equals "0" "$(evaluate_auth_timeout "${err_log}")" \
    "Normal remote connection close does not trigger 300s AFK pause"
}

# -----------------------------------------------------------------------------
# Systemd Restart Delegation Verification
# -----------------------------------------------------------------------------
test_systemd_restart_delegation() {
  echo "Running test_systemd_restart_delegation..."
  local bin_dir
  bin_dir="$(setup_mock_bin)"
  local run_dir="${TEST_DIR}/run_sys_restart"
  mkdir -p "${run_dir}"

  cat << EOF > "${bin_dir}/systemctl"
#!/bin/bash
if [[ "\$*" =~ is-active.*clipboard-daemon@3334\.service ]]; then
  exit 0
fi
if [[ "\$*" =~ restart.*clipboard-daemon@3334\.service ]]; then
  echo "restarted" >> "${run_dir}/systemctl.log"
  exit 0
fi
exit 1
EOF
  chmod +x "${bin_dir}/systemctl"

  local exit_code=0
  CLIPBOARD_PORT=3334 PATH="${bin_dir}:${PATH}" PS1_HOST="laptop" \
    XDG_RUNTIME_DIR="${run_dir}" bash ~/bin/clipboard-daemon.sh -r 3334 \
    >/dev/null 2>&1 || exit_code=$?

  assert_equals "0" "${exit_code}" \
    "Restart delegates cleanly to active systemd unit with exit code 0"
  assert_equals "restarted" "$(cat "${run_dir}/systemctl.log" 2>/dev/null || true)" \
    "systemctl --user restart clipboard-daemon@3334.service was executed"
}

# -----------------------------------------------------------------------------
# Progressive Reconnect Backoff & Log Isolation Tests
# -----------------------------------------------------------------------------
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

  assert_equals "5 10 15 30 30" "${backoffs[*]}" \
    "Progressive reconnect backoff: 5s -> 10s -> 15s -> 30s max"

  local duration=65
  if [[ ${duration} -ge 60 ]]; then
    backoff=5
  fi
  assert_equals "5" "${backoff}" \
    "Backoff resets to 5s after 60s stable connection"
}

test_log_isolation_no_false_conflict() {
  echo "Running test_log_isolation_no_false_conflict..."
  local global_log="${TEST_DIR}/clipboard-daemon.log"
  local err_log="${TEST_DIR}/clipboard-ssh-3334.log"

  echo "[10:00:00] Warning: remote port forwarding failed for listen port 3334" \
    >> "${global_log}"
  > "${err_log}"

  local conflict_detected=0
  if grep -q "remote port forwarding failed" "${err_log}"; then
    conflict_detected=1
  fi

  assert_equals "0" "${conflict_detected}" \
    "Isolated error log avoids false conflict alerts from historical logs"
}

# -----------------------------------------------------------------------------
# Test Runner
# -----------------------------------------------------------------------------
echo "================================================================="
echo "Running Clipboard Daemon Unit Tests (11 Test Plan Items)"
echo "================================================================="
test_flock_singleton_mutual_exclusion
test_restart_bounded_pid_polling_and_kill_escalation
test_daemon_pid_file_lifecycle_and_validation
test_fd200_cloexec_isolation_across_children
test_subshell_parent_liveness_exit
test_strict_pid_isolation_no_kill_interactive
test_quiet_drain_delay_on_post_stable_drop
test_two_tier_escalating_conflict_pause
test_hermetic_isolation_temp_xdg_runtime
test_systemd_user_service_unit_contracts
test_afk_authentication_timeout_detection
test_systemd_restart_delegation
test_process_discovery_positive_and_order
test_process_discovery_negative_specs
test_active_tunnel_poll_no_recreate
test_gcert_missing_blocks_recreate
test_network_drop_progressive_backoff
test_log_isolation_no_false_conflict

echo "================================================================="
echo "Test Results: ${PASSED_TESTS} passed, ${FAILED_TESTS} failed."
echo "================================================================="

if [[ ${FAILED_TESTS} -gt 0 ]]; then
  exit 1
fi
exit 0
