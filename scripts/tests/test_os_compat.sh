#!/usr/bin/env bash
# ==============================================================================
# Unit Test Suite for ~/lib/os_compat.sh
# ==============================================================================
# Granular tests covering Linux and mocked Darwin modes for os_compat.sh.
#
# Usage Example:
#   bash ~/scripts/tests/test_os_compat.sh
# ==============================================================================

set -euo pipefail

PASS_COUNT=0
FAIL_COUNT=0

source "${HOME}/lib/os_compat.sh"

assert_eq() {
  local expected="$1"
  local actual="$2"
  local msg="$3"
  if [[ "${expected}" == "${actual}" ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: ${msg} (expected='${expected}', actual='${actual}')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_true() {
  local msg="$1"
  shift
  if "$@"; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: ${msg}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_false() {
  local msg="$1"
  shift
  if ! "$@"; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: ${msg}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

test_os_detection_linux_mode() {
  OS_COMPAT_UNAME_S="Linux"
  assert_true "os::is_linux returns true on Linux" os::is_linux
  assert_false "os::is_macos returns false on Linux" os::is_macos
}

test_os_detection_darwin_mode() {
  OS_COMPAT_UNAME_S="Darwin"
  assert_true "os::is_macos returns true on Darwin" os::is_macos
  assert_true "os::is_darwin returns true on Darwin" os::is_darwin
  assert_false "os::is_linux returns false on Darwin" os::is_linux
  unset OS_COMPAT_UNAME_S
}

test_runtime_dir_darwin_mode() {
  local tmp_root
  tmp_root="$(mktemp -d)"
  OS_COMPAT_UNAME_S="Darwin"
  TMPDIR="${tmp_root}"
  local rdir
  rdir="$(os::runtime_dir)"
  assert_eq "${tmp_root}/runtime-$(id -u)" "${rdir}" "Darwin runtime_dir uses TMPDIR"
  assert_true "Darwin runtime_dir exists" test -d "${rdir}"
  unset OS_COMPAT_UNAME_S TMPDIR
  rm -rf "${tmp_root}"
}

test_base64_nowrap_encodes_multiline_without_newlines() {
  local input=$'line1\nline2\nline3\nline4_with_long_payload_to_exceed_76_columns_12345678901234567890'
  local encoded
  encoded="$(printf '%s' "${input}" | os::base64_nowrap)"
  if [[ "${encoded}" == *$'\n'* ]]; then
    echo "FAIL: os::base64_nowrap output contained newline" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  else
    PASS_COUNT=$((PASS_COUNT + 1))
  fi
  local decoded
  decoded="$(printf '%s' "${encoded}" | base64 -d)"
  assert_eq "${input}" "${decoded}" "base64 round-trip matches input"
}

test_pid_cmdline_returns_current_shell() {
  local cmdline
  cmdline="$(os::pid_cmdline "$$")"
  if [[ -n "${cmdline}" ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: os::pid_cmdline returned empty string for $$" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

test_pid_cmdline_fails_on_invalid_pid() {
  assert_false "os::pid_cmdline rejects non-numeric PID" os::pid_cmdline "not_a_pid"
  assert_false "os::pid_cmdline rejects non-existent PID" os::pid_cmdline "999999999"
}

test_pid_open_files_emits_clean_absolute_paths_linux() {
  local tmp_file
  tmp_file="$(mktemp)"
  exec 7<"${tmp_file}"
  local found=0
  local line
  while IFS= read -r line; do
    if [[ "${line}" != /* ]]; then
      echo "FAIL: os::pid_open_files emitted non-absolute line: ${line}" >&2
      FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
    if [[ "${line}" == "${tmp_file}" || "${line}" == "/private${tmp_file}" ]]; then
      found=1
    fi
  done < <(OS_COMPAT_UNAME_S="Linux" os::pid_open_files "$$")
  exec 7<&-
  rm -f "${tmp_file}"
  assert_eq "1" "${found}" "os::pid_open_files (Linux) detected open temp file"
}

test_pid_open_files_strips_lsof_prefixes_darwin_mock() {
  # Mock lsof output in Darwin mode to verify prefix stripping
  lsof() {
    printf 'p12345\nf0\nn/dev/null\nf3\nn/tmp/gmosh.log.WARNING.123\n'
  }
  local output
  output="$(OS_COMPAT_UNAME_S="Darwin" os::pid_open_files "12345")"
  unset -f lsof
  local expected=$'/dev/null\n/tmp/gmosh.log.WARNING.123'
  assert_eq "${expected}" "${output}" "os::pid_open_files (Darwin) strips p/f/n prefixes cleanly"
}

test_file_mtime_returns_epoch_seconds() {
  local tmp_file
  tmp_file="$(mktemp)"
  local mtime
  mtime="$(os::file_mtime "${tmp_file}")"
  rm -f "${tmp_file}"
  if [[ "${mtime}" =~ ^[0-9]+$ && "${mtime}" -gt 1700000000 ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: os::file_mtime returned invalid epoch '${mtime}'" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

test_file_mtime_darwin_stat_mock() {
  local tmp_file
  tmp_file="$(mktemp)"
  stat() {
    if [[ "$1" == "-f" && "$2" == "%m" ]]; then
      echo "1729000123"
      return 0
    fi
    return 1
  }
  local mtime
  mtime="$(OS_COMPAT_UNAME_S="Darwin" os::file_mtime "${tmp_file}")"
  unset -f stat
  rm -f "${tmp_file}"
  assert_eq "1729000123" "${mtime}" "os::file_mtime (Darwin) uses stat -f %m"
}

test_clipboard_copy_and_paste_darwin_mock() {
  local clip_file
  clip_file="$(mktemp)"
  pbcopy() {
    cat > "${clip_file}"
  }
  pbpaste() {
    cat "${clip_file}"
  }
  printf 'macOS_clipboard_test_payload' | OS_COMPAT_UNAME_S="Darwin" os::clipboard_copy
  local pasted
  pasted="$(OS_COMPAT_UNAME_S="Darwin" os::clipboard_paste)"
  unset -f pbcopy pbpaste
  rm -f "${clip_file}"
  assert_eq "macOS_clipboard_test_payload" "${pasted}" "os::clipboard_copy and os::clipboard_paste (Darwin) dispatch to pbcopy/pbpaste"
}

test_notify_darwin_terminal_notifier_and_osascript_fallback() {
  local tn_log
  tn_log="$(mktemp)"
  local mock_bin
  mock_bin="$(mktemp -d)"
  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
printf '%s\n' "$*" > "${TN_LOG_FILE}"
EOF
  chmod +x "${mock_bin}/terminal-notifier"

  TN_LOG_FILE="${tn_log}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    os::notify "Test Title" "Test Body" "normal" "test-group-1"
  local recorded
  recorded="$(cat "${tn_log}")"
  rm -f "${tn_log}" "${mock_bin}/terminal-notifier"
  assert_eq "-title Test Title -message Test Body -group test-group-1" "${recorded}" \
    "os::notify (Darwin) invokes terminal-notifier with -group when available"

  local osa_log
  osa_log="$(mktemp)"
  cat << 'EOF' > "${mock_bin}/osascript"
#!/usr/bin/env bash
printf '%s\n' "$*" > "${OSA_LOG_FILE}"
EOF
  chmod +x "${mock_bin}/osascript"
  OSA_LOG_FILE="${osa_log}" PATH="${mock_bin}:/usr/bin:/bin" OS_COMPAT_UNAME_S="Darwin" \
    os::notify 'Title "quoted"' 'Body "quoted"'
  local osa_recorded
  osa_recorded="$(cat "${osa_log}")"
  rm -rf "${osa_log}" "${mock_bin}"
  if [[ "${osa_recorded}" == *"display notification"* && "${osa_recorded}" == *"Title"* ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: os::notify (Darwin) osascript fallback did not format notification: ${osa_recorded}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

test_ping_once_ms_darwin_vs_linux_flags() {
  local ping_log
  ping_log="$(mktemp)"
  ping() {
    printf '%s\n' "$*" > "${ping_log}"
    echo "64 bytes from 8.8.8.8: icmp_seq=0 ttl=118 time=12.3 ms"
    return 0
  }
  local rtt_darwin
  rtt_darwin="$(OS_COMPAT_UNAME_S="Darwin" os::ping_once_ms "8.8.8.8")"
  assert_eq "-c 1 -W 1000 8.8.8.8" "$(cat "${ping_log}")" "os::ping_once_ms (Darwin) passes 1000ms to -W"
  assert_eq "12.3" "${rtt_darwin}" "os::ping_once_ms (Darwin) extracts RTT ms"

  local rtt_linux
  rtt_linux="$(OS_COMPAT_UNAME_S="Linux" os::ping_once_ms "8.8.8.8")"
  assert_eq "-c 1 -W 1 8.8.8.8" "$(cat "${ping_log}")" "os::ping_once_ms (Linux) passes 1s to -W"
  assert_eq "12.3" "${rtt_linux}" "os::ping_once_ms (Linux) extracts RTT ms"
  unset -f ping
  rm -f "${ping_log}"
}

test_pid_is_uninterruptible_darwin_mock() {
  local mock_state="Us+"
  ps() {
    echo "${mock_state}"
  }
  assert_true "os::pid_is_uninterruptible (Darwin) detects U state" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::pid_is_uninterruptible "$$"'
  mock_state="S+"
  assert_false "os::pid_is_uninterruptible (Darwin) returns false on S state" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::pid_is_uninterruptible "$$"'
  unset -f ps
}

test_is_mounted_and_unmount_lazy_darwin_mock() {
  mount() {
    echo "map -hosts on /System/Volumes/Data/net (autofs, nosuid, automounted, nobrowse)"
    echo "user@host:/tmp on /Users/marcelvaldez/mnt/gcloud (osxfuse, nodev, nosuid, synchronous)"
  }
  assert_true "os::is_mounted (Darwin) detects mounted path" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::is_mounted "/Users/marcelvaldez/mnt/gcloud"'
  assert_false "os::is_mounted (Darwin) rejects unmounted path" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::is_mounted "/Users/marcelvaldez/mnt/other"'
  unset -f mount

  local diskutil_args=""
  diskutil() {
    diskutil_args="$*"
    return 0
  }
  OS_COMPAT_UNAME_S="Darwin" os::unmount_lazy "/Users/marcelvaldez/mnt/gcloud"
  unset -f diskutil
  assert_eq "unmount force /Users/marcelvaldez/mnt/gcloud" "${diskutil_args}" \
    "os::unmount_lazy (Darwin) invokes diskutil unmount force"
}

test_play_audio_darwin_afplay_mock() {
  local audio_file
  audio_file="$(mktemp)"
  local afplay_log
  afplay_log="$(mktemp)"
  local mock_bin
  mock_bin="$(mktemp -d)"
  cat << 'EOF' > "${mock_bin}/afplay"
#!/usr/bin/env bash
printf '%s\n' "$1" > "${AFPLAY_LOG}"
EOF
  chmod +x "${mock_bin}/afplay"
  AFPLAY_LOG="${afplay_log}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    os::play_audio "${audio_file}"
  sleep 0.1
  local recorded
  recorded="$(cat "${afplay_log}" 2>/dev/null || true)"
  rm -rf "${audio_file}" "${afplay_log}" "${mock_bin}"
  assert_eq "${audio_file}" "${recorded}" "os::play_audio (Darwin) invokes afplay in background"
}

test_flock_mkdir_fallback_when_flock_absent() {
  local lock_key="test_lock_$$"
  local lock_dir="/tmp/os_compat_lock_${UID:-$(id -u)}_${lock_key}.d"
  rm -rf "${lock_dir}"
  # Force mkdir lockdir fallback by hiding flock in a subshell
  (
    command() {
      if [[ "${1:-}" == "-v" && "${2:-}" == "flock" ]]; then
        return 1
      fi
      builtin command "$@"
    }
    if ! os::flock_nb "${lock_key}"; then
      echo "FAIL: os::flock_nb should acquire unlocked lockdir" >&2
      exit 1
    fi
    # Simulate live foreign PID holding the lockdir
    sleep 10 &
    local holder_pid=$!
    printf '%s\n' "${holder_pid}" > "${lock_dir}/pid"
    if os::flock_nb "${lock_key}"; then
      kill "${holder_pid}" 2>/dev/null || true
      echo "FAIL: os::flock_nb should fail when live PID holds lockdir" >&2
      exit 1
    fi
    kill "${holder_pid}" 2>/dev/null || true
    wait "${holder_pid}" 2>/dev/null || true
    # Now that holder_pid is dead, os::flock_nb should recover stale lockdir
    if ! os::flock_nb "${lock_key}"; then
      echo "FAIL: os::flock_nb should recover stale lockdir when owner PID is dead" >&2
      exit 1
    fi
  )
  PASS_COUNT=$((PASS_COUNT + 3))
  rm -rf "${lock_dir}"
}

test_os_detection_linux_mode
test_os_detection_darwin_mode
test_runtime_dir_darwin_mode
test_base64_nowrap_encodes_multiline_without_newlines
test_pid_cmdline_returns_current_shell
test_pid_cmdline_fails_on_invalid_pid
test_pid_open_files_emits_clean_absolute_paths_linux
test_pid_open_files_strips_lsof_prefixes_darwin_mock
test_file_mtime_returns_epoch_seconds
test_file_mtime_darwin_stat_mock
test_clipboard_copy_and_paste_darwin_mock
test_notify_darwin_terminal_notifier_and_osascript_fallback
test_ping_once_ms_darwin_vs_linux_flags
test_pid_is_uninterruptible_darwin_mock
test_is_mounted_and_unmount_lazy_darwin_mock
test_play_audio_darwin_afplay_mock
test_flock_mkdir_fallback_when_flock_absent

echo "test_os_compat.sh: ${PASS_COUNT} passed, ${FAIL_COUNT} failed."
[[ ${FAIL_COUNT} -eq 0 ]]
