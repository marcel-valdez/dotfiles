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

test_notify_darwin_osascript_fallback_when_terminal_notifier_fails() {
  local mock_bin
  mock_bin="$(mktemp -d)"
  local osa_log
  osa_log="$(mktemp)"
  # Simulate Google Santa blocking the ad-hoc signed terminal-notifier (exit 126)
  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
exit 126
EOF
  cat << 'EOF' > "${mock_bin}/osascript"
#!/usr/bin/env bash
printf '%s\n' "$*" > "${OSA_LOG_FILE}"
EOF
  chmod +x "${mock_bin}/terminal-notifier" "${mock_bin}/osascript"
  local rc=0
  OSA_LOG_FILE="${osa_log}" PATH="${mock_bin}:/usr/bin:/bin" OS_COMPAT_UNAME_S="Darwin" \
    os::notify -g "santa-blocked" "Blocked Title" "Blocked Body" || rc=$?
  local osa_recorded
  osa_recorded="$(cat "${osa_log}")"
  rm -rf "${osa_log}" "${mock_bin}"
  assert_eq "0" "${rc}" "os::notify (Darwin) succeeds via osascript when terminal-notifier fails"
  if [[ "${osa_recorded}" == *"display notification"* && "${osa_recorded}" == *"Blocked Title"* ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: os::notify (Darwin) did not fall back to osascript after terminal-notifier failed: ${osa_recorded}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

test_ensure_emacs_daemon_wait_timeout_and_set_e_safety() {
  # Runs in separate bash processes so `set -e` is genuinely in effect: on
  # Bash >= 4.1 (e.g. Linux) a regression to `((attempts++))` (status 1 when
  # attempts is 0) aborts the wait loop, and an errexit-ignored context such as
  # `( ... ) || rc=$?` would mask that. macOS /bin/bash 3.2 does not abort on a
  # failing (( )), so there this mainly verifies EMACS_DAEMON_WAIT_SECS.
  local sock="os-compat-test-$$"
  local err_file
  err_file="$(mktemp)"
  local rc=0
  bash -c '
    set -e
    source "$1"
    os::emacs_socket_ready() { return 1; }
    os::emacs_bin() { echo /usr/bin/true; }
    pgrep() { return 0; }
    EMACS_DAEMON_WAIT_SECS=1 os::ensure_emacs_daemon --wait "$2"
  ' _ "${HOME}/lib/os_compat.sh" "${sock}" 2> "${err_file}" || rc=$?
  local err_out
  err_out="$(cat "${err_file}")"
  rm -f "${err_file}"
  assert_eq "1" "${rc}" "os::ensure_emacs_daemon --wait returns 1 once EMACS_DAEMON_WAIT_SECS elapses"
  if [[ "${err_out}" == *"Timed out waiting for Emacs daemon (${sock}) after 1s"* ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: os::ensure_emacs_daemon did not reach its timeout path under set -e: ${err_out}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi

  local ready_rc=0
  bash -c '
    set -e
    source "$1"
    _ready_calls=0
    os::emacs_socket_ready() { _ready_calls=$((_ready_calls + 1)); [[ ${_ready_calls} -ge 3 ]]; }
    os::emacs_bin() { echo /usr/bin/true; }
    pgrep() { return 0; }
    os::ensure_emacs_daemon --wait "$2"
  ' _ "${HOME}/lib/os_compat.sh" "${sock}" 2>/dev/null || ready_rc=$?
  assert_eq "0" "${ready_rc}" "os::ensure_emacs_daemon --wait returns 0 once the socket is ready (set -e safe)"
}

test_ssh_mux_only_policy() {
  _mux_only_with() {
    local SSH_MUX_ONLY="$1" OS_COMPAT_UNAME_S="$2"
    os::ssh_mux_only
  }
  assert_true "os::ssh_mux_only: SSH_MUX_ONLY=1 forces mux-only (even on Linux)" _mux_only_with 1 Linux
  assert_false "os::ssh_mux_only: SSH_MUX_ONLY=0 disables mux-only (even on Darwin)" _mux_only_with 0 Darwin
  assert_true "os::ssh_mux_only: defaults to mux-only on Darwin (gMac touch per login)" _mux_only_with "" Darwin
  assert_false "os::ssh_mux_only: defaults to direct SSH on Linux" _mux_only_with "" Linux
  unset -f _mux_only_with
}

test_ssh_master_alive_and_single_notification() {
  local mock_bin notice_dir
  mock_bin="$(mktemp -d)"
  notice_dir="$(mktemp -d)"
  local up_file="${notice_dir}/master_up"
  local notify_log="${notice_dir}/notify.log"
  # Mock ssh: only `ssh -O check` is expected; it succeeds while the master is "up".
  cat << 'EOF' > "${mock_bin}/ssh"
#!/usr/bin/env bash
[[ "$1 $2" == "-O check" && -f "${MOCK_MASTER_UP_FILE}" ]]
EOF
  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
printf '%s\n' "$*" >> "${MOCK_NOTIFY_LOG}"
EOF
  chmod +x "${mock_bin}/ssh" "${mock_bin}/terminal-notifier"
  _with_ssh_mocks() {
    PATH="${mock_bin}:/usr/bin:/bin" OS_COMPAT_UNAME_S="Darwin" \
      SSH_MASTER_NOTICE_DIR="${notice_dir}" MOCK_MASTER_UP_FILE="${up_file}" \
      MOCK_NOTIFY_LOG="${notify_log}" "$@"
  }

  assert_false "os::ssh_master_alive is false when no ControlMaster is up" \
    _with_ssh_mocks os::ssh_master_alive "user@gcloud"
  _with_ssh_mocks os::ssh_master_notify_missing "gcloud" "Knock daemon"
  _with_ssh_mocks os::ssh_master_notify_missing "gcloud" "Clipboard daemon"
  assert_eq "1" "$(grep -c "No shared SSH connection to gcloud" "${notify_log}" 2>/dev/null)" \
    "os::ssh_master_notify_missing notifies once per outage across daemons"
  touch "${up_file}"
  assert_true "os::ssh_master_alive is true when the ControlMaster is up" \
    _with_ssh_mocks os::ssh_master_alive "user@gcloud"
  _with_ssh_mocks os::ssh_master_notice_reset "gcloud"
  _with_ssh_mocks os::ssh_master_notify_missing "gcloud" "Knock daemon"
  assert_eq "2" "$(grep -c "No shared SSH connection to gcloud" "${notify_log}" 2>/dev/null)" \
    "os::ssh_master_notice_reset re-arms the notification for the next outage"
  unset -f _with_ssh_mocks
  rm -rf "${mock_bin}" "${notice_dir}"
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

test_cpu_count_darwin_mock_and_linux() {
  sysctl() {
    [[ "$*" == "-n hw.activecpu" ]] && echo "15"
  }
  assert_eq "15" "$(OS_COMPAT_UNAME_S="Darwin" os::cpu_count)" \
    "os::cpu_count (Darwin) reads sysctl hw.activecpu"
  unset -f sysctl

  getconf() {
    [[ "$*" == "_NPROCESSORS_ONLN" ]] && echo "8"
  }
  assert_eq "8" "$(OS_COMPAT_UNAME_S="Linux" os::cpu_count)" \
    "os::cpu_count (Linux) reads getconf _NPROCESSORS_ONLN"
  getconf() { echo "garbage"; }
  assert_eq "1" "$(OS_COMPAT_UNAME_S="Linux" os::cpu_count || true)" \
    "os::cpu_count falls back to 1 on unparsable output"
  unset -f getconf
}

test_load_averages_darwin_mock_and_linux_proc() {
  sysctl() {
    [[ "$*" == "-n vm.loadavg" ]] && echo "{ 2.31 2.14 2.04 }"
  }
  assert_eq "2.31 2.14 2.04" "$(OS_COMPAT_UNAME_S="Darwin" os::load_averages)" \
    "os::load_averages (Darwin) parses sysctl vm.loadavg"
  sysctl() { echo "{ n/a }"; }
  assert_false "os::load_averages (Darwin) fails on unparsable sysctl output" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::load_averages >/dev/null'
  unset -f sysctl

  local proc_root
  proc_root="$(mktemp -d)"
  echo "0.52 0.58 0.59 1/1234 5678" > "${proc_root}/loadavg"
  assert_eq "0.52 0.58 0.59" \
    "$(OS_COMPAT_UNAME_S="Linux" OS_COMPAT_PROC_ROOT="${proc_root}" os::load_averages)" \
    "os::load_averages (Linux) reads the first three /proc/loadavg fields"
  rm -f "${proc_root}/loadavg"
  assert_false "os::load_averages (Linux) fails when /proc/loadavg is missing" \
    eval 'OS_COMPAT_UNAME_S="Linux" OS_COMPAT_PROC_ROOT="${proc_root}" os::load_averages >/dev/null'
  rm -rf "${proc_root}"
}

test_memory_used_total_mb_darwin_vm_stat_mock() {
  sysctl() {
    [[ "$*" == "-n hw.memsize" ]] && echo "51539607552"
  }
  vm_stat() {
    cat << 'EOF'
Mach Virtual Memory Statistics: (page size of 16384 bytes)
Pages free:                                     8335.
Pages active:                                 721231.
Pages inactive:                               716516.
Pages speculative:                              3211.
Pages wired down:                             552875.
File-backed pages:                            363966.
Anonymous pages:                             1080000.
EOF
  }
  # used = 48 GiB - (8335 free + 363966 file-backed) * 16 KiB pages
  assert_eq "43334 49152" "$(OS_COMPAT_UNAME_S="Darwin" os::memory_used_total_mb)" \
    "os::memory_used_total_mb (Darwin) counts free + file-backed pages as available"
  vm_stat() { echo "Mach Virtual Memory Statistics: (page size of 16384 bytes)"; }
  assert_false "os::memory_used_total_mb (Darwin) fails without vm_stat page counts" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::memory_used_total_mb >/dev/null'
  unset -f sysctl vm_stat
}

test_memory_used_total_mb_linux_proc_meminfo() {
  local proc_root
  proc_root="$(mktemp -d)"
  cat << 'EOF' > "${proc_root}/meminfo"
MemTotal:       16384000 kB
MemFree:         2048000 kB
MemAvailable:    9000000 kB
Buffers:          512000 kB
Cached:          4096000 kB
SwapCached:       999999 kB
Shmem:            256000 kB
SReclaimable:     128000 kB
EOF
  # used = 16384000 - 2048000 + 256000 - 512000 - 4096000 - 128000 kB (SwapCached ignored)
  assert_eq "9625 16000" \
    "$(OS_COMPAT_UNAME_S="Linux" OS_COMPAT_PROC_ROOT="${proc_root}" os::memory_used_total_mb)" \
    "os::memory_used_total_mb (Linux) applies the tmux-mem-cpu-load /proc/meminfo formula"
  rm -rf "${proc_root}"
}

test_cpu_busy_percent_darwin_iostat_mock() {
  local iostat_log
  iostat_log="$(mktemp)"
  iostat() {
    printf '%s\n' "$*" > "${iostat_log}"
    cat << 'EOF'
      cpu    load average
 us sy id   1m   5m   15m
  7  5 88  4.65 3.71 3.83
  8  6 87  4.65 3.71 3.83
EOF
  }
  assert_eq "13" "$(OS_COMPAT_UNAME_S="Darwin" os::cpu_busy_percent 2)" \
    "os::cpu_busy_percent (Darwin) is 100 - idle of the last iostat sample"
  assert_eq "-n 0 -c 2 -w 2" "$(cat "${iostat_log}")" \
    "os::cpu_busy_percent (Darwin) samples iostat over the requested interval"
  iostat() { echo "iostat: unavailable"; }
  assert_false "os::cpu_busy_percent (Darwin) fails without iostat samples" \
    eval 'OS_COMPAT_UNAME_S="Darwin" os::cpu_busy_percent 1 >/dev/null'
  unset -f iostat
  rm -f "${iostat_log}"
}

test_cpu_busy_percent_linux_proc_stat() {
  assert_eq "47" "$(os::_cpu_busy_from_ticks 100 0 50 850 160 0 70 940)" \
    "os::_cpu_busy_from_ticks computes busy share of elapsed ticks"
  assert_eq "0" "$(os::_cpu_busy_from_ticks 5 5 5 5 5 5 5 5)" \
    "os::_cpu_busy_from_ticks reports 0 when no ticks elapsed"
  assert_false "os::_cpu_busy_from_ticks rejects non-numeric ticks" \
    os::_cpu_busy_from_ticks 1 2 3 x 5 6 7 8

  local proc_root
  proc_root="$(mktemp -d)"
  echo "cpu  100 0 50 850 0 0 0 0 0 0" > "${proc_root}/stat"
  assert_eq "0" "$(OS_COMPAT_UNAME_S="Linux" OS_COMPAT_PROC_ROOT="${proc_root}" os::cpu_busy_percent 1)" \
    "os::cpu_busy_percent (Linux) samples /proc/stat twice"
  rm -rf "${proc_root}"
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
test_notify_darwin_osascript_fallback_when_terminal_notifier_fails
test_ensure_emacs_daemon_wait_timeout_and_set_e_safety
test_ssh_mux_only_policy
test_ssh_master_alive_and_single_notification
test_ping_once_ms_darwin_vs_linux_flags
test_pid_is_uninterruptible_darwin_mock
test_is_mounted_and_unmount_lazy_darwin_mock
test_play_audio_darwin_afplay_mock
test_flock_mkdir_fallback_when_flock_absent
test_cpu_count_darwin_mock_and_linux
test_load_averages_darwin_mock_and_linux_proc
test_memory_used_total_mb_darwin_vm_stat_mock
test_memory_used_total_mb_linux_proc_meminfo
test_cpu_busy_percent_darwin_iostat_mock
test_cpu_busy_percent_linux_proc_stat

echo "test_os_compat.sh: ${PASS_COUNT} passed, ${FAIL_COUNT} failed."
[[ ${FAIL_COUNT} -eq 0 ]]
