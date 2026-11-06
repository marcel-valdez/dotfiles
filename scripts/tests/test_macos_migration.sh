#!/usr/bin/env bash
# ==============================================================================
# Unit & Integration Test Suite: macOS (Darwin) Dotfiles & Daemon Migration
# ==============================================================================
# Verifies all macOS-specific branches, scripts, and configuration files across:
# 1. ~/.googlerc Darwin CLIPBOARD_PORT=3335 and PS1_HOST=macbook detection
# 2. jetski_resurrect_motd.sh Darwin sysctl kern.boottime parsing
# 3. kitty/tab_bar.py get_cpu_temp() graceful None fallback without /sys/class
# 4. kitten-cmd live-PID /tmp/kitty_tmux-<pid> socket selection & dead cleanup
# 5. ~/bin/do-not-disturb Darwin enable, disable, toggle, and --status
# 6. ~/scripts/zen-mode.sh Darwin enable, status, and disable transitions
# 7. ~/.pomodororc & ~/.config/swiftbar/pomodoro.1s.sh lifecycle + audio hook
# 8. ~/scripts/whatsapp-notification.sh Darwin 0-arg daemon & 2-arg callback
# 9. ~/scripts/airgradient-notify-hook.sh & ~/.config/swiftbar/airgradient.10s.sh
# 10. ~/bin/alt-google-chrome Darwin binary path resolution
# 11. ~/scripts/setup_work_environment.sh Darwin port 3335 & sed gcert parsing
# 12. bootstrap_macos.sh --dry-run, launchd .plist XMLs, and JSON configs
#
# Usage Example:
#   bash ~/scripts/tests/test_macos_migration.sh
# ==============================================================================

set -euo pipefail

PASS_COUNT=0
FAIL_COUNT=0

TEST_TMP="$(mktemp -d /tmp/test_macos_migration.XXXXXX)"
cleanup() {
  rm -rf "${TEST_TMP}"
}
trap cleanup EXIT INT TERM

assert_eq() {
  local label="$1"
  local expected="$2"
  local actual="$3"
  if [[ "${expected}" == "${actual}" ]]; then
    echo "  [PASS] ${label}"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] ${label} (Expected: '${expected}', Actual: '${actual}')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_contains() {
  local label="$1"
  local needle="$2"
  local haystack="$3"
  if [[ "${haystack}" == *"${needle}"* ]]; then
    echo "  [PASS] ${label}"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] ${label} (Missing '${needle}' in '${haystack}')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

assert_true() {
  local label="$1"
  shift
  if "$@"; then
    echo "  [PASS] ${label}"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] ${label}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

echo "=============================================================================="
echo "Running macOS (Darwin) Migration Unit & Integration Test Suite"
echo "=============================================================================="

# ------------------------------------------------------------------------------
# Test 1: ~/.googlerc Darwin CLIPBOARD_PORT=3335 and PS1_HOST=macbook
# ------------------------------------------------------------------------------
echo "[Test 1] ~/.googlerc Darwin CLIPBOARD_PORT=3335 & PS1_HOST=macbook"
{
  mock_bin="${TEST_TMP}/t1_bin"
  fake_home="${TEST_TMP}/t1_home"
  mkdir -p "${mock_bin}" "${fake_home}"
  cat << 'EOF' > "${mock_bin}/uname"
#!/usr/bin/env bash
echo "Darwin"
EOF
  chmod +x "${mock_bin}/uname"
  cat << 'EOF' > "${mock_bin}/hostname"
#!/usr/bin/env bash
echo "marcelvaldez-macbookpro.roam.internal"
EOF
  chmod +x "${mock_bin}/hostname"

  real_googlerc="${HOME}/.googlerc.d/.googlerc"
  out="$(HOME="${fake_home}" REAL_GOOGLERC="${real_googlerc}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" bash -c '
    unset PS1_HOST CLIPBOARD_PORT
    source "${REAL_GOOGLERC}" >/dev/null 2>&1 || true
    printf "%s|%s\n" "${PS1_HOST:-}" "${CLIPBOARD_PORT:-}"
  ')"
  assert_eq "Darwin .googlerc sets PS1_HOST=macbook and CLIPBOARD_PORT=3335" \
    "macbook|3335" "${out}"

  mkdir -p "${fake_home}/.config/clipboard-daemon"
  echo "3336" > "${fake_home}/.config/clipboard-daemon/port"
  out_override="$(HOME="${fake_home}" REAL_GOOGLERC="${real_googlerc}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" bash -c '
    unset PS1_HOST CLIPBOARD_PORT
    source "${REAL_GOOGLERC}" >/dev/null 2>&1 || true
    printf "%s|%s\n" "${PS1_HOST:-}" "${CLIPBOARD_PORT:-}"
  ')"
  assert_eq "Per-machine ~/.config/clipboard-daemon/port overrides the Darwin default (3336)" \
    "macbook|3336" "${out_override}"
}

# ------------------------------------------------------------------------------
# Test 2: jetski_resurrect_motd.sh Darwin sysctl kern.boottime parsing
# ------------------------------------------------------------------------------
echo "[Test 2] jetski_resurrect_motd.sh Darwin sysctl kern.boottime parsing"
{
  mock_bin="${TEST_TMP}/t2_bin"
  mkdir -p "${mock_bin}"
  cat << 'EOF' > "${mock_bin}/sysctl"
#!/usr/bin/env bash
if [[ "$*" == "-n kern.boottime" ]]; then
  echo "{ sec = 1729000500, usec = 123456 } Tue Oct 15 14:01:40 2026"
  exit 0
fi
exit 1
EOF
  chmod +x "${mock_bin}/sysctl"

  boot_ts="$(PATH="${mock_bin}:${PATH}" bash -c '
    sysctl -n kern.boottime 2>/dev/null | sed -n "s/.*[^u]sec = \([0-9][0-9]*\).*/\1/p"
  ')"
  assert_eq "Darwin sysctl kern.boottime extracts epoch seconds cleanly" \
    "1729000500" "${boot_ts}"
  assert_true "jetski_resurrect_motd.sh contains kern.boottime fallback" \
    grep -q "kern.boottime" "${HOME}/.googlerc.d/scripts/jetski_resurrect_motd.sh"
}

# ------------------------------------------------------------------------------
# Test 3: kitty/tab_bar.py get_cpu_temp() graceful fallback
# ------------------------------------------------------------------------------
echo "[Test 3] ~/.config/kitty/tab_bar.py syntax and thermal fallback"
{
  py_test_script="${TEST_TMP}/test_tab_bar.py"
  cat << 'EOF' > "${py_test_script}"
import ast
import os
import pathlib
import sys

src = pathlib.Path.home() / ".config/kitty/tab_bar.py"
tree = ast.parse(src.read_text())
for node in tree.body:
    if isinstance(node, ast.FunctionDef) and node.name == "get_cpu_temp":
        code = compile(ast.Module(body=[node], type_ignores=[]), filename="<ast>", mode="exec")
        ns = {"os": os}
        exec(code, ns)
        # Call get_cpu_temp(); ensure it returns int or None without raising
        res = ns["get_cpu_temp"]()
        assert res is None or isinstance(res, int), f"Unexpected return: {res}"
        print("OK")
        sys.exit(0)
sys.exit(1)
EOF
  out="$(python3 "${py_test_script}")"
  assert_eq "tab_bar.py get_cpu_temp() executes cleanly without raising" "OK" "${out}"
}

# ------------------------------------------------------------------------------
# Test 4: kitten-cmd live-PID /tmp/kitty_tmux-<pid> socket selection
# ------------------------------------------------------------------------------
echo "[Test 4] kitten-cmd live-PID socket selection and stale socket cleanup"
{
  # Hermetic sandbox: globbing the real /tmp would pick up a running kitty's own
  # live socket (e.g. /tmp/kitty_tmux-<kitty pid>) and the cleanup branch could
  # delete real stale sockets.
  sock_dir="${TEST_TMP}/t4_sockets"
  mkdir -p "${sock_dir}"
  # The dead socket (PID 999997 > macOS pid_max) sorts *before* the live one, so
  # the loop must really visit and clean it up before selecting the live socket.
  dead_sock="${sock_dir}/kitty_tmux-0999997"
  live_sock="${sock_dir}/kitty_tmux-$$"
  touch "${dead_sock}" "${live_sock}"

  selected="$(SOCK_DIR="${sock_dir}" bash -c '
    chosen=""
    for candidate in "${SOCK_DIR}"/kitty_tmux-*; do
      [[ -e "${candidate}" ]] || continue
      cand_pid="${candidate##*-}"
      if [[ "${cand_pid}" =~ ^[0-9]+$ ]] && kill -0 "${cand_pid}" 2>/dev/null; then
        chosen="${candidate}"
        break
      else
        rm -f "${candidate}" 2>/dev/null || true
      fi
    done
    printf "%s" "${chosen}"
  ')"
  assert_eq "Live PID socket selected over dead PID socket" "${live_sock}" "${selected}"
  assert_true "Dead PID socket was cleaned up" test ! -e "${dead_sock}"
  assert_true "kitty-ssh-launch uses Bash 3.2-compatible read -n 1 (not read -N 1)" \
    grep -q "read -n 1" "${HOME}/.googlerc.d/bin/kitty-ssh-launch"
  assert_true "retry_after_key.sh guards fractional read -t 0.05 on BASH_VERSINFO[0] >= 4" \
    grep -q "BASH_VERSINFO\[0\] >= 4" "${HOME}/scripts/retry_after_key.sh"
  fzf_stripped="$(perl -pe 's/^[^\t]*\t//gm' <<< $'-42\twhich emacs')"
  assert_eq "__fzf_history__ strips negative history index prefix (-42\\t)" "which emacs" "${fzf_stripped}"

  # Hermetic end-to-end regression test for ~/.fzf.bash and __fzf_history__:
  # 1. Verifies PATH demotion of ${HOME}/.fzf/bin to the end of PATH.
  # 2. Verifies __fzf_defaults, __fzfcmd, __fzf_history_delete, __fzf_history__
  #    are defined in both non-interactive and interactive shells.
  # 3. Verifies single-line, multi-line, macOS Bash 3.2 unset-READLINE_POINT
  #    stdout mode, unset FZF_DEFAULT_OPTS_FILE/FZF_CTRL_R_OPTS, and no-match
  #    exit code 1 handling with zero leaked history numbers or concatenated
  #    neighbor entries.
  fzf_e2e_out="$(
    set -euo pipefail
    export HISTFILE=/dev/null
    export HISTCONTROL=
    export HISTSIZE=100
    export PATH="${HOME}/.fzf/bin:${PATH}"
    source "${HOME}/.fzf.bash"

    declare -F __fzf_defaults __fzfcmd __fzf_history_delete __fzf_history__ >/dev/null
    if [[ -d "${HOME}/.fzf/bin" ]]; then
      [[ "${PATH}" == *":${HOME}/.fzf/bin" ]] || {
        echo "FAIL: ~/.fzf/bin not demoted to end of PATH (${PATH})" >&2
        exit 1
      }
    fi

    _test_fzf_filter() {
      fzf --filter="$2" --print0 |
        command perl -0 -ne 'if ($. == 1) { chomp; print; $ok = 1; exit } END { exit($ok ? 0 : 1) }'
    }
    __fzfcmd() { echo _test_fzf_filter; }

    builtin history -c
    builtin history -s "echo alpha_first_cmd"
    builtin history -s $'echo multi_line_one\necho multi_line_two'
    builtin history -s "hg status --rev p4head"
    builtin history -s ": __fzf_test_sentinel__"

    # Path 1: Single-line selection (READLINE_POINT=0)
    READLINE_LINE="hg status"
    READLINE_POINT=0
    __fzf_history__
    [[ "${READLINE_LINE}" == "hg status --rev p4head" ]] || {
      echo "FAIL single-line: got [${READLINE_LINE}]" >&2
      exit 1
    }

    # Path 2: Multi-line selection (READLINE_POINT=0)
    READLINE_LINE="multi_line_two"
    READLINE_POINT=0
    __fzf_history__
    [[ "${READLINE_LINE}" == $'echo multi_line_one\necho multi_line_two' ]] || {
      echo "FAIL multi-line: got [${READLINE_LINE}]" >&2
      exit 1
    }

    # Path 3: macOS /bin/bash 3.2 unset READLINE_POINT stdout mode
    unset READLINE_POINT
    READLINE_LINE="alpha_first_cmd"
    stdout_res="$(__fzf_history__)"
    [[ "${stdout_res}" == "echo alpha_first_cmd" ]] || {
      echo "FAIL bash32-stdout: got [${stdout_res}]" >&2
      exit 1
    }

    # Path 4: Unset FZF_DEFAULT_OPTS_FILE and FZF_CTRL_R_OPTS post-source
    unset FZF_DEFAULT_OPTS_FILE FZF_CTRL_R_OPTS
    READLINE_LINE="hg status"
    READLINE_POINT=0
    __fzf_history__
    [[ "${READLINE_LINE}" == "hg status --rev p4head" ]] || {
      echo "FAIL unset-opts: got [${READLINE_LINE}]" >&2
      exit 1
    }

    # Path 5: No-match query (_test_fzf_filter exits 1 under set -euo pipefail)
    READLINE_LINE="__nonexistent_history_query_token_999__"
    READLINE_POINT=0
    __fzf_history__
    echo "PASS"
  )"
  assert_eq "~/.fzf.bash __fzf_history__ end-to-end (single-line, multi-line, bash3.2 stdout, no-match)" "PASS" "${fzf_e2e_out}"

  fzf_interactive_out="$(
    bash --norc -i -c '
      set -euo pipefail
      unset FZF_DEFAULT_OPTS_FILE
      source "${HOME}/.fzf.bash"
      declare -F __fzf_defaults __fzf_history__ >/dev/null
      if [[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]]; then
        declare -F __fzf_exec_awk __fzf_orig_completion_get_orig_func >/dev/null
      fi
      defs_out="$(__fzf_defaults "" "--read0")"
      [[ "${defs_out}" == *"--read0"* ]]
      printf "PASS"
    ' 2>/dev/null
  )"
  assert_eq "Interactive bash --norc -i ~/.fzf.bash defines modern 0.73.1 functions and --read0" "PASS" "${fzf_interactive_out}"
}

# ------------------------------------------------------------------------------
# Test 5: ~/bin/do-not-disturb Darwin enable, disable, toggle, and --status
# ------------------------------------------------------------------------------
echo "[Test 5] ~/bin/do-not-disturb Darwin state machine"
{
  fake_home="${TEST_TMP}/t5_home"
  mock_bin="${TEST_TMP}/t5_bin"
  mkdir -p "${fake_home}/lib" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"

  cat << 'EOF' > "${mock_bin}/shortcuts"
#!/usr/bin/env bash
echo "$*" >> "${TEST_SHORTCUTS_LOG}"
exit 0
EOF
  chmod +x "${mock_bin}/shortcuts"
  # Hermetic notifications: do-not-disturb announces state changes via os::notify,
  # which would otherwise invoke the real terminal-notifier/osascript.
  for _cmd in terminal-notifier osascript; do
    printf '#!/usr/bin/env bash\nexit 0\n' > "${mock_bin}/${_cmd}"
    chmod +x "${mock_bin}/${_cmd}"
  done
  unset _cmd

  sc_log="${TEST_TMP}/t5_shortcuts.log"
  export TEST_SHORTCUTS_LOG="${sc_log}"

  s0="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/do-not-disturb" --status)"
  assert_eq "Initial Darwin DND status is false" "false" "${s0}"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/do-not-disturb" enable
  s1="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/do-not-disturb" --status)"
  assert_eq "Darwin DND status after enable is true" "true" "${s1}"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/do-not-disturb" toggle
  s2="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/do-not-disturb" --status)"
  assert_eq "Darwin DND status after toggle is false" "false" "${s2}"

  for _i in {1..20}; do
    [[ -s "${sc_log}" ]] && break
    sleep 0.1
  done
  assert_contains "shortcuts run Set Do Not Disturb invoked" \
    "run Set Do Not Disturb" "$(cat "${sc_log}" 2>/dev/null || true)"
}

# ------------------------------------------------------------------------------
# Test 6: ~/scripts/zen-mode.sh Darwin enable, status, and disable
# ------------------------------------------------------------------------------
echo "[Test 6] ~/scripts/zen-mode.sh Darwin enable, status, and disable"
{
  fake_home="${TEST_TMP}/t6_home"
  mock_bin="${TEST_TMP}/t6_bin"
  mkdir -p "${fake_home}/lib" "${fake_home}/bin" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"
  cp "${HOME}/bin/do-not-disturb" "${fake_home}/bin/do-not-disturb"

  for cmd in shortcuts defaults osascript terminal-notifier; do
    cat << EOF > "${mock_bin}/${cmd}"
#!/usr/bin/env bash
echo "${cmd}:\$*" >> "${TEST_TMP}/t6_calls.log"
if [[ "${cmd}" == "defaults" && "\$1" == "read" ]]; then
  echo "0"
fi
exit 0
EOF
    chmod +x "${mock_bin}/${cmd}"
  done

  st_init="$(HOME="${fake_home}" PATH="${mock_bin}:${fake_home}/bin:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/zen-mode.sh" status)"
  assert_eq "Initial Darwin zen-mode status is off" "off" "${st_init}"

  HOME="${fake_home}" PATH="${mock_bin}:${fake_home}/bin:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/zen-mode.sh" enable
  st_on="$(HOME="${fake_home}" PATH="${mock_bin}:${fake_home}/bin:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/zen-mode.sh" status)"
  assert_eq "Darwin zen-mode status after enable is on" "on" "${st_on}"

  HOME="${fake_home}" PATH="${mock_bin}:${fake_home}/bin:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/zen-mode.sh" disable
  st_off="$(HOME="${fake_home}" PATH="${mock_bin}:${fake_home}/bin:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/zen-mode.sh" status)"
  assert_eq "Darwin zen-mode status after disable is off" "off" "${st_off}"

  calls="$(cat "${TEST_TMP}/t6_calls.log")"
  assert_contains "zen-mode toggled Dock autohide via defaults" \
    "defaults:write com.apple.dock autohide -bool true" "${calls}"
  assert_contains "zen-mode toggled menu bar autohide via osascript" \
    "osascript:-e tell application \"System Events\"" "${calls}"
}

# ------------------------------------------------------------------------------
# Test 7: ~/.pomodororc & ~/.config/swiftbar/pomodoro.1s.sh lifecycle
# ------------------------------------------------------------------------------
echo "[Test 7] ~/.pomodororc & ~/.config/swiftbar/pomodoro.1s.sh lifecycle"
{
  fake_home="${TEST_TMP}/t7_home"
  mock_bin="${TEST_TMP}/t7_bin"
  mkdir -p "${fake_home}/lib" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"
  cp "${HOME}/.pomodororc" "${fake_home}/.pomodororc"

  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
exit 0
EOF
  chmod +x "${mock_bin}/terminal-notifier"

  sb_idle="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh")"
  assert_contains "SwiftBar pomodoro idle shows tomato icon" "🍅" "${sb_idle}"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh" start
  sb_run="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh")"
  assert_contains "SwiftBar pomodoro running shows countdown timer" "🍅 " "${sb_run}"
  assert_contains "SwiftBar pomodoro running menu offers Pause" "Pause Pomodoro | bash=" "${sb_run}"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh" pause
  sb_pause="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh")"
  assert_contains "SwiftBar pomodoro paused shows pause icon" "⏸" "${sb_pause}"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/.config/swiftbar/pomodoro.1s.sh" stop
  assert_true "Pomodoro state file removed on stop" test ! -f "${fake_home}/.cache/pomodoro_state"
}

# ------------------------------------------------------------------------------
# Test 8: ~/scripts/whatsapp-notification.sh Darwin daemon & callback modes
# ------------------------------------------------------------------------------
echo "[Test 8] ~/scripts/whatsapp-notification.sh Darwin daemon & callback modes"
{
  fake_home="${TEST_TMP}/t8_home"
  mock_bin="${TEST_TMP}/t8_bin"
  mkdir -p "${fake_home}/lib" "${fake_home}/.config/whatsapp-notification" \
    "${fake_home}/projects/whatsapp-notification/bin" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"
  cp "${HOME}/lib/log_lib.sh" "${fake_home}/lib/log_lib.sh"
  echo 'WHATSAPP_TARGET_JIDS="12345@s.whatsapp.net,67890@s.whatsapp.net"' > "${fake_home}/.config/whatsapp-notification/config.env"

  cat << 'EOF' > "${fake_home}/projects/whatsapp-notification/bin/whatsapp-notification"
#!/usr/bin/env bash
if [[ "${1:-}" == "-h" ]]; then
  echo "Usage of whatsapp-notification: -target string"
  exit 0
fi
echo "WA_CLI_ARGS:$*" > "${TEST_WA_LOG}"
exit 0
EOF
  chmod +x "${fake_home}/projects/whatsapp-notification/bin/whatsapp-notification"

  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
echo "TN_ARGS:$*" > "${TEST_WA_TN_LOG}"
exit 0
EOF
  chmod +x "${mock_bin}/terminal-notifier"

  export TEST_WA_LOG="${TEST_TMP}/t8_wa.log"
  export TEST_WA_TN_LOG="${TEST_TMP}/t8_tn.log"

  # 1. 0-arg daemon launcher mode
  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/whatsapp-notification.sh"
  wa_logged="$(cat "${TEST_WA_LOG}")"
  assert_contains "whatsapp-notification.sh 0-arg mode passes -target JIDs from config.env" \
    "-target 12345@s.whatsapp.net,67890@s.whatsapp.net -exec" "${wa_logged}"

  # 2. 2-arg notification callback mode on Darwin
  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/scripts/whatsapp-notification.sh" "Alice" "Hello from WhatsApp"
  for _i in {1..20}; do
    [[ -s "${TEST_WA_TN_LOG}" ]] && break
    sleep 0.1
  done
  tn_logged="$(cat "${TEST_WA_TN_LOG}" 2>/dev/null || true)"
  assert_contains "whatsapp-notification.sh 2-arg Darwin mode dispatches via terminal-notifier" \
    "-title Alice -message Hello from WhatsApp -subtitle WhatsApp -group whatsapp-notification" "${tn_logged}"
}

# ------------------------------------------------------------------------------
# Test 9: airgradient-notify-hook.sh & SwiftBar airgradient.10s.sh
# ------------------------------------------------------------------------------
echo "[Test 9] ~/scripts/airgradient-notify-hook.sh & ~/.config/swiftbar/airgradient.10s.sh"
{
  fake_home="${TEST_TMP}/t9_home"
  mock_bin="${TEST_TMP}/t9_bin"
  mkdir -p "${fake_home}/lib" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"

  cat << 'EOF' > "${mock_bin}/terminal-notifier"
#!/usr/bin/env bash
echo "$*" > "${TEST_AG_TN_LOG}"
exit 0
EOF
  chmod +x "${mock_bin}/terminal-notifier"
  export TEST_AG_TN_LOG="${TEST_TMP}/t9_tn.log"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    AG_METRIC="rco2" AG_STATUS="CRITICAL" AG_SUMMARY="CO2 High" AG_BODY="1250 ppm" \
    bash "${HOME}/scripts/airgradient-notify-hook.sh"
  assert_contains "airgradient-notify-hook.sh sends notification with airgradient-rco2 group" \
    "-title CO2 High -message 1250 ppm -subtitle AirGradient -group airgradient-rco2" \
    "$(cat "${TEST_AG_TN_LOG}")"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    AG_METRIC="pm02" AG_URGENCY="critical" AG_TITLE="PM2.5 Alert" AG_VALUE_TEXT="42 ug/m3" \
    bash "${HOME}/scripts/airgradient-notify-hook.sh"
  assert_contains "airgradient-notify-hook.sh supports native AG_TITLE/AG_URGENCY/AG_VALUE_TEXT env vars" \
    "-title PM2.5 Alert -message Metric pm02: 42 ug/m3 (threshold unknown) -subtitle AirGradient -group airgradient-pm02" \
    "$(cat "${TEST_AG_TN_LOG}")"

  mkdir -p "${fake_home}/.config/airgradient-monitor"
  echo '{}' > "${fake_home}/.config/airgradient-monitor/config.json"

  cat << 'EOF' > "${mock_bin}/airgradient-monitor"
#!/usr/bin/env bash
echo "AG_ARGS:$*" > "${TEST_AG_BIN_LOG}"
if [[ "${1:-}" == "-genmon" ]]; then
  cat << 'XML'
<txt><span foreground='#ff5555'>CO2: 950ppm</span></txt>
<tool>PM2.5: 8 ug/m3
Temp: 22.4C</tool>
XML
  exit 0
fi
exit 1
EOF
  chmod +x "${mock_bin}/airgradient-monitor"
  export TEST_AG_BIN_LOG="${TEST_TMP}/t9_ag_bin.log"

  sb_ag_default="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" bash "${HOME}/.config/swiftbar/airgradient.10s.sh")"
  assert_eq "SwiftBar airgradient is disabled by default on gMac (empty output)" \
    "" "${sb_ag_default}"

  sb_ag="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" AIRGRADIENT_ENABLED=1 bash "${HOME}/.config/swiftbar/airgradient.10s.sh")"
  assert_contains "SwiftBar airgradient strips XML/Pango tags from <txt> when enabled" \
    "CO2: 950ppm" "${sb_ag}"
  assert_contains "SwiftBar airgradient includes <tool> lines in dropdown" \
    "PM2.5: 8 ug/m3" "${sb_ag}"
  assert_contains "SwiftBar airgradient passes explicit -config path on Darwin" \
    "-genmon -config ${fake_home}/.config/airgradient-monitor/config.json" \
    "$(cat "${TEST_AG_BIN_LOG}")"
}

# ------------------------------------------------------------------------------
# Test 10: ~/bin/alt-google-chrome Darwin binary resolution
# ------------------------------------------------------------------------------
echo "[Test 10] ~/bin/alt-google-chrome syntax and Darwin binary support"
{
  assert_true "alt-google-chrome passes bash -n syntax check" \
    bash -n "${HOME}/bin/alt-google-chrome"
  assert_true "alt-google-chrome resolves macOS Google Chrome.app bundle" \
    grep -q "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" "${HOME}/bin/alt-google-chrome"
}

# ------------------------------------------------------------------------------
# Test 11: ~/scripts/setup_work_environment.sh Darwin helpers
# ------------------------------------------------------------------------------
echo "[Test 11] ~/scripts/setup_work_environment.sh Darwin port 3335 & sed gcert parser"
{
  assert_true "setup_work_environment.sh passes bash -n syntax check" \
    bash -n "${HOME}/scripts/setup_work_environment.sh"

  # Test portable sed hours extraction used in get_gcert_ssh_hours_remaining
  hours="$(echo "SSH Certificate status: VALID (expires in 17h 42m)" | \
    sed -n 's/.*expires in \([0-9][0-9]*\)h.*/\1/p')"
  assert_eq "Portable sed extracts remaining gcert hours without grep -P" "17" "${hours}"

  # macOS (macFUSE FSKit): sshfs cannot fork after mounting and stays in the
  # foreground for the life of the mount, so it must be backgrounded instead of
  # blocking. FSKit volumes also stop listing their root directory ~30 s after
  # mounting, so the remote home is mounted once on ~/.gcloud_home and ~/notes,
  # ~/gtd and ~/gcloud_tmp become symlinks into it.
  fake_home="${TEST_TMP}/t11_home"
  mock_bin="${TEST_TMP}/t11_bin"
  # ~/notes: empty leftover mount point directory from the per-folder layout
  mkdir -p "${fake_home}/lib" "${mock_bin}" "${fake_home}/notes"
  ln -sf "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"
  cat << 'EOF' > "${fake_home}/lib/log_lib.sh"
log::info() { :; }
log::warn() { echo "[WARN] $*" >&2; }
log::error() { echo "[ERROR] $*" >&2; }
log::fatal() { echo "[FATAL] $*" >&2; exit 1; }
log::debug() { :; }
EOF
  export T11_MOUNTED="${TEST_TMP}/t11_mounted"
  : > "${T11_MOUNTED}"
  : > "${T11_MOUNTED}.calls"
  : > "${T11_MOUNTED}.pids"
  cat << 'EOF' > "${mock_bin}/sshfs"
#!/usr/bin/env bash
echo "$*" >> "${T11_MOUNTED}.calls"
echo "$$" >> "${T11_MOUNTED}.pids"
echo "${!#}" >> "${T11_MOUNTED}"
sleep 60 &
trap 'kill $! 2>/dev/null; exit 0' TERM
wait
EOF
  cat << 'EOF' > "${mock_bin}/mount"
#!/usr/bin/env bash
while read -r mp; do
  [[ -n "${mp}" ]] && echo "macfuse://mock on ${mp} (macfuse, fskit)"
done < "${T11_MOUNTED}"
EOF
  chmod +x "${mock_bin}/sshfs" "${mock_bin}/mount"

  t11_run_mount() {
    SWE_SCRIPT="${HOME}/scripts/setup_work_environment.sh" \
      HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
      SSHFS_LOG_DIR="${TEST_TMP}" \
      perl -e 'alarm 30; exec @ARGV' bash -c '
        source "${SWE_SCRIPT}"
        mount_remote_gcloud_folders
      ' </dev/null >/dev/null 2>&1
  }
  t11_links() {
    local name
    for name in notes gtd gcloud_tmp; do
      printf '%s=%s ' "${name}" "$(readlink "${fake_home}/${name}" 2>/dev/null || echo "not-a-symlink")"
    done
  }
  t11_expected_links="notes=${fake_home}/.gcloud_home/notes gtd=${fake_home}/.gcloud_home/gtd gcloud_tmp=${fake_home}/.gcloud_home/tmp "

  t11_rc=0
  t11_start="${SECONDS}"
  t11_run_mount || t11_rc=$?
  t11_elapsed=$(( SECONDS - t11_start ))
  assert_eq "mount_remote_gcloud_folders returns without blocking on FSKit sshfs (rc, fast)" \
    "0|fast" "${t11_rc}|$( (( t11_elapsed < 15 )) && echo fast || echo "slow:${t11_elapsed}s")"
  assert_eq "macOS mounts the remote home once, on ~/.gcloud_home" \
    "${fake_home}/.gcloud_home" "$(tr '\n' ' ' < "${T11_MOUNTED}" | sed 's/ $//')"
  assert_contains "macOS sshfs mounts the remote home directory" \
    ":/usr/local/google/home/${USER} ${fake_home}/.gcloud_home" "$(cat "${T11_MOUNTED}.calls")"
  assert_contains "macOS sshfs mounts use the macFUSE FSKit backend" \
    "-o backend=fskit" "$(cat "${T11_MOUNTED}.calls")"
  assert_eq "~/notes (empty mount point dir), ~/gtd and ~/gcloud_tmp become symlinks into ~/.gcloud_home" \
    "${t11_expected_links}" "$(t11_links)"

  t11_rc=0
  t11_run_mount || t11_rc=$?
  assert_eq "Re-running keeps the existing mount and symlinks (rc, sshfs calls, links)" \
    "0|1|${t11_expected_links}" "${t11_rc}|$(wc -l < "${T11_MOUNTED}.calls" | tr -d ' ')|$(t11_links)"

  rm -f "${fake_home}/gtd"
  mkdir -p "${fake_home}/gtd"
  echo "local data" > "${fake_home}/gtd/keep.txt"
  t11_rc=0
  t11_run_mount || t11_rc=$?
  assert_eq "A non-empty ~/gtd directory is left alone (rc=1, contents kept)" \
    "1|local data" "${t11_rc}|$(cat "${fake_home}/gtd/keep.txt" 2>/dev/null)"

  while read -r t11_pid; do
    kill -TERM "${t11_pid}" 2>/dev/null || true
  done < "${T11_MOUNTED}.pids"
}

# ------------------------------------------------------------------------------
# Test 12: bootstrap_macos.sh --dry-run (Mule-first), launchd .plist XMLs, and JSON configs
# ------------------------------------------------------------------------------
echo "[Test 12] bootstrap_macos.sh --dry-run (Mule-first), launchd plists, and JSON configs"
{
  dry_out="$(bash "${HOME}/.googlerc.d/scripts/bootstrap_macos.sh" --dry-run)"
  assert_contains "bootstrap_macos.sh --dry-run completes cleanly" \
    "Bootstrap complete!" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run uses /usr/local/bin/mule update" \
    "sudo /usr/local/bin/mule update" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run installs kitty via mule" \
    "sudo /usr/local/bin/mule install -noprompt kitty" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run symlinks Kitty.app binary into ~/bin/kitty" \
    "ln -sfn /Applications/kitty.app/Contents/MacOS/kitty ${HOME}/bin/kitty" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run ensures ~/.ssh/config contains Host gcloud" \
    "Ensure ~/.ssh/config contains Host gcloud" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run keeps airgradient-monitor disabled by default" \
    "airgradient-monitor is disabled by default on gMac" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run marks airgradient.10s.sh non-executable by default" \
    "chmod -x ${HOME}/.config/swiftbar/airgradient.10s.sh" "${dry_out}"
  assert_contains "bootstrap_macos.sh --dry-run runs go mod init/tidy before building whatsapp-notification" \
    "{ [[ -f go.mod ]] || go mod init whatsapp-notification; } && go mod tidy && CGO_ENABLED=1" "${dry_out}"
  assert_true "~/scripts/bootstrap_macos.sh and ~/.googlerc.d/scripts/bootstrap_macos.sh are identical" \
    cmp -s "${HOME}/scripts/bootstrap_macos.sh" "${HOME}/.googlerc.d/scripts/bootstrap_macos.sh"

  validate_py="${TEST_TMP}/validate_configs.py"
  cat << 'EOF' > "${validate_py}"
import json
import pathlib
import plistlib

home = pathlib.Path.home()
plists = [
    home / ".googlerc.d/launchd/com.marcelvaldez.knock-daemon.plist",
    home / ".config/launchd/com.marcelvaldez.clipboard-daemon.plist",
    home / ".config/launchd/com.marcelvaldez.whatsapp-notification.plist",
    home / ".config/launchd/com.marcelvaldez.airgradient-monitor.plist",
]
for p in plists:
    with p.open("rb") as f:
        data = plistlib.load(f)
    assert "Label" in data and "ProgramArguments" in data, f"Missing keys in {p}"
    for arg in data["ProgramArguments"]:
        assert "__HOME__" not in arg, f"Unexpanded __HOME__ in {p}"
        assert "/opt/homebrew/bin/bash" not in arg, f"Hardcoded Homebrew bash in {p}"

jsons = [
    home / ".config/karabiner/karabiner.json",
    home / ".config/linearmouse/linearmouse.json",
    home / ".config/airgradient-monitor/config.macos.json",
]
for j in jsons:
    with j.open("r", encoding="utf-8") as f:
        json.load(f)
print("VALID")
EOF
  val_res="$(python3 "${validate_py}")"
  assert_eq "All 4 launchd plists (no Homebrew bash dep) and 3 macOS JSON configs validate cleanly" "VALID" "${val_res}"
}

# ------------------------------------------------------------------------------
# Test 13: M5 Pro 14-inch Additions (fzf-at-google fd, beep afplay, kitty font)
# ------------------------------------------------------------------------------
echo "[Test 13] M5 Pro 14-inch additions (fzf-at-google fd, ~/bin/beep, ~/bin/notify, kitty.conf)"
{
  fake_home="${TEST_TMP}/t13_home"
  mock_bin="${TEST_TMP}/t13_bin"
  mkdir -p "${fake_home}/lib" "${fake_home}/res" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"
  touch "${fake_home}/res/beep.mp3"

  cat << 'EOF' > "${mock_bin}/afplay"
#!/usr/bin/env bash
echo "AFPLAY:$*" > "${TEST_BEEP_LOG}"
exit 0
EOF
  chmod +x "${mock_bin}/afplay"
  export TEST_BEEP_LOG="${TEST_TMP}/t13_beep.log"

  HOME="${fake_home}" PATH="${mock_bin}:${PATH}" OS_COMPAT_UNAME_S="Darwin" \
    bash "${HOME}/bin/beep" 1
  assert_contains "~/bin/beep on Darwin plays beep.mp3 via afplay" \
    "AFPLAY:${fake_home}/res/beep.mp3" "$(cat "${TEST_BEEP_LOG}")"

  assert_true "fzf-at-google.sh resolves fd fallback when fdfind is absent" \
    grep -q "command -v fd" "${HOME}/.googlerc.d/fzf-at-google.sh"
  assert_true "~/bin/notify includes darwin terminal-notifier/osascript support" \
    grep -q "darwin" "${HOME}/bin/notify"
  assert_true "kitty.conf enables macos_thicken_font 0.25 for 1080p external displays" \
    grep -q "^macos_thicken_font 0.25" "${HOME}/.config/kitty/kitty.conf"
  assert_true "kitty.conf sets shell_integration no-rc to avoid --posix on macOS Bash 3.2" \
    grep -q "^shell_integration no-rc" "${HOME}/.config/kitty/kitty.conf"
  assert_true ".tmux.conf scrubs POSIXLY_CORRECT from global tmux environment" \
    grep -q "^set-environment -gu POSIXLY_CORRECT" "${HOME}/.tmux.conf"
  assert_true ".bashrc scrubs POSIXLY_CORRECT and KITTY_BASH_INJECT" \
    grep -q "unset POSIXLY_CORRECT POSIX_PEDANTIC ENV KITTY_BASH_INJECT" "${HOME}/.bashrc"
  assert_true "bootstrap_macos.sh uses CGO_ENABLED=1 for whatsapp-notification (go-sqlite3)" \
    grep -q "CGO_ENABLED=1 GOOS=darwin GOARCH=arm64" "${HOME}/.googlerc.d/scripts/bootstrap_macos.sh"
}

# ------------------------------------------------------------------------------
# Test 14: Emacs & emacsclient / emacs-client / tty-server Daemon Lifecycle
# ------------------------------------------------------------------------------
echo "[Test 14] Emacs & emacsclient / emacs-client / tty-server daemon lifecycle"
{
  fake_home="${TEST_TMP}/t14_home"
  mock_bin="${TEST_TMP}/t14_bin"
  mkdir -p "${fake_home}/lib" "${fake_home}/bin" "${mock_bin}"
  cp "${HOME}/lib/os_compat.sh" "${fake_home}/lib/os_compat.sh"

  cat << 'EOF' > "${mock_bin}/emacs"
#!/usr/bin/env bash
echo "MOCK_EMACS:$*"
exit 0
EOF
  cat << 'EOF' > "${mock_bin}/emacsclient"
#!/usr/bin/env bash
echo "MOCK_EMACSCLIENT:$*"
exit 0
EOF
  chmod +x "${mock_bin}/emacs" "${mock_bin}/emacsclient"

  emacs_bin_resolved="$(
    PATH="${mock_bin}:/usr/bin:/bin" bash -c "
      source '${fake_home}/lib/os_compat.sh'
      source '${HOME}/.bash_functions'
      os::emacs_bin
    "
  )"
  assert_eq "os::emacs_bin bypasses function emacs() and resolves mock binary" \
    "${mock_bin}/emacs" "${emacs_bin_resolved}"

  emacs_func_out="$(
    PATH="${mock_bin}:/usr/bin:/bin" TERM="xterm-256color" bash -c "
      source '${fake_home}/lib/os_compat.sh'
      source '${HOME}/.bash_functions'
      emacs test_file.txt
    "
  )"
  assert_eq "function emacs() invokes real binary without recursion and preserves stdout" \
    "MOCK_EMACS:--no-window-system test_file.txt" "${emacs_func_out}"

  emacsclient_func_out="$(
    PATH="${mock_bin}:/usr/bin:/bin" EMACS_TTY_SERVER="tty-server" bash -c "
      source '${fake_home}/lib/os_compat.sh'
      source '${HOME}/.bash_functions'
      emacsclient test_file.txt
    "
  )"
  assert_eq "function emacsclient() ensures daemon and passes --socket-name and --tty" \
    "MOCK_EMACSCLIENT:--socket-name=tty-server --tty test_file.txt" "${emacsclient_func_out}"

  assert_true "org-mode-config.el loads cleanly when ~/notes and ~/gtd are absent on fresh laptop" \
    env HOME="${fake_home}" emacs -Q --batch -L "${HOME}/.emacs.d/lisp" -l "${HOME}/.emacs.d/lisp/org-mode-config.el"
}

# ------------------------------------------------------------------------------
# Test 15: ~/.hammerspoon/init.lua macOS 15+ Space-Moving & Multi-Workspace Alt-Tab
# ------------------------------------------------------------------------------
echo "[Test 15] ~/.hammerspoon/init.lua macOS 15+ space-moving & multi-workspace Alt-Tab"
{
  hs_init="${HOME}/.hammerspoon/init.lua"
  assert_true "init.lua defines ensureUserSpacesCount for auto-creating 6 Spaces" \
    grep -q "local function ensureUserSpacesCount" "${hs_init}"
  assert_true "init.lua defines moveWindowViaMissionControl for borderless (kitty) windows" \
    grep -q "local function moveWindowViaMissionControl" "${hs_init}"
  assert_true "init.lua clears hardware modifiers via :rawFlags(0) on synthetic mouse drag" \
    grep -q ":rawFlags(0)" "${hs_init}"
  assert_true "init.lua loads hs.ipc and binds hammerspoon://reload for live CLI debugging" \
    bash -c "grep -q '^require(\"hs.ipc\")' '${hs_init}' && grep -q 'hs.urlevent.bind(\"reload\"' '${hs_init}'"
  assert_true "init.lua reads the user's own Spaces shortcuts from com.apple.symbolichotkeys" \
    bash -c "grep -q 'local function getSymbolicHotkey' '${hs_init}' && grep -q 'com.apple.symbolichotkeys' '${hs_init}'"
  assert_true "init.lua no longer hardcodes a synthetic Ctrl+Fn+Arrow Space switch" \
    bash -c "! grep -q 'setFlags({ctrl = true, fn = true})' '${hs_init}'"
  assert_true "init.lua waits for physically held hotkey modifiers to be released" \
    grep -q "local function hotkeyModifiersHeld" "${hs_init}"
  assert_true "init.lua keeps strong references to pending move timers (GC-safe opAfter)" \
    bash -c "grep -q 'local function opAfter' '${hs_init}' && grep -q 'op.timers\[t\] = true' '${hs_init}'"
  assert_true "init.lua passes only integral values to integer mouse event fields" \
    bash -c "grep -q 'math.floor(evNum)' '${hs_init}' && grep -q 'local function roundPt' '${hs_init}'"
  assert_true "init.lua matches Mission Control thumbnails by title/shape (not nearest distance)" \
    grep -q "local function findWindowThumbnail" "${hs_init}"
  assert_true "init.lua also matches Mission Control titles without the ' - <App>' suffix (Chrome)" \
    grep -q "local function stripAppNameSuffix" "${hs_init}"
  assert_true "init.lua presses the target Space only after Mission Control settles, then falls back" \
    bash -c "grep -q 'local MC_SETTLE_SEC' '${hs_init}' && grep -q 'local function followDroppedWindow' '${hs_init}' && grep -q 'local function switchToTargetSpaceDirectly' '${hs_init}'"
  assert_true "init.lua never leaves Mission Control open when a move finishes" \
    grep -q "if op.mcOpen then pcall(hs.spaces.closeMissionControl) end" "${hs_init}"
  assert_true "init.lua unpins explicitly moved windows from stickyWindows" \
    grep -Fq "stickyWindows[op.winId] = false" "${hs_init}"
  assert_true "init.lua declares stickyWindows exactly once (shared by Sections 3, 4 and 6)" \
    test "$(grep -c '^local stickyWindows' "${hs_init}")" -eq 1
  assert_true "init.lua refreshes the Alt-Tab tracker right after a move (onWindowMovedToSpace)" \
    grep -q "^onWindowMovedToSpace = function" "${hs_init}"
  assert_true "init.lua shows an all-Spaces fading Desktop banner for Hammerspoon switches" \
    bash -c "grep -q 'local function showWorkspaceBanner' '${hs_init}' && grep -q 'canJoinAllSpaces' '${hs_init}'"
  assert_true "init.lua never shows the Desktop banner from OS-level hs.spaces.watcher callbacks" \
    bash -c "! grep -A3 'hs.spaces.watcher.new' '${hs_init}' | grep -q 'Banner'"
  assert_true "init.lua leaves Ctrl+Alt Desktop switches to macOS when its shortcut uses the same keys" \
    bash -c "grep -q 'local function symbolicEntryMatches' '${hs_init}' && grep -q 'local function syncSwitchBindings' '${hs_init}'"
  assert_true "init.lua shows the switch banner only once the Space has changed (armSwitchBanner)" \
    grep -q "local function armSwitchBanner" "${hs_init}"
  assert_true "init.lua tags keystrokes it posts so the switch-shortcut tap ignores them" \
    bash -c "grep -q 'SYNTHETIC_EVENT_TAG = ' '${hs_init}' && grep -q 'eventSourceUserData) == SYNTHETIC_EVENT_TAG' '${hs_init}'"
  assert_true "init.lua roots the switch-shortcut tap and re-checks shortcut ownership on plist changes" \
    bash -c "grep -q '_G.HS_KEEPALIVE.taps.switchShortcutTap = switchShortcutTap' '${hs_init}' && grep -q 'switchShortcutsWatcher = hs.pathwatcher.new' '${hs_init}'"
  assert_true "init.lua exposes workspaceMover for live IPC testing" \
    grep -q "^workspaceMover = {" "${hs_init}"
  assert_true "init.lua re-syncs the Elgato DisplayPort monitor on display wake (screensDidWake), not on unlock" \
    bash -c "grep -q 'event == w.screensDidWake' '${hs_init}' && grep -q 'scr:setMode(alt.w, alt.h, alt.scale, alt.freq, alt.depth)' '${hs_init}'"
  assert_true "init.lua roots the display re-sync watchers and exposes displayResync for IPC testing" \
    bash -c "grep -q '_G.HS_KEEPALIVE.watchers.resyncPowerWatcher = resyncPowerWatcher' '${hs_init}' && grep -q '^displayResync = {' '${hs_init}'"
  assert_true "init.lua re-syncs only a CR240DM on the Elgato dock's DisplayPort (no hardcoded display UUIDs)" \
    bash -c "grep -q 'monitorName = \"CR240DM\"' '${hs_init}' && grep -q 'dockVendor = \"Elgato Systems\"' '${hs_init}' && ! grep -q 'uuids = {' '${hs_init}'"
  assert_true "init.lua re-syncs a monitor only after it dropped out and came back since the displays slept" \
    bash -c "grep -q 'local function trackScreens(state, current, busy)' '${hs_init}' && grep -q 'if not requireReconnect or resyncState.reconnected\[uuid\] then' '${hs_init}'"
  assert_true "init.lua binds <WM_MOD>r to the same display re-sync on demand" \
    bash -c "grep -q 'local resyncHotkey = keepHotkey(WM_MOD, \"r\", function()' '${hs_init}' && grep -q 'resyncIfEligible(\"hotkey\",' '${hs_init}'"
  assert_true "init.lua leaves out screens that have no UUID yet when tracking drop-outs (no 'table index is nil' at wake)" \
    grep -Fq "if uuid then present[uuid] = true end" "${hs_init}"
  assert_true "init.lua sets the left monitor to contrast 50 and brightness 25 for 10 minutes, then 50, after an hour-plus display sleep" \
    bash -c "grep -q 'minSleepSec = 3600' '${hs_init}' && grep -q 'warmupLevel = 25' '${hs_init}' && grep -q 'warmupSec = 600' '${hs_init}' && grep -q 'brightness = 50' '${hs_init}' && grep -q 'contrast = 50' '${hs_init}'"
  assert_true "init.lua puts the left monitor's red, green and blue gains (VCP 0x16/0x18/0x1A) back to 50 at every display wake" \
    bash -c "grep -q 'color = 50' '${hs_init}' && grep -Fq 'colorVCPs = { { \"red\", \"0x16\" }, { \"green\", \"0x18\" }, { \"blue\", \"0x1A\" } }' '${hs_init}' && grep -Fq 'leftMonitor.startWarmup(asleepFor, asleepFor >= leftMonitor.config.minSleepSec)' '${hs_init}'"
  assert_true "init.lua binds <WM_MOD>6/7 and <WM_MOD>F6/F7 to step the left monitor's brightness and contrast by 25 (BetterDisplay DDC)" \
    bash -c "grep -q 'keepHotkey(WM_MOD, \"6\", function() leftMonitor.step(\"luminance\", -leftMonitor.config.step) end)' '${hs_init}' && grep -q 'keepHotkey(WM_MOD, \"7\", function() leftMonitor.step(\"luminance\", leftMonitor.config.step) end)' '${hs_init}' && grep -q 'keepHotkey(WM_MOD, \"f6\", function() leftMonitor.step(\"contrast\", -leftMonitor.config.step) end)' '${hs_init}' && grep -q 'keepHotkey(WM_MOD, \"f7\", function() leftMonitor.step(\"contrast\", leftMonitor.config.step) end)' '${hs_init}' && grep -q 'step = 25' '${hs_init}'"
  assert_true "init.lua defines buildAllWorkspacesChooserChoices for cross-workspace Alt-Tab" \
    grep -q "local function buildAllWorkspacesChooserChoices" "${hs_init}"
  assert_true "init.lua pre-populates default workspace prefix filter ^<N> " \
    grep -Fq 'initialChooserQuery = string.format("^%d ", currentSpaceIdx)' "${hs_init}"
  assert_true "init.lua defines focusSelectedWindowChoice to switch workspace and raise window" \
    grep -q "local function focusSelectedWindowChoice" "${hs_init}"
  assert_true "init.lua reuses single pre-allocated border canvas via ensureHighlightCanvas" \
    grep -q "local function ensureHighlightCanvas" "${hs_init}"
  assert_true "init.lua debounces border updates asynchronously via scheduleBorderUpdate" \
    grep -q "local function scheduleBorderUpdate" "${hs_init}"
  assert_true "init.lua caches app icons in appIconCache (getCachedAppIcon)" \
    grep -q "local function getCachedAppIcon" "${hs_init}"
  assert_true "init.lua never calls hs.axuielement on hs.processInfo.processID (no self-AX deadlock)" \
    bash -c "! grep -q 'processInfo.processID' '${hs_init}'"
  assert_true "init.lua roots altTabTap, watchers, filters, and timers in _G.HS_KEEPALIVE (no Lua GC drop)" \
    bash -c "grep -q '_G.HS_KEEPALIVE.taps.altTabTap = altTabTap' '${hs_init}' && grep -q '_G.HS_KEEPALIVE.timers.tapWatchdog = tapWatchdog' '${hs_init}'"
  assert_true "init.lua positions windowChooser on the focused window's display (getFocusedWindowScreen)" \
    grep -q "local function getFocusedWindowScreen" "${hs_init}"
  assert_true "init.lua Alt-Tab focuses exactly the chosen window, never via hs.application:activate() (it follows the app's last window to its Space)" \
    bash -c "grep -q 'local function focusWindowExactly' '${hs_init}' && ! grep -vE '^[[:space:]]*--' '${hs_init}' | grep -q ':activate('"
  assert_true "init.lua Alt-Tab skips hs.chooser's default didClose refocus for windowChooser (hands focus over itself)" \
    grep -q "if whichChooser == windowChooser then return end" "${hs_init}"
  assert_true "init.lua keeps the Alt-Tab focus hand-off timer referenced and cancels it on a new Alt-Tab" \
    bash -c "grep -q 'switcherFocusTimer = hs.timer.doAfter' '${hs_init}' && grep -q 'local function cancelSwitcherFocus' '${hs_init}'"
  assert_true "init.lua re-reads the focused Space before labeling Alt-Tab windows (currentSpaceInfo)" \
    grep -q "local function currentSpaceInfo" "${hs_init}"
  assert_true "init.lua Alt-Tab lists minimized windows, tagged [Minimized] (collectSwitcherWindows/recordMinimizedWindows)" \
    bash -c "grep -q 'local function collectSwitcherWindows' '${hs_init}' && grep -q 'local function recordMinimizedWindows' '${hs_init}' && grep -q '\\[Minimized\\]' '${hs_init}'"
  assert_true "init.lua exposes windowSwitcher for live IPC testing" \
    grep -q "^windowSwitcher = {" "${hs_init}"
  assert_true "init.lua Alt-Tab skips apps that don't answer Accessibility within 0.25s for 60s, keeping their windows as last seen (axGuard)" \
    bash -c "grep -Fq 'local axGuard = { timeout = 0.25, skipSec = 60, slow = {}, log = hs.logger.new(\"altTab\", \"info\") }' '${hs_init}' && grep -Fq 'el:setTimeout(axGuard.timeout)' '${hs_init}' && grep -Fq 'return ordered, minimized, skipped' '${hs_init}' && grep -Fq 'elseif skippedPids[meta.pid] then' '${hs_init}' && grep -Fq 'if lastSeen then' '${hs_init}'"
  assert_true "init.lua has no hardcoded /opt/homebrew/bin/bash paths" \
    bash -c "! grep -q '/opt/homebrew/bin/bash' '${hs_init}'"

  # Live checks against the running Hammerspoon over hs.ipc (skipped when it isn't running)
  # hs_ipc <timeout_secs> -c <lua chunk>: evaluates <lua chunk> in the running
  # Hammerspoon by sending it straight to its "Hammerspoon" hs.ipc message port
  # (the legacy msgID 0 request: a flag byte + the code). The reply holds any
  # printed output before the result, so print is silenced during the call, like
  # `hs -q` (e.g. the "-- Loading extension: json" notice the first time a check
  # touches hs.json); the handler restores print afterwards. The hs CLI isn't
  # used: it first asks LaunchServices whether Hammerspoon is running, which
  # always fails from a shell in a Background launchd session (such as a
  # long-lived tmux server; see `launchctl managername`), and then shows a modal
  # "launch it?" alert that ignores -t. SIGALRM still enforces a hard deadline.
  cat << 'EOF' > "${TEST_TMP}/hs_ipc.py"
import ctypes, ctypes.util, sys
cf = ctypes.CDLL(ctypes.util.find_library("CoreFoundation"))
vp = ctypes.c_void_p
cf.CFStringCreateWithCString.restype = vp
cf.CFStringCreateWithCString.argtypes = [vp, ctypes.c_char_p, ctypes.c_uint32]
cf.CFMessagePortCreateRemote.restype = vp
cf.CFMessagePortCreateRemote.argtypes = [vp, vp]
cf.CFDataCreate.restype = vp
cf.CFDataCreate.argtypes = [vp, ctypes.c_char_p, ctypes.c_long]
cf.CFMessagePortSendRequest.restype = ctypes.c_int32
cf.CFMessagePortSendRequest.argtypes = [vp, ctypes.c_int32, vp, ctypes.c_double, ctypes.c_double,
                                        vp, ctypes.POINTER(vp)]
cf.CFDataGetLength.restype = ctypes.c_long
cf.CFDataGetLength.argtypes = [vp]
cf.CFDataGetBytePtr.restype = vp
cf.CFDataGetBytePtr.argtypes = [vp]
timeout, code = float(sys.argv[1]), b"xprint = function() end; " + sys.argv[2].encode("utf-8")
port = cf.CFMessagePortCreateRemote(None, cf.CFStringCreateWithCString(None, b"Hammerspoon", 0x08000100))
if not port:
    sys.exit(69)  # Hammerspoon not running, or hs.ipc not loaded
reply = vp()
status = cf.CFMessagePortSendRequest(port, 0, cf.CFDataCreate(None, code, len(code)), timeout, timeout,
                                     vp.in_dll(cf, "kCFRunLoopDefaultMode"), ctypes.byref(reply))
if status != 0 or not reply.value:
    sys.exit(70)  # no reply in time
print(ctypes.string_at(cf.CFDataGetBytePtr(reply), cf.CFDataGetLength(reply)).decode("utf-8", "replace"))
EOF
  hs_ipc() {
    local secs="$1"
    [[ "$2" == "-c" ]] || return 64
    perl -e 'alarm shift; exec @ARGV or die "exec: $!"' "$(( secs + 2 ))" \
      /usr/bin/python3 "${TEST_TMP}/hs_ipc.py" "${secs}" "$3" </dev/null 2>&1
  }
  if pgrep -x Hammerspoon >/dev/null 2>&1 &&
     [[ "$(hs_ipc 3 -c 'return "pong"' || true)" == *pong* ]]; then
    hs_syntax="$(hs_ipc 5 -c "local f, e = loadfile('${hs_init}'); return f and 'SYNTAX_OK' or tostring(e)" || true)"
    assert_contains "init.lua compiles in Hammerspoon's Lua runtime (loadfile)" "SYNTAX_OK" "${hs_syntax}"
    hs_api="$(hs_ipc 3 -c "return type(workspaceMover) == 'table' and type(workspaceMover.moveFocusedWindowToAdjacentSpace) == 'function' and 'API_OK' or 'API_MISSING'" || true)"
    assert_contains "running Hammerspoon exposes workspaceMover move functions over IPC" "API_OK" "${hs_api}"
    # Alt-Tab rows (all workspaces, unfiltered, minimized windows included): each
    # reads "<workspace> <title> (0x<id>)  |  Workspace <n>[ tags] | <app> | ...".
    hs_rows="$(hs_ipc 5 -c "local s = windowSwitcher; if type(s) ~= 'table' or type(s.rows) ~= 'function' then return 'MISSING' end; local rows = s.rows(); for _, r in ipairs(rows) do if not r:match('^%d+ .+ %(0x%x+%)  |  Workspace %d+') then return 'BAD_ROW ' .. r end end; return 'ROWS_OK ' .. #rows" || true)"
    assert_contains "running Hammerspoon lists Alt-Tab rows as '<workspace> <title> (0x<id>)  |  Workspace <n>' (windowSwitcher.rows)" "ROWS_OK" "${hs_rows}"
    # An app that doesn't answer Accessibility in time (its probe is stubbed to
    # time out) is skipped and probed only once, its window is left out of the
    # rows but still tracked, and it is listed again once the app answers.
    alttab_expected="skipped=true probes=1 hidden=true kept=true back=true"
    hs_axguard="$(hs_ipc 5 -c "local ws = windowSwitcher; if type(ws) ~= 'table' or type(ws.axGuard) ~= 'table' or type(ws.collect) ~= 'function' or type(ws.known) ~= 'function' then return 'MISSING' end; local g = ws.axGuard; local w; for _, cand in ipairs(hs.window.orderedWindows()) do local a = cand:application(); if a and a:name() ~= 'Hammerspoon' and cand:isStandard() then w = cand break end end; if not w then return '${alttab_expected} (no window to test with)' end; local pid, wid = w:application():pid(), w:id(); local tag = string.format('(0x%x)', wid); local function listed() for _, r in ipairs(ws.rows()) do if r:find(tag, 1, true) then return true end end return false end; listed(); local origCount, origLog, probes = g.windowCount, g.log, 0; g.windowCount = function(a) if a:pid() == pid then probes = probes + 1 return nil end return origCount(a) end; g.log = { w = function() end }; local ok, res = pcall(function() local _, _, skipped = ws.collect(); local hidden = not listed(); local kept = ws.known(wid) ~= nil; g.windowCount = origCount; g.slow[pid] = nil; local back = listed(); return string.format('skipped=%s probes=%d hidden=%s kept=%s back=%s', tostring(skipped[pid] == true), probes, tostring(hidden), tostring(kept), tostring(back)) end); g.windowCount, g.log, g.slow[pid] = origCount, origLog, nil; return ok and res or ('ERROR ' .. tostring(res))" || true)"
    assert_contains "Alt-Tab skips an app that doesn't answer Accessibility (probed once), keeps its window tracked, and lists it again once it answers" "${alttab_expected}" "${hs_axguard}"

    # Pure helpers (resolution independent): a {enabled = 1} symbolichotkeys entry
    # means the macOS default Ctrl+Arrow, custom bindings are kept, and Chrome
    # thumbnails match without the " - Google Chrome - <profile>" title suffix.
    cat << 'EOF' > "${TEST_TMP}/t15_helpers.lua"
local wm = workspaceMover
if type(wm) ~= "table" or type(wm.symbolicHotkeyFromEntry) ~= "function" or
   type(wm.findWindowThumbnail) ~= "function" then
  return "HELPERS_MISSING"
end
local out = {}
local function hk(v) return v and string.format("%d/0x%X", v.keycode, v.flags) or "nil" end
table.insert(out, "enabled-no-params=" .. hk(wm.symbolicHotkeyFromEntry(81, { enabled = true })))
table.insert(out, "missing=" .. hk(wm.symbolicHotkeyFromEntry(79, nil)))
table.insert(out, "disabled=" .. hk(wm.symbolicHotkeyFromEntry(81, { enabled = false })))
table.insert(out, "custom=" .. hk(wm.symbolicHotkeyFromEntry(81,
  { enabled = true, value = { parameters = { 65535, 124, 0x8C0000 } } })))
table.insert(out, "desktop1-no-params=" .. hk(wm.symbolicHotkeyFromEntry(118, { enabled = true })))
local same = { x = 0, y = 0, w = 181, h = 100 }
local winFrame = { x = 0, y = 31, w = 2560, h = 1409 }
local chromeThumbs = {
  { AXTitle = "Other window", AXFrame = same },
  { AXTitle = "asrf: (2/5) Partition RolloutLibr…ases. – cl/1004110320 – Critique", AXFrame = same },
}
local chromeTitle = "asrf: (2/5) Partition RolloutLibrary diffs into per-stage JUnit XML test cases." ..
  " – cl/1004110320 – Critique - Google Chrome - Marcel (Work)"
local el = wm.findWindowThumbnail(chromeThumbs, chromeTitle, winFrame, "Google Chrome")
table.insert(out, "chrome-truncated=" .. tostring(el ~= nil and el.AXTitle == chromeThumbs[2].AXTitle))
local newTab = { { AXTitle = "Other window", AXFrame = same }, { AXTitle = "New Tab", AXFrame = same } }
el = wm.findWindowThumbnail(newTab, "New Tab - Google Chrome - Marcel (Work)", winFrame, "Google Chrome")
table.insert(out, "chrome-newtab=" .. tostring(el ~= nil and el.AXTitle == "New Tab"))
local kitty = { { AXTitle = "vim", AXFrame = same }, { AXTitle = "bash", AXFrame = same } }
el = wm.findWindowThumbnail(kitty, "bash", winFrame, "kitty")
table.insert(out, "exact=" .. tostring(el ~= nil and el.AXTitle == "bash"))
el = wm.findWindowThumbnail(chromeThumbs, "Unrelated - Google Chrome", winFrame, "Google Chrome")
table.insert(out, "ambiguous=" .. tostring(el == nil))
return table.concat(out, " ")
EOF
    hs_helpers="$(hs_ipc 5 -c "return dofile('${TEST_TMP}/t15_helpers.lua')" || true)"
    assert_eq "symbolichotkeys entries: {enabled=1} -> default Ctrl+Arrow, disabled -> none, custom kept" \
      "enabled-no-params=124/0x840000 missing=123/0x840000 disabled=nil custom=124/0x8C0000 desktop1-no-params=nil" \
      "$(echo "${hs_helpers}" | sed -n 's/ chrome-truncated=.*//p')"
    assert_eq "Mission Control thumbnails match Chrome titles without the app suffix; exact and ambiguous cases unchanged" \
      "chrome-truncated=true chrome-newtab=true exact=true ambiguous=true" \
      "$(echo "${hs_helpers}" | sed -n 's/.*\(chrome-truncated=\)/\1/p')"
    # Desktop-switch ownership: macOS owns a Hammerspoon binding only when its own
    # shortcut for the same action is enabled with the same keys (fn bit ignored).
    hs_owned="$(hs_ipc 5 -c "local m = workspaceMover.symbolicEntryMatches; if type(m) ~= 'function' then return 'MISSING' end; local function e(on, kc, fl) return { enabled = on, value = { parameters = { 65535, kc, fl } } } end; return table.concat({ 'same=' .. tostring(m(119, e(true, 19, 0xC0000), 19, 0xC0000)), 'disabled=' .. tostring(m(123, e(false, 22, 0xC0000), 22, 0xC0000)), 'stock-ctrl2=' .. tostring(m(119, e(true, 19, 0x40000), 19, 0xC0000)), 'arrow-fn=' .. tostring(m(81, e(true, 124, 0x8C0000), 124, 0xC0000)), 'absent79=' .. tostring(m(79, nil, 123, 0xC0000)), 'absent118=' .. tostring(m(118, nil, 18, 0xC0000)) }, ' ')" || true)"
    assert_eq "macOS owns a Desktop shortcut only when enabled with the same keys (fn ignored, defaults honoured)" \
      "same=true disabled=false stock-ctrl2=false arrow-fn=true absent79=false absent118=false" "${hs_owned}"
    # Display re-sync picks another refresh rate at the same resolution: 60 Hz
    # unless already at 60 Hz, else the closest other rate; nil when none exists.
    hs_resync="$(hs_ipc 5 -c "local a = displayResync and displayResync.alternateMode; if type(a) ~= 'function' then return 'MISSING' end; local function fake(cur, freqs) local modes = {} for i, f in ipairs(freqs) do modes[i] = { w = 1920, h = 1080, scale = 1, freq = f, depth = 8 } end return { currentMode = function() return { w = 1920, h = 1080, scale = 1, freq = cur, depth = 8 } end, availableModes = function() return modes end } end; local function alt(cur, freqs) local _, m = a(fake(cur, freqs)); return m and string.format('%g', m.freq) or 'nil' end; return table.concat({ '165=' .. alt(165, { 50, 60, 120, 144, 165 }), '60=' .. alt(60, { 50, 60, 120, 144, 165 }), '120=' .. alt(120, { 100, 120, 144 }), 'only60=' .. alt(60, { 60 }) }, ' ')" || true)"
    assert_eq "display re-sync alternates to 60 Hz, else the closest other refresh rate" \
      "165=60 60=50 120=100 only60=nil" "${hs_resync}"
    # Display re-sync eligibility: only a CR240DM on the Elgato dock's own
    # DisplayPort output, wherever it is in the arrangement, never one that macOS
    # reports behind a branch device (KVM or adapter); plus the system_profiler
    # serial and ioreg dock-tunnel parsers.
    cat << 'EOF' > "${TEST_TMP}/t15_resync.lua"
local d = displayResync
if type(d) ~= "table" or type(d.eligibleScreens) ~= "function" then return "MISSING" end
local function screen(name, id)
  return { name = function() return name end, id = function() return id end }
end
local builtin = screen("Built-in Retina Display", 1)
local left = screen("CR240DM (2)", 4)
local right = screen("CR240DM (1)", 3)
local serials = { ["1"] = 4251086178, ["3"] = 0, ["4"] = 1, ["5"] = 7 }
local function sink(serial, direct) return { product = "CR240DM", serial = serial, direct = direct } end
local function verdict(screens, dock)
  local targets, why = d.eligibleScreens(screens, serials, dock)
  local names = {}
  for _, t in ipairs(targets) do names[#names + 1] = t:name() end
  return (#names > 0 and table.concat(names, ",") or "none") .. ":" .. why
end
local results = {
  "direct-left=" .. verdict({ builtin, left, right }, { present = true, sinks = { sink(1, true), sink(0, false) } }),
  "direct-right=" .. verdict({ builtin, left, right }, { present = true, sinks = { sink(0, true), sink(1, false) } }),
  "stale=" .. verdict({ left, right }, { present = true, sinks = { sink(0, false) } }),
  "ambiguous=" .. verdict({ left, right }, { present = true, sinks = {} }),
  "kvm-only=" .. verdict({ right }, { present = true, sinks = { sink(0, false) } }),
  "other=" .. verdict({ screen("DELL U2720Q", 5) }, { present = true, sinks = {} }),
  "nodock=" .. verdict({ left, right }, { present = false, sinks = {} }),
}
local json = '{"SPDisplaysDataType":[{"spdisplays_ndrvs":[{"_name":"CR240DM","_spdisplays_displayID":"3"},'
  .. '{"_name":"Color LCD","_spdisplays_displayID":"1","_spdisplays_display-serial-number":"fd626d62"},'
  .. '{"_name":"CR240DM","_spdisplays_displayID":"4","_spdisplays_display-serial-number":"1"}]}]}'
local s = d.displaySerials(json)
results[#results + 1] = string.format("serials=%s,%s,%s", tostring(s["1"]), tostring(s["3"]), tostring(s["4"]))
local xml = [[<?xml version="1.0" encoding="UTF-8"?>
<plist version="1.0"><array><dict>
<key>Device Vendor Name</key><string>Elgato Systems</string>
<key>IORegistryEntryChildren</key><array>
<dict><key>IOObjectClass</key><string>IOPortTransportStateDisplayPort</string>
<key>ProductName</key><string>CR240DM</string><key>SerialNumber</key><integer>0</integer>
<key>BranchDeviceOUI</key><data>kMwk</data></dict>
<dict><key>IOObjectClass</key><string>IOPortTransportStateDisplayPort</string>
<key>ProductName</key><string>CR240DM</string><key>SerialNumber</key><integer>1</integer></dict>
<dict><key>IOObjectClass</key><string>IOPortTransportStateDisplayPort</string></dict>
</array></dict></array></plist>]]
local dock = d.dockDisplayPortSinks(xml, "Elgato Systems")
local parsed = {}
for _, k in ipairs(dock.sinks) do parsed[#parsed + 1] = k.serial .. (k.direct and "d" or "b") end
results[#results + 1] = string.format("dock=%s:%s other-vendor=%s", tostring(dock.present),
  table.concat(parsed, ","), tostring(d.dockDisplayPortSinks(xml, "Lenovo").present))
return table.concat(results, " | ")
EOF
    hs_eligible="$(hs_ipc 5 -c "return dofile('${TEST_TMP}/t15_resync.lua')" || true)"
    assert_eq "display re-sync only targets a CR240DM on the Elgato dock's DisplayPort, in any position" \
      "direct-left=CR240DM (2):on the dock's DisplayPort | direct-right=CR240DM (1):on the dock's DisplayPort | stale=CR240DM (2):the only CR240DM not behind the KVM | ambiguous=none:can't tell which CR240DM is on the dock's DisplayPort | kvm-only=none:CR240DM reaches the dock through a KVM or adapter | other=none:no CR240DM connected | nodock=none:Elgato Systems dock not attached | serials=4251086178,0,1 | dock=true:0b,1d other-vendor=false" \
      "${hs_eligible}"
    # Drop-out tracking: a re-sync is due only when a screen that dropped out
    # since the displays slept comes back; drop-outs during our own mode
    # switches (busy) and newly attached screens never count.
    hs_track="$(hs_ipc 5 -c "local t = displayResync and displayResync.trackScreens; if type(t) ~= 'function' then return 'MISSING' end; local function st(...) local s = { tracked = {}, missing = {}, reconnected = {} } for _, u in ipairs({ ... }) do s.tracked[u] = true end return s end; local a = st('A', 'B'); local r1 = t(a, { B = true }, false); local r2 = t(a, { A = true, B = true }, false); local b = st('A', 'B'); local r3 = t(b, { B = true }, true); local r4 = t(b, { A = true, B = true }, false); local c = st('A'); local r5 = t(c, { A = true, C = true }, false); local e = st('A', 'B'); t(e, { B = true }, false); t(e, { B = true }, false); local r6 = t(e, { A = true, B = true }, false); return string.format('drop=%s back=%s flagged=%s busy-drop=%s after-busy=%s new-screen=%s two-events-then-back=%s', tostring(r1), tostring(r2), tostring(a.reconnected.A), tostring(r3), tostring(r4), tostring(r5), tostring(r6))" || true)"
    assert_eq "display re-sync triggers only when a dropped-out monitor comes back" \
      "drop=false back=true flagged=true busy-drop=false after-busy=false new-screen=false two-events-then-back=true" "${hs_track}"
    # A screen without a UUID yet (mid-wake) is left out of the snapshot instead of
    # raising "table index is nil" (hs.screen.allScreens is stubbed for this call only).
    hs_uuids="$(hs_ipc 5 -c "local u = displayResync and displayResync.currentScreenUUIDs; if type(u) ~= 'function' then return 'MISSING' end; local orig = hs.screen.allScreens; hs.screen.allScreens = function() return { { getUUID = function() return nil end }, { getUUID = function() return 'A' end } } end; local ok, r = pcall(u); hs.screen.allScreens = orig; if not ok then return 'ERROR ' .. tostring(r) end; local n = 0; for _ in pairs(r) do n = n + 1 end; return string.format('A=%s count=%d', tostring(r.A), n)" || true)"
    assert_eq "display re-sync leaves out screens that have no UUID yet (currentScreenUUIDs)" "A=true count=1" "${hs_uuids}"
    # Left monitor: BetterDisplay request JSON (sorted keys, true -> null) and the
    # 0-100 clamped hotkey steps.
    hs_bright="$(hs_ipc 5 -c "local b = displayResync and displayResync.leftMonitor; if type(b) ~= 'table' then return 'MISSING' end; return b.requestJSON('id', { 'set', 'get' }, { UUID = 'U', feature = 'ddc', vcp = 'luminance', value = 25, max = true }) .. ' ' .. table.concat({ b.stepAfter(50, -25), b.stepAfter(10, -25), b.stepAfter(90, 25), b.stepAfter(0, 25) }, ',')" || true)"
    assert_eq "left monitor: BetterDisplay request JSON and 0-100 clamped steps of 25" \
      '{"uuid":"id","commands":["set","get"],"parameters":{"UUID":"U","feature":"ddc","max":null,"value":"25","vcp":"luminance"}} 25,0,100,25' "${hs_bright}"
    # The wake routine starts at every display wake (with a CR240DM connected when
    # the displays slept), as a warm-up with the contrast reset only after a sleep
    # of 60m+ (not 59m59s); one that a shorter display sleep interrupted resumes
    # without it. startWarmup and screen are stubbed, so the monitor is untouched.
    hs_warmup="$(hs_ipc 5 -c "local b = displayResync and displayResync.leftMonitor; if type(b) ~= 'table' then return 'MISSING' end; if b.pendingLevel then return '3599:false,3600:true,60:false; (skipped: a warm-up is in progress)' end; local W = hs.caffeinate.watcher; local orig, saved = { b.startWarmup, b.screen }, { b.sleptAt, b.targetUUID }; local calls = {}; b.startWarmup = function(s, c) calls[#calls + 1] = math.floor(s) .. ':' .. tostring(c) end; b.screen = function() return { getUUID = function() return 'T' end } end; local ok, err = pcall(function() b.sleptAt = nil; b.onPowerEvent(W.screensDidSleep, 1000); b.onPowerEvent(W.screensDidWake, 4599); b.onPowerEvent(W.screensDidSleep, 5000); b.onPowerEvent(W.screensDidWake, 8600); b.pendingLevel = 50; b.onPowerEvent(W.screensDidSleep, 9000); b.onPowerEvent(W.screensDidWake, 9060); b.screen = function() return nil end; b.onPowerEvent(W.screensDidSleep, 10000); b.onPowerEvent(W.screensDidWake, 20000) end); b.startWarmup, b.screen, b.pendingLevel = orig[1], orig[2], nil; b.sleptAt, b.targetUUID = saved[1], saved[2]; return ok and (table.concat(calls, ',') .. ';') or ('ERROR ' .. tostring(err))" || true)"
    assert_contains "left monitor wake routine runs at every display wake; its warm-up (contrast reset) only after a 60m+ sleep, and an interrupted one resumes" "3599:false,3600:true,60:false;" "${hs_warmup}"
    # The wake routine against a stand-in monitor (ddc and log stubbed, the
    # re-checks it schedules cancelled, so the monitor is untouched): after a long
    # sleep brightness 25, contrast 50 and RGB 50; after a short one only RGB 50.
    wake_expected="long=25/50/50/50/50 short=50/40/50/50/50 log=brightness 2 -> 25, contrast 37 -> 50, green 2 -> 50, blue 2 -> 50, then brightness 50 after 600s (displays slept 62m) / red 7 -> 50 (displays slept 10m)"
    hs_wake="$(hs_ipc 5 -c "local b = displayResync and displayResync.leftMonitor; if type(b) ~= 'table' then return 'MISSING' end; local scr = b.screen(); if not scr then return '${wake_expected} (no CR240DM connected)' end; if b.pendingLevel then return '${wake_expected} (skipped: a warm-up is in progress)' end; local saved, fake, logs = { b.ddc, b.log, b.targetUUID }, {}, {}; local prefix = 'wake: ' .. scr:name() .. ' '; b.ddc = function(_, vcp, value, cb) if value then fake[vcp] = value end cb(fake[vcp]) end; b.log = function(msg) logs[#logs + 1] = msg:sub(1, #prefix) == prefix and msg:sub(#prefix + 1) or msg end; b.targetUUID = scr:getUUID(); local function run(asleep, long, state) fake = state; local ok, err = pcall(b.startWarmup, asleep, long); b.token = b.token + 1; if b.timer then b.timer:stop(); b.timer = nil end; b.pendingLevel, b.holdContrast = nil, false; if not ok then return 'ERROR ' .. tostring(err) end; return string.format('%s/%s/%s/%s/%s', fake.luminance, fake.contrast, fake['0x16'], fake['0x18'], fake['0x1A']) end; local long = run(3720, true, { luminance = 2, contrast = 37, ['0x16'] = 50, ['0x18'] = 2, ['0x1A'] = 2 }); local short = run(600, false, { luminance = 50, contrast = 40, ['0x16'] = 7, ['0x18'] = 50, ['0x1A'] = 50 }); b.ddc, b.log, b.targetUUID = saved[1], saved[2], saved[3]; return 'long=' .. long .. ' short=' .. short .. ' log=' .. table.concat(logs, ' / ')" || true)"
    assert_contains "left monitor wake routine: brightness 25, contrast 50 and RGB 50 after a long sleep; only the RGB gains after a short one" "${wake_expected}" "${hs_wake}"
    # Round trip through BetterDisplay's notification integration: read the left
    # monitor's DDC luminance, contrast and RGB gains (passes without a request
    # when no CR240DM is connected).
    hs_ipc 5 -c "local b = displayResync and displayResync.leftMonitor; local scr = b and b.screen(); if not scr then _G.__t15bd = 'LEVEL_OK (no CR240DM connected)' return end; _G.__t15bd = 'PENDING'; local uuid = scr:getUUID(); b.ddc(uuid, 'luminance', nil, function(l) b.ddc(uuid, 'contrast', nil, function(c) b.ddc(uuid, '0x16', nil, function(r) b.ddc(uuid, '0x18', nil, function(g) b.ddc(uuid, '0x1A', nil, function(bl) _G.__t15bd = (l and c and r and g and bl) and string.format('LEVEL_OK brightness=%d contrast=%d rgb=%d/%d/%d', l, c, r, g, bl) or 'NO_ANSWER' end) end) end) end) end)" >/dev/null 2>&1 || true
    sleep 2
    hs_bd="$(hs_ipc 3 -c "local r = _G.__t15bd; _G.__t15bd = nil; return tostring(r)" || true)"
    assert_contains "left monitor brightness, contrast and RGB gains read back through BetterDisplay's DDC integration" "LEVEL_OK" "${hs_bd}"
    hs_resync_hotkey="$(hs_ipc 3 -c "local h = displayResync and displayResync.hotkey; if not h then return 'MISSING' end; for _, k in ipairs(hs.hotkey.getHotkeys()) do if k.idx == h.idx then return 'HOTKEY_ENABLED ' .. h.idx end end; return 'HOTKEY_DISABLED'" || true)"
    assert_contains "running Hammerspoon has the on-demand display re-sync hotkey (<WM_MOD>r) enabled" \
      "HOTKEY_ENABLED" "${hs_resync_hotkey}"
  else
    echo "  [SKIP] live Hammerspoon IPC checks (Hammerspoon not running or hs.ipc not loaded)"
  fi
}

echo "=============================================================================="
echo "macOS Migration Test Summary: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
echo "=============================================================================="
[[ ${FAIL_COUNT} -eq 0 ]]
