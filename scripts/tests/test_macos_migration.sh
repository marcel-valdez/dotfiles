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
  assert_true "init.lua exposes workspaceMover for live IPC testing" \
    grep -q "^workspaceMover = {" "${hs_init}"
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
  assert_true "init.lua has no hardcoded /opt/homebrew/bin/bash paths" \
    bash -c "! grep -q '/opt/homebrew/bin/bash' '${hs_init}'"

  # Live checks against the running Hammerspoon over hs.ipc (skipped when it isn't running)
  hs_cli="/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
  if [[ -x "${hs_cli}" ]] && pgrep -x Hammerspoon >/dev/null 2>&1 &&
     [[ "$("${hs_cli}" -t 3 -q -c 'return "pong"' </dev/null 2>/dev/null || true)" == *pong* ]]; then
    hs_syntax="$("${hs_cli}" -t 5 -q -c "local f, e = loadfile('${hs_init}'); return f and 'SYNTAX_OK' or tostring(e)" </dev/null 2>&1 || true)"
    assert_contains "init.lua compiles in Hammerspoon's Lua runtime (loadfile)" "SYNTAX_OK" "${hs_syntax}"
    hs_api="$("${hs_cli}" -t 3 -q -c "return type(workspaceMover) == 'table' and type(workspaceMover.moveFocusedWindowToAdjacentSpace) == 'function' and 'API_OK' or 'API_MISSING'" </dev/null 2>&1 || true)"
    assert_contains "running Hammerspoon exposes workspaceMover move functions over IPC" "API_OK" "${hs_api}"
  else
    echo "  [SKIP] live Hammerspoon IPC checks (Hammerspoon not running or hs.ipc unavailable)"
  fi
}

echo "=============================================================================="
echo "macOS Migration Test Summary: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
echo "=============================================================================="
[[ ${FAIL_COUNT} -eq 0 ]]
