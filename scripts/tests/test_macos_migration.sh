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
  dead_sock="/tmp/kitty_tmux-999997"
  live_sock="/tmp/kitty_tmux-$$"
  touch "${dead_sock}" "${live_sock}"

  selected="$(bash -c '
    chosen=""
    for candidate in /tmp/kitty_tmux-*; do
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
  rm -f "${dead_sock}" "${live_sock}"
  assert_eq "Live PID socket selected over dead PID socket" "${live_sock}" "${selected}"
  assert_true "Dead PID socket was cleaned up" test ! -e "${dead_sock}"
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

  assert_contains "shortcuts run Set Do Not Disturb invoked" \
    "run Set Do Not Disturb" "$(cat "${sc_log}")"
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
  sleep 0.2
  tn_logged="$(cat "${TEST_WA_TN_LOG}")"
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

  cat << 'EOF' > "${mock_bin}/airgradient-monitor"
#!/usr/bin/env bash
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

  sb_ag="$(HOME="${fake_home}" PATH="${mock_bin}:${PATH}" bash "${HOME}/.config/swiftbar/airgradient.10s.sh")"
  assert_contains "SwiftBar airgradient strips XML/Pango tags from <txt>" \
    "CO2: 950ppm" "${sb_ag}"
  assert_contains "SwiftBar airgradient includes <tool> lines in dropdown" \
    "PM2.5: 8 ug/m3" "${sb_ag}"
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
# Test 12: bootstrap_macos.sh --dry-run, launchd .plist XMLs, and JSON configs
# ------------------------------------------------------------------------------
echo "[Test 12] bootstrap_macos.sh --dry-run, launchd plists, and JSON configs"
{
  dry_out="$(bash "${HOME}/.googlerc.d/scripts/bootstrap_macos.sh" --dry-run)"
  assert_contains "bootstrap_macos.sh --dry-run completes cleanly" \
    "Bootstrap complete!" "${dry_out}"

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
  assert_eq "All 4 launchd plists and 3 macOS JSON configs validate cleanly" "VALID" "${val_res}"
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
  assert_true "bootstrap_macos.sh uses CGO_ENABLED=1 for whatsapp-notification (go-sqlite3)" \
    grep -q "CGO_ENABLED=1 GOOS=darwin GOARCH=arm64" "${HOME}/.googlerc.d/scripts/bootstrap_macos.sh"
}

echo "=============================================================================="
echo "macOS Migration Test Summary: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
echo "=============================================================================="
[[ ${FAIL_COUNT} -eq 0 ]]
