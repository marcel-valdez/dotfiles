#!/usr/bin/env bash
# ==============================================================================
# Regression tests: ~/scripts/tmux-session-switcher.sh
# ==============================================================================
# Verifies interactive session and window switching through the real ~/.tmux.conf
# display-popup binding (C-a C-j) on an isolated tmux server (private socket,
# throwaway HOME, real pty client):
#   1. Selecting another session with Down + Enter (default delta 0)
#   2. Selecting another session via fuzzy filter + Enter (default delta 0)
#   3. Selecting a specific window via Shift-Down / Shift-Up + Enter (delta +/-1)
#   4. Cycling windows and returning to delta 0 (Shift-Down, Shift-Up, Enter)
#   5. Resetting window delta when moving to another session (Shift-Down, Down, Enter)
#   6. Switching to a single-window session (delta 0 and non-zero delta)
#   7. Switching windows within the current session (Shift-Down + Enter)
#   8. Cancelling with Esc leaves the active session and window unchanged
#
# Usage Example:
#   bash ~/scripts/tests/test_tmux_session_switcher.sh
#   TMUX_BIN=/usr/bin/tmux bash ~/scripts/tests/test_tmux_session_switcher.sh
# ==============================================================================

set -uo pipefail

TMUX_BIN="${TMUX_BIN:-tmux}"
TMUX_CONF="${TMUX_CONF:-${HOME}/.tmux.conf}"
SWITCHER_SCRIPT="${SWITCHER_SCRIPT:-${HOME}/scripts/tmux-session-switcher.sh}"
PASS_COUNT=0
FAIL_COUNT=0
REAL_HOME="${HOME}"
unset TMUX TMUX_PANE

TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/test_tmux_session_switcher_XXXXXX")"
SERVER_SOCK=""
CLIENT_PID=""

cleanup_suite() {
  if [[ -n "${SERVER_SOCK}" ]]; then
    "${TMUX_BIN}" -S "${SERVER_SOCK}" kill-server 2>/dev/null || true
  fi
  if [[ -n "${CLIENT_PID}" ]]; then
    kill "${CLIENT_PID}" 2>/dev/null || true
    wait "${CLIENT_PID}" 2>/dev/null || true
  fi
  rm -rf "${TEST_ROOT}" 2>/dev/null || true
}
trap cleanup_suite EXIT

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

make_fake_home() {
  local home
  home="$(mktemp -d "${TEST_ROOT}/home_XXXXXX")"
  mkdir -p "${home}/scripts"
  ln -s "${SWITCHER_SCRIPT}" "${home}/scripts/tmux-session-switcher.sh"
  if [[ -e "${REAL_HOME}/bin" ]]; then
    ln -s "${REAL_HOME}/bin" "${home}/bin"
  fi
  if [[ -e "${REAL_HOME}/.tmux.conf.local" ]]; then
    ln -s "${REAL_HOME}/.tmux.conf.local" "${home}/.tmux.conf.local"
  fi
  echo "${home}"
}

write_driver_helper() {
  cat > "${TEST_ROOT}/driver.py" <<'PYEOF'
import fcntl
import json
import os
import pty
import re
import select
import struct
import subprocess
import sys
import termios
import time

tmux_bin = sys.argv[1]
tmux_conf = sys.argv[2]
fake_home = sys.argv[3]
sock = sys.argv[4]
cases = json.loads(sys.argv[5])

base_env = os.environ.copy()
base_env.pop("TMUX", None)
base_env.pop("TMUX_PANE", None)
base_env["HOME"] = fake_home
base_env["TERM"] = "xterm-256color"


def tmux(*args):
  return subprocess.run(
      [tmux_bin, "-S", sock, *args],
      capture_output=True,
      text=True,
      env=base_env,
  )


def strip_ansi(data: bytes) -> str:
  return re.sub(
      rb"\x1b\[[0-9;?]*[a-zA-Z]|\x1b\][^\x07]*\x07|\x1b[()][AB012]",
      b"",
      data,
  ).decode("utf-8", "replace")


KEY_MAP = {
    "UP": b"\x1b[A",
    "DOWN": b"\x1b[B",
    "SHIFT_UP": b"\x1b[1;2A",
    "SHIFT_DOWN": b"\x1b[1;2B",
    "ENTER": b"\r",
    "ESC": b"\x1b",
}


def decode_key(token: str) -> bytes:
  if token in KEY_MAP:
    return KEY_MAP[token]
  return token.encode("utf-8")


master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))

client = subprocess.Popen(
    [tmux_bin, "-S", sock, "-f", tmux_conf, "new-session", "-s", "alpha", "-n", "a1"],
    stdin=slave,
    stdout=slave,
    stderr=slave,
    env=base_env,
    close_fds=True,
)
os.close(slave)


def drain(duration=0.15) -> bytes:
  out = b""
  deadline = time.monotonic() + duration
  while time.monotonic() < deadline:
    if select.select([master], [], [], 0.02)[0]:
      try:
        chunk = os.read(master, 65536)
      except OSError:
        break
      if not chunk:
        break
      out += chunk
  return out


def wait_for_text(needle: str, timeout: float = 4.0) -> tuple[bool, str]:
  buf = b""
  deadline = time.monotonic() + timeout
  while time.monotonic() < deadline:
    if select.select([master], [], [], 0.03)[0]:
      try:
        chunk = os.read(master, 65536)
      except OSError:
        break
      if not chunk:
        break
      buf += chunk
      plain = strip_ansi(buf)
      if needle in plain:
        return True, plain
  return False, strip_ansi(buf)


def wait_for_popup_close(timeout: float = 3.0) -> None:
  deadline = time.monotonic() + timeout
  while time.monotonic() < deadline:
    drain(0.05)
    # Wait until no fzf process is running for this socket's fake HOME
    ps = subprocess.run(
        ["pgrep", "-f", f"{fake_home}/scripts/tmux-session-switcher.sh"],
        capture_output=True,
        text=True,
    )
    if ps.returncode != 0:
      drain(0.1)
      return


try:
  # Wait for client to attach
  attached = False
  for _ in range(80):
    if tmux("list-clients", "-F", "#{client_tty}").stdout.strip():
      attached = True
      break
    drain(0.05)
  if not attached:
    print(json.dumps({"error": "tmux client failed to attach"}))
    sys.exit(1)

  # Build deterministic session/window topology:
  #   alpha: windows 1 (a1), 2 (a2)
  #   beta:  windows 1 (b1), 2 (b2, active), 3 (b3)
  #   gamma: window  1 (g1)
  tmux("new-window", "-t", "alpha", "-n", "a2")
  tmux("new-session", "-d", "-s", "beta", "-n", "b1")
  tmux("new-window", "-t", "beta", "-n", "b2")
  tmux("new-window", "-t", "beta", "-n", "b3")
  tmux("new-session", "-d", "-s", "gamma", "-n", "g1")

  # Seed distinct pane markers at the bottom of each pane (so tail -height under
  # --preview-window=80%,follow displays them) with no same-column character
  # overlap between beta:2 and beta:3 so tmux incremental redraws emit the full
  # marker string.
  for target, marker in [
      ("beta:2", "BETA2FIRST"),
      ("beta:3", "QUIXOTE3RD"),
      ("gamma:1", "GAMMA1ONLY"),
  ]:
    tmux("send-keys", "-t", target, f"seq 1 40; printf '%s\\n' {marker}", "Enter")
  time.sleep(0.4)

  results = []
  for case in cases:
    # Reset deterministic window state before each case
    tmux("select-window", "-t", "alpha:1")
    tmux("select-window", "-t", "beta:2")
    tmux("select-window", "-t", "gamma:1")
    tmux("switch-client", "-t", case["start"])
    drain(0.15)

    # Open switcher via ~/.tmux.conf binding: prefix C-a (\x01) + C-j (\x0a)
    os.write(master, b"\x01\x0a")
    opened, preview_screen = wait_for_text("TMUX Sessions", timeout=4.0)
    # Give fzf a brief moment after header renders to process load:pos(...) and initial preview
    preview_screen += strip_ansi(drain(0.35))

    keys = case["keys"]
    for i, key in enumerate(keys):
      os.write(master, decode_key(key))
      chunk = strip_ansi(drain(0.35 if key in ("SHIFT_DOWN", "SHIFT_UP") else 0.25))
      # Record preview output strictly BEFORE the final ENTER/ESC closes the popup.
      if i < len(keys) - 1:
        preview_screen += chunk

    wait_for_popup_close(timeout=3.0)
    actual = tmux("list-clients", "-F", "#{session_name}:#{window_index}").stdout.strip()
    preview_ok = True
    for needle in case.get("expect_preview", []):
      if needle not in preview_screen:
        preview_ok = False
    results.append({
        "name": case["name"],
        "opened": opened,
        "preview_ok": preview_ok,
        "actual": actual,
        "expected": case["expected"],
    })

  print(json.dumps({"results": results}))
finally:
  tmux("kill-server")
  try:
    client.terminate()
    client.wait(timeout=2)
  except Exception:
    client.kill()
  try:
    os.close(master)
  except OSError:
    pass
PYEOF
}

test_session_switcher_end_to_end() {
  local home sock cases_json output
  home="$(make_fake_home)"
  sock="${TEST_ROOT}/switcher.sock"
  SERVER_SOCK="${sock}"

  cases_json='[
    {
      "name": "Down + Enter switches to next session active window (delta 0)",
      "start": "alpha:1",
      "keys": ["DOWN", "ENTER"],
      "expected": "beta:2",
      "expect_preview": ["BETA2FIRST"]
    },
    {
      "name": "Fuzzy filter + Enter switches to matching session (delta 0)",
      "start": "alpha:1",
      "keys": ["gamma", "ENTER"],
      "expected": "gamma:1",
      "expect_preview": ["GAMMA1ONLY"]
    },
    {
      "name": "Down + Shift-Down + Enter switches to next window (+1)",
      "start": "alpha:1",
      "keys": ["DOWN", "SHIFT_DOWN", "ENTER"],
      "expected": "beta:3",
      "expect_preview": ["BETA2FIRST", "QUIXOTE3RD"]
    },
    {
      "name": "Down + Shift-Up + Enter switches to previous window (-1)",
      "start": "alpha:1",
      "keys": ["DOWN", "SHIFT_UP", "ENTER"],
      "expected": "beta:1"
    },
    {
      "name": "Down + Shift-Down + Shift-Up + Enter returns to delta 0 and switches",
      "start": "alpha:1",
      "keys": ["DOWN", "SHIFT_DOWN", "SHIFT_UP", "ENTER"],
      "expected": "beta:2"
    },
    {
      "name": "Shift-Down then Down resets delta to 0 and switches to next session active window",
      "start": "alpha:1",
      "keys": ["SHIFT_DOWN", "DOWN", "ENTER"],
      "expected": "beta:2"
    },
    {
      "name": "Single-window session with Shift-Down + Enter switches cleanly",
      "start": "alpha:1",
      "keys": ["gamma", "SHIFT_DOWN", "ENTER"],
      "expected": "gamma:1"
    },
    {
      "name": "Shift-Down + Enter on current session switches window within session",
      "start": "alpha:1",
      "keys": ["SHIFT_DOWN", "ENTER"],
      "expected": "alpha:2"
    },
    {
      "name": "Esc cancels popup without changing session or window",
      "start": "alpha:1",
      "keys": ["DOWN", "ESC"],
      "expected": "alpha:1"
    }
  ]'

  output="$(python3 "${TEST_ROOT}/driver.py" "${TMUX_BIN}" "${TMUX_CONF}" "${home}" "${sock}" "${cases_json}")"
  if [[ -z "${output}" ]]; then
    assert_eq "json output" "" "driver produced output"
    return
  fi

  local parsed
  parsed="$(python3 -c '
import json, sys
data = json.loads(sys.stdin.read())
if "error" in data:
    err = data["error"]
    print(f"driver error\tFalse\tFalse\tok\t{err}")
    sys.exit(0)
for r in data["results"]:
    name, opened, preview_ok, expected, actual = r["name"], r["opened"], r["preview_ok"], r["expected"], r["actual"]
    print(f"{name}\t{opened}\t{preview_ok}\t{expected}\t{actual}")
' <<< "${output}")"

  if [[ -z "${parsed}" ]]; then
    assert_eq "parsed results" "" "driver output parsed cleanly"
    return
  fi

  while IFS=$'\t' read -r name opened preview_ok expected actual; do
    assert_eq "True" "${opened}" "${name} (popup rendered)"
    assert_eq "True" "${preview_ok}" "${name} (preview rendered)"
    assert_eq "${expected}" "${actual}" "${name}"
  done <<< "${parsed}"
}

main() {
  if ! command -v python3 >/dev/null 2>&1 || ! command -v "${TMUX_BIN}" >/dev/null 2>&1 || ! command -v fzf >/dev/null 2>&1; then
    echo "FAIL: tmux, fzf, and python3 are required" >&2
    return 1
  fi
  write_driver_helper
  test_session_switcher_end_to_end
  echo "tmux-session-switcher tests (${TMUX_BIN}): ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
  (( PASS_COUNT > 0 && FAIL_COUNT == 0 ))
}

main "$@"
