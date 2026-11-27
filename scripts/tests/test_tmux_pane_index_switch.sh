#!/usr/bin/env bash
# ==============================================================================
# Regression tests: ~/.tmux.conf prefix + Alt+<0-9> pane-index switching
# ==============================================================================
# Exercises, on an isolated tmux server (throwaway HOME, private socket, real
# pty client loaded with ~/.tmux.conf):
#   1. prefix + M-0..M-9 bindings are installed in the prefix table
#   2. prefix + Alt+<index> switches to the pane at that current pane_index
#   3. Adding/destroying panes renumbers pane_index and shortcuts follow the
#      updated indices (formerly valid index becomes out of bounds)
#   4. Switching while zoomed preserves zoom state; switching unzoomed stays
#      unzoomed
#   5. Out-of-bounds index leaves active pane unchanged and displays an
#      out-of-bounds error message with the valid index range
#   6. Works with both pane-base-index 0 (default) and pane-base-index 1
#
# Usage Example:
#   bash ~/scripts/tests/test_tmux_pane_index_switch.sh
#   TMUX_BIN=/usr/bin/tmux bash ~/scripts/tests/test_tmux_pane_index_switch.sh
# ==============================================================================

set -uo pipefail

TMUX_BIN="${TMUX_BIN:-tmux}"
TMUX_CONF="${TMUX_CONF:-${HOME}/.tmux.conf}"
PASS_COUNT=0
FAIL_COUNT=0
REAL_HOME="${HOME}"
unset TMUX TMUX_PANE

TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/test_tmux_pane_idx_XXXXXX")"
SERVER_SOCK=""

cleanup_suite() {
  if [[ -n "${SERVER_SOCK}" ]]; then
    "${TMUX_BIN}" -S "${SERVER_SOCK}" kill-server 2>/dev/null || true
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


master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))

client = subprocess.Popen(
    [tmux_bin, "-S", sock, "-f", tmux_conf, "new-session", "-s", "panetest"],
    stdin=slave,
    stdout=slave,
    stderr=slave,
    env=base_env,
    close_fds=True,
)
os.close(slave)


def drain(duration=0.12) -> str:
  buf = b""
  deadline = time.monotonic() + duration
  while time.monotonic() < deadline:
    if select.select([master], [], [], 0.02)[0]:
      try:
        chunk = os.read(master, 65536)
      except OSError:
        break
      if not chunk:
        break
      buf += chunk
  return re.sub(
      rb"\x1b\[[0-9;?]*[a-zA-Z]|\x1b\][^\x07]*\x07|\x1b[()][AB012]",
      b" ",
      buf,
  ).decode("utf-8", "replace")


def send_prefix_alt(digit: str) -> str:
  # Prefix is C-a (\x01), followed by Alt+<digit> (ESC + digit)
  os.write(master, b"\x01\x1b" + digit.encode("ascii"))
  return drain(0.25)


def active_state():
  r = tmux(
      "display-message",
      "-t",
      "panetest",
      "-p",
      "#{pane_index}|#{pane_id}|#{window_zoomed_flag}|#{window_panes}",
  )
  return r.stdout.strip()


def latest_status_message():
  # tmux show-messages lists newest entries first (tmux 3.6 & 3.8).
  r = tmux("show-messages")
  for ln in r.stdout.splitlines():
    if " message: " in ln:
      return ln.strip()
  return ""


def check(results, name, expected, actual):
  results.append({"name": name, "expected": expected, "actual": actual})


try:
  attached = False
  for _ in range(80):
    if tmux("list-clients", "-F", "#{client_tty}").stdout.strip():
      attached = True
      break
    drain(0.05)
  if not attached:
    print(json.dumps({"error": "tmux client failed to attach"}))
    sys.exit(1)

  results = []

  # 1. Verify M-0..M-9 are bound in prefix table to select-pane / out-of-bounds check
  keys_out = tmux("list-keys", "-T", "prefix").stdout
  for d in range(10):
    bound = any(
        f" M-{d} " in ln and f"select-pane -Z -t :.{d}" in ln
        for ln in keys_out.splitlines()
    )
    check(results, f"prefix M-{d} binding registered", "True", str(bound))

  # 2. Create 3 panes (0, 1, 2) and record their unique pane_ids
  tmux("split-window", "-h", "-t", "panetest")
  tmux("split-window", "-v", "-t", "panetest")
  drain(0.15)
  pane_ids = tmux("list-panes", "-t", "panetest", "-F", "#{pane_index}:#{pane_id}").stdout.strip().splitlines()
  id_map = dict(entry.split(":", 1) for entry in pane_ids)

  # Active pane is 2; jump to 0, then 1, then 2 (unzoomed)
  send_prefix_alt("0")
  check(results, "prefix+Alt+0 selects pane 0 (unzoomed)", f"0|{id_map['0']}|0|3", active_state())

  send_prefix_alt("1")
  check(results, "prefix+Alt+1 selects pane 1 (unzoomed)", f"1|{id_map['1']}|0|3", active_state())

  send_prefix_alt("2")
  check(results, "prefix+Alt+2 selects pane 2 (unzoomed)", f"2|{id_map['2']}|0|3", active_state())

  # 3. Zoom pane 2 and switch to pane 0 and pane 1 while preserving zoom
  tmux("resize-pane", "-Z", "-t", "panetest")
  drain(0.1)
  send_prefix_alt("0")
  check(results, "prefix+Alt+0 while zoomed selects pane 0 and keeps zoom", f"0|{id_map['0']}|1|3", active_state())

  send_prefix_alt("1")
  check(results, "prefix+Alt+1 while zoomed selects pane 1 and keeps zoom", f"1|{id_map['1']}|1|3", active_state())

  # Unzoom back to normal
  tmux("resize-pane", "-Z", "-t", "panetest")
  drain(0.1)

  # 4. Out-of-bounds index (5 when valid is 0-2) keeps pane 1 active and shows error message
  pty5 = send_prefix_alt("5")
  check(results, "prefix+Alt+5 out of bounds leaves active pane unchanged", f"1|{id_map['1']}|0|3", active_state())
  msg5 = latest_status_message()
  expected5 = "Pane index 5 is out of bounds (valid: 0-2)"
  check(
      results,
      "prefix+Alt+5 displays out-of-bounds error with valid range 0-2",
      "True",
      str(expected5 in pty5 and expected5 in msg5),
  )

  # 5. Destroy pane 1 -> old pane 2 renumbers to index 1; index 2 is now out of bounds
  tmux("kill-pane", "-t", "panetest:.1")
  drain(0.15)
  send_prefix_alt("0")
  check(results, "after kill, prefix+Alt+0 selects pane 0", f"0|{id_map['0']}|0|2", active_state())

  send_prefix_alt("1")
  check(results, "after kill, prefix+Alt+1 selects renumbered old pane 2", f"1|{id_map['2']}|0|2", active_state())

  pty2 = send_prefix_alt("2")
  check(results, "after kill, prefix+Alt+2 is out of bounds and keeps pane 1", f"1|{id_map['2']}|0|2", active_state())
  msg2 = latest_status_message()
  expected2 = "Pane index 2 is out of bounds (valid: 0-1)"
  check(
      results,
      "after kill, prefix+Alt+2 displays out-of-bounds error with valid range 0-1",
      "True",
      str(expected2 in pty2 and expected2 in msg2),
  )

  # 6. Single-pane window (only index 0) reports singular valid: 0
  tmux("kill-pane", "-t", "panetest:.1")
  drain(0.15)
  pty1 = send_prefix_alt("1")
  check(results, "single-pane window prefix+Alt+1 keeps pane 0", f"0|{id_map['0']}|0|1", active_state())
  msg1 = latest_status_message()
  expected1 = "Pane index 1 is out of bounds (valid: 0)"
  check(
      results,
      "single-pane window prefix+Alt+1 shows valid: 0",
      "True",
      str(expected1 in pty1 and expected1 in msg1),
  )

  # 7. Respect pane-base-index 1 on a new window (valid indices 1-2; index 0 out of bounds)
  tmux("set-window-option", "-g", "pane-base-index", "1")
  tmux("new-window", "-t", "panetest", "-n", "base1")
  tmux("split-window", "-h", "-t", "panetest")
  drain(0.15)
  b1_ids = dict(
      entry.split(":", 1)
      for entry in tmux("list-panes", "-t", "panetest", "-F", "#{pane_index}:#{pane_id}").stdout.strip().splitlines()
  )
  send_prefix_alt("1")
  check(results, "pane-base-index 1: prefix+Alt+1 selects pane 1", f"1|{b1_ids['1']}|0|2", active_state())

  send_prefix_alt("2")
  check(results, "pane-base-index 1: prefix+Alt+2 selects pane 2", f"2|{b1_ids['2']}|0|2", active_state())

  pty0 = send_prefix_alt("0")
  check(results, "pane-base-index 1: prefix+Alt+0 is out of bounds and keeps pane 2", f"2|{b1_ids['2']}|0|2", active_state())
  msg0 = latest_status_message()
  expected0 = "Pane index 0 is out of bounds (valid: 1-2)"
  check(
      results,
      "pane-base-index 1: prefix+Alt+0 shows valid: 1-2",
      "True",
      str(expected0 in pty0 and expected0 in msg0),
  )

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

test_pane_index_shortcuts() {
  local home sock output parsed name expected actual
  home="$(make_fake_home)"
  sock="${TEST_ROOT}/pane_idx.sock"
  SERVER_SOCK="${sock}"

  output="$(python3 "${TEST_ROOT}/driver.py" "${TMUX_BIN}" "${TMUX_CONF}" "${home}" "${sock}")"
  if [[ -z "${output}" ]]; then
    assert_eq "json output" "" "driver produced output"
    return
  fi

  parsed="$(python3 -c '
import json, sys
data = json.loads(sys.stdin.read())
if "error" in data:
    err = data["error"]
    print(f"driver error\tok\t{err}")
    sys.exit(0)
for r in data["results"]:
    name, expected, actual = r["name"], r["expected"], r["actual"]
    print(f"{name}\t{expected}\t{actual}")
' <<< "${output}")"

  while IFS=$'\t' read -r name expected actual; do
    assert_eq "${expected}" "${actual}" "${name}"
  done <<< "${parsed}"
}

main() {
  if ! command -v python3 >/dev/null 2>&1 || ! command -v "${TMUX_BIN}" >/dev/null 2>&1; then
    echo "FAIL: tmux and python3 are required" >&2
    return 1
  fi
  write_driver_helper
  test_pane_index_shortcuts
  echo "tmux pane-index shortcut tests (${TMUX_BIN}): ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
  (( PASS_COUNT > 0 && FAIL_COUNT == 0 ))
}

main "$@"
