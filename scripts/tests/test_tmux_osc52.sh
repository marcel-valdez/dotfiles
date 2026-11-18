#!/usr/bin/env bash
# ==============================================================================
# Regression tests: ~/.tmux.conf OSC 52 clipboard writes survive mosh/gmosh
# ==============================================================================
# Copies made in a cloudtop tmux reach the Mac clipboard as OSC 52 through
# gmosh (mosh 1.4.0). mosh forwards only ESC]52;c;<base64> and silently drops
# every other selection parameter, including the empty one tmux uses for its
# own copies. Exercises, on an isolated tmux server (throwaway HOME, private
# socket) whose client runs in a pty that this test reads:
#   1. load-buffer -w (async_clipboard_copy.sh, Emacs M-W) arrives with Pc "c"
#   2. copy mode + copy-pipe-and-cancel (tmux M-w / C-w) arrives with Pc "c"
#   3. an application's OSC 52 with Pc "c" is forwarded as "c"
#   4. an application's explicit selection (Pc "p") passes through unchanged
#   5. if mosh-server and mosh-client are installed: copies 1-3 cross a
#      loopback mosh hop (tmux -> mosh-server -> mosh-client -> terminal)
#
# Usage Example:
#   bash ~/scripts/tests/test_tmux_osc52.sh
#   TMUX_CONF=/path/to/tmux.conf bash ~/scripts/tests/test_tmux_osc52.sh
# ==============================================================================

set -uo pipefail

TMUX_CONF="${TMUX_CONF:-${HOME}/.tmux.conf}"
PASS_COUNT=0
FAIL_COUNT=0
REAL_HOME="${HOME}"
unset TMUX TMUX_PANE

TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/test_tmux_osc52_XXXXXX")"
cleanup_suite() {
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

# Prints the base64 of $1 on one line (GNU base64 wraps at 76 columns).
b64() {
  printf '%s' "$1" | base64 | tr -d '\n'
}

# Prints the selection parameter (Pc) of the first OSC 52 in capture file $1
# whose payload is the base64 of $2, or "<none>" if no such sequence arrived.
osc52_pc() {
  local seq
  seq="$(grep -ao $'\e'"]52;[a-z0-9]*;$(b64 "$2")" "$1" | head -n 1)"
  if [[ -z "${seq}" ]]; then
    echo "<none>"
    return
  fi
  seq="${seq#*]52;}"
  echo "${seq%%;*}"
}

# Writes a helper that runs a command in a pty and saves every byte the
# command writes to the terminal, i.e. what kitty would receive.
write_pty_capture_helper() {
  cat > "${TEST_ROOT}/ptyrun.py" <<'PYEOF'
import fcntl, os, pty, select, signal, struct, sys, termios, time
out_path, timeout, cmd = sys.argv[1], float(sys.argv[2]), sys.argv[3:]
pid, fd = pty.fork()
if pid == 0:
  fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 120, 0, 0))
  os.execvp(cmd[0], cmd)
buf, done, deadline = bytearray(), False, time.monotonic() + timeout
while time.monotonic() < deadline:
  if fd in select.select([fd], [], [], 0.05)[0]:
    try:
      data = os.read(fd, 65536)
    except OSError:
      break
    if not data:
      break
    buf += data
  if not done and os.waitpid(pid, os.WNOHANG)[0] == pid:
    done, deadline = True, min(deadline, time.monotonic() + 0.3)
if not done:
  for sig in (signal.SIGHUP, signal.SIGKILL):
    try:
      os.kill(pid, sig)
    except ProcessLookupError:
      break
    time.sleep(0.3)
    if os.waitpid(pid, os.WNOHANG)[0] == pid:
      done = True
      break
os.close(fd)  # close the master before a blocking wait (macOS pty drain)
if not done:
  os.waitpid(pid, 0)
open(out_path, 'wb').write(buf)
PYEOF
}

# Creates a throwaway HOME (no TPM plugins or status daemons start there) that
# still sees this machine's ~/.tmux.conf.local overrides. Prints its path.
make_fake_home() {
  local home
  home="$(mktemp -d "${TEST_ROOT}/home_XXXXXX")"
  if [[ -e "${REAL_HOME}/.tmux.conf.local" ]]; then
    ln -s "${REAL_HOME}/.tmux.conf.local" "${home}/.tmux.conf.local"
  fi
  echo "${home}"
}

# Waits until the server on socket $1 has a client; prints the client name.
wait_for_client() {
  local sock="$1" client="" _
  for _ in $(seq 1 80); do
    client="$(tmux -S "${sock}" list-clients -F '#{client_name}' 2>/dev/null | head -n 1)"
    if [[ -n "${client}" ]]; then
      echo "${client}"
      return 0
    fi
    sleep 0.1
  done
  return 1
}

# Pane script: a line for copy mode to copy, then two application OSC 52s.
pane_script() {
  printf '%s' "printf 'copy-mode-line\\n'; sleep 0.4;"
  printf '%s' " printf '\\033]52;c;$(b64 app-select-c)\\007'; sleep 0.2;"
  printf '%s' " printf '\\033]52;p;$(b64 app-select-p)\\007'; exec sleep 60"
}

# Runs load-buffer -w and a copy-mode copy against client $2 of socket $1.
drive_copies() {
  local sock="$1" client="$2" key
  printf '%s' load-buffer-w | tmux -S "${sock}" load-buffer -w -t "${client}" -
  sleep 0.5
  tmux -S "${sock}" copy-mode -t osc52
  for key in history-top start-of-line begin-selection end-of-line; do
    tmux -S "${sock}" send-keys -t osc52 -X "${key}"
  done
  tmux -S "${sock}" send-keys -t osc52 -X copy-pipe-and-cancel 'cat >/dev/null'
  sleep 1
}

test_tmux_sends_mosh_compatible_osc52() {
  local home sock cap runner client
  home="$(make_fake_home)"
  sock="${TEST_ROOT}/direct.sock"
  cap="${TEST_ROOT}/direct.cap"
  HOME="${home}" TERM=xterm-256color python3 "${TEST_ROOT}/ptyrun.py" "${cap}" 12 \
    tmux -S "${sock}" -f "${TMUX_CONF}" new-session -s osc52 "$(pane_script)" &
  runner=$!
  if ! client="$(wait_for_client "${sock}")"; then
    assert_eq "client" "none" "tmux client started in a pty"
    kill "${runner}" 2>/dev/null
    return
  fi
  sleep 1
  HOME="${home}" drive_copies "${sock}" "${client}"
  tmux -S "${sock}" kill-server 2>/dev/null
  wait "${runner}" 2>/dev/null

  assert_eq "c" "$(osc52_pc "${cap}" load-buffer-w)" \
    "load-buffer -w (async_clipboard_copy.sh, Emacs M-W) sends OSC 52 with Pc c"
  assert_eq "c" "$(osc52_pc "${cap}" copy-mode-line)" \
    "copy mode copy-pipe-and-cancel (tmux M-w / C-w) sends OSC 52 with Pc c"
  assert_eq "c" "$(osc52_pc "${cap}" app-select-c)" \
    "an application's OSC 52 with Pc c is forwarded as c"
  assert_eq "p" "$(osc52_pc "${cap}" app-select-p)" \
    "an application's explicit selection p passes through unchanged"
}

test_copies_cross_a_mosh_hop() {
  if ! command -v mosh-server >/dev/null 2>&1 || ! command -v mosh-client >/dev/null 2>&1; then
    echo "SKIP: mosh hop test (mosh-server/mosh-client not installed)"
    return
  fi
  local home sock cap conn port key runner client name
  home="$(make_fake_home)"
  sock="${TEST_ROOT}/mosh.sock"
  cap="${TEST_ROOT}/mosh.cap"
  conn="$(LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 mosh-server new -c 256 -i 127.0.0.1 \
    -p 61100:61999 -- env HOME="${home}" tmux -S "${sock}" -f "${TMUX_CONF}" \
    new-session -s osc52 "$(pane_script)" 2>/dev/null | grep '^MOSH CONNECT')"
  read -r _ _ port key <<< "${conn}"
  if [[ -z "${key:-}" ]]; then
    assert_eq "MOSH CONNECT" "${conn}" "mosh-server started on 127.0.0.1"
    return
  fi
  MOSH_KEY="${key}" LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 TERM=xterm-kitty \
    python3 "${TEST_ROOT}/ptyrun.py" "${cap}" 15 mosh-client 127.0.0.1 "${port}" &
  runner=$!
  if ! client="$(wait_for_client "${sock}")"; then
    assert_eq "client" "none" "tmux client started under mosh-server"
    kill "${runner}" 2>/dev/null
    return
  fi
  sleep 1.5
  HOME="${home}" drive_copies "${sock}" "${client}"
  sleep 0.5
  tmux -S "${sock}" kill-server 2>/dev/null
  wait "${runner}" 2>/dev/null

  for name in load-buffer-w copy-mode-line app-select-c; do
    assert_eq "c" "$(osc52_pc "${cap}" "${name}")" \
      "${name} reaches the terminal through mosh as OSC 52 Pc c"
  done
}

main() {
  local fn
  if ! command -v python3 >/dev/null 2>&1 || ! command -v tmux >/dev/null 2>&1; then
    echo "FAIL: tmux and python3 are required" >&2
    return 1
  fi
  write_pty_capture_helper
  for fn in $(declare -F | awk '{print $3}' | grep '^test_'); do
    "${fn}"
  done
  echo "tmux OSC 52 tests: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
  (( FAIL_COUNT == 0 ))
}

main "$@"
