#!/usr/bin/env bash
# ==============================================================================
# Unit & Integration Test Suite: ble.sh + fzf `**<Tab>` Completion (`dsr0` Fix)
# ==============================================================================
# Verifies that `~/.fzf.bash` suppresses `builtin printf '\e[5n'` (Device Status
# Report 5) inside `_fzf_complete` and `__fzf_generic_path_completion` whenever
# `ble.sh` is attached (`_ble_attached` non-empty), preventing terminal `\e[0n`
# (`dsr0`) replies from aborting candidate insertion or triggering `ble.sh`'s
# `unbound keyseq: dsr0` / `unused keyseq dsr0` visual bell.
#
# Usage Example:
#   ~/scripts/tests/test_blesh_fzf_completion.sh
# ==============================================================================

set -euo pipefail

BASHRC="${HOME}/.bashrc"
TEST_TMPDIR="$(mktemp -d /tmp/test_blesh_fzf_comp.XXXXXX)"
TMUX_SOCK="test_blesh_fzf_comp_$$"

cleanup() {
  tmux -L "${TMUX_SOCK}" kill-server >/dev/null 2>&1 || true
  rm -rf "${TEST_TMPDIR}"
}
trap cleanup EXIT

PASS_COUNT=0
FAIL_COUNT=0

run_test() {
  local test_name="$1"
  shift
  if "$@"; then
    echo "  [PASS] ${test_name}"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  [FAIL] ${test_name}" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

wait_for_blesh_prompt() {
  local session_name="$1"
  for _ in $(seq 1 80); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${session_name}" 2>/dev/null | rg -q '\[Exit:'; then
      sleep 0.25
      return 0
    fi
    sleep 0.1
  done
  return 1
}

# ------------------------------------------------------------------------------
# 1. Unit Flow: ~/.fzf.bash guards `builtin printf '\e[5n'` on `_ble_attached`
# ------------------------------------------------------------------------------
test_fzf_bash_guards_dsr5_in_completion_functions() {
  local out
  out="$(
    bash --norc -i -c '
      set -euo pipefail
      source "${HOME}/.fzf.bash"
      for fn in _fzf_complete __fzf_generic_path_completion; do
        def="$(declare -f "${fn}")"
        [[ "${def}" == *"_ble_attached"* ]] || { echo "missing guard in ${fn}"; exit 1; }
      done
      printf "OK"
    ' 2>/dev/null
  )"
  [[ "${out}" == "OK" ]]
}

# ------------------------------------------------------------------------------
# 2. Boundary Flow: Re-sourcing ~/.fzf.bash is idempotent (no duplicate guards)
# ------------------------------------------------------------------------------
test_fzf_bash_re_source_idempotency() {
  local out
  out="$(
    bash --norc -i -c '
      set -euo pipefail
      source "${HOME}/.fzf.bash"
      source "${HOME}/.fzf.bash"
      for fn in _fzf_complete __fzf_generic_path_completion; do
        def="$(declare -f "${fn}")"
        [[ "${def}" == *"_ble_attached"* ]] || { echo "missing guard in ${fn}"; exit 1; }
        if [[ "${def}" == *"_ble_attached"*"_ble_attached"* ]]; then
          echo "duplicate guard in ${fn}"
          exit 1
        fi
      done
      printf "OK"
    ' 2>/dev/null
  )"
  [[ "${out}" == "OK" ]]
}

# ------------------------------------------------------------------------------
# 3. Boundary Flow: Unattached Readline mode still emits `\e[5n`
# ------------------------------------------------------------------------------
test_detached_readline_retains_dsr5_when_ble_unattached() {
  local hex_out
  hex_out="$(
    _ble_attached="" bash -c '
      eval "[[ -n \${_ble_attached-} ]] || builtin printf '\''\\e[5n'\''"
    ' | od -An -tx1 | tr -d ' \n'
  )"
  [[ "${hex_out}" == "1b5b356e" ]] || return 1

  local attached_out
  attached_out="$(
    _ble_attached="1" bash -c '
      eval "[[ -n \${_ble_attached-} ]] || builtin printf '\''\\e[5n'\''"
    ' | od -An -tx1 | tr -d ' \n'
  )"
  [[ -z "${attached_out}" ]]
}

# ------------------------------------------------------------------------------
# 4. E2E Positive Flow: `kill **<Tab>` + Enter inserts PID with zero dsr0 bell
# ------------------------------------------------------------------------------
test_kill_star_star_tab_enter_inserts_pid_without_dsr0_warning() {
  local sess="e2e_kill_single"
  tmux -L "${TMUX_SOCK}" -f /dev/null new-session -d -s "${sess}" -x 120 -y 30 \
    "env TMUX_PANE=%0 bash --rcfile '${BASHRC}' -i"
  wait_for_blesh_prompt "${sess}" || return 1

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'kill **' Tab
  for _ in $(seq 1 40); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}" 2>/dev/null | rg -q 'PID'; then
      break
    fi
    sleep 0.1
  done

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" Enter
  sleep 0.35
  local pane_text
  pane_text="$(tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}")"
  tmux -L "${TMUX_SOCK}" kill-session -t "${sess}" 2>/dev/null || true

  if printf '%s\n' "${pane_text}" | rg -qi 'dsr0'; then
    echo "    Found unexpected dsr0 warning in pane:" >&2
    printf '%s\n' "${pane_text}" | head -n 6 >&2
    return 1
  fi
  printf '%s\n' "${pane_text}" | rg -q '^\$ kill [0-9]+' || {
    echo "    Expected '\$ kill <pid>', got:" >&2
    printf '%s\n' "${pane_text}" | head -n 6 >&2
    return 1
  }
}

# ------------------------------------------------------------------------------
# 5. E2E Multi-Select Flow: `kill **<Tab>` + Tab + Down + Tab + Enter
# ------------------------------------------------------------------------------
test_kill_star_star_tab_multiselect_inserts_multiple_pids() {
  local sess="e2e_kill_multi"
  tmux -L "${TMUX_SOCK}" -f /dev/null new-session -d -s "${sess}" -x 120 -y 30 \
    "env TMUX_PANE=%0 bash --rcfile '${BASHRC}' -i"
  wait_for_blesh_prompt "${sess}" || return 1

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'kill **' Tab
  for _ in $(seq 1 40); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}" 2>/dev/null | rg -q 'PID'; then
      break
    fi
    sleep 0.1
  done

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" Tab Tab Enter
  sleep 0.35
  local pane_text
  pane_text="$(tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}")"
  tmux -L "${TMUX_SOCK}" kill-session -t "${sess}" 2>/dev/null || true

  ! printf '%s\n' "${pane_text}" | rg -qi 'dsr0' || return 1
  printf '%s\n' "${pane_text}" | rg -q '^\$ kill [0-9]+ [0-9]+' || return 1
}

# ------------------------------------------------------------------------------
# 6. E2E Cancellation Flow: `kill **<Tab>` + Escape emits zero dsr0 warning
# ------------------------------------------------------------------------------
test_kill_star_star_tab_escape_cancels_without_dsr0_warning() {
  local sess="e2e_kill_esc"
  tmux -L "${TMUX_SOCK}" -f /dev/null new-session -d -s "${sess}" -x 120 -y 30 \
    "env TMUX_PANE=%0 bash --rcfile '${BASHRC}' -i"
  wait_for_blesh_prompt "${sess}" || return 1

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'kill **' Tab
  for _ in $(seq 1 40); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}" 2>/dev/null | rg -q 'PID'; then
      break
    fi
    sleep 0.1
  done

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" Escape
  sleep 0.35
  local pane_text
  pane_text="$(tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}")"
  tmux -L "${TMUX_SOCK}" kill-session -t "${sess}" 2>/dev/null || true

  ! printf '%s\n' "${pane_text}" | rg -qi 'dsr0'
}

# ------------------------------------------------------------------------------
# 7. E2E Path Flow: `ls **<Tab>` + Enter inserts file without dsr0 warning
# ------------------------------------------------------------------------------
test_path_star_star_tab_enter_inserts_path_without_dsr0_warning() {
  local workdir="${TEST_TMPDIR}/path_comp_dir"
  mkdir -p "${workdir}"
  touch "${workdir}/unique_target_file.txt"

  local sess="e2e_path_comp"
  tmux -L "${TMUX_SOCK}" -f /dev/null new-session -d -s "${sess}" -c "${workdir}" -x 120 -y 30 \
    "env TMUX_PANE=%0 bash --rcfile '${BASHRC}' -i"
  wait_for_blesh_prompt "${sess}" || return 1

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'ls **' Tab
  for _ in $(seq 1 40); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}" 2>/dev/null | rg -q 'unique_target_file\.txt'; then
      break
    fi
    sleep 0.1
  done

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" Enter
  sleep 0.35
  local pane_text
  pane_text="$(tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}")"
  tmux -L "${TMUX_SOCK}" kill-session -t "${sess}" 2>/dev/null || true

  ! printf '%s\n' "${pane_text}" | rg -qi 'dsr0' || return 1
  printf '%s\n' "${pane_text}" | rg -q '^\$ ls .*unique_target_file\.txt' || return 1
}

# ------------------------------------------------------------------------------
# 8. E2E Live Reload Flow: `source ~/.fzf.bash` in already-running ble.sh shell
# ------------------------------------------------------------------------------
test_re_sourcing_fzf_bash_in_live_blesh_session_fixes_kill_completion() {
  local sess="e2e_live_reload"
  tmux -L "${TMUX_SOCK}" -f /dev/null new-session -d -s "${sess}" -x 120 -y 30 \
    "env TMUX_PANE=%0 bash --rcfile '${BASHRC}' -i"
  wait_for_blesh_prompt "${sess}" || return 1

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'source ~/.fzf.bash' Enter
  sleep 0.4

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" 'kill **' Tab
  for _ in $(seq 1 40); do
    if tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}" 2>/dev/null | rg -q 'PID'; then
      break
    fi
    sleep 0.1
  done

  tmux -L "${TMUX_SOCK}" send-keys -t "${sess}" Enter
  sleep 0.35
  local pane_text
  pane_text="$(tmux -L "${TMUX_SOCK}" capture-pane -pt "${sess}")"
  tmux -L "${TMUX_SOCK}" kill-session -t "${sess}" 2>/dev/null || true

  ! printf '%s\n' "${pane_text}" | rg -qi 'dsr0' || return 1
  printf '%s\n' "${pane_text}" | rg -q '^\$ kill [0-9]+' || return 1
}

echo "Running ble.sh + fzf completion (dsr0) test suite..."
run_test ".fzf.bash guards DSR 5 in _fzf_complete and __fzf_generic_path_completion" \
  test_fzf_bash_guards_dsr5_in_completion_functions
run_test "re-sourcing .fzf.bash is idempotent" \
  test_fzf_bash_re_source_idempotency
run_test "unattached Readline mode still emits DSR 5" \
  test_detached_readline_retains_dsr5_when_ble_unattached
run_test "kill **<Tab> + Enter inserts numeric PID without dsr0 warning" \
  test_kill_star_star_tab_enter_inserts_pid_without_dsr0_warning
run_test "kill **<Tab> multi-select inserts multiple PIDs without dsr0 warning" \
  test_kill_star_star_tab_multiselect_inserts_multiple_pids
run_test "kill **<Tab> + Escape cancels cleanly without dsr0 warning" \
  test_kill_star_star_tab_escape_cancels_without_dsr0_warning
run_test "ls **<Tab> + Enter inserts path without dsr0 warning" \
  test_path_star_star_tab_enter_inserts_path_without_dsr0_warning
run_test "re-sourcing .fzf.bash in live ble.sh session works cleanly" \
  test_re_sourcing_fzf_bash_in_live_blesh_session_fixes_kill_completion

echo ""
echo "Summary: ${PASS_COUNT} passed, ${FAIL_COUNT} failed."
[[ ${FAIL_COUNT} -eq 0 ]]
