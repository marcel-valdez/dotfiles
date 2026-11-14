#!/usr/bin/env bash
# ==============================================================================
# Unit & Nested-Tmux Integration Test Suite for ~/bin/tmux-notify
# ==============================================================================
# Exercises:
#   1. Dynamic 80% x 80% layout sizing and multi-client min(W) / min(H)
#   2. Overflow switching to batcat -> bat -> less with pre-wrapped Markdown
#   3. macOS (Darwin + Bash 5.x) UTF-8 probe, PATH splicing, and re-exec guard
#   4. --select-pane, --session, -- end-of-options, and Escape vs CSI sequences
#   5. PID-liveness-guarded popup_* pruning and atomic slots/-only cleanup
#   6. Nested-tmux multi-client broadcast, sibling auto-close, HUP isolation,
#      and closed-FD TIMER_PID pager timeout enforcement
#
# Usage Example:
#   bash ~/scripts/tests/test_tmux_notify.sh
#   OS_COMPAT_UNAME_S=Darwin bash ~/scripts/tests/test_tmux_notify.sh
# ==============================================================================

set -uo pipefail

TMUX_NOTIFY="${TMUX_NOTIFY:-${HOME}/bin/tmux-notify}"
PASS_COUNT=0
FAIL_COUNT=0

TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/test_tmux_notify_XXXXXX")"
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

# Runs the layout engine. Sets LAYOUT_W, LAYOUT_H, LAYOUT_MODE, and BODY array.
layout() {
  local size="$1"
  shift
  local out
  out="$(env -u TMUX_NOTIFY_MAX_COLS bash "${TMUX_NOTIFY}" --print-layout \
    --client-size "${size}" "$@")"
  read -r LAYOUT_W LAYOUT_H LAYOUT_MODE <<< "$(head -n 1 <<< "${out}")"
  BODY=()
  while IFS= read -r row; do
    BODY+=("${row}")
  done < <(tail -n +2 <<< "${out}")
}

# Asserts no body row is wider than the popup interior (width minus border).
assert_rows_fit() {
  local msg="$1"
  local interior=$(( LAYOUT_W - 2 ))
  local row
  for row in "${BODY[@]}"; do
    if (( ${#row} > interior )); then
      echo "FAIL: ${msg}: row '${row}' (${#row}) > interior ${interior}" >&2
      FAIL_COUNT=$((FAIL_COUNT + 1))
      return
    fi
  done
  PASS_COUNT=$((PASS_COUNT + 1))
}

# Joins body rows back into words to check that wrapping lost nothing.
joined_words() {
  local IFS=' '
  # shellcheck disable=SC2048,SC2086 # word splitting is intended here.
  echo ${BODY[*]}
}

test_short_single_line_uses_min_width() {
  layout 192x50 "Build" "Done."
  assert_eq "30" "${LAYOUT_W}" "short message clamps to minimum width"
  assert_eq "3" "${LAYOUT_H}" "single row plus 2 border rows"
  assert_eq "inline" "${LAYOUT_MODE}" "short message stays inline"
  assert_eq " Done." "${BODY[0]}" "body row has 1 column left padding"
}

test_long_single_line_expands_up_to_80_pct_width() {
  local msg
  msg="Jetski Run Completed: Workspace [jetski-cli-notify]: This is a"
  msg+=" deliberately long single-line notification message that should"
  msg+=" clearly expand beyond the legacy 80-column limit all the way up to"
  msg+=" eighty percent of the two-hundred-column client screen width END."
  layout 200x50 "Knock" "${msg}"
  assert_eq "160" "${LAYOUT_W}" "long message expands to 80% of 200 cols (160)"
  assert_eq "inline" "${LAYOUT_MODE}" "fits vertically in inline mode"
  assert_eq "$(( ${#BODY[@]} + 2 ))" "${LAYOUT_H}" \
    "height counts every wrapped row plus border"
  assert_true "long message wraps across >=2 rows" test "${#BODY[@]}" -ge 2
  assert_rows_fit "long message rows fit the 160-col interior"
  assert_eq "${msg}" "$(joined_words)" "wrapping keeps every word in order"
}

test_multi_client_uses_smallest_client_dimensions() {
  local out w h mode
  out="$(env -u TMUX_NOTIFY_MAX_COLS bash "${TMUX_NOTIFY}" --print-layout \
    --client-size 284x71 --client-size 240x62 "Overflow" \
    "$(seq -f 'row %g' 1 80)" | head -n 1)"
  read -r w h mode <<< "${out}"
  assert_eq "192" "${w}" "80% of smaller width 240 is 192"
  assert_eq "49" "${h}" "80% of smaller height 62 is 49"
  assert_eq "pager" "${mode}" "80 rows on 62-row client enters pager mode"

  out="$(env -u TMUX_NOTIFY_MAX_COLS bash "${TMUX_NOTIFY}" --print-layout \
    --client-size 200x40 --client-size 150x60 "Asym" \
    "$(seq -f 'row %g' 1 80)" | head -n 1)"
  read -r w h mode <<< "${out}"
  assert_eq "120" "${w}" "asymmetric min width 150 -> 80% is 120"
  assert_eq "32" "${h}" "asymmetric min height 40 -> 80% is 32"
  assert_eq "pager" "${mode}" "asymmetric overflow enters pager mode"
}

test_first_line_is_first_row() {
  layout 192x50 "Three Lines" "$(printf 'line one\nline two\nline three')"
  assert_eq " line one" "${BODY[0]}" "first logical line is the first row"
  assert_eq "5" "${LAYOUT_H}" "three rows plus 2 border rows"
  assert_eq "inline" "${LAYOUT_MODE}" "three lines fit inline"
}

test_blank_lines_and_indentation_preserved() {
  layout 100x30 "Knock" "$(printf 'Question?\n\nChoices:\n  1. One\n  2. Two')"
  assert_eq "" "${BODY[1]// /}" "blank line is preserved"
  assert_eq "   1. One" "${BODY[3]}" "indentation is preserved"
  assert_eq "7" "${LAYOUT_H}" "five rows plus 2 border rows"
}

test_tabs_expand_and_carriage_returns_strip() {
  layout 100x30 "T" "$(printf 'a\tb\r\nc')"
  assert_eq " a    b" "${BODY[0]}" "tab expands to 4 spaces, CR stripped"
  assert_eq " c" "${BODY[1]}" "second line follows"
}

test_overflow_switches_to_pager_at_80_pct_size() {
  layout 100x25 "Overflow" "$(seq -f 'row %g' 1 60)"
  assert_eq "80" "${LAYOUT_W}" "overflow width is 80% of 100 (80)"
  assert_eq "20" "${LAYOUT_H}" "overflow height is 80% of 25 (20)"
  assert_eq "pager" "${LAYOUT_MODE}" "overflow switches to pager mode"
  assert_eq "60" "${#BODY[@]}" "pager preserves all 60 lines without truncation"
  assert_eq "row 1" "${BODY[0]:-}" "first row unpadded for Markdown highlighting"
  assert_eq "row 60" "${BODY[59]:-}" "last row preserved without +N marker"
}

test_exact_fit_has_no_marker() {
  layout 80x20 "Fit" "$(seq -f 'row %g' 1 14)"
  assert_eq "16" "${LAYOUT_H}" "14 rows + 2 border = 16 (80% of 20)"
  assert_eq "inline" "${LAYOUT_MODE}" "exact fit stays inline"
  assert_eq " row 14" "${BODY[-1]}" "no marker when everything fits"
}

test_narrow_client_scales_to_80_pct() {
  layout 60x20 "Knock" "$(printf 'word %.0s' {1..40})"
  assert_eq "48" "${LAYOUT_W}" "popup width is 80% of 60-col client (48)"
  assert_rows_fit "rows fit a narrow popup"
}

test_small_client_floors_at_min_width_when_screen_allows() {
  layout 35x20 "Knock" "Short"
  assert_eq "30" "${LAYOUT_W}" "35-col client floors 80% (28) up to 30 cols"
}

test_tiny_client_clamps_to_screen_width() {
  layout 20x10 "Knock" "$(printf 'word %.0s' {1..6})"
  assert_eq "20" "${LAYOUT_W}" "20-col tiny client clamps width <= client_w"
  assert_true "tiny client height <= client_h" test "${LAYOUT_H}" -le 10
  assert_rows_fit "rows fit tiny popup"
}

test_long_word_is_hard_broken() {
  layout 100x50 "Path" "$(printf 'x%.0s' {1..200})"
  assert_rows_fit "unbreakable word is split across rows"
  assert_eq "5" "${LAYOUT_H}" "200 chars at 76 per row is 3 rows + 2 border"
}

test_multibyte_wraps_by_characters() {
  local msg
  msg="$(printf 'á→ %.0s' {1..60})"
  layout 100x50 "UTF-8" "${msg}"
  assert_rows_fit "multibyte rows fit the interior by character count"
  assert_true "multibyte rows are not split short" test "${#BODY[0]}" -ge 70
}

test_multibyte_under_c_locale() {
  local err_file="${TEST_ROOT}/locale_stderr"
  local out
  out="$(LANG=C LC_ALL='' bash "${TMUX_NOTIFY}" --print-layout \
    --client-size 100x50 "UTF-8" "$(printf 'á→ %.0s' {1..60})" \
    2>"${err_file}" | sed -n 2p)"
  assert_true "C locale still counts characters" test "${#out}" -ge 70
  assert_eq "" "$(< "${err_file}")" "Linux locale probe emits zero stderr"

  out="$(OS_COMPAT_UNAME_S=Darwin LANG=C LC_ALL='' bash "${TMUX_NOTIFY}" \
    --print-layout --client-size 100x50 "UTF-8" \
    "$(printf 'á→ %.0s' {1..60})" 2>"${err_file}" | sed -n 2p)"
  assert_true "Darwin locale order still counts UTF-8 characters" \
    test "${#out}" -ge 70
  assert_eq "" "$(< "${err_file}")" "Darwin locale probe emits zero stderr"
}

test_macos_path_bootstrap_and_reexec_guard() {
  local stub_dir="${TEST_ROOT}/path_stub"
  mkdir -p "${stub_dir}"
  cat << 'EOF' > "${stub_dir}/tmux"
#!/usr/bin/env bash
if [[ "${1:-}" == "list-clients" ]]; then
  echo "/dev/pts/0 main 100 30 0"
  exit 0
fi
if [[ "${1:-}" == "display-popup" ]]; then
  printf '%s\n' "${PATH}" > "${PATH_RECORD_FILE}"
  exit 0
fi
exit 0
EOF
  chmod +x "${stub_dir}/tmux"

  local rec="${TEST_ROOT}/recorded_path.txt"
  PATH_RECORD_FILE="${rec}" _TMUX_NOTIFY_REEXEC=1 OS_COMPAT_UNAME_S=Darwin \
    PATH="${stub_dir}:/usr/bin:/bin" \
    bash "${TMUX_NOTIFY}" "Title" "Hello"
  local got_path
  got_path="$(< "${rec}")"
  # Leading stub_dir must still precede ~/bin and /usr/bin.
  assert_eq "${stub_dir}" "${got_path%%:*}" \
    "leading stub dir stays first in PATH"
  if [[ -d "${HOME}/bin" ]]; then
    local before_usr="${got_path%%/usr/bin*}"
    assert_true "HOME/bin is spliced before /usr/bin" \
      grep -Fq "${HOME}/bin:" <<< "${before_usr}"
  fi
}

test_title_only_and_end_of_options() {
  layout 192x50 "Just a message"
  assert_eq " Just a message" "${BODY[0]}" "single arg becomes the message"

  local out
  out="$(bash "${TMUX_NOTIFY}" --print-layout --client-size 100x30 -- \
    "--dash-title" "--dash-message" | sed -n 2p)"
  assert_eq " --dash-message" "${out}" "-- allows leading-dash title/message"

  local stub_dir="${TEST_ROOT}/empty_msg_stub"
  mkdir -p "${stub_dir}"
  cat << 'EOF' > "${stub_dir}/tmux"
#!/usr/bin/env bash
if [[ "${1:-}" == "list-clients" ]]; then
  echo "/dev/pts/0 main 100 30 0"
  exit 0
fi
if [[ "${1:-}" == "display-popup" ]]; then
  printf '%s\n' "$*" > "${POPUP_ARGS_FILE}"
  exit 0
fi
exit 0
EOF
  chmod +x "${stub_dir}/tmux"
  local args_file="${TEST_ROOT}/empty_msg_args.txt"
  POPUP_ARGS_FILE="${args_file}" PATH="${stub_dir}:${PATH}" \
    bash "${TMUX_NOTIFY}" -- "ExplicitTitle" ""
  assert_true "explicit title preserved when 2nd arg is empty" \
    grep -Fq -- "-T ExplicitTitle" "${args_file}"
}

test_invalid_client_size_fails() {
  bash "${TMUX_NOTIFY}" --print-layout --client-size 80by24 "T" "M" \
    >/dev/null 2>&1
  assert_eq "2" "$?" "malformed --client-size exits with status 2"
}

test_max_cols_override() {
  local out
  out="$(TMUX_NOTIFY_MAX_COLS=50 bash "${TMUX_NOTIFY}" --print-layout \
    --client-size 192x50 "T" "$(printf 'word %.0s' {1..40})" | head -n 1)"
  assert_eq "50" "${out%% *}" "TMUX_NOTIFY_MAX_COLS caps the width"
}

# Helper to build an isolated pager test directory with stub tmux & pagers.
setup_pager_test_env() {
  local dir="$1"
  mkdir -p "${dir}/bin" "${dir}/runtime"
  cat << 'EOF' > "${dir}/bin/tmux"
#!/usr/bin/env bash
case "${1:-}" in
  list-clients)
    echo "/dev/pts/1 main 100 25 0"
    ;;
  display-popup)
    shift
    while (( $# > 0 )); do
      if [[ "$1" == "-E" ]]; then
        printf '%s\n' "$2" > "${STUB_CMD_FILE}"
        exit 0
      fi
      shift
    done
    ;;
  select-pane)
    printf '%s\n' "$*" >> "${STUB_SELECT_PANE_FILE}"
    ;;
esac
exit 0
EOF
  chmod +x "${dir}/bin/tmux"

  cat << 'EOF' > "${dir}/bin/less"
#!/usr/bin/env bash
if [[ "${1:-}" == "--help" ]]; then
  echo "--status-line --mouse --incsearch --quit-on-intr --raw-control-chars"
  exit 0
fi
input_tmp="${PAGER_LOG}.in"
if (( $# > 0 )) && [[ -f "${!#}" ]]; then
  cat "${!#}" > "${input_tmp}"
else
  cat > "${input_tmp}"
fi
sed -E $'s/\x1b\\[[0-9;]*m//g' "${input_tmp}" > "${input_tmp}.plain"
printf 'LESS_ENV=%s\n' "${LESS-<unset>}" > "${PAGER_LOG}"
printf 'BIN=less\n' >> "${PAGER_LOG}"
printf 'ARGS=%s\n' "$*" >> "${PAGER_LOG}"
printf 'FIRST_LINE=%s\n' "$(head -n 1 "${input_tmp}.plain")" >> "${PAGER_LOG}"
printf 'MAX_LEN=%s\n' "$(awk '{ if (length($0) > m) m = length($0) } END { print m+0 }' "${input_tmp}.plain")" >> "${PAGER_LOG}"
exit 0
EOF
  chmod +x "${dir}/bin/less"
}

test_pager_prefers_batcat_over_bat_and_less() {
  local dir="${TEST_ROOT}/pager_batcat"
  setup_pager_test_env "${dir}"
  for b in batcat bat; do
    cat << EOF > "${dir}/bin/${b}"
#!/usr/bin/env bash
printf 'LESS_ENV=%s\n' "\${LESS-<unset>}" > "\${PAGER_LOG}"
printf 'BIN=${b}\n' >> "\${PAGER_LOG}"
printf 'ARGS=%s\n' "\$*" >> "\${PAGER_LOG}"
printf 'FIRST_LINE=%s\n' "\$(head -n 1 "\${!#}")" >> "\${PAGER_LOG}"
exit 0
EOF
    chmod +x "${dir}/bin/${b}"
  done

  local cmd_file="${dir}/cmd.txt"
  local pager_log="${dir}/pager.log"
  STUB_CMD_FILE="${cmd_file}" XDG_RUNTIME_DIR="${dir}/runtime" \
    PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" "Overflow" "$(seq -f '## Item %g' 1 40)"
  local runner_cmd
  runner_cmd="$(< "${cmd_file}")"
  LESS="-F" PAGER_LOG="${pager_log}" XDG_RUNTIME_DIR="${dir}/runtime" \
    PATH="${dir}/bin:${PATH}" \
    eval "${runner_cmd}" </dev/null

  assert_true "LESS env var is cleared" grep -Fxq "LESS_ENV=<unset>" "${pager_log}"
  assert_true "batcat preferred over bat and less" \
    grep -Fxq "BIN=batcat" "${pager_log}"
  assert_true "batcat receives plain markdown paging flags and less opts" \
    grep -Fq -- "--style=plain --language=markdown --paging=always --pager=less --status-line --mouse --incsearch --quit-on-intr --raw-control-chars" "${pager_log}"
  assert_true "wrapped.txt preserves Markdown heading at col 0" \
    grep -Fxq "FIRST_LINE=## Item 1" "${pager_log}"
}

test_pager_falls_back_to_bat_on_macos() {
  local dir="${TEST_ROOT}/pager_bat"
  setup_pager_test_env "${dir}"
  # Symlink core utilities needed without exposing /usr/bin/batcat.
  local util
  for util in bash env uname fold head tail wc tr sed awk grep cat mkdir rmdir rm mktemp sleep kill pgrep stat date id basename dirname; do
    local p
    p="$(command -v "${util}" 2>/dev/null || true)"
    [[ -n "${p}" ]] && ln -sf "${p}" "${dir}/bin/${util}"
  done
  cat << 'EOF' > "${dir}/bin/bat"
#!/usr/bin/env bash
printf 'LESS_ENV=%s\n' "${LESS-<unset>}" > "${PAGER_LOG}"
printf 'BIN=bat\n' >> "${PAGER_LOG}"
printf 'ARGS=%s\n' "$*" >> "${PAGER_LOG}"
exit 0
EOF
  chmod +x "${dir}/bin/bat"

  local cmd_file="${dir}/cmd.txt"
  local pager_log="${dir}/pager.log"
  OS_COMPAT_UNAME_S=Darwin TMUX_NOTIFY_NO_PATH_BOOTSTRAP=1 \
    STUB_CMD_FILE="${cmd_file}" TMPDIR="${dir}/runtime" \
    PATH="${dir}/bin" \
    bash "${TMUX_NOTIFY}" "MacOverflow" "$(seq -f 'row %g' 1 40)"
  local runner_cmd
  runner_cmd="$(< "${cmd_file}")"
  OS_COMPAT_UNAME_S=Darwin TMUX_NOTIFY_NO_PATH_BOOTSTRAP=1 LESS="-F" \
    PAGER_LOG="${pager_log}" TMPDIR="${dir}/runtime" PATH="${dir}/bin" \
    eval "${runner_cmd}" </dev/null

  assert_true "macOS fallback selects bat when batcat absent" \
    grep -Fxq "BIN=bat" "${pager_log}"
  assert_true "bat receives desired less flags" \
    grep -Fq -- "--pager=less --status-line --mouse --incsearch --quit-on-intr --raw-control-chars" "${pager_log}"
}

test_pager_falls_back_to_less_when_bat_absent() {
  local dir="${TEST_ROOT}/pager_less"
  setup_pager_test_env "${dir}"
  local util
  for util in bash env uname fold head tail wc tr sed awk grep cat mkdir rmdir rm mktemp sleep kill pgrep stat date id basename dirname; do
    local p
    p="$(command -v "${util}" 2>/dev/null || true)"
    [[ -n "${p}" ]] && ln -sf "${p}" "${dir}/bin/${util}"
  done

  local cmd_file="${dir}/cmd.txt"
  local pager_log="${dir}/pager.log"
  TMUX_NOTIFY_NO_PATH_BOOTSTRAP=1 STUB_CMD_FILE="${cmd_file}" \
    XDG_RUNTIME_DIR="${dir}/runtime" PATH="${dir}/bin" \
    bash "${TMUX_NOTIFY}" "LessOverflow" "$(seq -f 'row %g' 1 40)"
  local runner_cmd
  runner_cmd="$(< "${cmd_file}")"
  TMUX_NOTIFY_NO_PATH_BOOTSTRAP=1 LESS="-F" PAGER_LOG="${pager_log}" \
    XDG_RUNTIME_DIR="${dir}/runtime" PATH="${dir}/bin" \
    eval "${runner_cmd}" </dev/null

  assert_true "falls back to less when batcat/bat absent" \
    grep -Fxq "BIN=less" "${pager_log}"
  assert_true "less receives exact 5 long options" \
    grep -Fq -- "--status-line --mouse --incsearch --quit-on-intr --raw-control-chars" "${pager_log}"
}

test_pager_long_markdown_line_preserves_leading_text() {
  local dir="${TEST_ROOT}/pager_long_md"
  setup_pager_test_env "${dir}"
  local long_md
  long_md="OPENING_MARKER $(printf 'markdownword %.0s' {1..25})"
  long_md+=$'\n'"$(seq -f '- bullet %g' 1 35)"

  local cmd_file="${dir}/cmd.txt"
  local pager_log="${dir}/pager.log"
  TMUX_NOTIFY_NO_PATH_BOOTSTRAP=1 STUB_CMD_FILE="${cmd_file}" \
    XDG_RUNTIME_DIR="${dir}/runtime" PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" "MD" "${long_md}"
  local runner_cmd
  runner_cmd="$(< "${cmd_file}")"
  LESS="-F" PAGER_LOG="${pager_log}" XDG_RUNTIME_DIR="${dir}/runtime" \
    PATH="${dir}/bin:${PATH}" \
    eval "${runner_cmd}" </dev/null

  assert_true "opening text remains on line 1 of wrapped.txt" \
    grep -Fq "FIRST_LINE=OPENING_MARKER " "${pager_log}"
  local max_len
  max_len="$(sed -n 's/^MAX_LEN=//p' "${pager_log}")"
  # Client 100x25 -> popup_w=80 -> wrap_w = 80 - 4 = 76.
  assert_true "all wrapped lines fit <= popup_w - 4 (76)" \
    test "${max_len}" -le 76
}

test_select_pane_and_escape_vs_csi_sequences() {
  local dir="${TEST_ROOT}/select_pane"
  setup_pager_test_env "${dir}"
  local cmd_file="${dir}/cmd.txt"
  local pane_log="${dir}/select_pane.log"

  # 1. Arrow key + focus sequences (\e[A, \e[I, \e[O) must NOT dismiss early,
  #    followed by 'q' which dismisses interactively and runs select-pane.
  STUB_CMD_FILE="${cmd_file}" STUB_SELECT_PANE_FILE="${pane_log}" \
    XDG_RUNTIME_DIR="${dir}/runtime" PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" --select-pane "%42" -- "Focus" "Short body"
  local runner_cmd
  runner_cmd="$(< "${cmd_file}")"
  rm -f "${pane_log}"
  printf '\e[A\e[I\e[Oq' | STUB_SELECT_PANE_FILE="${pane_log}" \
    PATH="${dir}/bin:${PATH}" eval "${runner_cmd}" >/dev/null
  assert_true "interactive dismiss after CSI sequences calls select-pane %42" \
    grep -Fq -- "-t %42" "${pane_log}"

  # 2. Standalone Escape (\e with no trailing byte) dismisses interactively.
  STUB_CMD_FILE="${cmd_file}" XDG_RUNTIME_DIR="${dir}/runtime" \
    PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" --select-pane "%77" -- "Esc" "Short body"
  runner_cmd="$(< "${cmd_file}")"
  rm -f "${pane_log}"
  printf '\e' | STUB_SELECT_PANE_FILE="${pane_log}" \
    PATH="${dir}/bin:${PATH}" eval "${runner_cmd}" >/dev/null
  assert_true "standalone Escape calls select-pane %77" \
    grep -Fq -- "-t %77" "${pane_log}"

  # 3. Timeout expiration does NOT steal pane focus.
  TMUX_NOTIFY_TIMEOUT=1 STUB_CMD_FILE="${cmd_file}" \
    XDG_RUNTIME_DIR="${dir}/runtime" PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" --select-pane "%99" -- "Timeout" "Short body"
  runner_cmd="$(< "${cmd_file}")"
  rm -f "${pane_log}"
  ( sleep 2 ) | STUB_SELECT_PANE_FILE="${pane_log}" \
    PATH="${dir}/bin:${PATH}" eval "${runner_cmd}" >/dev/null
  assert_true "timeout does NOT call select-pane" test ! -e "${pane_log}"
}

test_pruner_preserves_live_pid_popup_dir() {
  local dir="${TEST_ROOT}/pruner"
  setup_pager_test_env "${dir}"
  local state_dir
  state_dir="$(XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    bash -c 'source "${HOME}/lib/os_compat.sh" && printf "%s/tmux-notify" "$(os::runtime_dir)"')"
  mkdir -p "${state_dir}/popup_dead00/pids" "${state_dir}/popup_live00/pids"
  : > "${state_dir}/popup_dead00/pids/999999"
  : > "${state_dir}/popup_live00/pids/$$"
  touch -t 202001010000 "${state_dir}/popup_dead00" "${state_dir}/popup_live00"

  local cmd_file="${dir}/cmd.txt"
  TMUX_NOTIFY_TIMEOUT=10 STUB_CMD_FILE="${cmd_file}" \
    XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    PATH="${dir}/bin:${PATH}" \
    bash "${TMUX_NOTIFY}" "PruneCheck" "Hi"

  assert_true "expired popup dir with dead PID is pruned" \
    test ! -d "${state_dir}/popup_dead00"
  assert_true "expired popup dir with live PID is preserved" \
    test -d "${state_dir}/popup_live00"
}

wait_until() {
  local timeout_loops="$1"
  shift
  local i
  for (( i = 0; i < timeout_loops; i++ )); do
    if "$@"; then
      return 0
    fi
    sleep 0.05
  done
  return 1
}

test_e2e_multi_client_broadcast_and_sibling_autoclose() {
  command -v tmux >/dev/null 2>&1 || return 0

  local dir="${TEST_ROOT}/e2e"
  mkdir -p "${dir}/runtime"
  local inner_sock="${dir}/inner.sock"
  local outer1_sock="${dir}/outer1.sock"
  local outer2_sock="${dir}/outer2.sock"
  local outer_sc_sock="${dir}/outer_sc.sock"

  cleanup_e2e_tmux() {
    local s
    for s in "${outer1_sock}" "${outer2_sock}" "${outer_sc_sock}" "${inner_sock}"; do
      tmux -S "${s}" kill-server 2>/dev/null || true
      rm -f "${s}" 2>/dev/null || true
    done
  }

  # Start inner tmux server with 'main' and 'scratch' sessions.
  TMUX='' tmux -S "${inner_sock}" -f /dev/null new-session -d -s main -x 200 -y 50
  TMUX='' tmux -S "${inner_sock}" -f /dev/null new-session -d -s scratch -x 80 -y 20

  # Attach 2 outer clients to 'main' (200x50 and 120x30) and 1 to 'scratch' (80x20).
  TMUX='' tmux -S "${outer1_sock}" -f /dev/null new-session -d -s host1 -x 200 -y 50 \
    "TMUX='' tmux -S '${inner_sock}' attach-session -t main"
  TMUX='' tmux -S "${outer2_sock}" -f /dev/null new-session -d -s host2 -x 120 -y 30 \
    "TMUX='' tmux -S '${inner_sock}' attach-session -t main"
  TMUX='' tmux -S "${outer_sc_sock}" -f /dev/null new-session -d -s host_sc -x 80 -y 20 \
    "TMUX='' tmux -S '${inner_sock}' attach-session -t scratch"

  wait_until 60 bash -c \
    "[[ \$(tmux -S '${inner_sock}' list-clients 2>/dev/null | wc -l) -eq 3 ]]"

  local inner_pid
  inner_pid="$(tmux -S "${inner_sock}" display-message -p '#{pid}')"
  local inner_tmux_env="${inner_sock},${inner_pid},0"

  local e2e_state_dir
  e2e_state_dir="$(XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    bash -c 'source "${HOME}/lib/os_compat.sh" && printf "%s/tmux-notify" "$(os::runtime_dir)"')"

  # 1. Overflow pager broadcast + synchronized sibling auto-close on 'q'.
  TMUX_NOTIFY_TIMEOUT=4 XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    TMUX="${inner_tmux_env}" \
    bash "${TMUX_NOTIFY}" "E2EPager" "$(seq -f '## Line %g' 1 60)" &
  local notify_pid=$!

  wait_until 80 bash -c \
    "tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'E2EPager' && \
     tmux -S '${outer2_sock}' capture-pane -p -t host2 | grep -Fq 'E2EPager'"
  assert_eq "0" "$?" "both outer clients render E2EPager popup"

  local scratch_cap
  scratch_cap="$(tmux -S "${outer_sc_sock}" capture-pane -p -t host_sc)"
  assert_true "scratch popup session client excluded from broadcast" \
    test "${scratch_cap#*E2EPager}" == "${scratch_cap}"

  # Dismiss on outer1 via 'q'; both outer1 and outer2 must close and clean grp_dir.
  tmux -S "${outer1_sock}" send-keys -t host1 q
  wait "${notify_pid}" 2>/dev/null || true
  wait_until 80 bash -c \
    "! tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'E2EPager' && \
     ! tmux -S '${outer2_sock}' capture-pane -p -t host2 | grep -Fq 'E2EPager'"
  assert_eq "0" "$?" "dismissing Client 1 auto-closes Client 2 pager popup"

  wait_until 40 bash -c \
    "[[ -z \"\$(ls -A '${e2e_state_dir}' 2>/dev/null)\" ]]"
  assert_eq "0" "$?" "popup group directory deleted after all slots finish"

  # 2. Client detach (HUP) isolation: detaching Client 2 keeps Client 1 open.
  TMUX_NOTIFY_TIMEOUT=4 XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    TMUX="${inner_tmux_env}" \
    bash "${TMUX_NOTIFY}" "HupTest" "Stay open on client 1" &
  notify_pid=$!
  wait_until 80 bash -c \
    "tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'HupTest' && \
     tmux -S '${outer2_sock}' capture-pane -p -t host2 | grep -Fq 'HupTest'"

  tmux -S "${outer2_sock}" kill-server 2>/dev/null || true
  sleep 0.25
  assert_true "Client 1 popup remains open after Client 2 disconnects (HUP)" \
    bash -c "tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'HupTest'"

  tmux -S "${outer1_sock}" send-keys -t host1 Enter
  wait "${notify_pid}" 2>/dev/null || true
  wait_until 60 bash -c \
    "[[ -z \"\$(ls -A '${e2e_state_dir}' 2>/dev/null)\" ]]"
  assert_eq "0" "$?" "group directory cleaned after remaining client dismisses"

  # 3. TIMER_PID enforces TMUX_NOTIFY_TIMEOUT in pager mode after outer timeout.
  TMUX_NOTIFY_TIMEOUT=2 XDG_RUNTIME_DIR="${dir}/runtime" TMPDIR="${dir}/runtime" \
    TMUX="${inner_tmux_env}" \
    timeout 1s bash "${TMUX_NOTIFY}" "TimerTest" "$(seq -f 'row %g' 1 60)" \
    2>/dev/null || true
  assert_true "TimerTest popup still open right after 1s outer caller timeout" \
    bash -c "tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'TimerTest'"
  wait_until 80 bash -c \
    "! tmux -S '${outer1_sock}' capture-pane -p -t host1 | grep -Fq 'TimerTest' && \
     [[ -z \"\$(ls -A '${e2e_state_dir}' 2>/dev/null)\" ]]"
  assert_eq "0" "$?" "TIMER_PID closes pager popup at 2s and removes grp_dir"

  cleanup_e2e_tmux
}

main() {
  local fn
  for fn in $(declare -F | awk '{print $3}' | grep '^test_'); do
    "${fn}"
  done
  echo "tmux-notify tests: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
  (( FAIL_COUNT == 0 ))
}

main "$@"
