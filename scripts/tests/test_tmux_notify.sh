#!/usr/bin/env bash
# ==============================================================================
# Unit Test Suite for ~/bin/tmux-notify popup layout
# ==============================================================================
# Exercises the layout engine through `tmux-notify --print-layout`, which
# prints "<popup_w> <popup_h>" followed by the rendered body, so no tmux
# client is needed.
#
# Usage Example:
#   bash ~/scripts/tests/test_tmux_notify.sh
# ==============================================================================

set -uo pipefail

TMUX_NOTIFY="${TMUX_NOTIFY:-${HOME}/bin/tmux-notify}"
PASS_COUNT=0
FAIL_COUNT=0

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

# Runs the layout engine. Sets LAYOUT_W, LAYOUT_H and the BODY array.
layout() {
  local size="$1"
  shift
  local out
  out="$(env -u TMUX_NOTIFY_MAX_COLS bash "${TMUX_NOTIFY}" --print-layout \
    --client-size "${size}" "$@")"
  read -r LAYOUT_W LAYOUT_H <<< "$(head -n 1 <<< "${out}")"
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
  assert_eq " Done." "${BODY[0]}" "body row has 1 column left padding"
}

test_long_single_line_wraps_to_80_cols() {
  local msg
  msg="Jetski Run Completed: Workspace [jetski-cli-notify]: This is a"
  msg+=" deliberately long single-line notification message that should"
  msg+=" clearly exceed the width of the popup box END."
  layout 192x50 "Knock" "${msg}"
  assert_eq "80" "${LAYOUT_W}" "long message caps popup at 80 columns"
  assert_eq "$(( ${#BODY[@]} + 2 ))" "${LAYOUT_H}" \
    "height counts every wrapped row plus border"
  assert_true "long message wraps to several rows" test "${#BODY[@]}" -ge 3
  assert_rows_fit "long message rows fit the interior"
  assert_eq "${msg}" "$(joined_words)" "wrapping keeps every word in order"
}

test_first_line_is_first_row() {
  layout 192x50 "Three Lines" "$(printf 'line one\nline two\nline three')"
  assert_eq " line one" "${BODY[0]}" "first logical line is the first row"
  assert_eq "5" "${LAYOUT_H}" "three rows plus 2 border rows"
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

test_overflow_truncates_at_end() {
  layout 80x20 "Overflow" "$(seq -f 'row %g' 1 60)"
  assert_eq "16" "${LAYOUT_H}" "height capped at client height minus margin"
  assert_eq " row 1" "${BODY[0]}" "first row stays visible on overflow"
  assert_eq " ... (+47 more lines)" "${BODY[-1]}" "last row is overflow marker"
}

test_exact_fit_has_no_marker() {
  layout 80x20 "Fit" "$(seq -f 'row %g' 1 14)"
  assert_eq "16" "${LAYOUT_H}" "14 rows fit exactly"
  assert_eq " row 14" "${BODY[-1]}" "no marker when everything fits"
}

test_narrow_client_shrinks_popup() {
  layout 60x20 "Knock" "$(printf 'word %.0s' {1..40})"
  assert_eq "56" "${LAYOUT_W}" "popup width is client width minus margin"
  assert_rows_fit "rows fit a narrow popup"
}

test_tiny_client_clamps_to_min_width() {
  layout 20x10 "Knock" "$(printf 'word %.0s' {1..20})"
  assert_eq "30" "${LAYOUT_W}" "popup never narrower than 30 columns"
  assert_rows_fit "rows fit the minimum-width popup"
}

test_long_word_is_hard_broken() {
  layout 192x50 "Path" "$(printf 'x%.0s' {1..200})"
  assert_rows_fit "unbreakable word is split across rows"
  assert_eq "5" "${LAYOUT_H}" "200 chars at 76 per row is 3 rows"
}

test_multibyte_wraps_by_characters() {
  local msg
  msg="$(printf 'á→ %.0s' {1..60})"
  layout 192x50 "UTF-8" "${msg}"
  assert_rows_fit "multibyte rows fit the interior by character count"
  assert_true "multibyte rows are not split short" test "${#BODY[0]}" -ge 70
}

test_multibyte_under_c_locale() {
  local out
  out="$(LANG=C LC_ALL='' bash "${TMUX_NOTIFY}" --print-layout \
    --client-size 192x50 "UTF-8" "$(printf 'á→ %.0s' {1..60})" | sed -n 2p)"
  assert_true "C locale still counts characters" test "${#out}" -ge 70
}

test_title_only_defaults_title() {
  layout 192x50 "Just a message"
  assert_eq " Just a message" "${BODY[0]}" "single arg becomes the message"
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

main() {
  local fn
  for fn in $(declare -F | awk '{print $3}' | grep '^test_'); do
    "${fn}"
  done
  echo "tmux-notify layout tests: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
  (( FAIL_COUNT == 0 ))
}

main "$@"
