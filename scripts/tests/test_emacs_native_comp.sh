#!/usr/bin/env bash
# ==============================================================================
# Regression tests: Emacs native compilation keeps working on macOS 26+
# ==============================================================================
# The libgccjit bundled with Emacs.app (GCC 14.3 in the emacsformacosx.com
# build) runs an embedded GCC driver that guesses the default
# -mmacosx-version-min from the Darwin kernel version as (Darwin major - 9).
# That held for macOS 11-15, but Apple then jumped to macOS 26: on macOS 27
# (Darwin 27) the driver hands "-mmacosx-version-min=18.0" to the assembler,
# Apple clang (Xcode 26+) rejects 16-25 as "invalid version number", and every
# native compilation fails with "libgccjit.so: error: error invoking gcc
# driver". ~/.emacs.d/early-init.el loads ~/.emacs.d/lisp/native-comp-darwin.el,
# which passes the real macOS version in `native-comp-driver-options`.
# Checks:
#   1. the module appends -mmacosx-version-min=<macOS major>.0 (macOS 26+
#      only) and keeps Emacs' default driver options
#   2. the variable is already bound before comp.el loads, so async JIT
#      workers (spawned by comp-run.el without comp.el) inherit it
#   3. loading it twice still adds the flag once
#   4. a -mmacosx-version-min the user already set is left alone
#   5. a synchronous native-compile produces a loadable .eln with native code
#   6. startup path: an Emacs daemon whose throwaway HOME holds only
#      early-init.el + the module JIT-compiles a byte-compiled package in an
#      async worker, swaps in the native code, and logs no native compilation
#      errors
#
# Usage Example:
#   bash ~/scripts/tests/test_emacs_native_comp.sh
#   EMACS=/path/to/Emacs EMACS_D=/path/to/.emacs.d \
#     bash ~/scripts/tests/test_emacs_native_comp.sh
# ==============================================================================

set -uo pipefail

EMACS="${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}"
EMACSCLIENT="${EMACSCLIENT:-$(dirname "${EMACS}")/bin/emacsclient}"
EMACS_D="${EMACS_D:-${HOME}/.emacs.d}"
MODULE="${EMACS_D}/lisp/native-comp-darwin.el"
EARLY_INIT="${EMACS_D}/early-init.el"
PASS_COUNT=0
FAIL_COUNT=0

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "SKIP: macOS-only test"
  exit 0
fi
if [[ ! -x "${EMACS}" ]]; then
  echo "SKIP: no Emacs at ${EMACS}"
  exit 0
fi
if ! "${EMACS}" -Q --batch --eval \
    '(kill-emacs (if (and (fboundp (quote native-comp-available-p))
                          (native-comp-available-p)) 0 1))' 2>/dev/null; then
  echo "SKIP: ${EMACS} has no native compilation"
  exit 0
fi

# No "//" in paths: Emacs' substitute-in-file-name (used by `load`) reads
# ".../T//x" as "/x", and macOS' TMPDIR ends with a slash.
TMP_BASE="${TMPDIR:-/tmp}"
TEST_ROOT="$(mktemp -d "${TMP_BASE%/}/test_emacs_native_comp_XXXXXX")"
DAEMON_NAME="test-native-comp-$$"
cleanup_suite() {
  "${EMACSCLIENT}" -s "${DAEMON_NAME}" --eval '(kill-emacs)' >/dev/null 2>&1 || true
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

# Show the most telling error line of the last batch run.
show_last_error() {
  local err="${TEST_ROOT}/batch.err" line
  line="$(grep -m1 -oE "clang: error[^\"\\]*" "${err}" 2>/dev/null \
    || grep -m1 -E 'error invoking|[Ee]rror' "${err}" 2>/dev/null)"
  [[ -n "${line}" ]] && echo "      ${line}" >&2
}

# Elisp that prints `native-comp-driver-options` space-separated.
PRINT_OPTS='(progn (require (quote comp))
  (princ (mapconcat (function identity) native-comp-driver-options " ")))'

# emacs_batch ARGS... : run `emacs -Q --batch ARGS`, stdout only.
emacs_batch() {
  "${EMACS}" -Q --batch "$@" 2>"${TEST_ROOT}/batch.err"
}

PROBE_EL="${TEST_ROOT}/src/nc-probe.el"
mkdir -p "${TEST_ROOT}/src" "${TEST_ROOT}/sync"
cat > "${PROBE_EL}" <<'EOF'
;;; nc-probe.el --- native-comp probe -*- lexical-binding: t -*-
(defun nc-probe-add (a b) (+ a b))
(provide 'nc-probe)
;;; nc-probe.el ends here
EOF

MACOS_VERSION="$(sw_vers -productVersion)"
MACOS_MAJOR="${MACOS_VERSION%%.*}"
DEFAULT_OPTS="$(emacs_batch --eval "${PRINT_OPTS}")"
if (( MACOS_MAJOR > 15 )); then
  EXPECTED_OPTS="${DEFAULT_OPTS:+${DEFAULT_OPTS} }-mmacosx-version-min=${MACOS_MAJOR}.0"
else
  EXPECTED_OPTS="${DEFAULT_OPTS}"
fi

# Informational: is the toolchain bug still there without the workaround?
if emacs_batch --eval "(native-compile \"${PROBE_EL}\" \"${TEST_ROOT}/plain.eln\")" \
    >/dev/null && [[ -f "${TEST_ROOT}/plain.eln" ]]; then
  echo "NOTE: plain 'emacs -Q' native-compile works here (macOS ${MACOS_VERSION})."
else
  echo "NOTE: plain 'emacs -Q' native-compile fails here (macOS ${MACOS_VERSION}):"
  show_last_error 2>&1
fi

# --- 1. The module adds the real macOS version, keeping Emacs' defaults ------
actual="$(emacs_batch -l "${MODULE}" --eval "${PRINT_OPTS}")"
assert_eq "${EXPECTED_OPTS}" "${actual}" \
  "native-comp-driver-options with native-comp-darwin.el"

# --- 2. ...and it is bound before comp.el loads ------------------------------
# Async JIT workers are spawned by comp-run.el, which does not load comp.el,
# and only inherit native-comp-driver-options if it is bound at that point.
if (( MACOS_MAJOR > 15 )); then expected="${EXPECTED_OPTS}"; else expected="<unbound>"; fi
actual="$(emacs_batch -l "${MODULE}" --eval '(progn (require (quote comp-run))
  (princ (if (boundp (quote native-comp-driver-options))
             (mapconcat (function identity) native-comp-driver-options " ")
           "<unbound>")))')"
assert_eq "${expected}" "${actual}" \
  "native-comp-driver-options as inherited by async workers (comp.el not loaded)"

# --- 3. Loading the module twice adds the flag once --------------------------
actual="$(emacs_batch -l "${MODULE}" --eval "(require (quote comp))" \
  -l "${MODULE}" --eval "${PRINT_OPTS}")"
assert_eq "${EXPECTED_OPTS}" "${actual}" "loading native-comp-darwin.el twice"

# --- 4. A user-supplied -mmacosx-version-min wins ----------------------------
actual="$(emacs_batch \
  --eval '(setq native-comp-driver-options (list "-Wl,-w" "-mmacosx-version-min=15.0"))' \
  -l "${MODULE}" --eval "${PRINT_OPTS}")"
assert_eq "-Wl,-w -mmacosx-version-min=15.0" "${actual}" \
  "user-supplied -mmacosx-version-min is left alone"

# --- 5. Synchronous native-compile works -------------------------------------
actual="$(emacs_batch -l "${MODULE}" --eval "
(progn
  (require 'comp)
  (native-compile \"${PROBE_EL}\" \"${TEST_ROOT}/sync/nc-probe.eln\")
  (load \"${TEST_ROOT}/sync/nc-probe.eln\" nil t)
  (princ (format \"%S %S\"
                 (native-comp-function-p (symbol-function 'nc-probe-add))
                 (nc-probe-add 2 3))))")"
assert_eq "t 5" "${actual}" "native-compile yields a loadable native .eln"
[[ "${actual}" == "t 5" ]] || show_last_error

# --- 6. Startup: early-init.el + async JIT compilation in a daemon -----------
FAKE_HOME="${TEST_ROOT}/home"
mkdir -p "${FAKE_HOME}/.emacs.d/lisp" "${FAKE_HOME}/pkg"
cp "${EARLY_INIT}" "${FAKE_HOME}/.emacs.d/early-init.el" 2>/dev/null
cp "${MODULE}" "${FAKE_HOME}/.emacs.d/lisp/" 2>/dev/null
cp "${PROBE_EL}" "${FAKE_HOME}/pkg/nc-probe.el"
# Loading a .elc that has no .eln yet is what triggers JIT native compilation.
"${EMACS}" -Q --batch -f batch-byte-compile "${FAKE_HOME}/pkg/nc-probe.el" \
  >/dev/null 2>&1
cat > "${FAKE_HOME}/.emacs.d/init.el" <<EOF
(add-to-list 'load-path "${FAKE_HOME}/pkg")
(require 'nc-probe)
EOF

HOME="${FAKE_HOME}" "${EMACS}" --daemon="${DAEMON_NAME}" \
  >"${TEST_ROOT}/daemon.log" 2>&1
client() { "${EMACSCLIENT}" -s "${DAEMON_NAME}" --eval "$1" 2>/dev/null; }

# A fresh eln-cache also JIT-compiles the built-in libraries loaded at startup
# (package-activate, ...), so allow up to 3 minutes.
state=""
for _ in $(seq 1 720); do
  state="$(client '(if (and (featurep (quote comp-run))
                            (or comp-files-queue (> (comp--async-runnings) 0)))
                       "busy" "idle")')"
  [[ "${state}" == '"idle"' ]] && break
  sleep 0.25
done
assert_eq '"idle"' "${state}" "daemon finished its async native compilations"

eln_count="$(find "${FAKE_HOME}/.emacs.d/eln-cache" -name 'nc-probe-*.eln' \
  2>/dev/null | wc -l | tr -d ' ')"
assert_eq "1" "${eln_count}" "startup JIT compile wrote nc-probe-*.eln"

actual="$(client "(native-comp-function-p (symbol-function 'nc-probe-add))")"
assert_eq "t" "${actual}" "daemon swapped nc-probe-add to native code"

# Compile failures only: package lint such as "the function ‘x’ is not known
# to be defined" is not what this test is about.
NC_ERROR_RE='(native-compiler): \\(clang: error\\|libgccjit\\.so: error\\|Error: \\|Internal native compiler error\\)'
actual="$(client "(if (get-buffer \"*Warnings*\")
                     (with-current-buffer \"*Warnings*\"
                       (count-matches \"${NC_ERROR_RE}\" (point-min) (point-max)))
                   0)")"
assert_eq "0" "${actual}" "no native compilation errors during startup"
if [[ "${actual}" != "0" ]]; then
  client "(with-current-buffer \"*Warnings*\"
            (goto-char (point-min))
            (re-search-forward \"^.*${NC_ERROR_RE}.*$\" nil t)
            (substring-no-properties (match-string 0)))" \
    | cut -c1-240 | sed 's/^/      /' >&2
fi

echo "test_emacs_native_comp: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
[[ "${FAIL_COUNT}" -eq 0 ]]
