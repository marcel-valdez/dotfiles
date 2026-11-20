#!/usr/bin/env bash
# ==============================================================================
# Regression tests: an unreadable directory entry must not break file watches
# ==============================================================================
# On macOS Emacs watches files with kqueue, and `file-notify-add-watch` on a
# directory reads the attributes of every entry in it. A single entry it
# cannot stat -- e.g. ~/.gcloud_home after its sshfs server died ("Device not
# configured") -- made the call fail with a bare file-error instead of the
# documented file-notify-error. helm-find-files only expects
# file-notify-error from its directory watcher, so C-x C-f in ~ showed
#   In 'Find Files' source: 'helm-find-files-get-candidates'
#    (file-error "Getting attributes" "Device not configured" ".gcloud_home")
# instead of the listing. ~/.emacs.d/lisp/file-notify-resilient.el re-signals
# such errors as file-notify-error (a file-error subtype), so callers log a
# message and carry on without a watcher. Checks, in a batch Emacs with the
# installed packages (helm checks are skipped if helm is not installed):
#   1. watching a directory whose entries cannot be stat'ed (mode 0644)
#      succeeds or raises file-notify-error -- never a bare file-error
#   2. handlers for file-error still catch it
#   3. helm-ff-directory-files lists that directory and logs a message
#   4. helm-find-files-get-candidates (the C-x C-f source) lists it
#   5. simulated dead mount (the watch backend fails with "Device not
#      configured" on .gcloud_home): the listing still comes back, with a
#      logged message naming the error
#   6. a healthy directory is still watched (valid descriptor)
#   7. filenotify's own errors (missing directory) pass through unchanged
#   8. ~/.emacs loads the module
#
# Usage Example:
#   bash ~/scripts/tests/test_emacs_file_notify.sh
#   EMACS=/path/to/emacs EMACS_D=/path/to/.emacs.d \
#     bash ~/scripts/tests/test_emacs_file_notify.sh
# ==============================================================================

set -uo pipefail

if [[ -z "${EMACS:-}" ]]; then
  EMACS="$(command -v emacs || true)"
fi
EMACS_D="${EMACS_D:-${HOME}/.emacs.d}"
INIT_FILE="${INIT_FILE:-${HOME}/.emacs}"
MODULE="${EMACS_D}/lisp/file-notify-resilient.el"
PASS_COUNT=0
FAIL_COUNT=0

if [[ -z "${EMACS}" || ! -x "${EMACS}" ]]; then
  echo "SKIP: no Emacs found"
  exit 0
fi

ERR_FILE="$(mktemp "${TMPDIR:-/tmp}/test_emacs_file_notify_XXXXXX")"
trap 'rm -f "${ERR_FILE}"' EXIT

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

# assert_in "a|b" ACTUAL MSG : ACTUAL must be one of the |-separated values.
assert_in() {
  local allowed="$1"
  local actual="$2"
  local msg="$3"
  if [[ "|${allowed}|" == *"|${actual}|"* ]]; then
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "FAIL: ${msg} (expected one of '${allowed}', actual='${actual}')" >&2
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
}

# All checks run in one batch Emacs; each prints "key=value" on stdout, and
# batch-mode `message' output (what helm logs) goes to stderr (ERR_FILE). The
# module is loaded with NOERROR so that, without it, the checks show stock
# behavior.
OUT="$("${EMACS}" -Q --batch \
  --eval "(load \"${EMACS_D}/early-init.el\" t t)" \
  --eval '(progn (require (quote package)) (package-activate-all))' \
  --eval "(load \"${MODULE}\" t t)" --eval '
(progn
  (require (quote filenotify))
  (defun t--res (key thunk)
    (princ (format "%s=%s\n" key
                   (condition-case err (funcall thunk)
                     (error (format "signal:%s" (car err)))))))
  (defun t--names (cands)
    (mapconcat (function identity)
               (sort (delete "" (delete "." (delete ".."
                       (mapcar (lambda (c)
                                 (file-name-nondirectory
                                  (directory-file-name (if (consp c) (cdr c) c))))
                               cands))))
                     (function string<))
               ","))
  (let* ((root (file-name-as-directory (make-temp-file "test-file-notify-" t)))
         (locked (file-name-as-directory (expand-file-name "locked" root)))
         (mounts (file-name-as-directory (expand-file-name "home" root)))
         (plain (file-name-as-directory (expand-file-name "plain" root)))
         (helm-p (require (quote helm-files) nil t)))
    (make-directory locked) (make-directory mounts) (make-directory plain)
    (write-region "" nil (expand-file-name "a.txt" locked))
    (make-directory (expand-file-name "sub" locked))
    (write-region "" nil (expand-file-name "b.txt" mounts))
    (make-directory (expand-file-name ".gcloud_home" mounts))
    (set-file-modes locked #o644)     ; readable, but entries cannot be stat-ed
    (unwind-protect
        (progn
          (t--res "watch_locked"
            (lambda ()
              (condition-case nil
                  (progn (file-notify-rm-watch
                          (file-notify-add-watch locked (quote (change)) (function ignore)))
                         "ok")
                (file-notify-error "file-notify-error"))))
          (t--res "file_error_handler"
            (lambda ()
              (condition-case nil
                  (progn (file-notify-rm-watch
                          (file-notify-add-watch locked (quote (change)) (function ignore)))
                         "ok")
                (file-error "caught"))))
          (if (not helm-p)
              (princ "helm=missing\n")
            (t--res "helm_list_locked"
              (lambda () (t--names (helm-ff-directory-files locked t))))
            (t--res "helm_ffgc_locked"
              (lambda () (let ((helm-pattern locked))
                           (t--names (helm-find-files-get-candidates)))))
            (let ((dead (lambda (orig file &rest args)
                          (if (file-exists-p (expand-file-name ".gcloud_home" file))
                              (signal (quote file-error)
                                      (list "Getting attributes"
                                            "Device not configured" ".gcloud_home"))
                            (apply orig file args))))
                  (backends (quote (file-notify--add-watch-kqueue
                                    file-notify--add-watch-inotify
                                    file-notify--add-watch-gfilenotify
                                    file-notify--add-watch-w32notify))))
              (dolist (fn backends) (when (fboundp fn) (advice-add fn :around dead)))
              (unwind-protect
                  (t--res "helm_list_dead_mount"
                    (lambda () (t--names (helm-ff-directory-files mounts t))))
                (dolist (fn backends) (when (fboundp fn) (advice-remove fn dead))))))
          (t--res "watch_plain"
            (lambda ()
              (let ((desc (file-notify-add-watch plain (quote (change)) (function ignore))))
                (prog1 (if (file-notify-valid-p desc) "valid" "invalid")
                  (file-notify-rm-watch desc)))))
          (t--res "watch_missing"
            (lambda ()
              (condition-case err
                  (progn (file-notify-add-watch (expand-file-name "nope/x" root)
                                                (quote (change)) (function ignore))
                         "no-error")
                (file-notify-error (format "file-notify-error:%s" (cadr err)))))))
      (set-file-modes locked #o755)
      (delete-directory root t))))' 2>"${ERR_FILE}")"

value() { sed -n "s/^$1=//p" <<< "${OUT}" | head -1; }
logged() { grep -qE "$1" "${ERR_FILE}" && echo yes || echo no; }

# --- 1-2. Watching a directory whose entries cannot be stat-ed ---------------
assert_in "ok|file-notify-error" "$(value watch_locked)" \
  "file-notify-add-watch on a directory with unreadable entries"
assert_in "ok|caught" "$(value file_error_handler)" \
  "file-error handlers still catch failed watches"

# --- 3-5. helm keeps listing ----------------------------------------------------
if [[ "$(value helm)" == "missing" ]]; then
  echo "SKIP: helm not installed; helm checks skipped"
else
  assert_eq "a.txt,sub" "$(value helm_list_locked)" \
    "helm-ff-directory-files lists a directory with unreadable entries"
  if [[ "$(value watch_locked)" == "file-notify-error" ]]; then
    assert_eq "yes" "$(logged 'file-notify-error.*Permission denied')" \
      "the failed watch is logged as a message"
  fi
  assert_eq "a.txt,sub" "$(value helm_ffgc_locked)" \
    "helm-find-files-get-candidates (C-x C-f) lists it"
  assert_eq ".gcloud_home,b.txt" "$(value helm_list_dead_mount)" \
    "C-x C-f listing survives a dead mount (Device not configured)"
  assert_eq "yes" "$(logged 'file-notify-error.*Device not configured.*\.gcloud_home')" \
    "the dead mount is logged as a message"
fi

# --- 6-7. Healthy watches and filenotify's own errors are unchanged ----------
assert_eq "valid" "$(value watch_plain)" "a healthy directory is still watched"
assert_eq "file-notify-error:Directory does not exist" "$(value watch_missing)" \
  "filenotify's own errors pass through unchanged"

# --- 8. The init file loads the module ----------------------------------------
if grep -qE "^[[:space:]]*\(require 'file-notify-resilient" "${INIT_FILE}"; then
  actual="yes"
else
  actual="no"
fi
assert_eq "yes" "${actual}" "${INIT_FILE} requires file-notify-resilient"

echo "test_emacs_file_notify: ${PASS_COUNT} passed, ${FAIL_COUNT} failed"
[[ "${FAIL_COUNT}" -eq 0 ]]
