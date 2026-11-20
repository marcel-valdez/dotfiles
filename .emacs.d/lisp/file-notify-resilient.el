;;; file-notify-resilient.el --- A failed file watch must not break its caller -*- lexical-binding: t; -*-

;;; Commentary:

;; `file-notify-add-watch' is documented to signal `file-notify-error' when
;; a file cannot be watched, and callers rely on that: helm-find-files, for
;; one, catches it, logs a message and lists the directory without a
;; watcher.  But on macOS the kqueue backend reads the attributes of every
;; entry of a watched directory, and one entry it cannot stat -- such as
;; ~/.gcloud_home after its sshfs server died ("Device not configured") --
;; makes the whole call fail with a bare `file-error'.  That escaped helm,
;; so C-x C-f in ~ showed
;;   In `Find Files' source: `helm-find-files-get-candidates'
;;    (file-error "Getting attributes" "Device not configured" ".gcloud_home")
;; instead of the listing.
;;
;; Re-signal such errors as `file-notify-error', as documented.  It is a
;; subtype of `file-error', so handlers for `file-error' or `error' still
;; match.  Watches that work, and filenotify's own errors, are unchanged.
;;
;; Test: ~/scripts/tests/test_emacs_file_notify.sh

;;; Code:

(defun file-notify-resilient--add-watch (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, re-signaling a bare `file-error' as documented.
`file-notify-add-watch' signals `file-notify-error' when a file cannot be
watched; backends must not leak other `file-error's to its callers."
  (condition-case err
      (apply orig-fun args)
    (file-notify-error (signal (car err) (cdr err)))
    (file-error (signal 'file-notify-error (cdr err)))))

(with-eval-after-load 'filenotify
  (advice-add 'file-notify-add-watch :around
              #'file-notify-resilient--add-watch))

(provide 'file-notify-resilient)
;;; file-notify-resilient.el ends here
