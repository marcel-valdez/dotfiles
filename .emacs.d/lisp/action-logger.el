;;; action-logger.el --- Log Emacs actions -*- lexical-binding: t; -*-

;;; Commentary:
;; This package logs commands to a file. It excludes commands
;; listed in action-logger-ignore-commands.

;;; Code:

(defgroup action-logger nil
  "Logging Emacs actions."
  :group 'tools)

(defcustom action-logger-log-file "~/.emacs_action_log"
  "The file to which actions will be logged."
  :type 'string
  :group 'action-logger)

(defcustom action-logger-ignore-commands
  '(self-insert-command
    backward-char
    backward-kill-word
    backward-list
    backward-word
    beginning-of-buffer
    beginning-of-line
    end-of-buffer
    end-of-line
    exit-isearch
    forward-char
    forward-list
    forward-word
    helm-M-x
    helm-confirm-and-exit-minibuffer
    helm-execute-persistent-action
    helm-keyboard-quit
    helm-next-line
    helm-previous-line
    ignore
    indent-for-tab-command
    isearch-backward
    isearch-forward
    isearch-repeat-backward
    isearch-repeat-forward
    keyboard-quit
    left-char
    mouse-drag-region
    mouse-set-point
    move-beginning-of-line
    move-end-of-line
    mwheel-scroll
    newline
    next-line
    page-down
    page-up
    previous-line
    recenter-top-bottom
    redo
    right-char
    scroll-down-command
    scroll-up-command
    set-mark-command
    split-window-right
    undo
    yank
    )
  "A list of commands to ignore and not log."
  :type '(repeat symbol)
  :group 'action-logger)

(defun action-logger--format-timestamp ()
  "Return the current time as a string."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun action-logger--log-action ()
  "Log the current command if it's a named command and not ignored."
  (let ((cmd this-command))
    (when (and cmd
               (symbolp cmd)
               (commandp cmd)
               (not (memq cmd action-logger-ignore-commands)))
      (with-temp-buffer
        (insert (action-logger--format-timestamp) " " (symbol-name cmd) "\n")
        (let ((inhibit-message t))
          (append-to-file (point-min) (point-max) action-logger-log-file))))))

(defun action-logger-start ()
  "Start logging actions, ensuring the hook is only added once."
  (interactive)
  (if (member #'action-logger--log-action pre-command-hook)
        (message "Action logger already running.")
    (add-hook 'pre-command-hook #'action-logger--log-action)
      (message "Action logger started. Logging to %s" action-logger-log-file)))

(defun action-logger-stop ()
  "Stop logging actions."
  (interactive)
  (remove-hook 'pre-command-hook #'action-logger--log-action)
  (let ((inhibit-message t))
    (message "Action logger stopped.")))

(provide 'action-logger)
;;; action-logger.el ends here
