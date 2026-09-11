;;; marcel-core-macros.el --- Summary  -*- lexical-binding: t; -*-
;;; This file contains completely package independent core macros for my .emacs config

;;; Commentary:
;;; Use this function to load packages safely.

;;; Code:
(defmacro with-library (symbol &rest body)
  "If it can successfully 'require the given SYMBOL, then execute the BODY.
If the SYMBOL cannot be 'require'd, then the BODY is not executed and no error
is raised.
If the BODY fails during execution, the error is allowed to bubble up, it is not caught."
  `(when
       (condition-case err
           (progn (require ',symbol) t)
         (error
          (message
           (format "Error while requiring package %s\n%s" ',symbol err))
          nil))
     ,@body))

(put 'with-library 'lisp-indent-function 1)

(defvar async-clipboard-copy-exe-str
  (expand-file-name "~/.googlerc.d/scripts/async_clipboard_copy.sh")
  "Path to the unified asynchronous multi-target clipboard copy script.")

(defun async-copy-to-all-clipboards (&optional beg end)
  "Asynchronously copy active region or latest kill-ring text to all clipboards.
Dispatches to tmux buffer, local desktop clipboard, and remote reverse SSH
tunnel via `async_clipboard_copy.sh`. Fully non-blocking via `make-process`."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((text (cond
                ((and beg end)
                 (buffer-substring-no-properties beg end))
                ((use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((and kill-ring (car kill-ring))
                 (current-kill 0 t))
                (t nil))))
    (cond
     ((or (null text) (string-empty-p text))
      (message "Nothing to copy (no active region or kill-ring entry)."))
     ((not (file-executable-p async-clipboard-copy-exe-str))
      (message "Clipboard copy script '%s' is not executable."
               async-clipboard-copy-exe-str))
     (t
      (condition-case err
          (let* ((char-count (length text))
                 (line-count (length (split-string text "\n" nil)))
                 (proc (make-process
                        :name "async-clipboard-copy"
                        :buffer nil
                        :command (list async-clipboard-copy-exe-str "--sync" "--notify-none")
                        :connection-type 'pipe
                        :sentinel
                        (lambda (_proc event)
                          (if (string-prefix-p "finished" event)
                              (message "Copied %d chars (%d lines) to all clipboards (tmux, desktop, remote)."
                                       char-count line-count)
                            (message "Clipboard copy failed: %s"
                                     (string-trim event)))))))
            (process-send-string proc text)
            (process-send-eof proc))
        (error
         (message "Clipboard copy process error: %s" (error-message-string err))))))))

;; Backward compatibility aliases
(defalias 'copy-to-remote-clipboard 'async-copy-to-all-clipboards)
(defalias 'copy-to-tmux 'async-copy-to-all-clipboards)



(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(provide 'marcel-core-macros)
;;; marcel-core-macros.el ends here
