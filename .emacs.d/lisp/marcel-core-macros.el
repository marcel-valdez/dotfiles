;;; marcel-core-macros.el --- Summary
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

(defvar copy-to-remote-clipboard-exe-str)

(setq copy-to-remote-clipboard-exe-str "~/bin/copy-to-remote-clipboard")

(defun copy-to-remote-clipboard(&optional beg end)
  "Send contents of region bounded by BEG and END to the remote clipboard.
The command used to copy to the remote clipboard is the script defined by the
variable copy-to-remote-clipboard-exe-str"

  (interactive "r")
  (let ((return-code
        (shell-command-on-region beg end copy-to-remote-clipboard-exe-str nil nil "*Errors*" t)))
    (if
        (eq return-code 0)
        (display-message-or-buffer "Copied to remote clipboard"))))


(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(provide 'marcel-core-macros)
;;; marcel-core-macros.el ends here
