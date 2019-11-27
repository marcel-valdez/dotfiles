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

(defvar run-on-save-cmd "")

(defun run-on-save ()
  "Execute the file in the current buffer.
If variable run-on-save-cmd is set for the current buffer, then it runs that
command and psses the saved buffer filename as the sole parameter."
  (when (bound-and-true-p run-on-save-mode)
    (display-message-or-buffer
     (if (string= run-on-save-cmd "")
         (shell-command-to-string buffer-file-name)
       (shell-command-to-string concat(run-on-save-cmd buffer-file-name))))))

(define-minor-mode run-on-save-mode
  "Execute a script every time a file is saved."
  :lighter "run-on-save"
  (make-local-variable 'run-on-save-cmd))

(add-hook 'after-save-hook #'run-on-save)

(provide 'marcel-core-macros)
;;; marcel-core-macros.el ends here
