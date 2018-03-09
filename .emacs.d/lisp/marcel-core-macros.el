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

(provide 'marcel-core-macros)
;;; marcel-core-macros.el ends here
