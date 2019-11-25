;;; package --- Summary
;;; Core macros with custom functions for unique emacs behavior.

;;; Commentary:
;;; Some functions here are provided because available functions in
;;; Emacs packages don't behave as I'd like to.


;;; Code:
(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
 "Function used to safely load a SYMBOL package with a custom function BODY.
The BODY only executes if the symbol is available in the system."
  `(when
       (condition-case err
           (progn (require ',symbol) t)
         (error
          (message
           (format "Failed to configure package <%s>\n%s" ',symbol err))
          nil))
     ,@body))

(put 'with-library 'lisp-indent-function 1)

(defun move-text-internal (arg)
 "Move a text region ARG lines."
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region or current line ARG lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region or current line ARG lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(provide 'marcel-macros)

;;; marcel-macros.el ends here
