(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
  `(condition-case err
       (progn
         (require ',symbol)
         ,@body)
     (error
      (message
       (format "Failed to configure package or library <%s>\n%s" ',symbol err))
      nil)))

(put 'with-library 'lisp-indent-function 1)
