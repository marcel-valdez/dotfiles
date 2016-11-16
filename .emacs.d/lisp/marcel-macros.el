(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
  `(when
       (condition-case err
           (progn (require ',symbol) t)
         (error
          (message
           (format "Failed to configure package <%s>\n%s" ',symbol err))
          nil))
     ,@body))

(put 'with-library 'lisp-indent-function 1)
