(provide 'marcel-core-macros)

(defmacro with-library (symbol &rest body)
  "Executes the &rest body if it is able to successfully 'require the given
symbol. If the symbol cannot be 'require'd, then the &rest body is not executed
and no error is raised. If the &rest body fails during execution, the error is
allowed to bubble up, it is not caught."
  `(when
       (condition-case err
           (progn (require ',symbol) t)
         (error
          (message
           (format "Error while requiring package %s\n%s" ',symbol err))
          nil))
     ,@body))

(put 'with-library 'lisp-indent-function 1)
