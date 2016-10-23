(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
	 (require ',symbol)
	 ,@body)
     (error (message (format "I guess we don't have %s available." ',symbol))
	    nil)))
(put 'with-library 'lisp-indent-function 1)
