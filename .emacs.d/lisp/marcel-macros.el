(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)
     (error (message (format "I guess we don't have %s available." ',symbol))
            nil)))
(put 'with-library 'lisp-indent-function 1)


(defun add-subdirs-to-list (base-path seed-list)
  "Build a new list made up of the seed-list plus all recursive subdirectory
paths in base-path"
  (progn
    (mapc (lambda (file-entry)
            (let ((name (car file-entry)) ;; get 1st element in entry
                  (is-folder (cadr file-entry))) ;; get 2nd elemnt in entry
              (when (and is-folder
                         (not (equal name "."))
                         (not (equal name "..")))
                (add-to-list 'seed-list (concat base-path "/" name)))))
          (directory-files-and-attributes base-path))
    seed-list))
