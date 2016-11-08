(provide 'marcel-macros)

(defmacro with-library (symbol &rest body)
  `(when
       (condition-case err
           (progn (require ',symbol) t)
         (error
          (mesage (format "Package or library %s is missing.\n%s" ',symbol err))
          nil))
     ,@body))


(put 'with-library 'lisp-indent-function 1)

(defun to-flat-list (a-value)
  "turns the value into a flat (single dimension) list
if value is a scalar value, then it returns the same value
if value is a flat list, it returns a copy of the list
if value is a nested list, then it returns all leaf elements in a single list"
  (if (listp a-value)
        (mapcar
         (lambda (item) (append (to-flat-list item)))
         a-value)
    a-value))

(defun add-subdirs-of-all-top-level-dirs (top-dirs)
  "Create a list with the recursive subdirectories of a list of top level
directories. Includes the top-level directories in the result."
   (let ((result top-dirs))
     (mapc
      (lambda (top-dir)
        (setq result (append result (get-subdirs-recursively top-dir))))
      top-dirs)
     result))


(defun get-subdirs-recursively (top-dir)
  "Build a new list made up of  all recursive subdirectory paths under top-dir"
    (remove nil
     (mapcar (lambda (file-path)
              (when (file-directory-p file-path) file-path))
             (directory-files-recursively top-dir ".+" t))))
