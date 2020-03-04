;; buffer-tail
;; Written by elbeardmorez
;; Copied from stackoverflow (http://stackoverflow.com/a/6341139/10771)

;; Keeps focus on the bottom of a given buffer

;; Install by putting it in your load path, and doing a (require 'buffer-tail)
;; in your init.el

;; Usage:
;; Provides the tail-buffer/toggle function to toggle the tailling of a buffer
;; can force it to "on" or "off"
;; (tail-buffer/toggle "foo.log")

; alist of 'buffer-name / timer' items
(defvar tail-buffer-alist nil)

(defun tail-buffer/tail (name)
  "Follow buffer tails"
  (cond ((or (equal (buffer-name (current-buffer)) name)
         (string-match "^ \\*Minibuf.*?\\*$" (buffer-name (current-buffer)))))
        ((get-buffer name)
      (with-current-buffer (get-buffer name)
        (goto-char (point-max))
        (let ((windows (get-buffer-window-list (current-buffer) nil t)))
          (while windows (set-window-point (car windows) (point-max))
         (with-selected-window (car windows) (recenter -3)) (setq windows (cdr windows))))))))

(defun tail-buffer/toggle-internal (name &optional force)
  "Toggle tailing of buffer NAME when called non-interactively, a FORCE arg of 'on' or 'off' can be used to ensure a given state for buffer NAME"
   (let ((toggle (cond (force force) ((assoc name tail-buffer-alist) "off") (t "on")) ))
    (if (not (or (equal toggle "on") (equal toggle "off")))
      (error "invalid 'force' arg. required 'on'/'off'")
      (progn
        (while (assoc name tail-buffer-alist)
           (cancel-timer (cdr (assoc name tail-buffer-alist)))
           (setq tail-buffer-alist (remove* name tail-buffer-alist :key 'car :test 'equal)))
        (if (equal toggle "on")
            (add-to-list 'tail-buffer-alist
                         (cons name (run-at-time t 1 'tail-buffer/tail name))))
        (message "toggled 'tail buffer' for '%s' %s" name toggle)))))

(defun tail-buffer/toggle (name)
  "Toggle tailing of buffer NAME when called non-interactively, a FORCE arg of 'on' or 'off' can be used to ensure a given state for buffer NAME"
  (interactive "bToggle tail status for buffer: ")
  (tail-buffer/toggle-internal name nil))

(defun tail-buffer/toggle-current (&optional force)
  "Toggle tailing the current buffer."
  (interactive)
  (tail-buffer/toggle-internal (buffer-name) nil))

(provide 'tail-buffer)
