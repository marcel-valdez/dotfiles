
;;; Code:
(setq lexical-binding t)

(defun rotate-windows (arg)
    "Rotate your windows; use the prefix argument to rotate the other direction.
ARG number of lines to move."
    (interactive "P")
    (if (not (> (count-windows) 1))
        (message "You can't rotate a single window!")
      (let* ((rotate-times (prefix-numeric-value arg))
             (direction (if (or (< rotate-times 0) (equal arg '(4)))
                            'reverse 'identity)))
        (dotimes (_ (abs rotate-times))
          (dotimes (i (- (count-windows) 1))
            (let* ((w1 (elt (funcall direction (window-list)) i))
                   (w2 (elt (funcall direction (window-list)) (+ i 1)))
                   (b1 (window-buffer w1))
                   (b2 (window-buffer w2))
                   (s1 (window-start w1))
                   (s2 (window-start w2))
                   (p1 (window-point w1))
                   (p2 (window-point w2)))
              (set-window-buffer-start-and-point w1 b2 s2 p2)
              (set-window-buffer-start-and-point w2 b1 s1 p1))))
        (if (> arg 0)
            (select-window (previous-window))
                  (select-window (next-window))))))

(defun move-text-internal (arg)
  "ARG number of lines to move the text."
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
   "Move region (transient-mark-mode active) or current line arg lines down.
ARG number of lines to move the text"
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line arg lines up.
ARG number of lines to move the text"
   (interactive "*p")
   (move-text-internal (- arg)))

(setq exec-temp-bind:temp-fn nil)
(setq exec-temp-bind:kbd-mapping nil)
(setq exec-temp-bind:repeat-fn nil)

(defun exec-temp-bind (repeat-fn kbd-mapping)
  "Execute REPEAT-FN and temporarily map KBD-MAPPING to REPEAT-FN."
  (interactive)
  (funcall repeat-fn)

  (setq exec-temp-bind:repeat-fn repeat-fn)
  (setq exec-temp-bind:kbd-mapping kbd-mapping)
  (setq exec-temp-bind:temp-fn
        (lambda () (interactive)
;;          (display-message-or-buffer
;;           (concat "lambda exec-temp-bind:kbd-mapping => " exec-temp-bind:kbd-mapping))
          (exec-temp-bind exec-temp-bind:repeat-fn exec-temp-bind:kbd-mapping)))

;;  (display-message-or-buffer
;;   (concat "exec-temp-bind:kbd-mapping " exec-temp-bind:kbd-mapping))

  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd kbd-mapping)
       (lambda () (interactive) (funcall exec-temp-bind:temp-fn)))
     map)))

(provide 'kbd-shortcut-functions)
;;; kbd-shortcut-functions.el ends here
