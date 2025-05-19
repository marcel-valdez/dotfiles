
;;; Code:
(require 'google)
(require 'ubdiff)
(require 'compilation-colorization)
(require 'google3-eglot)
(require 'llm-goose)

;; goose-v3.5-m => 128k input tokens, 8k output tokens, Takes *minutes* to
;;                 answer.
;;   - This is WAY too slow for quick questions / answers, may be good for
;;     large tasks.
;; goose-v3.5-s => 128k input tokens, 8k output tokesn, Takes *## seconds* to
;;                 answer.
;;   - This should be good enough for coding tasks.
;; goose-v3.5-xs => 32k input tokens, 8k output tokens, Takes *# seconds* to
;;                  answer.
;;   - This is too imprecise for coding tasks, but good for consultation
;;     questions, not for solving problems.

(setf llm-goose-default-it
      (make-llm-goose :model "goose-v3.5-s"))
(setq llm-default-model "goose-v3.5-s")

(defun eglot-hook-fn ()
  (local-set-key (kbd "C-x <f2>") #'eglot-rename)
  ;; Use manual activation of current symbol documentation
  (local-set-key (kbd "C-c C-d") #'eldoc))

(defun config-python-eldoc-mode ()
  ;; increase the delay significantly to prevent freezes
  (setq-local eldoc-idle-delay 2)
  (setq-local eldoc-echo-area-use-multiline-p t)
  (setq-local eldoc-echo-area-display-truncation-message nil)
  ;; Only display the documentation in the echo-area, not in the buffer.
  ;; The other value is eldoc-display-in-buffer
  ;; (setq-local eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer))
  (setq-local eldoc-display-functions #'eldoc-display-in-echo-area))


(with-eval-after-load 'google3-eglot
  (add-hook 'eglot-managed-mode-hook #'eglot-hook-fn)
  (add-hook 'python-mode-hook #'config-python-eldoc-mode))
;; TODO(b/156179120): Remove if/when adopted into eglot.el or google.el
;; Also see: https://github.com/joaotavora/eglot/issues/466
(setq eglot-sync-connect 0)
;; See: https://groups.google.com/a/google.com/g/emacs-users/c/nqWnsf40nYA/m/PBQvLgxvAgAJ
(setq eglot-events-buffer-size 0)
(setq eldoc-idle-delay 0.666)
(setq google3-eglot-compose 't)
(google3-eglot-setup)

(defvar at-office-is-work-laptop)
(setq at-office-is-work-laptop (eq (system-name) "marcelvaldez-glaptop"))
(defvar at-office-is-workstation)
(setq at-office-is-workstation (eq (system-name) "marcelvaldez.mtv.corp.google.com"))
(defvar at-office-is-gcloud)
(setq at-office-is-gcloud (eq (system-name) "marcelvaldez.c.googlers.com"))

(defun marcelvaldez-project-desktop-setup ()
  "Configure desktop saving for Marcel Valdez projects."
  (let* ((cwd default-directory)
         (project-root "/google/src/cloud/marcelvaldez/")
         (project-prefix-length (length project-root)))
    (when (and (string-prefix-p project-root cwd) ; Check if the current directory starts with the project root.
               (> (length cwd) project-prefix-length)) ; Check if the current directory is longer than the project root.

      (let* ((project-subdir (substring cwd project-prefix-length)) ; Extract the subdirectory relative to the project root.
             (first-path-part (car (split-string project-subdir "/" t)))) ; Extract the first path segment.
        (when first-path-part
          (let ((tmux-session-name (string-trim-right (shell-command-to-string "tmux display-message -p '#S'"))))

            (when (string= first-path-part tmux-session-name) ; Check if the path segment matches the tmux session name.
              (let* ((desktop-save-dir (expand-file-name first-path-part "~/.emacs.d/desktop-saves/"))
                     (desktop-save-file (expand-file-name ".emacs.desktop" desktop-save-dir)))

                (unless (file-directory-p desktop-save-dir)
                  (make-directory desktop-save-dir t))
                (setq desktop-dirname desktop-save-dir)
                (setq desktop-path desktop-save-file)

                (when (file-exists-p desktop-save-file)
                  ;; Prompt the user before loading the desktop.
                  (if (y-or-n-p (format "Load desktop from %s? " desktop-save-file))
                      ;; Only restore if user answers 'y'
                      (progn
                        (desktop-read desktop-save-dir)
                        (desktop-save-mode 1))))
                ))))))))

(add-hook 'emacs-startup-hook #'marcelvaldez-project-desktop-setup) ; Add the function to the emacs startup hook.

(defun at-office/get-buffer-id ()
  "Get the full file path if visiting a file, otherwise show the buffer name."
    (let ((file-name (buffer-file-name))
          (buf-name (buffer-name)))
      (if file-name file-name buf-name)))

(defun at-office/get-region-start-end-columns ()
  "Return a list (start-column end-column) for the active region.
Returns nil if the region is not active."
  (if (use-region-p)
      (let ((start-pos (region-beginning))
            (end-pos (region-end)))
        (list
         (save-excursion
           (goto-char start-pos)
           (current-column))
         (save-excursion
           (goto-char end-pos)
           (current-column))))
    nil))

(defun at-office/get-buffer-prompt ()
  "Get a prompt for the full file path if visiting a file, otherwise prompt with the buffer name."
  (let ((file-name (buffer-file-name))
        (buf-name (buffer-name)))
    (if file-name
        (format "The file being modified in the emacs buffer is: %s" file-name)
      (format "The emacs buffer is a transient buffer with no file open and it is named: %s" buf-name))))

(defun at-office/extract-diff-block (input)
  "Return a single multi-line string between >>>START-DIFF<<< and >>>END-DIFF<<< in INPUT."
  (let ((start-marker ">>>START-DIFF<<<")
        (end-marker ">>>END-DIFF<<<")
        (lines (split-string input "\n"))
        (collecting nil)
        (result '()))
    (dolist (line lines)
      ;;(message "%s" line)
      (let
          ((trimmed-line (string-trim line)))
        (cond
         ((string= trimmed-line start-marker)
          (setq collecting t))
         ((string= trimmed-line end-marker)
          (setq collecting nil))
         (collecting
          (push line result)))))
    (mapconcat #'identity (nreverse result) "\n")))

(defun at-office/create-diff-hunk-buffer (content)
  "Create a new buffer named *goose-diff-hunk* and insert CONTENT into it."
  (let ((buf (get-buffer-create "*goose-diff-hunk*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (diff-mode))
    (display-buffer buf)))

(defun llm-goose-gen (&optional buffer-context)
  "Ask Goose a question and see the answer.

 The full query is formed by combining BUFFER-CONTEXT and any
 user prompt provided by the user.

 When called interactively:
 1. BUFFER-CONTEXT is determined as follows:
    - If a region is active: its content is used.
    - Else if `llm-goose-ask-behavior' is `min-to-point': the buffer
      content from `point-min' to `point' is used.
 2. Then, the user is prompted for a prompt.

 When called non-interactively, BUFFER-CONTEXT and user prompt
 should be provided as strings.  If the user prompt is an empty string
 or nil, it may be ignored or handled as appropriate by the combination logic."
  (interactive
   (let* ((buffer-text (buffer-string))
          (buffer-id (at-office/get-buffer-id))
          (region-active (use-region-p))
          (region-start (if region-active (region-beginning) (point)))
          (region-end (if region-active (region-end) (point)))
          (buffer-name-prompt (at-office/get-buffer-prompt))
          (buffer-or-region-lines-prompt
           (if region-active
               (format "The active region has %d lines." (count-lines region-start region-end))
               (format "The buffer has %d total lines." (count-lines (point-min) (point-max)))))
          (user-prompt (read-string "Prompt: "))
          (context-info
           (if region-active
               (let ((region-columns (at-office/get-region-start-end-columns))
                 (region-column-start (car region-columns))
                 (region-column-end (cadr region-columns)))
               (format "The selected region starts at line %d and ends at line %d, starts at column %d and ends at column %d. "
                       (line-number-at-pos region-start)
                       (line-number-at-pos region-end)
                       region-column-start
                       region-column-end))
             (format "The cursor is at line %d. " (line-number-at-pos (point)))))
          (base-prompt
           (format
            ">>>START-GENERAL-REQUEST-INSTRUCTIONS<<<
You are an expert software engineer. You are helping me modify code in emacs.
If the help I request is a code modification, please provide the answer in a format that can be applied via emacs ediff using >>>START-DIFF<<< to mark the beginning of the diff and >>>END-DIFF<<< to mark the end, otherwise, provide the answer as a normal text, it is necessary you use those exact markers for the diff beginning and end, because they'll be post-processed by an emacs function that has those strings hardcoded.

The buffer's contents will be within the sections marked >>>START-BUFFER:buffer-identifier<<< and >>>END-BUFFER:buffer-identifier<<< where buffer-identifier is a placeholder for the name of the file or buffer being modified.
>>>END-GENERAL-REQUEST-INSTRUCTIONS<<<

>>>START-EMACS-REGION-AND-CURSOR-CONTEXT<<<
%s
%s
%s
>>>END-EMACS-REGION-AND-CURSOR-CONTEXT<<<

>>>START-BUFFER:%s<<<
%s
>>>END-BUFFER:%s<<<
" buffer-name-prompt buffer-or-region-lines-prompt context-info buffer-id buffer-text buffer-id))
          (full-prompt (if
                            (and user-prompt (not (string-empty-p user-prompt)))
                            (format "%s
>>>START-USER-PROVIDED-PROMPT<<<
%s
>>>END-USER-PROVIDED-PROMPT<<<" base-prompt user-prompt)
                         base-prompt))
          (final-prompt (replace-regexp-in-string "\\\\" "\\\\\\\\" full-prompt)))
     (with-current-buffer (get-buffer-create "*goose answer*")
       (visual-line-mode +1)
       (goto-char (point-max))
       ;; Use the combined final-prompt
       (insert (format "
===== BEGIN =====
%s
" final-prompt))
       (pop-to-buffer (current-buffer)))
     (llm-chat-async
      llm-goose-default-it
      (llm-make-chat-prompt final-prompt :temperature 0.15)
      (lambda (response)
        (with-current-buffer (get-buffer-create "*goose answer*")
          (goto-char (point-max))
          ;;; Unescape the response before inserting it
          ;;; (let ((unescaped-response (replace-regexp-in-string "\\\\" "\\\\\\\\" response)))
          (let ((unescaped-response (replace-regexp-in-string "\\\\(.)" "\\1" response)))
            (insert unescaped-response))
          (insert "
===== END =====
"))
        ;; Open a diff-hunk buffer if the response contains a diff hunk.
        (let* ((diff-hunk (at-office/extract-diff-block response))
               (diff-found-p (not (string-empty-p (or diff-hunk "")))))
          (cond (diff-found-p (at-office/create-diff-hunk-buffer diff-hunk)))))
      (lambda (type err)
        (with-current-buffer (get-buffer-create "*goose answer*")
          (goto-char (point-max))
          (let* ((err-msg (if (and (listp err) (assoc :msg err) (stringp (cdr (assoc :msg err))))
                              (cdr (assoc :msg err))
                            (if (stringp err)
                                err
                               (format "Error Type: %S, Error: %S" type err))))
                 )
            (insert (format "%s" err-msg))
            (insert "
===== ERROR =====
"))))))))


(provide 'at-office)
;;; at-office.el ends here
