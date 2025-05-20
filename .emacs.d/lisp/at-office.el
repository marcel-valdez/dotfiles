;; *-* lexical-binding: t -*-
;;; package --- at-office
;;; Commentary:
;;;   This package provides functions to interact with the Goose LLM,
;;;   and to configure the Emacs environment for my work.


(require 'google)
(require 'ubdiff) ; For diffing
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

;;;###autoload
(setf llm-goose-default-it
      (make-llm-goose :model "goose-v3.5-s"))
(setq llm-default-model "goose-v3.5-s")

;;;###autoload
(defun eglot-hook-fn ()
  "Set up keybindings for Eglot in the current buffer.

This function is added as a hook to `eglot-managed-mode-hook`.
It defines the following keybindings:

  - C-x <f2>: Rename the current symbol using `eglot-rename`.
  - C-c C-d: Display documentation for the current symbol using `eldoc`."
  (local-set-key (kbd "C-x <f2>") #'eglot-rename)
  ;; Use manual activation of current symbol documentation
  (local-set-key (kbd "C-c C-d") #'eldoc))

;;;###autoload
(defun config-python-eldoc-mode ()
  "Configure `eldoc-mode` for Python.

This function is added as a hook to `python-mode-hook`.  It configures eldoc to:
  - Increase the delay to prevent freezes.
  - Use multiline display in the echo area.
  - Not display truncation messages.
  - Only display documentation in the echo area."
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

(defun at-office/extract-block (input start-marker end-marker)
  "Return a single multi-line string between START-MARKER and END-MARKER in INPUT."
  (let ((lines (split-string input "\n"))
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

(defun at-office/extract-diff-block (input)
  "Return a single multi-line string between >>>START-DIFF<<< and >>>END-DIFF<<< in INPUT."
  (at-office/extract-block input ">>>START-DIFF<<<" ">>>END-DIFF<<<"))

(defun at-office/extract-new-content-block (input)
  "Return a single multi-line string between >>>START-NEW-BUFFER<<< and >>>END-NEW-BUFFER<<< in INPUT."
  (at-office/extract-block input ">>>START-NEW-BUFFER<<<" ">>>END-NEW-BUFFER<<<"))

(defun at-office/buffer-visible-on-selected-frame-p (buffer-name)
  "Return non-nil if BUFFER-NAME is visible in a window on the selected frame."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (seq-some
       (lambda (win)
         (and (eq (window-buffer win) buf)
              (eq (window-frame win) (selected-frame))))
       (window-list)))))

(defmacro at-office/create-buffer-in-same-window (source-buffer-name new-buffer-name &rest body)
  "Create NEW-BUFFER-NAME, run BODY inside it, and display it in the same window where SOURCE-BUFFER-NAME is visible."
  `(let ((new-buf (get-buffer-create ,new-buffer-name))
         (target-window
          (get-buffer-window ,source-buffer-name 'visible)))
     (with-current-buffer new-buf
       ,@body)
     (if target-window
         (set-window-buffer target-window new-buf)
       (pop-to-buffer new-buf))
     new-buf))

(defun at-office/create-diff-hunk-buffer (content)
  "Create a new buffer named *goose-diff-hunk* and insert CONTENT into it."
  (at-office/create-buffer-in-same-window "*goose answer*" "*goose-diff-hunk*"
                                          (erase-buffer)
                                          (insert content)
                                          (diff-mode)))

(defun at-office/create-new-contents-buffer (content original-buffer-name original-buffer-file)
  "Create a new buffer named *goose-diff-hunk* and insert CONTENT into it."
  (at-office/create-buffer-in-same-window "*goose answer*" "*goose-new-content*"
                                          (when buffer-read-only
                                            (setq buffer-read-only nil))
                                          (erase-buffer)
                                          (insert content))
      (ediff-buffers original-buffer-name "*goose-new-content*"))


(defvar at-office/llm-goose/current-buffer-name nil)
(defvar at-office/llm-goose/current-buffer-file nil)

(defun at-office/llm-goose (input-prompt get-diff-response-p)
  "Generate code or text using the Goose LLM.

This function facilitates interaction with the Goose LLM, providing a
structured way to query and receive responses.  It constructs a detailed
prompt that includes:

  - The current buffer's content.
  - The buffer's identifier (file path or buffer name).
  - The total number of lines in the buffer (or the number of lines in the
    selected region).
  - The cursor position (or the selected region's boundaries).
  - User-provided prompt (if any).

The function then sends this prompt to Goose, displays the response in a
new buffer, and if the response contains a diff-hunk, it opens a separate
buffer in `ediff-mode` to preview and apply the changes.

Parameters:
  INPUT-PROMPT: A string containing the user's specific instructions or
                question for Goose.  If empty or nil, Goose will be
                queried with the context of the current buffer only.

User Interaction:
  - The function prompts the user for an `input-prompt` before sending the
    request to Goose.
  - If a diff-hunk is returned, the user can review and apply the changes
    in the opened `ediff-mode` buffer.

Return Value:
  This function does not return a value directly.  It displays the Goose
  response in a buffer and may open another buffer for diffs.

Side Effects:
  - Creates and populates a buffer named `*goose answer*` with the
    Goose response.
  - Creates and populates a buffer named `*goose-diff-hunk*` if a diff-hunk
    is present in the response.
  - Opens a new window for the `*goose-diff-hunk*` buffer if it was created.
  - May modify the current buffer if a diff-hunk is applied.

Example:
  To ask Goose to add a docstring to the current function:
  (llm-goose-gen \"Add a docstring to the current function.\")

  To ask Goose to fix a bug in the selected region:
  (llm-goose-gen \"Fix the bug in the selected region.\")

  To ask Goose a general question about the current buffer:
  (llm-goose-gen \"\")

  To ask Goose a question without any context:
  (llm-goose-gen nil)"
  (let* ((buffer-text (buffer-string))
         (buffer-id (at-office/get-buffer-id))
         (current-buffer-name (buffer-name))
         (current-buffer-file (buffer-file-name))
         (region-active (use-region-p))
         (region-start (if region-active (region-beginning) (point)))
         (region-end (if region-active (region-end) (point)))
         (buffer-name-prompt (at-office/get-buffer-prompt))
         (buffer-or-region-lines-prompt
          (if region-active
              (format "The active region has %d lines." (count-lines region-start region-end))
            (format "The buffer has %d total lines." (count-lines (point-min) (point-max)))))
         (context-info
          (if region-active
              (let* ((region-columns (at-office/get-region-start-end-columns))
                     (region-column-start (car region-columns))
                     (region-column-end (cadr region-columns)))
                (format "The selected region starts at line %d and ends at line %d, starts at column %d and ends at column %d. "
                        (line-number-at-pos region-start)
                        (line-number-at-pos region-end)
                        region-column-start
                        region-column-end))
            (format "The cursor is at line %d. " (line-number-at-pos (point)))))
         (diff-prompt "If the help I request is a code modification, please provide the answer in a format that can be applied via emacs ediff using >>>START-DIFF<<< to mark the beginning of the diff and >>>END-DIFF<<< to mark the end, otherwise, provide the answer as a normal text, it is necessary you use those exact markers for the diff beginning and end, because they'll be post-processed by an emacs function that has those strings hardcoded.

If you provide a diff block, turn into a perfect patch creator and make completely certain that the line numbers for diff sections are completely correct (with no offset) and they match the buffer contents line for line, otherwise the patch can not be applied. Try yourself to apply the patch using the line numbers you specify in the response to make sure it is correct in all instances, if they don't match then fix it, re-check it is correct, if not repeat the process until you have a correct patch and then reply to me with the perfectly applicable diff block in the response.

DO NOT WRAP THE DIFF BLOCK IN AN ADDITIONAL ```diff markdown-style block.")
         (new-content-prompt "If the help I request is a code modification, you MUST rewrite the entire buffer and put the entire new buffer contents in the response using EXACTLY >>>START-NEW-BUFFER<<< and EXACTLY >>>END-NEW-BUFFER<<< to mark beginning and end of the new buffer's context, you MUST add a new line EXACTLY after >>>START-NEW-BUFFER<<< and another EXACTLY right before >>>END-NEW-BUFFER<<<, an emacs function will remove them when using your contents. YOU MUST NOT PROVIDE A ```diff block, INSTEAD YOU MUST PROVIDE THE NEW ENTIRE BUFFER CONTENTS.")
         (base-prompt
          (format
           ">>>START-GENERAL-REQUEST-INSTRUCTIONS<<<
You are an expert software engineer. You are helping me modify code in emacs.

The buffer's contents of my current emacs session will be within the sections marked >>>START-BUFFER:buffer-identifier<<< and >>>END-BUFFER:buffer-identifier<<< where buffer-identifier is a placeholder for the name of the file or buffer being modified.

Note that the new line right after >>>START-BUFFER:buffer-identifier<<< is added for clarity and it NOT part of the buffer content and also the new line right before >>>END-BUFFER:buffer-identifier<<< is also added for clarity and is NOT part of the buffer content.

%s

>>>END-GENERAL-REQUEST-INSTRUCTIONS<<<

>>>START-EMACS-REGION-AND-CURSOR-CONTEXT<<<
%s
%s
%s
>>>END-EMACS-REGION-AND-CURSOR-CONTEXT<<<

>>>START-BUFFER:%s<<<
%s
>>>END-BUFFER:%s<<<" (if get-diff-response-p diff-prompt new-content-prompt) buffer-name-prompt buffer-or-region-lines-prompt context-info buffer-id buffer-text buffer-id))
         (full-prompt (if
                          (and input-prompt (not (string-empty-p input-prompt)))
                          (format "%s
>>>START-USER-PROVIDED-PROMPT<<<%s>>>END-USER-PROVIDED-PROMPT<<<" base-prompt input-prompt)
                        base-prompt))
         ;; Replace backslah with 2 backslashes so protobuf can parse the string.
         (final-prompt (replace-regexp-in-string "\\\\" "\\\\\\\\" full-prompt)))
    (setq at-office/llm-goose/current-buffer-name current-buffer-name)
    (setq at-office/llm-goose/current-buffer-file current-buffer-file)
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
     (llm-make-chat-prompt final-prompt :temperature 0.1)
     (lambda (response)
       (with-current-buffer (get-buffer-create "*goose answer*")
         (goto-char (point-max))
          ;;; Unescape the response before inserting it into the buffer.
         (let ((unescaped-response (replace-regexp-in-string "\\\\(.)" "\\1" response)))
           (insert unescaped-response))
         (insert "
===== END =====
"))
       ;; Open a diff-hunk buffer if the response contains a diff hunk.
       (let* ((diff-hunk (at-office/extract-diff-block response))
              (diff-found-p (not (string-empty-p (or diff-hunk ""))))
              (new-contents (at-office/extract-new-content-block response))
              (new-contents-found-p (not (string-empty-p (or new-contents "")))))
         (if diff-found-p
             (at-office/create-diff-hunk-buffer diff-hunk)
           (if new-contents-found-p
               (at-office/create-new-contents-buffer
                new-contents
                at-office/llm-goose/current-buffer-name
                at-office/llm-goose/current-buffer-file)))))
     (lambda (err msg)
       (with-current-buffer (get-buffer-create "*goose answer*")
         (goto-char (point-max))
         (insert err msg)
         (insert "
===== ERROR =====
"))))))

;; (defun llm-goose-diff (input-prompt)
;;   (interactive
;;    (list (read-string "Prompt: ")))
;;   (at-office/llm-goose input-prompt t))

(defun llm-goose-gen (input-prompt)
  (interactive
   (list (read-string "Prompt: ")))
  (at-office/llm-goose input-prompt nil))


(provide 'at-office)
;;; at-office.el ends here
