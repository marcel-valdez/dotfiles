
;;; Code:
(require 'google)
(require 'ubdiff)
(require 'compilation-colorization)
(require 'google3-eglot)
(require 'llm-goose)

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

(provide 'at-office)
;;; at-office.el ends here
