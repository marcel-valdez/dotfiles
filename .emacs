;;; .emacs --- Summary
;;; Commentary:
;;; This is a sensible configuration for my home computer

;;; Code:

;;; package installer settings
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/"))
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/external/emacs-jedi/"))

(package-initialize)
(add-to-list 'package-archives
             '("stable-melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpa-gnu" . "https://elpa.gnu.org/packages/"))

;; initialize packages
(require 'cl)
(require 'marcel-macros)
;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 cursor-type '(hbar . 2)                ; Underline-shaped cursor
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 ; delete-by-moving-to-trash t            ; Delete files to trash
 ; fill-column 80                         ; Set width for automatic line breaks
 gc-cons-threshold (* 8 1024 1024)      ; We're not using Game Boys anymore
 help-window-select t                   ; Focus new help windows when opened
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-screen t               ; Disable start-up screen
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 native-comp-async-report-warnings-errors 'silent ; Skip compilation error buffers
 read-process-output-max (* 1024 1024)  ; Increase read size per process
 recenter-positions '(5 top bottom)     ; Set re-centering positions
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 2                        ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; Use a single space after dots
 show-help-function nil                 ; Disable help text everywhere
 tab-always-indent 'complete            ; Indent first then try completions
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 use-short-answers t                    ; Replace yes/no prompts with y/n
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t                     ; Stretch cursor to the glyph width
 tags-revert-without-query 1)           ; Don't ask to reload TAGS file.

(blink-cursor-mode 0)                   ; Prefer a still cursor
; (delete-selection-mode 1)               ; Replace region when inserting text
(global-subword-mode 1)                 ; always enable identification of JavaStyleWords
(column-number-mode)                    ; show column number on status bar
(global-auto-revert-mode)               ; automatically refresh file if they are changed from underneath
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(menu-bar-mode -1)                      ; Disable the top menu bar

(with-library cl-generic)
;; (with-library better-defaults)
(with-library use-package)
(with-library use-package-ensure)
(with-library tail-buffer)

(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

(if window-system
    (progn
      ;; These are all undesirable wasted space.
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (load-theme 'modus-vivendi t)
      (global-display-line-numbers-mode t)
      ;; emacs control the cursor type in GUI mode.
      (setq-default cursor-type '(bar . 3))
      ;; We're actually able to control the font in GUI mode
      ;;  '(default ((t (:family "Azeret Mono" :foundry "NONE" :slant normal :weight normal :height 105 :width normal))))
      (set-face-attribute 'default nil :family "Azeret Mono" :height 105)))

;;; re-binds certain keys when inside a TMUX session
(if (display-graphic-p)
    ;; if emacs is run as a GUI window
    (use-package multi-term
      :ensure t
      :config
      (with-library multi-term
        ;; start an emacs server so editors use an emacs buffer
        (if (not (daemonp))
            (server-start))
        ;; start multi-term custom configurations
        (global-unset-key (kbd "C-t"))
        (add-to-list 'term-unbind-key-list "C-t")
        (setq multi-term-program "/bin/bash")
        ;; start new terminal
        (global-set-key (kbd "C-t C-n")
                        (lambda () (interactive) (multi-term)))
        ;; switch to next terminal within same buffer
        (global-set-key (kbd "C-t <C-tab>")
                        (lambda () (interactive) (multi-term-next)))
        ;; switch to previous terminal within same buffer
        (global-set-key (kbd "C-t <C-iso-lefttab>")
                        (lambda () (interactive) (multi-term-prev)))
        ;; toggle showing/hiding the dedicated terminal window
        (global-set-key (kbd "C-t C-d")
                        (lambda () (interactive) (multi-term-dedicated-toggle)))))
  ;; if emacs is run as a terminal application
  (with-library in-tmux
    (use-package helm-dash
      :ensure t
      :config
      (with-library helm-dash
        (setq helm-dash-browser-func 'eww)))))

(use-package fzf
  :ensure t
  :config
  (setq fzf/executable (concat (getenv "HOME") "/modules/fzf/bin/fzf")))

(use-package lua-mode
  :ensure t)
(use-package zeal-at-point
  :ensure t
  :config
  (with-library zeal-at-point
    (global-set-key "\C-cd" 'zeal-at-point)
    (setq zeal-at-point-zeal-version "0.6.1")
    (add-to-list 'zeal-at-point-mode-alist '(ruby-mode . ("ruby2" "ruby")))))

(use-package xclip
  :ensure t
  :config
  (with-library xclip
    (xclip-mode)))
(use-package multiple-cursors :ensure t)
(use-package helm-ls-git :ensure t)
(use-package elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-timeout 2)
  (setq elpy-formatter 'black)
  (setq elpy-rpc-virtualenv-path 'current))
(use-package flycheck :ensure t)
(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package flymake
  :ensure t
  :config
  (add-hook 'sh-mode-hook 'flymake-mode))
;;  (defun flycheck-set-pylint ()
;;    (setq flycheck-checker 'python-pylint))
;;  (add-hook 'python-mode-hook 'flycheck-set-pylint))
(use-package flyspell-correct
  :ensure t
  :config
  (with-library flyspell-correct
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)))

(use-package org
  :config
  (defun custom:org-mode-hook ()
    (org-indent-mode t))

  (add-hook 'org-mode-hook 'custom:org-mode-hook)
  (require 'org-inlinetask)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-deadline-warning-days 7)
  (setq org-tags-match-list-sublevels 'indented)
  ;; Also see: (describe-variable 'org-stuck-projects)
  (setq org-stuck-projects
      '("+PROJECT/-DEFERRED-CANCELLED-PAUSED-DONE" ("DOING" "TODO" "APPT" "WAITING") nil
        "\\<IGNORE\\>"))
  ;; Also see: (describe-variable 'org-agenda-custom-commands)
  (setq org-agenda-custom-commands
       '(("A" . "Overall Commands")
         ("Ap" "All Projects" ((tags "PROJECT")))
         ("P" . "Personal Entries") ;; prefix P command
         ("Pa" "All Relevant Personal Items"
          ((agenda ""
                   ((org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           (tags "-TODO={DONE\\|CANCELLED}"))
          ((org-agenda-tag-filter-preset '("+{PERSONAL\\|BOTH}"))))
         ("Pd" "Daily Action List"
          ;; See: https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
          ((agenda ""
                   ((org-agenda-span 1)
                    (org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           ;; See: https://orgmode.org/manual/Matching-tags-and-properties.html
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"2\"-STYLE=\"habit\""))
          ((org-agenda-tag-filter-preset '("+{PERSONAL\\|BOTH}"))))
         ("W" . "Work Commands") ;; prefix W command
         ("Wa" "All Relevant Work Items"
          ((agenda ""
                   ((org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           (tags "-TODO={DONE\\|CANCELLED}")))
         ("Wd" "Daily Action List"
           ;; See: https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
          ((agenda ""
                   ((org-agenda-span 1)
                    (org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           ;; See: https://orgmode.org/manual/Matching-tags-and-properties.html
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"2\"-STYLE=\"habit\""))
          ((org-agenda-tag-filter-preset '("+{WORK\\|BOTH}"))))
         ("R" "Weekly Review"
          ((agenda "" ((org-agenda-span 7)))
           (stuck "") ;; See: https://orgmode.org/manual/Stuck-projects.html
           (tags "PROJECT+TODO=\"\"")
           (todo "WAITING")
           (todo "DEFERRED")
           (todo "PAUSED")
           ))
         ("D" "Daily Action List"
          ((agenda ""
                   ((org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 0)))))))
  (defun custom:org-gtd ()
    (interactive)
    (find-file "~/gtd/gtd.org"))
  (global-set-key (kbd "C-c C-g") 'custom:org-gtd)

  (setq org-log-repeat 'time)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "APPT(a)" "DOING(o)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)" "PAUSED(p)")))

  (setq org-todo-keyword-faces
        '(
          ;; Item is ready to be done at the earliest opportunity or by deadline
          ("TODO" . (:foreground "dodgerblue1" :weight bold))
          ;; Used to tag an activity that can *only* be done at the specified time and date.
          ("APPT" . (:foreground "dodgerblue3"))
          ;; Tasks that are currently being worked on
          ("DOING" . (:foreground "aquamarine1" :weight bold))
          ;; Task that is waiting on a response or availability of something or someone.
          ("WAITING" . (:foreground "darkorange3"))
          ;; Completed task
          ("DONE" . (:foreground "darkgreen" :weight bold))
          ;; Task will no longer be done, but for some reason I kept it on file
          ("CANCELLED" . (:foreground "tan3"))
          ;; Task is paused for the time being, in favour of another one, but should
          ;; be picked up as soon as another work stream is done.
          ("PAUSED" . (:foreground "rosybrown"))
          ;; Task that is defined but shouldn't be picked up at earliest opportunity.
          ;; The reason should be included in the task notes.
          ("DEFERRED" . (:foreground "dodgerblue4"))))

  (setq org-capture-templates
        '(("g" "GTD Entries")
          ("gt" "Task" entry (file+olp "~/gtd/gtd.org" "Tasks")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (js . t)
                                 (dot . t)
                                 (emacs-lisp . t)
                                 (lisp . t)))
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-agenda-text-search-extra-files
        (directory-files-recursively "~/notes/" "md$"))
  (setq org-agenda-files '("~/notes/" "~/gtd/")))

(use-package markdown-mode
  :ensure t
  :config
  (with-library markdown-mode
    (defun markdown-enable-flyspell()
      (flyspell-mode))
    (add-hook 'markdown-mode-hook 'markdown-enable-flyspell)))

(use-package web-beautify
  :ensure t
  :config
  (with-library web-beautify
                 (eval-after-load 'js2-mode
                   '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
                 (eval-after-load 'json-mode
                   '(defin-key json-mode-map (kbd "C-c b") 'web-beautify-js))
                 (eval-after-load 'sgml-mode
                   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
                 (eval-after-load 'css-mode
                   '(define-key 'css-mode-map (kbd "C-c b") 'web-beautify-css))))

;; set whitespace visualization
(use-package whitespace
  :ensure t
  :config
  (with-library whitespace
    (global-whitespace-mode)
    (setq show-trailing-whitespace t)
    (setq whitespace-style '(tab-mark trailing lines-tail face))
    (defun set-prog-column-limit () (setq-local whitespace-line-column 100))
    (add-hook 'prog-mode-hook 'set-prog-column-limit)
    (defun set-text-column-limit () (setq-local whitespace-line-column 100))
    (add-hook 'text-mode-hook 'set-text-column-limit)))

;; set indentation configuration
(defun set-custom-indent ()
  "Set custom indentation settings (two spaces) in all modes."
  (setq-local js2-basic-offset 4)
  (setq-local js2-bounce-indent-p t)
  (setq-local js-indent-level 4)
  (setq-local cperl-indent-level 2)
;;  (setq-local sh-basic-offset 2)
;;  (setq-local sh-indentation 2)
  (setq-local smie-indent-basic 2)
;;  (setq-local c-basic-offset 2)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq-local css-indent-offset 2))
(add-hook 'prog-mode-hook 'set-custom-indent)

;; setup telephone-line
(use-package telephone-line
  :ensure t
  :config
  (require 'telephone-line)
  (require 'telephone-line-config)
  (telephone-line-mode t))

;;;; global editing settings and overrides
;; use undo-tree instead of built-in undo
(use-package undo-tree
  :ensure t
  :config
  (with-library undo-tree
    (global-undo-tree-mode)))
;; always enable auto-complete
(use-package auto-complete
  :ensure t
  :config
  (with-library auto-complete
    (add-hook 'after-init-hook 'global-auto-complete-mode)
    (with-library company
      (add-hook 'company-mode (lambda () (auto-complete-mode -1))))))
;; company-mode
(use-package company
  :ensure t
  :config
  (with-library company
    (add-hook 'after-init-hook (lambda () (global-company-mode -1)))
    (with-library auto-complete
      (add-hook 'auto-complete-mode (lambda () (company-mode -1))))))


(use-package jedi
  :ensure t
  :init
  (setq jedi:setup-keys t)
  (setq jedi:server-args '("--log-traceback"))
  :config
  (with-library jedi
    (defun jedi:python-mode-hook ()
      (with-library elpy
        (setq elpy-modules
              (delq 'elpy-module-company
                    (delq 'elpy-module-flymake elpy-modules)))
        (add-hook 'elpy-mode-hook 'flycheck-mode)
        (add-hook 'elpy-mode-hook 'auto-complete-mode)
        (company-mode -1)
        (elpy-mode t))
      (setq jedi:complete-on-dot t)
      (setq-local whitespace-line-column 100))
    (add-hook 'python-mode-hook 'jedi:setup)
    (add-hook 'python-mode-hook 'jedi:python-mode-hook)))

;;;; custom bindings
;; move a line up-down easily
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
;; Sometimes Alt-Up/Down is detected as ESC up/down (wezterm-specific)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
;; sometimes End is detected as <select> (wezterm-specific)
(global-set-key (kbd "<select>") 'move-end-of-line)

;; switch between windows quickly
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") (lambda () (interactive) (other-window 1)))
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window -1)))
;; switch between frames quickly
(global-unset-key (kbd "M-C-j"))
(global-set-key (kbd "M-C-j") (lambda () (interactive) (other-frame 1)))
(global-unset-key (kbd "M-C-k"))
(global-set-key (kbd "M-C-k") (lambda () (interactive) (other-frame -1)))

;; configure helm
(use-package helm
  :ensure t
  :config
    (helm-mode 1)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-s o") 'helm-occur)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (set-face-attribute 'helm-selection nil
                        :background "gray1"
                        :foreground "cornflowerblue"))

(use-package imenu-list
  :ensure t)

;; configure rotate-window
(use-package rotate
  :ensure t
  :config
  (with-library rotate
    (global-set-key (kbd "C-x t")
                    (lambda () (interactive) (rotate-window)))
    ;; switch to vertical split layout
    (global-set-key (kbd "C-x M-\\") 'rotate:even-horizontal)
    ;; switch to horizontal split layout
    (global-set-key (kbd "C-x M--") 'rotate:even-vertical)
    ;; cycle through layouts
    (global-set-key (kbd "C-x l")
                    (lambda () (interactive) (rotate-layout)))))
;; cycle through window sizes
(use-package cycle-resize
  :ensure t
  :config
  (with-library cycle-resize
    (global-set-key (kbd "C-x -") 'cycle-resize-window-vertically)
    (global-set-key (kbd "C-x \\") 'cycle-resize-window-horizontally)
    (setq cycle-resize-steps '(75 66 50 33))))

;; remap undo-redo using undo-tree
(use-package undo-tree
  :ensure t
  :config
  (with-library undo-tree
    (global-unset-key (kbd "C-/"))
    (global-set-key (kbd "C-/")
                    (lambda () (interactive) (undo-tree-undo)))
    (global-unset-key (kbd "C-."))
    (global-set-key (kbd "C-.")
                    (lambda () (interactive) (undo-tree-redo)))))

;; puts all backup files in .emacs.d/backup directory rather than on
;; the same folder as the file being edited
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      backup-by-copying t ; don't delink hardlinks
      version-control t ; user version numbers on backups
      delete-old-versions t ; automatically delete old backups
      kept-new-versions 20 ; how many of the pre-saved versions to keep
      kept-old-versions 5; how many of the post-saved versions to keep
      )

;; mark instances forward of whatever is marked in the region
(use-package multiple-cursors
  :ensure t
  :config
  (with-library multiple-cursors
    (global-unset-key (kbd "M-3"))
    (global-set-key (kbd "M-3") 'mc/mark-more-like-this-extended)
    ;; mark instances backwards of whatever is marked in the region
    (global-unset-key (kbd "M-#"))
    (global-set-key (kbd "M-#") 'mc/mark-previous-like-this)
    ;; mark all instances of whatever is marked in the region
    (global-unset-key (kbd "C-x M-3"))
    (global-set-key (kbd "C-x M-3") 'mc/mark-all-dwim)
    ;; create a separate caret for every line under the cursor
    (global-unset-key (kbd "C-x M-l"))
    (global-set-key (kbd "C-x M-l") 'mc/edit-lines)))

(use-package erc-status-sidebar
  :ensure t
  :config
  (with-library erc-status-sidebar))

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (with-library flycheck-checkbashisms
    ;; Check 'echo -n' usage
    (setq flycheck-checkbashisms-newline t)
    ;; Check non-POSIX issues but required to be supported  by Debian Policy 10.4
    ;; Setting this variable to non nil made flycheck-checkbashisms-newline effects
    ;; regardless of its value
    (setq flycheck-checkbashisms-posix t)

    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup))

    (defun set-flycheck-checkbashisms ()
      (setq flycheck-checker 'sh-checkbashisms))
    (add-hook 'sh-mode 'set-flycheck-checkbashisms)))

(use-package csharp-mode
  :ensure t
  :config
  (with-library csharp-mode
    (with-library company
      (add-hook 'csharp-mode #'company-mode))
    (with-library omnisharp
      (add-hook 'csharp-mode #'omnisharp-mode))
    (with-library flycheck
      (add-hook 'csharp-mode #'flycheck-mode))))

;; omnisharp configuration
;; Start the omnisharp server with the following command:
;; (omnisharp-start-omnisharp-server)
(use-package omnisharp
  :ensure t
  :config
  (with-library omnisharp
    (with-library company
      (eval-after-load 'company '(add-to-list 'company-backend #'company-omnisharp)))
    (defun marcel-omnisharp-keys  ()
      (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
      (local-set-key (kbd "C-c C-c") 'recompile))
    (with-library csharp-mode
      (add-hook 'csharp-mode 'marcel-omnisharp-keys t))))

(use-package js2-mode
  :ensure t
  :config
  (with-library js2-mode
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
    (defun marcel-js2-keys ()
      (local-set-key (kbd "M-j") (lambda () (interactive) (other-window 1))))
    (add-hook 'js2-mode 'marcel-js2-keys)))

(use-package eslint-fix
  :ensure t
  :config
  (with-library eslint-fix
    (setq eslint-fix-executable "/home/marcel/.nvm/versions/node/v13.9.0/bin/eslint")
    (eval-after-load 'js-mode
      '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))))

(use-package flymake-jslint
  :ensure t
  :config
  (with-library flymake-jslint
    (setq flymake-jslint-command "/home/marcel/.nvm/versions/node/v13.9.0/bin/jslint")
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook 'flymake-jslint-load))
    (eval-after-load 'js-mode
      '(add-hook 'js-mode-hook 'flymake-jslint-load))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(display-time-mode t)
 '(elpy-formatter 'black)
 '(elpy-rpc-virtualenv-path 'current)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (javascript-mode omnisharp csharp-mode erc-status-sidebar markdown-mode flyspell-correct flycheck helm-descbinds helm-dash company-ctags ggtags helm-gtags telephone-line use-ttf use-package use-package-ensure-system-package zeal-at-point flycheck-checkbashisms graphviz-dot-mode flyspell-correct-helm helm-flycheck better-defaults jedi elpy company xclip helm git helm-grepint helm-helm-commands helm-ispell helm-ls-git helm-proc helm-pydoc helm-rubygems-org helm-themes helm-wordnet helm-xref hgignore-mode undo-tree rotate rjsx-mode multiple-cursors multi-term markdownfmt markdown-toc markdown-preview-mode helm-git gtags flycheck-tip flycheck-package cycle-resize auto-complete)))
 '(safe-local-variable-values (quote ((xref-etags-mode . t))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:inherit popup-face))))
 '(bg:erc-color-face2 ((t (:background "blue1"))))
 '(company-preview-common ((t (:inherit (company-tooltip-selection company-tooltip)))))
 '(company-tooltip ((t (:background "black" :foreground "gray46"))))
 '(company-tooltip-common ((t (:foreground "brightwhite" :weight extra-bold))))
 '(company-tooltip-scrollbar-thumb ((t (:background "brightcyan"))))
 '(company-tooltip-scrollbar-track ((t (:background "cyan"))))
 '(company-tooltip-selection ((t (:background "brightblack"))))
 '(completions-common-part ((t (:foreground "dodgerblue1"))))
 '(custom-comment-tag ((t (:foreground "deepskyblue1"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "dodgerblue2" :weight bold :height 1.2))))
 '(custom-variable-obsolete ((t (:foreground "dodgerblue2"))))
 '(custom-variable-tag ((t (:foreground "dodgerblue2" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :extend t :background "darkolivegreen4"))))
 '(diff-file-header ((t (:extend t :background "gray1" :weight bold))))
 '(diff-header ((t (:extend t :background "gray16"))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "darkgreen"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "firebrick3"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :background "firebrick4"))))
 '(font-lock-comment-face ((t (:foreground "gray27"))))
 '(font-lock-function-name-face ((t (:foreground "dodgerblue2"))))
 '(font-lock-string-face ((t (:foreground "tan"))))
 '(fringe ((t (:background "gray15"))))
 '(helm-selection ((t (:background "gray1" :foreground "cornflowerblue"))))
 '(help-key-binding ((t (:background "gray10" :foreground "royalblue" :box (:line-width (1 . -1) :color "gray5")))))
 '(highlight ((t (:background "midnightblue"))))
 '(highlight-indent-face ((t (:background "gray23"))))
 '(minibuffer-prompt ((t (:foreground "dodgerblue2"))))
 '(org-agenda-structure ((t (:foreground "royalblue"))))
 '(org-drawer ((t (:foreground "royalblue"))))
 '(org-hide ((t (:inherit fixed-pitch :foreground "gray7"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "royalblue"))))
 '(package-status-dependency ((t (:inherit package-status-installed :foreground "chartreuse4"))))
 '(package-status-installed ((t (:inherit font-lock-comment-face :foreground "seagreen"))))
 '(region ((t (:background "gray16"))))
 '(sh-quoted-exec ((t (:foreground "chocolate1"))))
 '(shadow ((t (:foreground "gray27"))))
 '(show-paren-match ((t (:background "steelblue"))))
 '(telephone-line-accent-active ((t (:inherit mode-line :background "gray11" :foreground "brightwhite"))))
 '(telephone-line-accent-inactive ((t (:inherit mode-line-inactive :background "gray35" :foreground "gray74"))))
 '(telephone-line-warning ((t (:inherit warning :foreground "firebrick" :strike-through nil :underline nil))))
 '(whitespace-line ((t (:background "gray12")))))

;;; .emacs ends here
