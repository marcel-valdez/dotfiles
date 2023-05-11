;;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; The main goal of this configuration is to help you be more
;; productive; if you have ideas, praise or complaints, direct them to
;; emacs-users@google.com (http://g/emacs-users).  We'd especially like to hear
;; from you if you can think of ways to make this configuration better for the
;; next Noogler.
;;
;; If you want to learn more about Emacs at Google, see http://go/emacs.

;;; Code:
(setq inhibit-startup-screen t)
(setq load-prefer-newer t)

;; load ~/.emacs.d/lisp scripts (these are mine)
(add-to-list 'load-path "~/.emacs.d/lisp")
;; load all subdirectories in ~/emacs.d/lisp
;; these are packages I could not install with package manager
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
;; add custom themes to themes load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))


;; TODO: cancel auto-complete mode when company-mode is enabled, as they
;; interfere
;; core macros used for basic functionality
(require 'marcel-core-macros)

;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; loads packages only available at the office
;; uses the 'google package by default.
(with-library at-office)
(with-library helm-xref)
(with-library flycheck
  (global-flycheck-mode)
  (setq flycheck-pylintrc nil))

;; Remove the menu bar (get one extra line of real estate
(menu-bar-mode -1)
;; Enable the mouse when running inside xterm
(xterm-mouse-mode t)
;; Disable the audible bell
(setq visible-bell t)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; 3 lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling

(if window-system
    (progn
      (use-package default-text-scale
        :ensure t
        :config
        (default-text-scale-mode))
      (global-display-line-numbers-mode t)
      (custom-set-faces '(default ((t (:family "Azeret Mono")))))
      (load-theme 'company-dark t)
;;      (global-unset-key (kbd "C-x C-+"))
;;      (global-set-key (kbd "C-x C-+") '(lambda () (interactive) (change-font-height +2)))
;;      (global-unset-key (kbd "C-x C--"))
      ;;      (global-set-key (kbd "C-x C--") '(lambda () (interactive) (change-font-height -2)))
      (global-display-line-numbers-mode)
      (scroll-bar-mode -1)))
;; If we are in TMUX within an X environment
(if (and (getenv "TMUX") (getenv "DISPLAY"))
    ;; use xclip for copy-pasting
    (with-library xclip (xclip-mode 1)))
;; if running in a separate X window
(if (display-graphic-p)
    (with-library multi-term
      ;; start an emacs server so editors use an emacs buffer
      (setq-local server-name (concat "server" (getenv "DISPLAY")))
      (server-start)
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
                      (lambda () (interactive) (multi-term-dedicated-toggle))))
  (with-library in-tmux))

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "/usr/bin/google-chrome")

;; Command history configuration
(savehist-mode 1)
;; Save search strings across sessions
(setq savehist-additional-variables (list 'search-ring 'regexp-search-ring))
;; end: Command history configuration

;; desktop package configuration
(setq desktop-auto-save-timeout 10)
(setq desktop-path (list "~/.emacs.d/desktop-save/default"))
;; don't auto-load/auto-save previously stored desktop
(desktop-save-mode 0)
;; end: desktop package configuration

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
           (tags "-TODO={DONE\\|CANCELLED}"))
          ((org-agenda-tag-filter-preset '("+{WORK\\|BOTH}"))))
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

(use-package org-notify
  :ensure t
  :config
  (org-notify-add 'default
                  '(:time "1h" :actions -notify)
                  '(:time "15m" :actions -notify))
  (org-notify-add 'important
                  '(:time "4h" :actions -notify)
                  '(:time "1h" :actions -notify)
                  '(:time "15m" :actions -notify)
                  '(:time "2m" :actions -notify :audible t)
                  '(:time "-15m" :actions -notify))
  (org-notify-add 'event
                  '(:time "1d" :actions -notify)
                  '(:time "1h" :actions -notify))
  :init
  (org-notify-start))

;(use-package centaur-tabs :ensure t
;  :hook (emacs-startup . centaur-tabs-mode)
;  :init
;  (setq centaur-tabs-set-modified-marker t
;        centaur-tabs-set-modified-marker "*"
;        centaur-tabs-cycle-scope 'tabs)
;  :config
;  (centaur-tabs-mode t)
;  )

(use-package perspective
  ;:bind
  ;("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(with-library helm-config
  (helm-mode 1)
  (helm-autoresize-mode)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-s o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (set-face-attribute 'helm-selection nil
;;                      :background "purple"
                      :foreground "white"))
;; puts all backup files in the .emacs.d/backup directory, rather than on the
;; same folder as the file being edited
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)    ; and how many of the old

;; always enable identifications of JavaStyleWords
(global-subword-mode)
;; always enable refresh files from disk
(global-auto-revert-mode)
;; always enable whitespace visualization
;; (global-whitespace-mode)
(setq whitespace-line-column 80)
;; only highlight tab chars and trailing whitespace
(setq whitespace-style '(tab-mark trailing lines-tail face))

;; switch between windows quickly
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") (lambda () (interactive) (other-window -1)))
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-W") 'copy-to-remote-clipboard)

;; Wezterm Fixes: Ctrl+Alt+i (complete-symbol) registers as ESC <C-tab>
(global-set-key (kbd "ESC <C-tab>") 'complete-symbol)

;; swap windows
(with-library kbd-shortcut-functions
  (global-set-key (kbd "C-x t")
                  (lambda () (interactive) (rotate-windows 1)))
  ;; this one is not very useful, and we may want to re-use C-x T
  ;; to mean something else related to window management
  (global-set-key (kbd "C-x T")
                  (lambda () (interactive) (rotate-windows -1)))
  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "ESC <up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down)
  (global-set-key (kbd "ESC <down>") 'move-text-down))

;; switch between frames quickly
(global-unset-key (kbd "C-M-k"))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (other-frame 1)))
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") (lambda () (interactive) (other-frame -1)))

;; split windows with sensible keys
(global-unset-key (kbd "C-x |"))
(global-set-key (kbd "C-x |") (lambda () (interactive) (split-window-horizontally)))
(global-unset-key (kbd "C-x _"))
(global-set-key (kbd "C-x _") (lambda () (interactive) (split-window-vertically)))

;; Flashes the current line after navigating.
(use-package nav-flash
  :ensure t
  :config)

(use-package lsp-treemacs
  :ensure t
  :config)

(use-package lsp-ui
  :ensure t
  :config)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :config)

(use-package imenu-list
  :ensure t)

(use-package rotate
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-x M--")
                    (lambda () (interactive) (rotate:even-vertical)))
    (global-set-key (kbd "C-x M-\\")
                      (lambda () (interactive) (rotate:even-horizontal)))))

(with-library at-office
  (if at-office-is-work-laptop
      (progn
        (use-package elpy
          :ensure t)

        (use-package jedi
          :ensure t
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
              (setq jedi:setup-keys t)
              (setq jedi:complete-on-dot t)
              (setq-local whitespace-line-column 100))
            (add-hook 'python-mode-hook 'jedi:setup)
            (add-hook 'python-mode-hook 'jedi:python-mode-hook))))))

(use-package company
  :ensure t
  :config
  ;; Use company mode whenever we have eglot
  (if (not at-office-is-work-laptop)
      (progn
        (add-hook 'python-mode-hook 'company-mode))))

;; configure bash shellcheck
(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; configure the status bar using telephone-line
(use-package telephone-line
  :ensure t
  :config
  (require 'telephone-line)
  (require 'telephone-line-config)
  (setq telephone-line-lhs
        '((nil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (evil    . (telephone-line-minor-mode-segment
                      telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode t))

;;(with-library auto-complete (global-auto-complete-mode))
(with-library undo-tree
  ;; remap undo-redo using undo-tree
  (global-undo-tree-mode)
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")
                  (lambda () (interactive) (undo-tree-undo)))
  (global-unset-key (kbd "M-Z"))
  (global-set-key (kbd "M-Z")
                  (lambda () (interactive) (undo-tree-redo))))

(with-library multiple-cursors
  ;; extended mode to mark instances, multiple M-3 simply add forward
  (global-set-key (kbd "M-3") 'mc/mark-more-like-this-extended)
  ;; mark instances backwards of whatever is marked in the region
  (global-set-key (kbd "M-#") 'mc/mark-previous-like-this)
  ;; mark all instances of whatever is marked in the region
  (global-set-key (kbd "C-x M-3") 'mc/mark-all-dwim)
  ;; create a separate caret for every line under the cursor
  (global-set-key (kbd "C-x M-l") 'mc/edit-lines))

(defun custom-java-mode-hook ()
  ;; TODO: Make this only work for .java files, but not for other
  ;;       types of files opened after entering java-mode
  (setq whitespace-line-column 100)
  (let ((oldmap (cdr (assoc 'java-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "M-j") nil)
    (define-key newmap (kbd "M-j") (lambda () (interactive) (other-window -1)))
    (define-key newmap (kbd "M-k") nil)
    (define-key newmap (kbd "M-k") (lambda () (interactive) (other-window 1)))
    (define-key newmap (kbd "C-M-j") nil)
    (define-key newmap (kbd "C-M-j") (lambda () (interactive) (other-frame -1)))
    (define-key newmap (kbd "C-M-k") nil)
    (define-key newmap (kbd "C-M-k") (lambda () (interactive) (other-frame 1)))

    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(java-mode . ,newmap) minor-mode-overriding-map-alist)
  ))

(add-hook 'java-mode-hook 'custom-java-mode-hook)

;; set indentation configuration
(defun set-custom-indent ()
  (setq-local standard-indent 2)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local cperl-indent-level 2)
  (setq-local c-basic-offset 2)
  (setq-local sh-basic-offset 2)
  (setq-local js2-basic-offset 2)
  (setq-local js2-bounce-indent-p t)
  (setq-local js-indent-level 2)
  (setq-local graphviz-dot-indent-width 2))
(add-hook 'prog-mode-hook 'set-custom-indent)

;;; .emacs ends here

;; theme configuration starts here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("6068d911f0ad3f9e6834d4849038ef3a317510f23683ff9656da7d49a5ab3ed5" "d4890c4d8d262c61decb7c0e43b1dc5c92b378e9acada6c04d9e94f00cc70ead" "4badd47b5ba16df46b849137903f2210d344f3c7021e979ff8ed68b8c3827d84" default))
 '(graphviz-dot-indent-width 2 t)
 '(line-number-mode t)
 '(package-selected-packages
   '(jedi elpy rotate lua-mode telephone-line use-package multiple-cursors multi-term helm-flycheck helm-xref windresize async xclip undo-tree))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Azeret Mono"))))
 '(clang-include-fixer-highlight ((t (:background "white"))))
 '(undo-tree-visualizer-active-branch-face ((t (:foreground "color-231" :weight bold))))
 '(undo-tree-visualizer-default-face ((t (:foreground "brightwhite"))))
 '(whitespace-line ((t (:background "color-238")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
