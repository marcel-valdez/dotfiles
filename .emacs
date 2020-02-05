;;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(setq inhibit-startup-screen t)

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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.org/packages/"))

;; core macros used for basic functionality
(require 'marcel-core-macros)
;; core package required by many other packages
(with-library cl)
;; loads packages only available at the office
(with-library at-office)
;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (with-library better-defaults)
(with-library use-package)
(with-library use-package-ensure)

(use-package helm-xref :ensure t)
(use-package flycheck
  :ensure t
  :config
  (with-library flycheck
    (global-flycheck-mode)))

;; Remove the menu bar (get one extra line of real estate
(menu-bar-mode -1)
;; If we are in TMUX within an X environment
(if (and (getenv "TMUX") (getenv "DISPLAY"))
    (if (is-wsl-p)
        ;; if we're in Linux WSL, use custom clipboard scripts
        (use-package wsl-copy
          :load-path "lisp/"
          ;; setup keybindings
          :bind (("C-x M-c" . wsl/copy-region)
                 ("C-x M-v" . wsl/copy-paste))
          :config
          (with-library wsl-copy))
      (progn
        ;; use xclip for copy-pasting
        (use-package xclip
          :ensure t
          :config
          (with-library xclip (xclip-mode 1)))
        ;; if xclip is not available for whatever reason, use simpleclip
        (use-package simpleclip
          :if (not (featurep 'xclip))
          :ensure t
          :config
          ;; double-check xclip does not exist, in case there are race-conditions
          (unless (featurep 'xclip)
            (with-library simpleclip (simpleclip-mode 1)))))))
;; if running in a separate X window
(if (display-graphic-p)
    (use-package multi-term
      :ensure t
      :config
      (with-library multi-term
        ;; start an emacs server so editors use an emacs buffer
        (setq-local server-name (concatenate 'string "server" (getenv "DISPLAY")))))
  (progn
    (use-package multi-term
      :ensure t
      :config
      (with-library multi-term
        ;; start an emacs server so editors use an emacs buffer
        (setq-local server-name "default")
        (with-library in-tmux)))))

(with-library server
  (unless (server-running-p server-name)
    (server-start)))

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "~/modules/chrome-remote-scripts/open-cantata-url")

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

;; configure ispell
(use-package ispell
  :ensure t
  :config
  (with-library ispell
    (setq ispell-program-name "/usr/bin/aspell")))

(use-package helm
  :ensure t
  :config
  (with-library helm
    (helm-mode 1)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-s o") 'helm-occur)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (set-face-attribute 'helm-selection nil
                        :background "purple"
                        :foreground "white")))

;; puts all backup files in the .emacs.d/backup directory, rather than on the
;; same folder as the file being edited
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package multi-term
  :ensure t
  :config
  (with-library multi-term
    ;; start multi-term custom configurations
    ;; (setq server-window 'pop-to-buffer)
    (setq server-window 'display-buffer-other-frame)
    (global-unset-key (kbd "C-t"))
    (add-to-list 'term-unbind-key-list "C-t")
    (setq multi-term-program "/bin/bash")
    ;; start new terminal
    (global-set-key (kbd "C-t C-n") 'multi-term)
    ;; switch to next terminal within same buffer
    (global-set-key (kbd "C-t TAB")
                    (lambda () (interactive) (exec-temp-bind
                                              'multi-term-next
                                              "TAB")))
    ;; switch to previous terminal within same buffer
    (global-set-key (kbd "C-t S-TAB")
                    (lambda () (interactive) (exec-temp-bind
                                              'multi-term-prev
                                              "S-TAB")))
    ;; toggle showing/hiding the dedica
    (global-set-key (kbd "C-t C-d")
                    (lambda () (interactive) (exec-temp-bind
                                              'multi-term-dedicated-toggle
                                              "C-d")))))

;; always enable identifications of JavaStyleWords
(global-subword-mode)
;; always enable refresh files from disk
(global-auto-revert-mode)
;; always enable whitespace visualization
(global-whitespace-mode)
(setq whitespace-line-column 80)
;; only highlight tab chars and trailing whitespace
(setq whitespace-style '(tab-mark trailing lines-tail face))
;; configure the status bar using telephone-line
(use-package telephone-line
             :ensure t
             :config
             (require 'telephone-line)
             (require 'telephone-line-config)
               (telephone-line-mode t))

;; switch between windows quickly
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") (lambda () (interactive) (other-window 1)))
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window -1)))

;; swap windows
(with-library kbd-shortcut-functions
  (global-set-key (kbd "C-x t")
                  (lambda () (interactive) (rotate-windows 1)))
  ;; this one is not very useful, and we may want to re-use C-x T
  ;; to mean something else related to window management
  (global-set-key (kbd "C-x T")
                  (lambda () (interactive) (rotate-windows -1)))
  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down))

;; switch between frames quickly
(defun init:next-frame ()
  "Move focus to the next frame."
  (interactive)
  (other-frame 1))
(defun init:prev-frame ()
  "Move focus to the previous frame."
  (interactive)
  (other-frame -1))
(global-unset-key (kbd "M-C-j"))
(global-set-key (kbd "M-C-j") 'init:next-frame)
(global-unset-key (kbd "M-C-k"))
(global-set-key (kbd "M-C-k") 'init:prev-frame)

;; split windows with sensible keys
(global-unset-key (kbd "C-x \\"))
(global-set-key (kbd "C-x \\") (lambda () (interactive) (split-window-horizontally)))
(global-unset-key (kbd "C-x -"))
(global-set-key (kbd "C-x -") (lambda () (interactive) (split-window-vertically)))
;; resize windows with sensible keys
(use-package cycle-resize
  :ensure t
  :config
  (with-library cycle-resize
    (global-unset-key (kbd "C-x C-_"))
    (global-set-key (kbd "C-x C-_")
                    (lambda () (interactive) (exec-temp-bind
                                              'cycle-resize-window-vertically
                                              "C-_")))
    (global-unset-key (kbd "C-x C-\\"))
    (global-set-key (kbd "C-x C-\\")
                    (lambda () (interactive) (exec-temp-bind
                                              'cycle-resize-window-horizontally
                                              "C-\\")))
    ;; You also can configure the dimensions (in %) the package will cycle through
    ;; By default, it is: 80% -> 50% -> 20% -> 50%, and so on...
    (setq cycle-resize-steps '(75 50 25 50))))

;;; Configure company
;;; It should be disabled when auto-complete mode is activated.
(use-package company
  :ensure t
  :config
  (with-library company
    (with-library auto-complete
      (defun disable-company () (company-mode -1))

      (add-hook 'auto-complete-mode 'disable-company))))

;;; Enable auto-complete by default, but if company-mode is enabled, then
;;; disable auto-complete mode.
(use-package auto-complete
  :ensure t
  :config
  (with-library auto-complete
    (global-auto-complete-mode)
    (with-library company
      (defun disable-auto-complete () (auto-complete-mode -1))
      (add-hook 'company-mode-hook 'disable-auto-complete))))

(use-package linear-undo
  :ensure t
  :config
  (with-library linear-undo
    (linear-undo-mode t)
    (global-unset-key (kbd "C-_"))
    (global-set-key (kbd "C-_")
                    (lambda () (interactive) (linear-undo/undo-1)))
    (global-unset-key (kbd "M--"))
    (global-set-key (kbd "M--")
                    (lambda () (interactive) (linear-undo/redo-1)))))

(use-package multiple-cursors
  :ensure t
  :config
  (with-library multiple-cursors
    ;; extended mode to mark instances, multiple M-3 simply add forward
    (global-set-key (kbd "M-3") 'mc/mark-more-like-this-extended)
    ;; mark instances backwards of whatever is marked in the region
    (global-set-key (kbd "M-#") 'mc/mark-previous-like-this)
    ;; mark all instances of whatever is marked in the region
    (global-set-key (kbd "C-x M-3") 'mc/mark-all-dwim)
    ;; create a separate caret for every line under the cursor
    (global-set-key (kbd "C-x M-l") 'mc/edit-lines)))

(defun custom-java-mode-hook ()
  "Set options that only apply to 'java-mode'."
  ;; TODO: Make this only work for .java files, but not for other
  ;;       types of files opened after entering java-mode
  (setq whitespace-line-column 100)
  (let ((oldmap (cdr (assoc 'java-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "M-j") nil)
    (define-key newmap (kbd "M-j") (lambda () (interactive) (other-window 1)))
    (define-key newmap (kbd "M-k") nil)
    (define-key newmap (kbd "M-k") (lambda () (interactive) (other-window -1)))
    (define-key newmap (kbd "C-M-j") nil)
    (define-key newmap (kbd "C-M-j") (lambda () (interactive) (other-frame 1)))
    (define-key newmap (kbd "C-M-k") nil)
    (define-key newmap (kbd "C-M-k") (lambda () (interactive) (other-frame -1)))

    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(java-mode . ,newmap) minor-mode-overriding-map-alist)
  ))

(add-hook 'java-mode-hook 'custom-java-mode-hook)

;; Start server with: omnisharp-start-omnisharp-server
(use-package omnisharp
  :ensure t
  :config
  (with-library omnisharp
    (with-library company
      (eval-after-load
          'company
        '(add-to-list 'company-backends 'company-omnisharp)))

    (defun marcel-csharp-mode-setup ()
      ;; disable auto-complete mode and enable company-mode
      (with-library auto-complete (auto-complete-mode nil))
      (with-library company (company-mode t))
      (with-library flycheck (flycheck-mode t))
      (omnisharp-mode t)
      (local-set-key (kbd "M-,") 'pop-tag-mark)
      (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
      (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
      (local-set-key (kbd "C-c C-c") 'recompile))

    (with-library csharp-mode
      (add-hook 'csharp-mode-hook 'marcel-csharp-mode-setup))))

(defun configure-general-programming ()
  "Configures settings that apply to any programming language.
At the moment it configures indentation and paren highlighting"

  (with-library paren
    (show-paren-mode t))
  ;; set indentation configuration
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

(add-hook 'prog-mode-hook 'configure-general-programming)

;;; .emacs ends here

;; theme configuration starts here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-tasks company-reviewers company-bbdb company-nxml company-css company-capf
		   (company-dabbrev-code company-keywords))))
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("6068d911f0ad3f9e6834d4849038ef3a317510f23683ff9656da7d49a5ab3ed5" "d4890c4d8d262c61decb7c0e43b1dc5c92b378e9acada6c04d9e94f00cc70ead" "4badd47b5ba16df46b849137903f2210d344f3c7021e979ff8ed68b8c3827d84" default)))
 '(graphviz-dot-indent-width 2 t)
 '(line-number-mode t)
 '(package-selected-packages
   (quote
    (async chef-mode company cycle-resize graphviz-dot-mode helm helm-mt helm-xref hl-anything linear-undo markdown-mode markdown-preview-eww markdown-preview-mode markdownfmt mc-extras multi-term multiple-cursors omnisharp powershell use-package windresize xclip)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-include-fixer-highlight ((t (:background "white"))))
 '(company-preview-common ((t (:inherit (company-tooltip-selection company-tooltip)))))
 '(company-scrollbar-bg ((t (:background "cyan"))))
 '(company-scrollbar-fg ((t (:background "brightcyan"))))
 '(company-tooltip ((t (:background "black" :foreground "color-244"))))
 '(company-tooltip-common ((t (:foreground "brightwhite" :weight extra-bold))))
 '(company-tooltip-selection ((t (:background "brightblack"))))
 '(helm-selection ((t (:background "brightblack" :foreground "green"))))
 '(region ((t (:background "color-236"))))
 '(whitespace-line ((t (:background "color-239" :foreground "color-250")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(load-theme 'company-dark)
(put 'scroll-left 'disabled nil)
