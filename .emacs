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
(add-to-list 'package-archives '("elpa" . "http://elpa.org/packages/"))


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
  (global-flycheck-mode))

;; Remove the menu bar (get one extra line of real estate
(menu-bar-mode -1)
;; If we are in TMUX within an X environment
(if (and (getenv "TMUX") (getenv "DISPLAY"))
    ;; use xclip for copy-pasting
    (with-library xclip (xclip-mode 1)))
;; if running in a separate X window
(if (display-graphic-p)
    (with-library multi-term
      ;; start an emacs server so editors use an emacs buffer
      (setq-local server-name (concatenate 'string "server" (getenv "DISPLAY")))
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

(with-library helm-config
  (helm-mode 1)
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
      kept-old-versions 5    ; and how many of the old
      )


;; always enable identifications of JavaStyleWords
(global-subword-mode)
;; always enable refresh files from disk
(global-auto-revert-mode)
;; always enable whitespace visualization
(global-whitespace-mode)
(setq whitespace-line-column 80)
;; only highlight tab chars and trailing whitespace
(setq whitespace-style '(tab-mark trailing lines-tail face))

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
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") (lambda () (interactive) (other-frame 1)))
(global-unset-key (kbd "C-M-k"))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (other-frame -1)))

;; split windows with sensible keys
(global-unset-key (kbd "C-x |"))
(global-set-key (kbd "C-x |") (lambda () (interactive) (split-window-horizontally)))
(global-unset-key (kbd "C-x _"))
(global-set-key (kbd "C-x _") (lambda () (interactive) (split-window-vertically)))


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
 '(google-lsp-kythe-server "/google/data/ro/teams/grok/tools/kythe_languageserver")
 '(graphviz-dot-indent-width 2 t)
 '(line-number-mode t)
 '(package-selected-packages
   '(telephone-line use-package multiple-cursors multi-term helm-flycheck helm-xref windresize async xclip undo-tree))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-include-fixer-highlight ((t (:background "white")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(load-theme 'company-dark)
