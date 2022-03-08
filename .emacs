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
(setq inhibit-startup-screen t)
(menu-bar-mode -1)

;; initialize packages
(require 'cl)
(require 'marcel-macros)
;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(with-library cl-generic)
;; (with-library better-defaults)
(with-library use-package)
(with-library use-package-ensure)
(with-library tail-buffer)

;; Core emacs packages configuratons
;; Don't ask to reload TAGS file.
(setq tags-revert-without-query 1)

;;; re-binds certain keys when inside a TMUX session
(if (display-graphic-p)
    ;; if emacs is run as a GUI window
    (use-package multi-term
      :ensure t
      :config
      (with-library multi-term
        ;; start an emacs server so editors use an emacs buffer
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
                        (lambda () (interactive) (multi-term-dedicated-toggle)))))
  ;; if emacs is run as a terminal application
  (with-library in-tmux
    (use-package helm-dash
      :ensure t
      :config
      (with-library helm-dash
        (setq helm-dash-browser-func 'eww)))))

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
(use-package elpy :ensure t)
(use-package flycheck :ensure t)
;;  (defun flycheck-set-pylint ()
;;    (setq flycheck-checker 'python-pylint))
;;  (add-hook 'python-mode-hook 'flycheck-set-pylint))
(use-package flyspell-correct
  :ensure t
  :config
  (with-library flyspell-correct
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)))

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
    (defun set-prog-column-limit () (setq-local whitespace-line-column 80))
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
    (add-hook 'python-mode-hook 'jedi:python-mode-hook)))
;; always enable identification of JavaStyleWords
(global-subword-mode)
;; show column number on status bar
(column-number-mode)
;; automatically refresh file if they are changed from underneath
(global-auto-revert-mode)

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
  (with-library helm-config
    (helm-mode 1)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-s o") 'helm-occur)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (set-face-attribute 'helm-selection nil
                        :background "brightblack"
                        :foreground "green")))

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
 '(elpy-formatter 'black)
 '(elpy-rpc-virtualenv-path 'current)
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
 '(company-preview-common ((t (:inherit (company-tooltip-selection company-tooltip)))))
 '(company-scrollbar-bg ((t (:background "cyan"))))
 '(company-scrollbar-fg ((t (:background "brightcyan"))))
 '(company-tooltip ((t (:background "black" :foreground "color-244"))))
 '(company-tooltip-common ((t (:foreground "brightwhite" :weight extra-bold))))
 '(company-tooltip-selection ((t (:background "brightblack"))))
 '(helm-selection ((t (:background "brightblack" :foreground "green"))))
 '(region ((t (:background "color-235"))))
 '(whitespace-line ((t (:background "color-237" :foreground "color-250")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; .emacs ends here
