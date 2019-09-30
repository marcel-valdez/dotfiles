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
;;(add-to-list 'package-archives '("elpa-gnu", "http://elpa.gnu.org/packages/"))
(setq inhibit-startup-screen t)

;; initialize packages
(require 'marcel-macros)
;; (with-library better-defaults)

;;; re-binds certain keys when inside a TMUX session
(if (display-graphic-p)
    ;; if emacs is run as a GUI window
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
                      (lambda () (interactive) (multi-term-dedicated-toggle))))
  ;; if emacs is run as a terminal application
  (with-library in-tmux))

(with-library xclip (xclip-mode))
(with-library multiple-cursors)
(with-library helm-ls-git)
(with-library elpy)
(with-library flycheck
  (setq flycheck-checker 'python-pylint))
(with-library flyspell-correct
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper))
(with-library markdown-mode
  (defun markdown-enable-flyspell()
    (flyspell-mode))
  (add-hook 'markdown-mode-hook 'markdown-enable-flyspell))


;; set whitespace visualization
(with-library whitespace
  (global-whitespace-mode)
  (setq show-trailing-whitespace t)
  (setq whitespace-style '(tab-mark trailing lines-tail face))
  (defun set-prog-column-limit () (setq-local whitespace-line-column 80))
  (add-hook 'prog-mode-hook 'set-prog-column-limit)
  (defun set-text-column-limit () (setq-local whitespace-line-column 100))
  (add-hook 'text-mode-hook 'set-text-column-limit))

;; set indentation configuration
(defun set-custom-indent ()
  (setq-local js2-basic-offset 2)
  (setq-local js2-bounce-indent-p t)
  (setq-local js-indent-level 2)
  (setq-local cperl-indent-level 2)
  (setq-local sh-basic-offset 2)
  (setq-local sh-indentation 2)
  (setq-local smie-indent-basic 2)
  (setq-local c-basic-offset 2)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq-local css-indent-offset 2))
(add-hook 'prog-mode-hook 'set-custom-indent)


;;;; global editing settings and overrides
;; use undo-tree instead of built-in undo
(with-library undo-tree
  (global-undo-tree-mode))
;; always enable auto-complete
(with-library auto-complete)
;;  (add-hook 'after-init-hook 'global-auto-complete-mode))
;; company-mode
(with-library company)
;;  (add-hook 'after-init-hook (lambda () (global-company-mode -1))))

(with-library jedi
  (defun jedi:python-mode-hook ()
    (setq elpy-modules
          (delq 'elpy-module-company
                (delq 'elpy-module-flymake elpy-modules)))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-hook 'elpy-mode-hook 'auto-complete-mode)
    (company-mode -1)
    (elpy-mode t)
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)
    (setq-local whitespace-line-column 100))
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:python-mode-hook))
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
(with-library helm-config
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-s o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list))

;; configure rotate-window
(with-library rotate
  (global-set-key (kbd "C-x t")
                  (lambda () (interactive) (rotate-window)))
  ;; switch to vertical split layout
  (global-set-key (kbd "C-x M-\\") 'rotate:even-horizontal)
  ;; switch to horizontal split layout
  (global-set-key (kbd "C-x M--") 'rotate:even-vertical)
  ;; cycle through layouts
  (global-set-key (kbd "C-x l")
                  (lambda () (interactive) (rotate-layout))))
;; cycle through window sizes
(with-library cycle-resize
  (global-set-key (kbd "C-x -") 'cycle-resize-window-vertically)
  (global-set-key (kbd "C-x \\") 'cycle-resize-window-horizontally)
  (setq cycle-resize-steps '(75 66 50 33)))

;; remap undo-redo using undo-tree
(with-library undo-tree
              (global-unset-key (kbd "C-/"))
              (global-set-key (kbd "C-/")
                              (lambda () (interactive) (undo-tree-undo)))
              (global-unset-key (kbd "C-."))
              (global-set-key (kbd "C-.")
                              (lambda () (interactive) (undo-tree-redo))))

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
              (global-set-key (kbd "C-x M-l") 'mc/edit-lines))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xclip helm git helm-grepint helm-helm-commands helm-ispell helm-ls-git helm-proc helm-pydoc helm-rubygems-org helm-themes helm-wordnet helm-xref hgignore-mode undo-tree rotate rjsx-mode multiple-cursors multi-term markdownfmt markdown-toc markdown-preview-mode helm-git gtags flycheck-tip flycheck-package cycle-resize auto-complete)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-face ((t (:inherit popup-face)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; .emacs ends here
