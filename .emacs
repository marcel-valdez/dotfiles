;; package installer settings
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp/"))

(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq inhibit-startup-screen t)

;; initialize packages
(require 'marcel-macros)
(require 'undo-tree)
(require 'auto-complete)
(require 'multiple-cursors)
(require 'whitespace)
;; re-binds certain keys when inside a TMUX session
(require 'in-tmux)


;;;; global editing settings and overrides
;; use undo-tree instead of built-in undo
(with-library undo-tree
  (global-undo-tree-mode))
;; always enable auto-complete
(global-auto-complete-mode)
;; always enable identification of JavaStyleWords
(global-subword-mode)
;; always enable whitespace visualization
(global-whitespace-mode)
;; configure recommened column width
(setq whitespace-line-column 80)
;; show column number on status bar
(column-number-mode)
;; show tabs, trailing whitespace and long lines (+80)
(setq whitespace-style '(tab-mark trailing lines-tail face))
;; automatically refresh file if they are changed from underneath
(global-auto-revert-mode)

;;;; custom bindings
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
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
