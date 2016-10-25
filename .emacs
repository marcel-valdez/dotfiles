;;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; Welcome to Emacs (http://go/emacs).
;;
;; If you see this file, your homedir was just created on this workstation.
;; That means either you are new to Google (in that case, welcome!) or you
;; got yourself a faster machine.
;;
;; Either way, the main goal of this configuration is to help you be more
;; productive; if you have ideas, praise or complaints, direct them to
;; emacs-users@google.com (http://g/emacs-users).  We'd especially like to hear
;; from you if you can think of ways to make this configuration better for the
;; next Noogler.
;;
;; If you want to learn more about Emacs at Google, see http://go/emacs.

;;; Code:

;; load ~/.emacs.d/lisp scripts (these are mine)
(add-to-list 'load-path "~/.emacs.d/lisp")
;; load all subdirectories in ~/emacs.d/lisp
;; these are packages I could not install with package manager
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Use the 'google' package by default.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)
(add-to-list 'package-archives '("marmalade", "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa", "https://melpa.org/packages/"))

;; core macros used for basic functionality
(require 'marcel-core-macros)
;; loads packages only available at the office
(with-library at-office)

;; if running in a separate X window
(if (display-graphic-p)
    (progn (require 'multi-term)
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
  (progn (require 'in-tmux)))
(require 'undo-tree)
(require 'multiple-cursors)

;; start an emacs server so that new emacs ansi-term can open a new
;; buffer instead of opening emacs within emacs

;;;; global editing settings and overrides
(global-auto-complete-mode)
(global-undo-tree-mode)
;; always enable identifications of JavaStyleWords
(global-subword-mode)
;; always enable whitespace visualization
(global-whitespace-mode)
(setq whitespace-line-column 80)
(setq whitespace-style '(tab-mark trailing lines-tail face))
;; show the current line number in the status bar

;;;; custom bindings
;; switch between windows quickly
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") (lambda () (interactive) (other-window 1)))
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window -1)))

;; switch between frames quickly
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") (lambda () (interactive) (other-frame 1)))
(global-unset-key (kbd "C-M-k"))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (other-frame -1)))

;; remap undo-redo using undo-tree
(with-library undo-tree
              (global-unset-key (kbd "C-/"))
              (global-set-key (kbd "C-/")
                              (lambda (interactive) (undo-tree-undo)))
              (global-unset-key (kbd "C-."))
              (global-set-key (kbd "C-.")
                              (lambda () (interactive) (undo-tree-redo))))

;;; start multiple-cursors bindings
;; extended mode to mark instances, multiple M-3 simply add forward
(global-set-key (kbd "M-3") 'mc/mark-more-like-this-extended)
;; mark instances backwards of whatever is marked in the region
(global-set-key (kbd "M-#") 'mc/mark-previous-like-this)
;; mark all instances of whatever is marked in the region
(global-set-key (kbd "C-x M-3") 'mc/mark-all-dwim)
;; create a separate caret for every line under the cursor
(global-set-key (kbd "C-x M-l") 'mc/edit-lines)

(defun custom-java-mode-hook ()
  ;; TOOD: Make this only work for .java files, but not for other
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
;;; .emacs ends here

;; theme configuration starts here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(tab-width 2)
 '(line-number-mode t)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
