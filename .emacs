(setq inhibit-startup-screen 1)

;; package installer settings
(add-to-list 'load-path "~/.emacs.d/lisp/")

(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; initialize packages

;; re-binds certain keys when inside a TMUX session
(require 'marcel-macros)
(with-library auto-complete
  ;; always enable auto-complete
  (global-auto-complete-mode))
;; always enable identification of JavaStyleWords
(global-subword-mode)
(with-library whitespace
  ;; always enable whitespace visualization
  (global-whitespace-mode)
  ;; configure recommened column width
  (setq whitespace-line-column 80)
  ;; show tabs, trailing whitespace and long lines (+80)
  (setq whitespace-style '(tab-mark trailing lines-tail face)))

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

(with-library semantic
  (setq top-level-semantic-c-include-paths
        '("/cygdrive/c/Program Files (x86)/Arduino For Microduino/hardware/Microduino"
          "/usr/include"
          "/cygdrive/c/Users/marce/OneDrive/Documentos/Arduino/libraries"))
  (setq semantic-c-dependency-system-include-path
        (add-subdirs-of-all-top-level-dirs top-level-semantic-c-include-paths)))

;; remap undo-redo using undo-tree
(with-library undo-tree
  ;; global editing settings and overrides
  ;; use undo-tree instead of built-in undo
  (global-undo-tree-mode)
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/") (lambda () (interactive) (undo-tree-undo)))
  (global-unset-key (kbd "C-."))
  (global-set-key (kbd "C-.")
                  (lambda () (interactive) (undo-tree-redo))))
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

;; if running in a separate X window
(if (display-graphic-p)
    (progn
      ;; start an emacs server so editors use an emacs buffer
      (server-start)
      ;; start multi-term custom configurations
      (with-library multi-term
        (global-unset-key (kbd "C-t"))
        (add-to-list 'term-unbind-key-list "C-t")
        (add-to-list 'term-unbind-key-list "M-j")
        (add-to-list 'term-unbind-key-list "M-k")
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
  (progn (with-library in-tmux)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (multi-term ac-c-headers ac-clang csharp-mode dos undo-tree multiple-cursors company-arduino auto-complete)))
 '(show-paren-mode t)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))
(put 'upcase-region 'disabled nil)

