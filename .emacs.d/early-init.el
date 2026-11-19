;;; early-init.el --- Runs before package activation and ~/.emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Only settings that must be in place before packages are activated
;; belong here; everything else stays in ~/.emacs.

;;; Code:

;; Native compilation on macOS 26+ (see the file's commentary).
(load (expand-file-name "lisp/native-comp-darwin" user-emacs-directory) t t)

;;; early-init.el ends here
