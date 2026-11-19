;;; native-comp-darwin.el --- Keep native compilation working on macOS 26+ -*- lexical-binding: t; -*-

;;; Commentary:

;; libgccjit runs an embedded GCC driver that guesses the default
;; -mmacosx-version-min from the Darwin kernel version: macOS = Darwin - 9.
;; That held for macOS 11-15 (Darwin 20-24), but Apple then jumped to
;; macOS 26.  On macOS 27 (Darwin 27) the driver asks the assembler for
;; macOS 18.0; Apple clang (Xcode 26 and later) rejects every version from
;; 16 to 25 as "invalid version number", so each native compilation fails
;; with "libgccjit.so: error: error invoking gcc driver".
;;
;; Passing the real macOS version in `native-comp-driver-options' skips the
;; guess.  The variable must already be bound when compilation starts:
;; async JIT workers are spawned by comp-run.el without loading comp.el
;; (which defines the variable) and only inherit it if it is bound.  So this
;; file sets it right away, starting from Emacs' darwin default; comp.el's
;; defcustom keeps a pre-bound value.  Loaded from early-init.el, i.e. before
;; package activation can trigger the first JIT compilation.
;;
;; Does nothing on other systems, on macOS 15 and older (where GCC's guess
;; is right), or when a -mmacosx-version-min is already set.  Drop it once
;; Emacs.app bundles a libgccjit that maps Darwin to macOS 26+ correctly.
;;
;; Test: ~/scripts/tests/test_emacs_native_comp.sh

;;; Code:

(require 'seq)

(defvar native-comp-driver-options)

(defconst native-comp-darwin--emacs-default '("-Wl,-w")
  "Emacs' own darwin default for `native-comp-driver-options' (see comp.el).")

(defun native-comp-darwin--macos-major ()
  "Return the major version of the running macOS as an integer, or nil."
  (or (ignore-errors                    ; ~0.05 ms
        (with-temp-buffer
          (insert-file-contents
           "/System/Library/CoreServices/SystemVersion.plist")
          (when (re-search-forward
                 "<key>ProductVersion</key>[[:space:]]*<string>\\([0-9]+\\)\\."
                 nil t)
            (string-to-number (match-string 1)))))
      (ignore-errors                    ; ~10 ms
        (let ((version (car (process-lines "/usr/bin/sw_vers"
                                           "-productVersion"))))
          (when (string-match "\\`\\([0-9]+\\)\\." version)
            (string-to-number (match-string 1 version)))))))

(defun native-comp-darwin-set-version-min ()
  "Append -mmacosx-version-min=<macOS major>.0 to `native-comp-driver-options'.
Skip it on macOS 15 and older, or if a -mmacosx-version-min is already set."
  (let ((major (native-comp-darwin--macos-major))
        (options (if (boundp 'native-comp-driver-options)
                     native-comp-driver-options
                   native-comp-darwin--emacs-default)))
    (when (and major
               (> major 15)             ; macOS 26+; there is no 16-25.
               (not (seq-some (lambda (opt)
                                (string-prefix-p "-mmacosx-version-min=" opt))
                              options)))
      (setq native-comp-driver-options
            (append options
                    (list (format "-mmacosx-version-min=%d.0" major)))))))

(when (and (eq system-type 'darwin)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (native-comp-darwin-set-version-min))

(provide 'native-comp-darwin)
;;; native-comp-darwin.el ends here
