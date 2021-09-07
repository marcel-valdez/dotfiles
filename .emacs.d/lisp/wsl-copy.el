;;; wsl-copy.el --- Summary
;;; WSL Linux Clipboard functionality.

;;; Commentary:
;;; This package contains functions required to copy/paste on the clipboard for
;;; Emacs running in Linux under the Windows Subsystem for Linux.

;;; Code:
;;; TODO: Check if /usr/bin/xclip exists.
;;; TODO: Use DISPLAY variable if available, :0 otherwise


(defun wsl-copy/check-display ()
  "Set the DISPLAY environment variable if not set already."
    (if (not (getenv "DISPLAY"))
        (setenv "DISPLAY" ":0")))

(defun wsl-copy/copy-region (start end)
  "Paste the selected region in the host Windows system."
  (interactive "r")
  (wsl-copy/check-display)
  (if (use-region-p)
      (call-process-region start end "/usr/bin/xclip"))
  (message "Copied region onto clipboard."))

(defun wsl-copy/copy-kill-ring ()
  "Paste the Emacs kill ring in the host Windows system."
  (wsl-copy/check-display)
  (process-send-string "/usr/bin/xclip" kill-ring)
  (message "Copied kill ring onto clipboard."))


(defun wsl-copy/paste ()
  "Copy the Windows clipboard onto the current buffer."
  (interactive)
  (wsl-copy/check-display)
  (mapc 'insert (shell-command-to-string "/usr/bin/xclip -o"))
  (message "Pasted clipboard onto buffer."))


(provide 'wsl-copy)
;;; wsl-copy.el ends here