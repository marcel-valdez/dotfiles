
;;; company-dark-theme -- Sets up dark theme for company-mode

;;; Commentary:
;; Currently used solely for the Company tooltip

;;; Code:
(deftheme company-dark
  "Dark theme for company-mode tooltip")

(let ((class '((class color) (min-colors 257)))
      (terminal-class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'company-dark

   ;; Additional modes
   ;; Company tweaks.
   '(company-tooltip
     ((t :inherit default
         :background "#403D3D")))

   '(company-scrollbar-bg
     ((t :background "#232526")))

   '(company-scrollbar-fg
     ((t :background "#E6DB74")))

   '(company-tooltip-selection
     ((t :inherit font-lock-function-name-face)))

   '(company-tooltip-common
     ((t :inherit font-lock-constant-face)))

   ;; modifies the comment fonts
   ;;'(font-lock-comment-face ((t (:foreground "#888888" :slant italic))))

))

(provide-theme 'company-dark)
;;; company-dark-theme.el ends here
