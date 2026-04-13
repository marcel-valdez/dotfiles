;;; package --- org-mode-config
;;; Commentary:
;;;    This package provides my personal org-mode config which implements
;;;    a customized GTD workflow for software engineering task management
;;;    and planning.

;;; Code:
(use-package org
  :config
  (defun custom:org-mode-hook ()
    (org-indent-mode t))

  (add-hook 'org-mode-hook 'custom:org-mode-hook)
  (require 'org-inlinetask)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-deadline-warning-days 7)
  (setq org-tags-match-list-sublevels 'indented)
  ;; Also see: (describe-variable 'org-stuck-projects)
  (setq org-stuck-projects
      '("+PROJECT/-DEFERRED-CANCELLED-PAUSED-DONE" ("DOING" "TODO" "APPT" "WAITING") nil
        "\\<IGNORE\\>"))
  ;; Also see: (describe-variable 'org-agenda-custom-commands)
  (setq org-agenda-custom-commands
       '(("A" . "Overall Commands")
         ("Ap" "All Projects" ((tags "PROJECT")))
         ("P" . "Personal Entries") ;; prefix P command
         ("Pa" "All Relevant Personal Items"
          ((agenda ""
                   ((org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           (tags "-TODO={DONE\\|CANCELLED}"))
          ((org-agenda-tag-filter-preset '("+{PERSONAL\\|BOTH}"))))
         ("Pd" "Daily Action List"
          ;; See: https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
          ((agenda ""
                   ((org-agenda-span 1)
                    (org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           ;; See: https://orgmode.org/manual/Matching-tags-and-properties.html
           (tags "+TODO={DOING\\|TODO}+NOW-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+UNPLANNED-STYLE=\"habit\"")
           (tags "+TODO={DOING}+CURRENT+MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={DOING}+CURRENT-MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={TODO\\|WAITING}+CURRENT+MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={TODO\\|WAITING}+CURRENT-MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO\\|WAITING}+NEXT-CURRENT-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+LATER-NEXT-CURRENT-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"0\"-CURRENT-NEXT-LATER-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"2\"-STYLE=\"habit\""))
          ((org-agenda-tag-filter-preset '("+{PERSONAL\\|BOTH}"))))
         ("W" . "Work Commands") ;; prefix W command
         ("Wa" "All Relevant Work Items"
          ((agenda ""
                   ((org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           (tags "-TODO={DONE\\|CANCELLED}"))
          ((org-agenda-tag-filter-preset '("+{WORK\\|BOTH}"))))
         ("Wd" "Daily Action List"
           ;; See: https://orgmode.org/manual/Filtering_002flimiting-agenda-items.html
          ((agenda ""
                   ((org-agenda-span 1)
                    (org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 1)))
           ;; See: https://orgmode.org/manual/Matching-tags-and-properties.html
           (tags "+TODO={DOING\\|TODO}+NOW-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+UNPLANNED-STYLE=\"habit\"")
           (tags "+TODO={DOING}+CURRENT+MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={DOING}+CURRENT-MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={TODO\\|WAITING}+CURRENT+MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={TODO\\|WAITING}+CURRENT-MUST_DO-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO\\|WAITING}+NEXT-CURRENT-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+LATER-NEXT-CURRENT-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"0\"-CURRENT-NEXT-LATER-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"0\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"-PRIORITY=\"0\"-PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO={DOING\\|TODO}+PRIORITY=\"2\"-STYLE=\"habit\"")
           (tags "+TODO=\"WAITING\"+PRIORITY=\"2\"-STYLE=\"habit\""))
          ((org-agenda-tag-filter-preset '("+{WORK\\|BOTH}"))))
         ("R" "Weekly Review"
          ((agenda "" ((org-agenda-span 7)))
           (stuck "") ;; See: https://orgmode.org/manual/Stuck-projects.html
           (tags "PROJECT+TODO=\"\"")
           (todo "WAITING")
           (todo "DEFERRED")
           (todo "PAUSED")
           ))
         ("D" "Daily Action List"
          ((agenda ""
                   ((org-agenda-ndays 1)
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down tag-up))))
                    (org-deadline-warning-days 0)))))))

  (defun custom:org-gtd ()
    (interactive)
    (find-file "~/gtd/gtd.org"))
  (global-set-key (kbd "C-c C-g") 'custom:org-gtd)

  (setq org-log-repeat 'time)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "APPT(a)" "DOING(o)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)" "PAUSED(p)")))

  (setq org-todo-keyword-faces
        '(
          ;; Item is ready to be done at the earliest opportunity or by deadline
          ("TODO" . (:foreground "dodgerblue1" :weight bold))
          ;; Used to tag an activity that can *only* be done at the specified time and date.
          ("APPT" . (:foreground "dodgerblue3"))
          ;; Tasks that are currently being worked on
          ("DOING" . (:foreground "aquamarine1" :weight bold))
          ;; Task that is waiting on a response or availability of something or someone.
          ("WAITING" . (:foreground "darkorange3"))
          ;; Completed task
          ("DONE" . (:foreground "darkgreen" :weight bold))
          ;; Task will no longer be done, but for some reason I kept it on file
          ("CANCELLED" . (:foreground "tan3"))
          ;; Task is paused for the time being, in favour of another one, but should
          ;; be picked up as soon as another work stream is done.
          ("PAUSED" . (:foreground "rosybrown"))
          ;; Task that is defined but shouldn't be picked up at earliest opportunity.
          ;; The reason should be included in the task notes.
          ("DEFERRED" . (:foreground "dodgerblue4"))))

  (setq org-capture-templates
        '(("g" "GTD Entries")
          ("gt" "Task" entry (file+olp "~/gtd/gtd.org" "Tasks")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (js . t)
                                 (dot . t)
                                 (emacs-lisp . t)
                                 (lisp . t)))
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-agenda-text-search-extra-files
        (directory-files-recursively "~/notes/" "md$"))
  (setq org-agenda-files '("~/notes/" "~/gtd/")))

(provide 'org-mode-config)
;;; org-mode-config.el ends here
