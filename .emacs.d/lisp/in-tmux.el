
;;; Commentary:
;;; Remaps certain keys if we are inside a TMUX session
;;; Code:
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          ;; shift
          (if (= x 2)
              (setq tkey "S-"))
          ;; alt
          (if (= x 3)
              (setq tkey "M-"))
          ;; alt + shift
          (if (= x 4)
              (setq tkey "M-S-"))
          ;; ctrl
          (if (= x 5)
              (setq tkey "C-"))
          ;; ctrl + shift
          (if (= x 6)
              (setq tkey "C-S-"))
          ;; ctrl + alt
          (if (= x 7)
              (setq tkey "C-M-"))
          ;; ctrl + alt + shift
          (if (= x 8)
              (setq tkey "C-M-S-"))

          ;; arrows
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
          ;; home
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
          ;; end
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
          ;; page up
          (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
          ;; page down
          (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
          ;; insert
          (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; delete
          (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; Ctrl+1
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d q" x)) (kbd (format "%s1" tkey)))
          ;; Ctrl+2
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d r" x)) (kbd (format "%s2" tkey)))
          ;; Ctrl+3
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d s" x)) (kbd (format "%s3" tkey)))
          ;; Ctrl+4
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d t" x)) (kbd (format "%s4" tkey)))
          ;; Ctrl+5
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d u" x)) (kbd (format "%s5" tkey)))
          ;; f1
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
          ;; f2
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
          ;; f3
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
          ;; f4
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
          ;; f5
          (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
          ;; f6
          (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
          ;; f7
          (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
          ;; f8
          (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
          ;; f9
          (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
          ;; f10
          (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
          ;; f11
          (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
          ;; f12
          (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
          ;; f13
          (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
          ;; f14
          (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
          ;; f15
          (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
          ;; f16
          (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
          ;; f17
          (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
          ;; f18
          (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
          ;; f19
          (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
          ;; f20
          (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

          (setq x (+ x 1))
          ))

      ;; These keys are available in xterm starting from version 216
      ;; if the modifyOtherKeys resource is set to 1.
      (dolist (bind '((5 9   [C-tab])
                      (5 13  [C-return])
                      (5 39  [?\C-\'])
                      (5 44  [?\C-,])
                      (5 45  [?\C--])
                      (5 46  [?\C-.])
                      (5 47  [?\C-/])
                      (5 48  [?\C-0])
                      (5 49  [?\C-1])
                      ;; Not all C-DIGIT keys have a distinct binding.
                      (5 57  [?\C-9])
                      (5 59  [?\C-\;])
                      (5 61  [?\C-=])
                      (5 92  [?\C-\\])

                      (6 33  [?\C-!])
                      (6 34  [?\C-\"])
                      (6 35  [?\C-#])
                      (6 36  [?\C-$])
                      (6 37  [?\C-%])
                      (6 38  [?\C-&])
                      (6 40  [?\C-\(])
                      (6 41  [?\C-\)])
                      (6 42  [?\C-*])
                      (6 43  [?\C-+])
                      (6 58  [?\C-:])
                      (6 60  [?\C-<])
                      (6 62  [?\C->])
                      (6 63  [(control ??)])

                      ;; These are the strings emitted for various C-M-
                      ;; combinations for keyboards whose Meta and Alt
                      ;; modifiers are on the same key (usually labeled "Alt").
                      (13 9  [C-M-tab])
                      (13 13 [C-M-return])

                      (13 39 [?\C-\M-\'])
                      (13 44 [?\C-\M-,])
                      (13 45 [?\C-\M--])
                      (13 46 [?\C-\M-.])
                      (13 47 [?\C-\M-/])
                      (13 48 [?\C-\M-0])
                      (13 49 [?\C-\M-1])
                      (13 50 [?\C-\M-2])
                      (13 51 [?\C-\M-3])
                      (13 52 [?\C-\M-4])
                      (13 53 [?\C-\M-5])
                      (13 54 [?\C-\M-6])
                      (13 55 [?\C-\M-7])
                      (13 56 [?\C-\M-8])
                      (13 57 [?\C-\M-9])
                      (13 59 [?\C-\M-\;])
                      (13 61 [?\C-\M-=])
                      (13 92 [?\C-\M-\\])

                      (14 33  [?\C-\M-!])
                      (14 34  [?\C-\M-\"])
                      (14 35  [?\C-\M-#])
                      (14 36  [?\C-\M-$])
                      (14 37  [?\C-\M-%])
                      (14 38  [?\C-\M-&])
                      (14 40  [?\C-\M-\(])
                      (14 41  [?\C-\M-\)])
                      (14 42  [?\C-\M-*])
                      (14 43  [?\C-\M-+])
                      (14 58  [?\C-\M-:])
                      (14 60  [?\C-\M-<])
                      (14 62  [?\C-\M->])
                      (14 63  [(control meta ??)])

                      (7 9  [C-M-tab])
                      (7 13 [C-M-return])

                      (7 32 [?\C-\M-\s])
                      (7 39 [?\C-\M-\'])
                      (7 44 [?\C-\M-,])
                      (7 45 [?\C-\M--])
                      (7 46 [?\C-\M-.])
                      (7 47 [?\C-\M-/])
                      (7 48 [?\C-\M-0])
                      (7 49 [?\C-\M-1])
                      (7 50 [?\C-\M-2])
                      (7 51 [?\C-\M-3])
                      (7 52 [?\C-\M-4])
                      (7 53 [?\C-\M-5])
                      (7 54 [?\C-\M-6])
                      (7 55 [?\C-\M-7])
                      (7 56 [?\C-\M-8])
                      (7 57 [?\C-\M-9])
                      (7 59 [?\C-\M-\;])
                      (7 61 [?\C-\M-=])
                      (7 92 [?\C-\M-\\])

                      (8 33  [?\C-\M-!])
                      (8 34  [?\C-\M-\"])
                      (8 35  [?\C-\M-#])
                      (8 36  [?\C-\M-$])
                      (8 37  [?\C-\M-%])
                      (8 38  [?\C-\M-&])
                      (8 40  [?\C-\M-\(])
                      (8 41  [?\C-\M-\)])
                      (8 42  [?\C-\M-*])
                      (8 43  [?\C-\M-+])
                      (8 58  [?\C-\M-:])
                      (8 60  [?\C-\M-<])
                      (8 62  [?\C-\M->])
                      (8 63  [(control meta ??)])

                      (2 9   [S-tab])
                      (2 13  [S-return])

                      (6 9   [C-S-tab])
                      (6 13  [C-S-return])))
        (define-key key-translation-map
          (format "\e[27;%d;%d~" (nth 0 bind) (nth 1 bind)) (nth 2 bind))
        ;; For formatOtherKeys=1, the sequence is a bit shorter (bug#13839).
        (define-key key-translation-map
          (format "\e[%d;%du" (nth 1 bind) (nth 0 bind)) (nth 2 bind)))
      )
  )

(provide 'in-tmux)
;;; in-tmux.el ends here
