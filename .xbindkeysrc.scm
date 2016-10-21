;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of xbindkeys guile configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This configuration is guile based.
;;   http://www.gnu.org/software/guile/guile.html
;; any functions that work in guile will work here.
;; see EXTRA FUNCTIONS:

;; Version: 1.8.6

;; If you edit this file, do not forget to uncomment any lines
;; that you change.
;; The semicolon(;) symbol may be used anywhere for comments.

;; To specify a key, you can use 'xbindkeys --key' or
;; 'xbindkeys --multikey' and put one of the two lines in this file.

;; A list of keys is in /usr/include/X11/keysym.h and in
;; /usr/include/X11/keysymdef.h
;; The XK_ is not needed.

;; List of modifier:
;;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).


;; The release modifier is not a standard X modifier, but you can
;; use it if you want to catch release instead of press events

;; By defaults, xbindkeys does not pay attention to modifiers
;; NumLock, CapsLock and ScrollLock.
;; Uncomment the lines below if you want to use them.
;; To dissable them, call the functions with #f


;;;;EXTRA FUNCTIONS: Enable numlock, scrolllock or capslock usage
;;(set-numlock! #t)
;;(set-scrolllock! #t)
;;(set-capslock! #t)

;;;;; Scheme API reference
;;;;
;; Optional modifier state:
;; (set-numlock! #f or #t)
;; (set-scrolllock! #f or #t)
;; (set-capslock! #f or #t)
;; 
;; Shell command key:
;; (xbindkey key "foo-bar-command [args]")
;; (xbindkey '(modifier* key) "foo-bar-command [args]")
;; 
;; Scheme function key:
;; (xbindkey-function key function-name-or-lambda-function)
;; (xbindkey-function '(modifier* key) function-name-or-lambda-function)
;; 
;; Other functions:
;; (remove-xbindkey key)
;; (run-command "foo-bar-command [args]")
;; (grab-all-keys)
;; (ungrab-all-keys)
;; (remove-all-keys)
;; (debug)


;; Examples of commands:

(xbindkey '(control alt shift q) "xbindkeys_show")

;; set directly keycode (here control + f with my keyboard)

;; specify a mouse button
;; (xbindkey '(control "b:2") "xterm")

;;(xbindkey '(shift mod2 alt s) "xterm -geom 50x20+20+20")

;; set directly keycode (control+alt+mod2 + f with my keyboard)
;; (xbindkey '(alt "m:4" mod2 "c:0x29") "xterm")

;; Control+Shift+a  release event starts rxvt
;;(xbindkey '(release control shift a) "rxvt")

;; Control + mouse button 2 release event starts rxvt
;;(xbindkey '(releace control "b:2") "rxvt")


;; Extra features
;; (xbindkey-function '(control a)
;;      	   (lambda ()
;;      	     (display "Hello from Scheme!")
;;      	     (newline)))

;; (xbindkey-function '(shift p)
;;      	   (lambda ()
;;      	     (run-command "xterm")))


;; Double click test
;; (xbindkey-function '(control w)
;;      	   (let ((count 0))
;;      	     (lambda ()
;;      	       (set! count (+ count 1))
;;      	       (if (> count 1)
;;      		   (begin
;;      		    (set! count 0)
;;      		    (run-command "xterm"))))))

;; Time double click test:
;;  - short double click -> run an xterm
;;  - long  double click -> run an rxvt
;; (xbindkey-function '(shift w)
;;      	   (let ((time (current-time))
;;      		 (count 0))
;;      	     (lambda ()
;;      	       (set! count (+ count 1))
;;      	       (if (> count 1)
;;      		   (begin
;;      		    (if (< (- (current-time) time) 1)
;;      			(run-command "xterm")
;;      			(run-command "rxvt"))
;;      		    (set! count 0)))
;;      	       (set! time (current-time)))))


;; Chording keys test: Start differents program if only one key is
;; pressed or another if two keys are pressed.
;; If key1 is pressed start cmd-k1
;; If key2 is pressed start cmd-k2
;; If both are pressed start cmd-k1-k2 or cmd-k2-k1 following the
;;   release order
;; (define (define-chord-keys key1 key2 cmd-k1 cmd-k2 cmd-k1-k2 cmd-k2-k1)
;;     "Define chording keys"
;;   (let ((k1 #f) (k2 #f))
;;     (xbindkey-function key1 (lambda () (set! k1 #t)))
;;     (xbindkey-function key2 (lambda () (set! k2 #t)))
;;     (xbindkey-function (cons 'release key1)
;;      	       (lambda ()
;;      		 (if (and k1 k2)
;;      		     (run-command cmd-k1-k2)
;;      		     (if k1 (run-command cmd-k1)))
;;      		 (set! k1 #f) (set! k2 #f)))
;;     (xbindkey-function (cons 'release key2)
;;      	       (lambda ()
;;      		 (if (and k1 k2)
;;      		     (run-command cmd-k2-k1)
;;      		     (if k2 (run-command cmd-k2)))
;;      		 (set! k1 #f) (set! k2 #f)))))
;; These are too buggy for it to be worth having, use alternate bindings

;; sends a tmux key to the terminal when the focused window is a terminal
;; otherwise it sens a key combination using xdotool
(use-modules (ice-9 threads))

(define (fire-and-remap xbind_key tmux_key xdotool_key)
   (yield)
  ;; (lambda ()
    ;; seems like no matter what I do, the subsequent xdotool call will be
    ;; grabbed by xbindkeys, no matter how long the timeout is
    ;; TODO: another possible way may be by using "worker" threads that do not
    ;; share context, so that the worker thread that re-maps the key does not
    ;; share a call stack with the calling thread
    (usleep 10000) ;; 10,000 microseconds = 10 milliseconds
    ;; (display (string-append (to-str (current-time)) ": Firing xdotool event for " xdotool_key "\n"))
    (system* "xdotool" "key" xdotool_key)
    (usleep 10000) ;; 10,000 microseconds = 10 milliseconds
    (remap-when-terminal xbind_key tmux_key xdotool_key)
    ;; )
  )

(define (remap-when-terminal-fire-and-forget xbind_key tmux_key xdotool_key)
  (let ((result 0))
    (lambda ()
      (set! result (system*
		    (string-append (getenv "HOME") "/bin/focused-window-is-program") "terminal"
		    ))
      (if (= result 0)
	  (system* "tmux" "send-keys" tmux_key)
	  (begin
	    (display (string-append (to-str (current-time)) ": GRABBED " xdotool_key "\n" ))
	    (remove-xbindkey xbind_key)
	    ;; should instead be a "signal" for a running thread to re-map the key
	    ;; completely unaway of the context of this function
	    ;; SEE: http://community.schemewiki.org/?guile-asyncs-batch-processing
	    (begin-thread (fire-and-remap xbind_key tmux_key xdotool_key))
	    )
	  )
      (display "finish remap-when-ter...\n")
      )
    )
  )

(define (remap-when-terminal xbind_key tmux_key xdotool_key)
   (xbindkey-function xbind_key (remap-when-terminal-fire-and-forget xbind_key tmux_key xdotool_key))
)

;; bind ctrl+;
(remap-when-terminal '("m:0x4" "c:47") "C-\\;" "ctrl+semicolon")

;; bind ctrl+;
(remap-when-terminal '("m:0x4" "c:60") "C-." "ctrl+period")

;; ctrl + home
;; (remap-when-terminal '("m:0x4" "c:110") "C-home" "ctrl+Home")

;; ctrl + end
;;(remap-when-terminal '("m:0x4" "c:115") "C-end" "ctrl+End")

(define (release-modifiers)
  (lambda ()
    ;; (display "xbindkeys: Releasing modifiers" )
    (run-command "~/bin/release-modifiers")
  )
)
;; ctrl + '
(xbindkey-function '(m:0x4 c:48) (release-modifiers))
;; ctrl + shift + '
(xbindkey-function '(m:0x5 c:48) (release-modifiers))
;; alt + '
(xbindkey-function '(m:0x8 c:48) (release-modifiers))
;; alt + shift + '
(xbindkey-function '(m:0x9 c:48) (release-modifiers))
;; ctrl + alt  + '
(xbindkey-function '(m:0xc c:48) (release-modifiers))
;; ctrl + shift + alt  + '
(xbindkey-function '(m:0xd c:48) (release-modifiers))

(define (to-str obj)
  (format #f "~a" obj)
)

;; executes a command when 2 key combinations happen
;; the only problem with this, is that xbindkeys intercepts
;; key2 no matter what
(define (define-chord-keys-cmd key1 key2 cmd)
  "Define chording key"
  (let ((key2-registered #f))
    (xbindkey-function key1 (lambda ()
      ;;(display (string-append (to-str key1) " pressed\n"))
      (if (not key2-registered)
        (begin
          ;;(display (string-append "registering " (to-str key2) "\n"))
          (set! key2-registered #t)
          (xbindkey-function key2 (lambda ()
            ;;(display (string-append (to-str key2) " pressed\n"))
            (run-command cmd)
          ))
        )
      )
    ))
  )
)

;; ctrl + \ + left
(define-chord-keys-cmd '(control c:51) '(control Left)
  "~/projects/x-window-shortcuts/x-window-move left")
;; ctrl + \ + right
(define-chord-keys-cmd '(control c:51) '(control Right)
  "~/projects/x-window-shortcuts/x-window-move right")
;; ctrl + \ + up
(define-chord-keys-cmd '(control c:51) '(control Up)
  "~/projects/x-window-shortcuts/x-window-move up")
;; ctrl + \ + down
(define-chord-keys-cmd '(control c:51) '(control Down)
  "~/projects/x-window-shortcuts/x-window-move down")
;; ctrl + \ + pageup
(define-chord-keys-cmd '(control c:51) '(control Prior)
  "~/projects/x-window-shortcuts/x-window-move maximize")
;; ctrl + \ + pagedown
(define-chord-keys-cmd '(control c:51) '(control Next)
  "~/projects/x-window-shortcuts/x-window-move minimize")
;; ctrl + \ + comma
(define-chord-keys-cmd '(control c:51) '(control comma)
  "~/projects/x-window-shortcuts/x-window-move left-screen")
;; ctrl + \ + period
(define-chord-keys-cmd '(control c:51) '(control period)
  "~/projects/x-window-shortcuts/x-window-move right-screen")

(define (define-xdotool-remap source_guile_key target_xdotool_key)
  (let ()
    (xbindkey-function (cons 'release source_guile_key)
      (lambda ()
        (run-command (string-append "xdotool key --clearmodifiers " target_xdotool_key))
      )
    )
  )
)

;; remaps Alt+Shift+2 to F2 key via xdotool
(define-xdotool-remap '(m:0x9 c:11) "F2")
;; remaps Alt+Shift+3 to F3 key via xdotool
(define-xdotool-remap '(m:0x9 c:12) "F3")
;; remaps Alt+Shift+4 to F4 key via xdotool
(define-xdotool-remap '(m:0x9 c:13) "F4")
;; remaps Alt+Shift+5 to F5 key via xdotool
(define-xdotool-remap '(m:0x9 c:14) "F5")

;; Example:
;;   Shift + b:1                   start an xterm
;;  Shift + b:3                   start an rxvt
;;   Shift + b:1 then Shift + b:3  start gv
;;   Shift + b:3 then Shift + b:1  start xpdf

;; (define-chord-keys '(shift "b:1") '(shift "b:3")
;;   "xterm" "rxvt" "gv" "xpdf")

;; Here the release order have no importance
;; (the same program is started in both case)
;; (define-chord-keys '(alt "b:1") '(alt "b:3")
;;   "gv" "xpdf" "xterm" "xterm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of xbindkeys guile configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


