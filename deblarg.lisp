;;
;; deblarg.lisp - Debugger for use with :TINY-REPL
;;

;;; TODO:
;;; - how about using swank? conium?
;;; - try getting more specific source location with *read-intern*

(defpackage :deblarg
  (:documentation
   "A crappy half-assed debugger for your enjoyment and frustration. But at
least you can type things using RL.")
  (:use :cl :dlib :char-util :table-print :keymap :terminal :terminal-ansi
	:rl :fatchar :fatchar-io :tiny-repl #+sbcl :sb-introspect)
  (:export
   #:deblarg
   #:*default-interceptor*
   #:*interceptor-condition*
   #:*visual-mode*
   #:toggle
   #:active-p
   #:activate
   ))
(in-package :deblarg)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation specifc functions are:
;;
;; debugger-backtrace n          - Show N frames of normal backtrace
;; debugger-wacktrace n          - Alternate backtrace
;; debugger-show-source n        - Show the source for frame N
;; debugger-show-locals n        - Show local variables for frame N
;; debugger-internal-frame       - Return the best approximation of the error
;;                                 frame.
;; debugger-eval-in-frame n form - Return the result of evaluating FORM in
;;                                 frame N.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation independent functions

;; Visual mode

(defparameter *box_drawings_light_horizontal*
  #+(or sbcl cmu) #\box_drawings_light_horizontal
  #+(or ccl lispworks) #\U+2500
  #-(or sbcl cmu ccl lispworks) #\-)

(defparameter *box_drawings_light_vertical_and_left*
  #+(or sbcl cmu) #\box_drawings_light_vertical_and_left
  #+(or ccl lispworks) #\U+2524
  #-(or sbcl cmu ccl lispworks) #\|)

(defparameter *box_drawings_light_vertical_and_right*
  #+(or sbcl cmu) #\box_drawings_light_vertical_and_right
  #+(or ccl lispworks) #\U+251C
  #-(or sbcl cmu ccl lispworks) #\|)

(defun horizontal-line (tt &optional note)
  (terminal-color tt :blue :default)
  (if note
      (progn
	(terminal-write-string tt (s+
			     *box_drawings_light_horizontal*
			     *box_drawings_light_horizontal*
			     *box_drawings_light_vertical_and_left*))
	(terminal-color tt :white :black)
	(terminal-write-string tt (s+ " " note " "))
	(terminal-color tt :blue :default)
	(terminal-write-char tt *box_drawings_light_vertical_and_right*)
	(terminal-format tt "~v,,,va"
		   (- (terminal-window-columns tt) (length (s+ note)) 6)
		   *box_drawings_light_horizontal*
		   *box_drawings_light_horizontal*)
	(terminal-color tt :default :default)
	(terminal-write-char tt #\newline))
      ;; no note, just a line
      (progn
      	(terminal-color tt :blue :default)
	(terminal-format tt "~v,,,va~%"
		   (1- (terminal-window-columns tt))
		   *box_drawings_light_horizontal*
		   *box_drawings_light_horizontal*)
	(terminal-color tt :default :default))))

(defun sanitize-line (line)
  (when line
    (if (> (length line) 0)
	(apply #'s+ (map 'list #'char-util:displayable-char line))
	line)))

(defun visual ()
  (let ((tt *visual-term*))
    (terminal-get-size tt)
    (with-saved-cursor (tt)
      (let* ((source-height (truncate (/ (terminal-window-rows tt) 3)))
	     (stack-height (min 10 source-height))
	     (command-height (- (terminal-window-rows tt)
				(+ stack-height source-height 2)))
	     (command-top (- (terminal-window-rows tt) (1- command-height)))
	     (src (or (ignore-errors
			(debugger-source *current-frame* source-height))
		      '("Unavailable.")))
	     (path (or (ignore-errors
			 (debugger-source-path *current-frame*))
		       '("Unknown")))
	     (stack (or (ignore-errors
			  (debugger-backtrace-lines stack-height))
			'("????"))))
	;; Source area
	;;(terminal-clear tt)
	(terminal-move-to tt (+ source-height stack-height 2) 0)
	(terminal-erase-above tt)
	(terminal-home tt)
	(loop :with line :and sp = src
	   :for i :from 0 :below source-height :do
	   (setf line (car sp))
	   (if line
	       (progn
		 (setf line (sanitize-line line))
		 (terminal-format tt "~a~%"
			    (subseq line
				    0 (min (- (terminal-window-columns tt) 2)
					   (length line))))
		 (setf sp (cdr sp)))
	       (terminal-format tt "~~~%")))
	(horizontal-line tt path)
	;; Stack area
	(loop :with line :and sp = stack
	   :for i :from 0 :below stack-height :do
	   (setf line (car sp))
	   (if line
	       (progn
		 (terminal-format tt "~a~%"
			    (subseq line
				    0 (min (1- (terminal-window-columns tt))
					   (length line))))
		 (setf sp (cdr sp)))
	       (terminal-format tt "~~~%")))
	(horizontal-line tt)
	;; Command area
	(terminal-set-scrolling-region tt command-top (terminal-window-rows tt))
	(terminal-move-to tt (1- (terminal-window-rows tt)) 0)
	(terminal-finish-output tt)))))

(defvar *fake-term* nil "Workaround for problems with crunch.")

(defun start-visual ()
  (cond
    (*visual-mode*
     (when (not *visual-term*)
       (setf *visual-term* (make-instance 'terminal-ansi))
       (terminal-start *visual-term*))
     (let ((tt *visual-term*))
       (terminal-get-size tt)
       (terminal-move-to tt (1- (terminal-window-rows tt)) 0)
       (terminal-finish-output tt)))
    ;; @@@ temporary workaround
    ;; ((typep *terminal* 'terminal-crunch:terminal-crunch)
    ;;  (setf *fake-term* *terminal*
    ;; 	   *terminal* (make-instance 'terminal-ansi)))
    ))

(defun reset-visual ()
  (cond
    (*visual-term*
     (let ((tt *visual-term*))
       (terminal-set-scrolling-region tt nil nil)
       (terminal-move-to tt (1- (terminal-window-rows tt)) 0)
       (terminal-finish-output tt)
       #| (terminal-end tt) |#))
    ;; @@@ temporary workaround
    ;; (*fake-term*
    ;;  (terminal-done *terminal*)
    ;;  (setf *terminal* *fake-term*
    ;; 	   *fake-term* nil))
    ))

(defun debugger-up-frame-command (&optional foo)
  (declare (ignore foo))
  (debugger-up-frame)
  (visual))

(defun debugger-down-frame-command (&optional foo)
  (declare (ignore foo))
  (debugger-down-frame)
  (visual))

(defun list-restarts (rs)
  #|
  (format *debug-io* "Restarts are:~%")
  (loop :with i = 0 :for r :in rs :do
     (format *debug-io* "~&")
     (print-span `((:fg-cyan ,(format nil "~d" i)) ": "))
     (when (not (ignore-errors (progn (format *debug-io* "~s ~a~%"
					      (restart-name r) r) t)))
       (format *debug-io* "Error printing restart ")
       (print-unreadable-object (r *debug-io* :type t :identity t)
	 (format *debug-io* "~a" (restart-name r)))
       (terpri *debug-io*))
     (incf i))
  |#
  (format *terminal* "Restarts are:~%")
  (loop :with i = 0 :for r :in rs :do
     (format *terminal* "~&")
     (print-span `((:fg-cyan ,(princ-to-string i)) ": "))
     (when (not (ignore-errors (progn (format *terminal* "~s ~a~%"
					      (restart-name r) r) t)))
       (format *terminal* "Error printing restart ")
       (print-unreadable-object (r *terminal* :type t :identity t)
	 (format *terminal* "~a" (restart-name r)))
       (terpri *terminal*))
     (incf i))
  )

(defun debugger-prompt (e p)
;;;  (format *debug-io* "Debug ~d~a" *repl-level* p)
  (when *visual-mode*
    (visual))
  (fresh-line *debug-io*)
  (rl::editor-write-string		; XXX
   e
   (format nil "Debug ~d~a" *repl-level* p))
;  (finish-output *debug-io*)
  nil)


(defun debugger-help ()
  (print-span
   `("Debugger help:" #\newline
(:fg-cyan ":h") "      " (:fg-white "Show this help.") #\newline
(:fg-cyan ":e") "      " (:fg-white "Show the error again.") #\newline
(:fg-cyan ":a") "      " (:fg-white "Abort to top level.") #\newline
(:fg-cyan ":c") "      " (:fg-white "Invoke continue restart.") #\newline
(:fg-cyan ":q") "      " (:fg-white "Quit the whatever.") #\newline
(:fg-cyan ":r") "      " (:fg-white "Show restarts.") #\newline
(:fg-cyan ":b") "      " (:fg-white "Backtrace stack.") #\newline
(:fg-cyan ":w") "      " (:fg-white "Wacktrace.") #\newline
(:fg-cyan ":s [n]") "  " (:fg-white "Show source for a frame N, which defaults to the current frame.") #\newline
(:fg-cyan ":l [n]") "  " (:fg-white "Show local variables for a frame N, which defaults to the current frame.") #\newline))
 #+tdb-has-breakpoints
(print-span `(
(:fg-cyan ":lbp") "    " (:fg-white "List breakpointss.") #\newline
(:fg-cyan ":sbp") "    " (:fg-white "Set breakpoints on function.")  #\newline
(:fg-cyan ":tbp") "    " (:fg-white "Toggle breakpoints.")  #\newline
(:fg-cyan ":abp") "    " (:fg-white "Activate breakpoints.")  #\newline
(:fg-cyan ":dbp") "    " (:fg-white "Deactivate breakpoints.")  #\newline
(:fg-cyan ":xbp") "    " (:fg-white "Delete breakpoints.")  #\newline))
(print-span `(
(:fg-cyan "number") "     " (:fg-white "Invoke that number restart (from the :r list).") #\newline
(:fg-cyan "...") "      " (:fg-white "Or just type a some lisp code.") #\newline))
(list-restarts (cdr (compute-restarts *interceptor-condition*))))

(defun debugger-snargle (arg)
  "Magic command just for me."
  (error "Pizza ~s ~s." arg (type-of arg)))

(defun toggle-visual-mode (state)
  (declare (ignore state))
  (setf *visual-mode* (not *visual-mode*))
  (if *visual-mode*
      (start-visual)
      (reset-visual)))

;;; @@@ I actually want to take defcommand out of lish and make it be generic.
;;; And then also the command completion from lish to generic completion.
;;; Then we can define debugger commands nicely with completion and the whole
;;; shebang.

(defun debugger-interceptor (value state)
  "Handle special debugger commands, which are usually keywords."
  (let ((restarts (cdr (compute-restarts *interceptor-condition*))))
    (labels
	((do-restart (r)
	   (format *debug-io* "~:(~a~).~%" r)
	   ;; This is like find-restart, but omits the most recent abort
	   ;; which is this debugger's.
	   (let ((borty (find r restarts :key #'restart-name)))
	     (if (not borty)
		 (format *debug-io* "Can't find an ~a restart!~%" r)
		 (invoke-restart-interactively borty)))))
      (cond
	;; We use keywords as commands, just in case you have a variable or some
	;; other symbol clash. I dunno. I 'spose we could use regular symbols,
	;; and have a "print" command.
	((typep value 'keyword)
	 (let ((ks (string value)))
	   ;; :r<n> restart keywords - to be compatible with CLisp
	   (when (and (> (length ks) 1) (equal (aref ks 0) #\R))
	     (let ((n (parse-integer (subseq ks 1))))
	       ;; (invoke-restart-interactively (nth n (compute-restarts)))))
	       ;; (format t "[Invoking restart ~d (~a)]~%" n (nth n restarts))
	       (invoke-restart-interactively (nth n restarts))))
	   (or
	    (case value
	      (:b (debugger-backtrace (read-arg state)) t)
	      (:w (debugger-wacktrace (read-arg state)) t)
	      (:r (list-restarts restarts) t)
	      (:s (debugger-show-source (read-arg state)) t)
	      (:l (debugger-show-locals (read-arg state)) t)
	      ((:h :help) (debugger-help) t)
	      (:z (debugger-snargle    (read-arg state)) t)
	      (:v (toggle-visual-mode  (read-arg state)) t)
	      (:u (debugger-up-frame   (read-arg state)) t)
	      (:d (debugger-down-frame (read-arg state)) t)
	      (:f (debugger-set-frame  (read-arg state)) t)
	      (:t (debugger-top-frame  (read-arg state)) t)
	      (:e (print-condition *interceptor-condition*) t)
	      (:a (do-restart 'abort) t)
	      (:c (do-restart 'continue) t)
	      ((:q :quit)
	       (when (y-or-n-p "Really quit?")
		 (format *debug-io* "We quit.~%")
		 (nos:exit-lisp))
	       t))
	    #+tdb-has-breakpoints
	    (case value
	      ((:lb :lbp :list)	      (list-breakpoints) t)
	      ((:sb :sbp :set)	      (set-func-breakpoint
				       (eval (read-arg state))) t)
	      ((:tb :tbp :toggle)     (toggle-breakpoint (read-arg state)) t)
	      ((:ab :abp :activate)   (activate-breakpoint (read-arg state)) t)
	      ((:db :dbp :deactivate) (deactivate-breakpoint (read-arg state)) t)
	      ((:xb :xbp :delete)     (delete-breakpoint (read-arg state)) t)
	      ))))
	;; symbols that aren't keywords
	((typep value 'symbol)
	 (case (intern (string value) :deblarg)
	   (backtrace (debugger-backtrace (read-arg state)) t)
	   (source    (debugger-show-source (read-arg state)) t)
	   (locals    (debugger-show-locals (read-arg state)) t)
	   (help      (debugger-help) t)
	   (abort     (do-restart 'abort) t)
	   (continue  (do-restart 'continue) t)
	   (next      t)
	   (step      t)
	   (out       t)
	   ))
	;; Numbers invoke that numbered restart.
	((typep value 'number)
	 (if (and (>= value 0) (< value (length restarts)))
	     (invoke-restart-interactively (nth value restarts))
	     (format *debug-io*
		     "~a is not a valid restart number.~%" value)))))))

;; (defun try-to-reset-curses ()
;;   "If curses is loaded and active, try to reset the terminal to a sane state
;; so when we get in error in curses we can type at the debugger."
;;   (when (find-package :curses)
;;     (funcall (find-symbol (symbol-name '#:endwin) (find-package :curses)))))

;; @@@ It might be nice if we could avoid this duplication and just call the
;; one in terminal-ansi.
(defun try-to-reset-terminal ()
  "Try to reset the terminal to a sane state so when we get in error in some
program that messes with the terminal, we can still type at the debugger."
  (flet ((out (s) (format *terminal-io* "~c~a" #\escape s)))
    ;; First reset the terminal driver to a sane state.
    (nos:reset-terminal-modes)
    ;; Then try to reset the terminal itself to a sane state, assuming an ANSI
    ;; terminal. We could just do ^[c, which is quite effective, but it's
    ;; pretty drastic, and usually clears the screen and can even resize the
    ;; window, which is so amazingly annoying. So let's just try do individual
    ;; things that need resetting.  This is pretty much the idea of
    ;; termcap/info reset string, usually the "rs2", since "rs" usually just
    ;; does ^[c.
    (when (typep *terminal* 'terminal-ansi:terminal-ansi)
      (mapcar
       #'out '(" F"  ;; 7 bit controls
	       "[0m" ;; color and attributes
	       ">"   ;; normal keypad
	       "#@"  ;; default char set
	       "m"   ;; memory unlock
	       "[4l" ;; replace mode (vs insert mode)
	       "[?4l" ;; jump scroll (vs smooth scroll)
	       "[?25h" ;; show the cursor
	       "[?9l"  ;; Don't send position on mouse press
	       "[?47l" ;; Use normal screen buffer
	       )))
    (finish-output)))

(defvar *debugger-keymap* nil "Keymap for the debugger.")
(defvar *debugger-escape-keymap* nil "Escape key Keymap for the debugger.")

(defun setup-keymap ()
  (setf *debugger-keymap* (copy-keymap rl:*normal-keymap*))
  (define-key *debugger-keymap* (meta-char #\i) 'debugger-up-frame-command)
  (define-key *debugger-keymap* (meta-char #\o) 'debugger-down-frame-command)
  (setf *debugger-escape-keymap*
;;	(add-keymap rl::*escape-raw-keymap*
;;		    (build-escape-map *debugger-keymap*)))
	(build-escape-map *debugger-keymap*))
  (define-key *debugger-keymap* #\escape '*debugger-escape-keymap*))

(defun print-condition (c)
  (print-span
   `((:fg-white "Condition: ")
     (:fg-red (:underline ,(princ-to-string (type-of c))) #\newline
	      ,(princ-to-string c) #\newline))))

(defun deblarg (c hook &optional frame)
  "Entry point for the debugger, used as the debugger hook."
  (declare (ignore hook))		;@@@ wrong
  (setf *saved-frame* (or frame (debugger-internal-frame)))
  (when (not *debugger-keymap*)
    (setup-keymap))
  (unwind-protect
    (progn
      ;;(try-to-reset-curses)
      (try-to-reset-terminal)
      (start-visual)
      (when (> *repl-level* 20)
	(format t "Something has probably gone wrong, so I'm breaking.~%")
	;; Abort assumes a restart is active, which may not be the case.
	;; But break seems to work.
	(break))
      (format *debug-io* "Entering the debugger.~a~%"
	      (if (= (random 4) 1) (s+ " Blarg"
				       (if (zerop (random 2)) #\. #\!)) ""))
      (with-standard-io-syntax
	(let ((*interceptor-condition* c)
	      (*current-frame* *saved-frame*)
	      ;; Reset reader vars to sane values:
	      ;; [probably uneeded since we use with-standard-io-syntax]
	      (*read-suppress* nil)
	      (*read-base* 10)
	      (*read-eval* t)
	      ;; printer vars
	      (*print-readably* nil)
	      (*print-length* 50)	; something reasonable?
	      (*print-circle* t))
	  (print-condition c)
	  (list-restarts (compute-restarts c))
	  (tt-finish-output)
	  (tiny-repl :interceptor #'debugger-interceptor
		     :prompt-func #'debugger-prompt
		     :keymap *debugger-keymap*
		     :output *debug-io*
		     :debug t
		     :no-announce t))))
;;;    (Format *debug-io* "Exiting the debugger level ~d~%" *repl-level*)
    (reset-visual)))

; (defvar *repl-debug* nil
;   "True to invoke the debugger when a error occurs.")

(defun in-emacs-p ()
  "Return true if we're being run under Emacs, like probably in SLIME."
  (d-getenv "EMACS"))

(defun activate ()
  (when (not (in-emacs-p))
    (format *debug-io* "Activating the DEBLARGger.~%")
    (setf *debugger-hook* 'deblarg)
    (activate-stepper)))

(defvar *saved-debugger-hook* nil
  "The old value of *debugger-hook*, so we can restore it.")

(defun toggle ()
  "Toggle the debugger on and off."
  (when (not (in-emacs-p))
    (if (eq *debugger-hook* 'deblarg)
	(setf *debugger-hook* *saved-debugger-hook*)
	(setf *saved-debugger-hook* *debugger-hook*
	      *debugger-hook* 'deblarg))))

(defun active-p ()
  "Return true if the debugger is set to activate."
  (eq *debugger-hook* 'deblarg))

;; Remove temporary features
#+tbd-has-breakpoints (d-remove-feature :tdb-has-breakpoints)

;; EOF
