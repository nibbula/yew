;;
;; deblarg.lisp - Debugger for use with :TINY-REPL
;;

;;; TODO:
;;; - how about using swank? conium?
;;; - try getting more specific source location with *read-intern*

(in-package :deblarg)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation specifc functions are:
;;
;; For line mode:
;;
;; debugger-backtrace n          - Show N frames of normal backtrace
;; debugger-wacktrace n          - Alternate backtrace
;; debugger-show-source n        - Show the source for frame N
;; debugger-show-locals n        - Show local variables for frame N
;; debugger-internal-frame       - Return the best approximation of the error
;;                                 frame.
;; debugger-eval-in-frame n form - Return the result of evaluating FORM in
;;                                 frame N.
;; For visual mode:
;; debugger-source frame source-height
;; debugger-source-path frame
;; debugger-backtrace-lines n

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
		 (print-stack-line line :width (tt-width))
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
     ;; (when (not *visual-term*)
     ;;   (setf *visual-term* (make-instance 'terminal-ansi))
     ;;   (terminal-start *visual-term*))
     (setf *visual-term* *terminal*)
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
       #| (terminal-end tt) |#)
     (setf *visual-term* nil))
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

;; @@@ This hackishly knows too much about RL.
(defun debugger-prompt (e p)
  (when *visual-mode*
    (visual))
  ;; (fresh-line *debug-io*)
  (rl::editor-write-string		; XXX
   e
   (format nil "Debug ~d~a" *repl-level* p))
;  (finish-output *debug-io*)
  nil)

;;; @@@ I actually want to take defcommand out of lish and make it be generic.
;;; And then also the command completion from lish to generic completion.
;;; Then we can define debugger commands nicely with completion and the whole
;;; shebang.

(defstruct debugger-command
  name
  aliases)

(defparameter *debugger-commands* (make-hash-table :test #'equalp)
  "Table of debugger commands.")

(defparameter *debugger-command-list* nil
  "List of debugger commands.")

(defun debugger-command-list ()
  (or *debugger-command-list*
      (setf *debugger-command-list*
	    (remove-duplicates
	     (loop :for v :being :the :hash-values :of *debugger-commands*
		:collect v)
	     :test #'equalp))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-name (symbol)
    (symbolify (s+ #\_ symbol) :package :deblarg)))

(defmacro define-debugger-command (name aliases &body body)
  `(progn
     (defun ,(command-name name) (state restarts)
       (declare (ignorable state restarts))
       ,@body)
     (loop :for a :in ',aliases :do
	(setf (gethash a *debugger-commands*)
	      (make-debugger-command :name ',name :aliases ',aliases)))))

(defun get-command (keyword)
  (gethash keyword *debugger-commands*))

(defun debugger-help ()
  (tt-format "Debugger help:~%")
  (output-table
   (make-table-from
    (nconc
     (loop :for c :in (debugger-command-list)
	:collect
	(list
	 (span-to-fat-string
	  `(:cyan ,(loop :for a :in (debugger-command-aliases c)
		      :collect (s+ (prin1-to-string a) #\space))))
	 (span-to-fat-string
	  `(:white ,(documentation
		     (command-name (debugger-command-name c)) 'function)))))
     `(,(mapcar
	  #'span-to-fat-string
	  '((:fg-cyan "number")
	    (:fg-white "Invoke that number restart (from the :r list).")))
       ,(mapcar
	  #'span-to-fat-string
	  '((:fg-cyan "...")
	    (:fg-white "Or just type a some lisp code."))))))
   (make-instance 'terminal-table:terminal-table-renderer)
   *terminal* :print-titles nil :trailing-spaces nil)
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

(defun do-restart (r restarts)
  (format *debug-io* "~:(~a~).~%" r)
  ;; This is like find-restart, but omits the most recent abort
  ;; which is this debugger's.
  (let ((borty (find r restarts :key #'restart-name)))
    (if (not borty)
	(format *debug-io* "Can't find an ~a restart!~%" r)
	(invoke-restart-interactively borty))))

(defmacro define-commands (var)
  `(progn
     ,@(loop :for c :across (symbol-value var)
	  :collect
	  `(define-debugger-command ,(first c) ,(second c) ,@(cddr c)))))

(defun eval-print (vals)
  (format *debug-io* "~&~{~s~^ ;~%~}~%" vals))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *base-commands*
    #((backtrace  (:b) "Backtrace stack."
       (debugger-backtrace (read-arg state)))
      (wacktrace  (:w) "Wacktrace." (debugger-wacktrace (read-arg state)))
      (restarts   (:r) "Show restarts." (list-restarts restarts))
      (source     (:src)
       "Show source for a frame N, which defaults to the current frame."
       (debugger-show-source (read-arg state)))
      (locals     (:l)
       "Show local variables for frame N, which defaults to the current frame."
       (debugger-show-locals (read-arg state)))
      (snargle    (:z) "Snargle" (debugger-snargle (read-arg state)))
      (visual     (:v) "Toggle visual mode."
       (toggle-visual-mode (read-arg state)))
      (up-frame   (:u) "Up a frame." (debugger-up-frame (read-arg state)))
      (down-frame (:d) "Down a frame." (debugger-down-frame (read-arg state)))
      (set-frame  (:f) "Set the frame." (debugger-set-frame (read-arg state)))
      (top        (:t) "Go to the top frame."
       (debugger-top-frame (read-arg state)))
      (eval-in    (:ev) "Evaluate in frame."
       (eval-print
	(multiple-value-list
	 (debugger-eval-in-frame (read-arg state) (read-arg state)))))
      (error      (:e)   "Show the error again."
       (print-condition *interceptor-condition*))
      (abort      (:a)   "Abort to top level."
       (do-restart 'abort restarts))
      (continue   (:c)   "Invoke continue restart."
       (do-restart 'continue restarts))
      ;; (next       (:n)   (debugger-next))
      ;; (step       (:s)   (debugger-step))
      ;; (out        (:o)   (debugger-out))
      (help       (:h :help) "Show this help." (debugger-help))
      (quit       (:q :quit) "Quit the whatever."
       (when (y-or-n-p "Really quit?")
	 (format *debug-io* "We quit.~%"))))))
(define-commands *base-commands*)

#+tdb-has-breakpoints
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *breakpoint-commands*
      #((list-breakpoints (:lb :lbp :list) "List breakpointss."
	 (list-breakpoints))
	(set-breakpoint (:sb :sbp :set)
	 "Set breakpoints on function."
	 (set-func-breakpoint (eval (read-arg state))))
	(toggle-breakpoint (:tb :tbp :toggle)
	 "Toggle breakpoints."
	 (toggle-breakpoint (read-arg state)) t)
	(activate-breakpoint (:ab :abp :activate)
	 "Activate breakpoints."
	 (activate-breakpoint (read-arg state)) t)
	(deactivate-breakpoint (:db :dbp :deactivate)
	 "Deactivate breakpoints."
	 (deactivate-breakpoint (read-arg state)) t)
	(delete-breakpoint (:xb :xbp :delete)
	 "Delete breakpoints."
	 (delete-breakpoint (read-arg state))))))
  (define-commands *breakpoint-commands*))

;; Remember we have to return non-NIL if we want to tell the REPL that we
;; handled it.
(defun debugger-interceptor (value state)
  "Handle special debugger commands, which are usually keywords."
  (let ((restarts (cdr (compute-restarts *interceptor-condition*))))
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
	 (let ((cmd (get-command value)))
	   (when cmd
	     (funcall (command-name (debugger-command-name cmd))
		      state restarts)
	     t))))
      ;; symbols that aren't keywords
      ((typep value 'symbol)
       (let ((sym (command-name value)))
	 (if (fboundp sym)
	     (progn
	       (funcall sym state restarts)
	       t)
	     nil)))
      ;; Numbers invoke that numbered restart.
      ((typep value 'integer)
       (if (and restarts (>= value 0) (< value (length restarts)))
	   (invoke-restart-interactively (nth value restarts))
	   (format *debug-io*
		   "~a is not a valid restart number.~%" value))
       t)
      (t nil))))

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

;; @@@ This hackishly knows too much about RL.
(defun debugger-redraw (e)
  (if *visual-term*
    (let ((tt *visual-term*))
      (terminal-clear tt)
      (terminal-beginning-of-line tt)
      (terminal-erase-to-eol tt)
      (setf (rl:screen-col e) 0)
      (debugger-prompt rl:*line-editor* "> ")
      (terminal-finish-output tt))
    (rl::redraw-command e))
  nil)

(defvar *debugger-keymap* nil "Keymap for the debugger.")
(defvar *debugger-escape-keymap* nil "Escape key Keymap for the debugger.")

(defun setup-keymap ()
  (setf *debugger-keymap* (copy-keymap rl:*normal-keymap*))
  (loop :for key :in (keys-bound-to 'rl::redraw-command rl:*normal-keymap*)
     :do (define-key *debugger-keymap* key 'debugger-redraw))
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
  (with-new-terminal (:ansi *terminal*
			    :device-name (nos:file-handle-terminal-name
					  (nos:stream-system-handle *debug-io*))
			    :output-stream (make-broadcast-stream *debug-io*))
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
	      (*print-circle* t)
	      )
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
    (reset-visual))))

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
