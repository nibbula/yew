;;;
;;; deblarg.lisp - Debugger.
;;;

;; This would normally be used with TINY-REPL or LISH.

;;; TODO:
;;; - how about using swank? conium?
;;; - try getting more specific source location with *read-intern*

(in-package :deblarg)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implementation specifc methods
;;

;; For line mode:

(def-debug backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can.")

(def-debug old-backtrace (n)
  "An old style backtrace in case the normal one doesn't work for you.")

(def-debug show-source (n)
  "Show the source for frame N.")

(def-debug show-locals (n)
  "Show the local variables for frame N.")

(def-debug eval-in-frame (n form)
  "Return the result of evaluating FORM in frame N.")

(def-debug up-frame (&optional count)
  "Move the current frame up by ‘count’ which defaults to 1.")

(def-debug down-frame (&optional count)
  "Move the current frame down by ‘count’ which defaults to 1.")

(def-debug set-frame (frame)
  "Set the current frame to ‘frame’.")

(def-debug top-frame (count)
  "Set the current frame to the top frame.")

(def-debug hook ()
  "Return the current debugger hook." :no-object t)

(def-debug set-hook (symbol)
  "Set the debugger hook to the function named by ‘symbol’." :no-object t)

;; For visual mode:

(def-debug source (frame &optional source-height)
  "Return up to ‘source-height’ lines of source code for ‘frame’ if we can.")

(def-debug source-path (frame)
  "Return the name of the source file for ‘frame’ if we can.")

(def-debug backtrace-lines (n)
  "Return ‘n’ lines of backtrace from the current frame.")

(def-debug activate-stepper (&key quietly)
  "Activate the stepper." :no-object t :quiet t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implementation independent functions
;;

;; Visual mode

(defconstant +box_drawings_light_horizontal+         (code-char #x2500)) ; ─
(defconstant +box_drawings_light_vertical_and_left+  (code-char #x2524)) ; ┤
(defconstant +box_drawings_light_vertical_and_right+ (code-char #x251c)) ; ├

(defun horizontal-line (tt &optional note)
  (terminal-color tt :blue :default)
  (if note
      (progn
	(terminal-write-string tt (s+
			     +box_drawings_light_horizontal+
			     +box_drawings_light_horizontal+
			     +box_drawings_light_vertical_and_left+))
	(terminal-color tt :white :black)
	(terminal-write-string tt (s+ " " note " "))
	(terminal-color tt :blue :default)
	(terminal-write-char tt +box_drawings_light_vertical_and_right+)
	(terminal-format tt "~v,,,va"
		   (- (terminal-window-columns tt) (length (s+ note)) 6)
		   +box_drawings_light_horizontal+
		   +box_drawings_light_horizontal+)
	(terminal-color tt :default :default)
	(terminal-write-char tt #\newline))
      ;; no note, just a line
      (progn
      	(terminal-color tt :blue :default)
	(terminal-format tt "~v,,,va~%"
		   (1- (terminal-window-columns tt))
		   +box_drawings_light_horizontal+
		   +box_drawings_light_horizontal+)
	(terminal-color tt :default :default))))

(defun sanitize-line (line)
  (when line
    (if (> (olength line) 0)
	;; (apply #'s+ (map 'list #'char-util:displayable-char line))
	(with-output-to-fat-string (stream)
	  (let (dc new-c)
	    (flet ((print-char-with-effects-from (c ec)
		     (setf new-c (make-fatchar :c c))
		     (copy-fatchar-effects ec new-c)
		     (princ new-c stream)))
	      (omapn (lambda (c)
		       (typecase c
			 (fatchar
			  (setf dc (char-util:displayable-char (fatchar-c c)))
			  (if (equal dc (fatchar-c c))
			      (princ c stream)
			      (typecase dc
				(character
				 (print-char-with-effects-from dc c))
				(string
				 (omapn 
				  (_ (print-char-with-effects-from _ c))
				  dc)))))
			 (t
			  (princ (char-util:displayable-char c) stream))))
		     (if (typep line 'fat-string) (fat-string-string line) line)
		     ))))
	;; (apply #'fs+ (map 'list #'char-util:displayable-char line))
	line)))

(defun visual ()
  (with-slots (visual-term current-frame) *deblarg*
    (let ((tt visual-term))
      (terminal-get-size tt)
      (with-saved-cursor (tt)
	(let* ((source-height (truncate (/ (terminal-window-rows tt) 3)))
	       (stack-height (min 10 source-height))
	       (command-height (- (terminal-window-rows tt)
				  (+ stack-height source-height 2)))
	       (command-top (- (terminal-window-rows tt) (1- command-height)))
	       (src (or
		     ;; (ignore-errors
		     ;;   (dd-source current-frame source-height))
		     (handler-case
			 (dd-source current-frame source-height)
		       (condition (c)
			 (list (format nil "~w" c))))
		     '("Unavailable.")))
	       (path (or (ignore-errors
			   (dd-source-path current-frame))
			 '("Unknown")))
	       (stack (or (ignore-errors
			    (dd-backtrace-lines stack-height))
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
		   ;;(terminal-format tt "~a~%"
		   (terminal-write-line
		    tt (osubseq line
				0 (min (- (terminal-window-columns tt) 2)
				       (olength line))))
		   (setf sp (cdr sp)))
		 (terminal-format tt "~~~%")))
	  (horizontal-line tt path)
	  ;; Stack area
	  (loop :with line :and sp = stack
	     :for i :from 0 :below stack-height :do
	     (setf line (car sp))
	     (if line
		 (progn
		   (print-stack-line line :width (terminal-window-columns tt))
		   (setf sp (cdr sp)))
		 (terminal-format tt "~~~%")))
	  (horizontal-line tt)
	  ;; Command area
	  (terminal-set-scrolling-region tt command-top
					 (terminal-window-rows tt))
	  (terminal-move-to tt (1- (terminal-window-rows tt)) 0)
	  (terminal-finish-output tt))))))

(defvar *fake-term* nil "Workaround for problems with crunch.")

(defun start-visual ()
  (with-slots (visual-mode visual-term term) *deblarg*
    (cond
      (visual-mode
       ;; (when (not visual-term)
       ;;   (setf visual-term (make-instance 'terminal-ansi))
       ;;   (terminal-start visual-term))
       ;;(setf visual-term *terminal*)
       (setf visual-term term)
       (let ((tt visual-term))
	 (terminal-get-size tt)
	 (terminal-move-to tt (1- (terminal-window-rows tt)) 0)
	 (terminal-finish-output tt)))
      ;; @@@ temporary workaround
      ;; ((typep *terminal* 'terminal-crunch:terminal-crunch)
      ;;  (setf *fake-term* *terminal*
      ;; 	   *terminal* (make-instance 'terminal-ansi)))
      )))

(defun reset-visual ()
  (with-slots (visual-term) *deblarg*
    (cond
      (visual-term
       (let ((tt visual-term))
	 (terminal-set-scrolling-region tt nil nil)
	 (terminal-move-to tt (1- (terminal-window-rows tt)) 0)
	 (terminal-finish-output tt)
	 #| (terminal-end tt) |#)
       (setf visual-term nil))
      ;; @@@ temporary workaround
      ;; (*fake-term*
      ;;  (terminal-done *terminal*)
      ;;  (setf *terminal* *fake-term*
      ;; 	   *fake-term* nil))
      )))

(defun debugger-up-frame-command (&optional foo)
  (declare (ignore foo))
  (dd-up-frame)
  (visual))

(defun debugger-down-frame-command (&optional foo)
  (declare (ignore foo))
  (dd-down-frame)
  (visual))

(defun list-restarts (rs)
  (with-slots (term) *deblarg*
    (print-span '((:underline "Restarts") " are:" #\newline))
    (loop :with i = 0 :for r :in rs :do
       ;; (format *debug-term* "~&")
       (print-span `((:fg-cyan ,(princ-to-string i)) ": "))
       (when (not (ignore-errors (progn (format term "~s ~a~%"
						(restart-name r) r) t)))
	 (format term "Error printing restart ")
	 (print-unreadable-object (r term :type t :identity t)
	   (format term "~a" (restart-name r)))
	 (terpri term))
       (incf i))))

(defun debugger-prompt (e p)
  (declare (ignore e))
  (when (debugger-visual-mode *deblarg*)
    (visual))
  (let ((string (format nil "Debug ~d~a" *repl-level* p)))
    (format t "~a" string)
    t))

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
     (defun ,(command-name name) (repl-state restarts)
       (declare (ignorable repl-state restarts))
       ,@body)
     (loop :for a :in ',aliases :do
	(setf (gethash a *debugger-commands*)
	      (make-debugger-command :name ',name :aliases ',aliases)))))

(defun define-debugger-alias (alias command)
  "Make ALIAS be another way to invoke COMMAND. COMMAND must already be a
debugger command."
  (let ((cmd (get-command command)))
    (if (not cmd)
	(format *debug-io*
		"I can't make an alias to a non-existent command ~s." command)
	(progn
	  (push alias (debugger-command-aliases cmd))
	  (setf (gethash alias *debugger-commands*) cmd)))))

(defun get-command (keyword)
  (gethash keyword *debugger-commands*))

(defun debugger-help ()
  (with-slots (term condition) *deblarg*
    (terminal-format term "Debugger help:~%")
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
     term :print-titles nil :trailing-spaces nil)
    (list-restarts (cdr (compute-restarts condition)))))

(defun debugger-snargle (arg)
  "Magic command just for me."
  (error "Pizza ~s ~s." arg (type-of arg)))

(defun toggle-visual-mode (repl-state)
  (declare (ignore repl-state))
  (with-slots (visual-mode) *deblarg*
    (setf visual-mode (not visual-mode))
    (if visual-mode
	(start-visual)
	(reset-visual))))

(defun restart-number (n restarts)
  "Return the restart number N in RESTARTS or NIL if it's not in RESTARTS."
  (when (and restarts (>= n 0) (< n (length restarts)))
    (nth n restarts)))

(defun clear-auto-restart (&key (report-p t))
  "Turn off automatic restarts."
  (with-slots (repeat-restart repeat-condition) *thread*
    (setf repeat-restart nil
	  repeat-condition nil)
    (when report-p
      (format *debug-io* "Stopping automatic restarts.~%"))))

(defun do-restart (r restarts &key keep-p)
  "Invoke restart named R from the list of RESTARTS. If KEEP-P is true, 
keep the current automatic restarts set, otherwise clear them."
  (format *debug-io* "~:(~a~).~%" r)
  ;; This is like find-restart, but omits the most recent abort
  ;; which is this debugger's.
  (let ((borty (find r restarts :key #'restart-name)))
    (if (not borty)
	(format *debug-io* "Can't find an ~a restart!~%" r)
	(progn
	  (when (not keep-p)
	    (clear-auto-restart :report-p nil))
	  (invoke-restart-interactively borty)))))

(defun restart-until (r restarts)
  "Repeatedly invoke a restart until we get a different condition."
  (with-slots (condition) *deblarg*
    (with-slots (repeat-condition repeat-restart) *thread*
      (let ((real-restart (or (restart-number r restarts)
			      (find r restarts :key #'restart-name))))
	(if (not real-restart)
	    (format *debug-io* "Can't find an '~a' restart!~%" r)
	    (progn
	      (setf repeat-condition condition
		    repeat-restart (restart-name real-restart))
	      (do-restart repeat-restart restarts :keep-p t)))))))

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
       (print-span '((:underline "Backtrace") #\: #\newline))
       (dd-backtrace (read-arg repl-state)))
      (wacktrace  (:w) "Wacktrace."
       (print-span '((:underline "Backtrace") #\: #\newline))
       (dd-backtrace (read-arg repl-state)))
      (old-backtrace  (:ob) "Old backtrace."
       (dd-old-backtrace (read-arg repl-state)))
      (restarts   (:r) "Show restarts." (list-restarts restarts))
      (source     (:src)
       "Show source for a frame N, which defaults to the current frame."
       (dd-show-source (read-arg repl-state)))
      (locals     (:l)
       "Show local variables for frame N, which defaults to the current frame."
       (dd-show-locals (read-arg repl-state)))
      (snargle    (:z) "Snargle" (debugger-snargle (read-arg repl-state)))
      (visual     (:v) "Toggle visual mode."
       (toggle-visual-mode (read-arg repl-state)))
      (up-frame   (:u) "Up a frame."
       (dd-up-frame (read-arg repl-state)))
      (down-frame (:d) "Down a frame."
       (dd-down-frame (read-arg repl-state)))
      (set-frame  (:f) "Set the frame."
       (dd-set-frame (read-arg repl-state)))
      (top        (:t) "Go to the top frame."
       (dd-top-frame (read-arg repl-state)))
      (eval-in    (:ev) "Evaluate in frame N."
       (eval-print
	(multiple-value-list
	 (let ((arg2 (read-arg repl-state))
	       (arg1 (read-arg repl-state)))
	 (dd-eval-in-frame arg1 arg2)))))
      (error      (:e)   "Show the error again."
       (print-condition (debugger-condition *deblarg*)))
      (abort      (:a :abort :pop)   "Abort to top level."
       (do-restart 'abort restarts))
      (continue   (:c)   "Invoke continue restart."
       (do-restart 'continue restarts))
      (restart-until (:ru)   "Invoke restart until you get a different error."
       (restart-until (read-arg repl-state) restarts))
      (auto-restart-clear (:arc) "Clear the automatic restart."
       (clear-auto-restart))
      ;; (next       (:n)   (dd-next))
      ;; (step       (:s)   (dd-step))
      ;; (out        (:o)   (dd-out))
      (alias      (:ali :alias) "Define a alias command."
       (defalias (read-arg repl-state) (read-arg repl-state)))
      (help       (:h :help) "Show this help." (debugger-help))
      (quit       (:q :quit) "Quit the whatever."
       (when (y-or-n-p "Really quit? ")
	 (format *debug-io* "We quit.~%")
	 (finish-output)
	 (dlib:exit-system))))))
(define-commands *base-commands*)

#+tdb-has-breakpoints
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *breakpoint-commands*
      #((list-breakpoints (:lb :lbp :list) "List breakpointss."
	 (list-breakpoints))
	(set-breakpoint (:sb :sbp :set)
	 "Set breakpoints on function."
	 (set-func-breakpoint (eval (read-arg repl-state))))
	(toggle-breakpoint (:tb :tbp :toggle)
	 "Toggle breakpoints."
	 (toggle-breakpoint (read-arg repl-state)) t)
	(activate-breakpoint (:ab :abp :activate)
	 "Activate breakpoints."
	 (activate-breakpoint (read-arg repl-state)) t)
	(deactivate-breakpoint (:db :dbp :deactivate)
	 "Deactivate breakpoints."
	 (deactivate-breakpoint (read-arg repl-state)) t)
	(delete-breakpoint (:xb :xbp :delete)
	 "Delete breakpoints."
	 (delete-breakpoint (read-arg repl-state))))))
  (define-commands *breakpoint-commands*))

;; Remember we have to return non-NIL if we want to tell the REPL that we
;; handled it.
(defun debugger-interceptor (value repl-state)
  "Handle special debugger commands, which are usually keywords."
  (let ((restarts (cdr (compute-restarts (debugger-condition *deblarg*)))))
    (cond
      ;; We use keywords as commands, just in case you have a variable or some
      ;; other symbol clash. I dunno. I 'spose we could use regular symbols,
      ;; and have a "print" command.
      ((typep value 'keyword)
       (let ((ks (string value)) n)
	 ;; :r<n> restart keywords - to be compatible with CLisp
	 (if (and (> (length ks) 1) (equal (aref ks 0) #\R)
		  (setf n (parse-integer ks :start 1 :junk-allowed t)))
	     ;; (invoke-restart-interactively (nth n (compute-restarts)))))
	     ;; (format t "[Invoking restart ~d (~a)]~%" n (nth n restarts))
	     (invoke-restart-interactively (nth n restarts))
	     (let ((cmd (get-command value)))
	       (when cmd
		 (funcall (command-name (debugger-command-name cmd))
			  repl-state restarts)))))
	 t)
      ;; symbols that aren't keywords
      ((typep value 'symbol)
       (let ((sym (command-name value)))
	 (if (fboundp sym)
	     (progn
	       (funcall sym repl-state restarts)
	       t)
	     nil)))
      ;; Numbers invoke that numbered restart.
      ((typep value 'integer)
       (let ((r (restart-number value restarts)))
	 (if r
	     (invoke-restart-interactively r)
	     (format *debug-io*
		     "~a is not a valid restart number.~%" value)))
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
  ;; This whole thing assumes too much!
  #|
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
    (finish-output))
  |#
  ;; Instead, let's try:
  ;; (when *debug-io*
  ;;   (format *debug-io* "wtf you badger ~s~%" *terminal*))
  (when (and *terminal* (typep *terminal* 'terminal:terminal))
    (setf (terminal-input-mode *terminal*) :char)
    (setf (terminal-input-mode *terminal*) :line)
    (terminal-reset *terminal*)))

;; @@@ This hackishly knows too much about RL.
(defun debugger-redraw (e)
  (with-slots (visual-term) *deblarg*
    (if visual-term
	(progn
	  (terminal-clear visual-term)
	  (terminal-beginning-of-line visual-term)
	  (terminal-erase-to-eol visual-term)
	  (setf (rl:screen-col e) 0)
	  (debugger-prompt rl:*line-editor* "> ")
	  (terminal-finish-output visual-term))
	(rl::redraw-command e))
    nil))

(defvar *debugger-keymap* nil "Keymap for the debugger.")
(defvar *debugger-escape-keymap* nil "Escape key Keymap for the debugger.")

(defun setup-keymap ()
  (setf *debugger-keymap* (copy-keymap rl:*normal-keymap*
				       :name '*debugger-keymap*))
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
  (with-slots (term) *deblarg*
    (when (not (ignore-errors
		(print-span
		 `((:fg-white "Condition: ")
		   (:fg-red (:underline ,(princ-to-string (type-of c)))
			    #\newline
			    ,(princ-to-string c) #\newline)))
		t))
      (format term "Error printing condition ")
      (print-unreadable-object (c term :type t :identity t))
      (terpri term)
      (describe c))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-deblargger ((frame condition) &body body)
    `(let ((*deblarg*
	     (make-deblargger :condition ,condition :saved-frame ,frame)))
       (when (not *thread*)
	 ;; @@@ This isn't really thread local yet.
	 (setf *thread* (make-thread)))
       (when (not *debugger-keymap*)
	 (setup-keymap))
       (with-new-debugger-io (*deblarg*)
	 ,@body))))

(defun deblarg (c hook &optional frame)
  "Entry point for the debugger, used as the debugger hook."
  (declare (ignore hook))		;@@@ wrong
  (with-deblargger (frame c)
    (with-slots (condition) *deblarg*
      (with-slots (repeat-condition repeat-restart) *thread*
	(when repeat-restart
	  (if (typep condition (type-of repeat-condition))
	      (do-restart repeat-restart (compute-restarts c) :keep-p t)
	      (clear-auto-restart)))))
	      ;; (progn
	      ;; 	(setf repeat-restart nil
	      ;; 	      repeat-condition nil)
	      ;; 	(format *debug-io* "Stopping automatic restarts.~%"))))))
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
		(if (= (random 4) 1)
		    (s+ " Blarg" (if (zerop (random 2)) #\. #\!)) ""))
	(with-standard-io-syntax
	  ;; Reset reader vars to sane values:
	  ;; [probably uneeded since we use with-standard-io-syntax]
	  (let ((*read-suppress* nil)
		(*read-base* 10)
		(*read-eval* t)
		;; printer vars
		(*print-readably* nil)
		(*print-length* 50)	; something reasonable?
		(*print-pretty* nil)
		(*print-circle* nil))
	    (print-condition c)
	    (list-restarts (compute-restarts c))
	    (terminal-finish-output (debugger-term *deblarg*))
	    (tiny-repl :interceptor #'debugger-interceptor
		       :prompt-func #'debugger-prompt
		       :keymap *debugger-keymap*
		       :output *debug-io*
		       :debug t
		       :terminal (debugger-term *deblarg*)
		       :no-announce t))))
;;;    (Format *debug-io* "Exiting the debugger level ~d~%" *repl-level*)
      (reset-visual))))

(defun in-emacs-p ()
  "Return true if we're being run under Emacs, like probably in SLIME."
  (nos:env "EMACS"))

(defvar *saved-debugger-hook* nil
  "The old value of *debugger-hook*, so we can restore it.")

(defun activate (&key quietly #| full |#)
  (when (and (not (in-emacs-p)) (not (active-p)))
    (when (not quietly)
      (format *debug-io* "Activating the DEBLARGger.~%"))
    (dd-set-hook 'deblarg)
    (dd-activate-stepper :quietly quietly)))

(defun deactivate (&key quietly #| full |#)
  (when (and (not (in-emacs-p)) (active-p))
    (when (not quietly)
      (format *debug-io* "Deactivating the DEBLARGger.~%"))
    (dd-set-hook *saved-debugger-hook*)))

(defun toggle ()
  "Toggle the debugger on and off."
  (when (not (in-emacs-p))
    (if (eq (dd-hook) 'deblarg)
	(dd-set-hook *saved-debugger-hook*)
	(progn
	  (setf *saved-debugger-hook* (dd-hook))
	  (dd-set-hook 'deblarg)))))

(defun active-p ()
  "Return true if the debugger is set to activate."
  (eq (dd-hook) 'deblarg))

;; Remove temporary features
#+tbd-has-breakpoints (d-remove-feature :tdb-has-breakpoints)

;; EOF
