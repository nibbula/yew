;;
;; tiny-debug.lisp - Multi platform minimal command line debugger
;;

;; $Revision: 1.5 $

;;; - debugger improvements.
;;;   - we should probably investigate the feasibility of having the system
;;;     debugger use our readline on the various implementations, or for
;;;     that matter making the system repl use our readline.
;;; - how about using swank?

(defpackage :tiny-debug
  (:documentation "Multi-platform minimal command line debugger")
  (:use :cl :tiny-repl)
  (:export
   #:tiny-debug
   #:*default-interceptor*
   #:toggle
   #:active-p
   ))
(in-package :tiny-debug)

(defvar *current-frame* nil
  "Current frame number. Frames are numbered from the top or innermost 0 to
the outermost. When entering the debugger the current frame is 0.")

(defvar *saved-frame* nil
  "Implementation handle to frame that the debugger started from.")

(defun debugger-sorry (x)
  "What to say when we can't do something."
  (format *debug-io* "~%Sorry, don't know how to ~a on ~a. ~
		       Snarf some slime!~%" x (lisp-implementation-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation specifc functions
;;
;; debugger-backtrace n     - Show N frames of normal backtrace
;; debugger-wacktrace n     - Alternate backtrace
;; debugger-show-source n   - Show the source for frame N
;; debugger-show-locals n   - Show local variables for frame N
;; debugger-internal-frame  - Return the best approximation of the error frame.

#+sbcl
(defun print-frame (f)
  "Print a frame."
  (labels ((print-lambda-list (f arg-list)
	     (loop :for v :in arg-list
		:do
		(cond
		  ((sb-di::debug-var-p v)
		   ;;(sb-di::debug-var-symbol v)
		   (let ((vv (handler-case
				 (sb-di::debug-var-value v f)
			       (condition () '#:|<Unavailable>|))))
		     (when (not (or (eql vv '#:|<Unavailable>|)
				    (typep vv 'condition)))
		       (format *debug-io* " ~s" vv))))
		  ((and (listp v) (eql (car v) :rest))
		   (format *debug-io* " &rest")
		   (print-lambda-list f (cdr v)))))))
    (let* ((loc     (sb-di:frame-code-location f))
	   (dbg-fun (sb-di:code-location-debug-fun loc))
	   (name    (sb-di:debug-fun-name dbg-fun)))
      (format *debug-io* "(")
      (if (symbolp name)
	  (let ((pkg (symbol-package name)))
	    (if (not (eql (find-package :cl) pkg))
		(format *debug-io* "~(~a~)::" (package-name pkg)))))
      (format *debug-io* "~(~a~)" name)
      (print-lambda-list f (sb-di::debug-fun-lambda-list dbg-fun))
      (format *debug-io* ")"))))


#+sbcl
(defun sbcl-start-frame ()
  ;; introduced sometime after 1.0.57
  (let ((sym (find-symbol "BACKTRACE-START-FRAME" :sb-debug)))
    (if sym
     (funcall sym :debugger-frame)
     (sb-di:top-frame))))

#+sbcl
(defun debugger-wacktrace (n)
  "Our own backtrace for SBCL."
  ;; (or *saved-frame* (sb-di:top-frame))
  (loop
     :with f = (sbcl-start-frame)
     :and i = 0
     :do
     (format *debug-io* "~3d " (sb-di:frame-number f))
     (print-frame f)
     (terpri *debug-io*)
     (setf f (sb-di:frame-down f))
     (incf i)
     :until (and (not f) (>= i n))))

#+ccl
(defun debugger-wacktrace (n)
  "Our own backtrace for CCL."
  (declare (ignore n))
  (let ((frames '()) (*print-readably* nil))
    (ccl:map-call-frames
     #'(lambda (frame-ptr context)
	 (push (list frame-ptr context) frames))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (setf frames (nreverse frames))
    (loop :with i = 0
       :for (f context) :in frames :do
       (let ((fun (ccl:frame-function f context)))
	 (format *debug-io* "~3a (~a~{ ~s~})~%" i (ccl:function-name fun)
		 (ccl:frame-supplied-arguments f context)))
       (incf i))))

#-(or ccl sbcl)
(defun debugger-wacktrace (n)
  (declare (ignore n)) (debugger-sorry "wacktrace"))

#+ccl
(defun OLD-get-frame (n)
  (let ((f 0) frame-ptr context)
    (ccl:map-call-frames
     #'(lambda (p c)
	 (when (= f n)
	   (setf frame-ptr p
		 context c)
	   (incf f)))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (values frame-ptr context)))

#+ccl
(defun get-frame (n)
  (let ((f 0) frame-ptr context)
    (ccl:map-call-frames
     #'(lambda (p c)
	 (when (= f (1+ n))
	   (setf frame-ptr p
		 context c))
	 (incf f)))
    (values frame-ptr context)))

;; (ccl:map-call-frames
;;  #'(lambda (p c)
;;      (multiple-value-bind (func pc) (ccl:frame-function p c)
;;        (format t "~s ~s ~4d ~s~%~s~%" p c pc func
;; 	       (ccl:source-note-filename (ccl:function-source-note func))))))

#+ccl
(defun debugger-show-source (n)
  (when (not n)
    (setf n 0))
  (let ((*print-readably* nil))
    (multiple-value-bind (pointer context) (get-frame n)
      (multiple-value-bind (func pc) (ccl:frame-function pointer context)
	(let ((note (if pc
			(or (ccl:find-source-note-at-pc func pc)
			    (ccl:function-source-note func))
			(ccl:function-source-note func))))
	  (when note
	    (cond
	      ((ccl:source-note-text note)
	       (format *debug-io* "~a~%" (ccl:source-note-text note)))
	      ((ccl:source-note-filename note)
	       (with-open-file (stream (ccl:source-note-filename note))
		 (file-position stream (ccl:source-note-start-pos note))
		 (let* ((len (- (ccl:source-note-end-pos note)
				(ccl:source-note-start-pos note)))
			(str (make-array (list len) :element-type 'character)))
		   (read-sequence str stream)
		   (format *debug-io* "~a~%" str)))))))))))

#+sbcl
(defun debugger-show-source (n)
  (when (not n)
    (setf n 0))
  ;; @@@ handle NO-DEBUG-BLOCKS !!
  ;; (sb-debug::code-location-source-form
  (let* ((loc	   (sb-di:frame-code-location (frame-number n)))
	 (src	   (sb-di::code-location-debug-source loc))
	 (filename (sb-c::debug-source-namestring src))
	 (dbg-fun  (sb-di:code-location-debug-fun loc))
	 (name	   (sb-di:debug-fun-name dbg-fun)))
    (with-open-file (str filename)
      (format t "~s ~s" name filename))))

#-(or ccl sbcl)
(defun debugger-show-source (n)
  (declare (ignore n)) (debugger-sorry "show source"))

#+ccl
(defun debugger-show-locals (n)
  (let ((*print-readably* nil))
    (multiple-value-bind (pointer context) (get-frame n)
    (loop :for (name . value) :in (ccl:frame-named-variables pointer context)
       :do
       (format *debug-io* "~a = ~a~%" name value)))))

#+sbcl
(defun frame-number (n)
  "Return the internal frame object given a frame number."
  (let ((result 
	 (loop :with f = (sb-di:top-frame)
	    :for fn :from 0 :below n
	    :do (setf f (sb-di:frame-down f))
	    :finally (return f))))
    (assert (= (sb-di:frame-number result) n))
    result))

;; @@@ This really doesn't work right yet
#+sbcl
(defun debugger-show-locals (n)
  (when (not n)
    (setf n *current-frame*))
  (format *debug-io* "Locals for frame ~d:~%" n)
  (let* ((cur (frame-number *current-frame*))
	 (fun (sb-di:frame-debug-fun cur)))
    (if (sb-di:debug-var-info-available fun)
	(let* ((*print-readably* nil)
	       (loc (sb-di:frame-code-location cur)))
	  (loop :for v :in (sb-di:ambiguous-debug-vars fun "")
	     :do
	     (when (eq (sb-di:debug-var-validity v loc) :valid)
              (format *debug-io* "~S~:[#~W~;~*~] = ~S~%"
                      (sb-di:debug-var-symbol v)
                      (zerop (sb-di:debug-var-id v))
                      (sb-di:debug-var-id v)
                      (sb-di:debug-var-value v cur))))))))

;; test fun for locals:
;; (defun foo (x)
;;   (declare (optimize (debug 3)))
;;   (let ((a "hi") (b "bye")) (+ x 23) (format t "~a ~a~%" a b)))

#-(or sbcl ccl)
(defun debugger-show-locals (n)
  (declare (ignore n)) (debugger-sorry "show locals"))

#|
#+ecl (defvar *stack-base* 0)

#+ecl
(defun ecl-args (frame)
  (let ((base (or (si::sch-frs-base si::*frs-top* *stack-base*)
		  (1+ (si::frs-top)))))
    (loop :with i :and name
       :for f :from base :until (si::frs-top)
       :do
       (setf i (- (si::frs-ihs f) *stack-base* 1))
       :if (and (plusp i) (= i frame) (not (si::fixnump (si::frs-tag f))))
       :collect (si::frs-tag f))))

(si::ihs-env i)
|#

#+ecl
(defun ecl-backtrace (n)
  ;; (loop :for i :from 0 :below (min (si::ihs-top) n)
  ;;    :do (format *debug-io* "~a ~a~%" (si::ihs-fun i) #|(ecl-args i)|#))
  (let* ((top (if n (min n (si::ihs-top)) (si::ihs-top)))
	 (stack (reverse (loop :for i :from 1 :below top
			    :collect (si::ihs-fun i)))))
    (loop :for s :in stack :and i = 0 :then (1+ i)
       :do (format *debug-io* "~3d: ~w~%" i s))))

;; As you may know, this is very implementation specific.
(defun debugger-backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the innermost N contexts, if we can."
  #+sbcl (if n (sb-debug:backtrace n) (sb-debug:backtrace))
;  #+sbcl (sbcl-wacktrace)
  #+ccl (loop :with i = 0
	   :for b :in (ccl::backtrace-as-list)
	   :do (format *debug-io* "~(~3d ~a~)~%" i b) (incf i)
	   :while (or (null n) (and (numberp n) (< i n))))
;  #+clisp (system::print-backtrace :mode 4)  ; @@@ pick different modes?
  ;; Or perhaps
  #+clisp (catch 'debug (system::debug-backtrace "4"))
  #+lispworks (dbg:output-backtrace :full)
  #+ecl (ecl-backtrace n)
  #-(or sbcl ccl clisp lispworks ecl)
  (debugger-sorry "backtrace"))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  #+sbcl (sb-di::top-frame)
  #+ccl ccl:*top-error-frame*
  ;; We don't want to be sorry here, so just be wrong.
  #-(or sbcl ccl) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation independent functions

(defun list-restarts (rs)
  (format *debug-io* "Restarts are:~%")
  (loop :with i = 0 :for r :in rs :do
     (format *debug-io* "~&~d: " i)
     (when (not (ignore-errors (progn (format *debug-io* "~a~%" r) t)))
       (format *debug-io* "Error printing restart ")
       (print-unreadable-object (r *debug-io* :type t :identity t)
	 (format *debug-io* "~a" (restart-name r)))
       (terpri *debug-io*))
     (incf i)))

(defun debug-prompt (e p)
;  (format *debug-io* "Debug ~d~a" *repl-level* p)
  (tiny-rl::editor-write-string		; XXX
   e
   (format nil "Debug ~d~a" *repl-level* p))
;  (finish-output *debug-io*)
  nil)

(declaim (special *interceptor-condition*))

(defun debugger-help ()
  (format *debug-io* "Tiny Debugger help:
:h      Show this help.
:e      Show the error again.
:a      Abort to top level.
:q      Quit the whatever.
:r      Show restarts.
:b      Backtrace stack.
:w      Wacktrace.
:s [n]	Show source for a frame N, which defaults to the current frame.
:l [n]	Show local variables for a frame N, which defaults to the current frame.
number  Invoke that number restart (from the :r list).
...     Or just type a some lisp code.
~%")
  (list-restarts (cdr (compute-restarts *interceptor-condition*))))

(defun debugger-snargle ()
  "Magic command just for me."
  (error "Pizza."))

(defun debugger-interceptor (value state)
  "Handle special debugger commands, which are usually keywords."
  (cond
    ;; We use keywords as commands, just in case you have a variable or some
    ;; other symbol clash. I dunno. I 'spose we could use regular symbols,
    ;; and have a "print" command.
    ((typep value 'keyword)
     (let ((ks (string value))
	   (rs (cdr (compute-restarts *interceptor-condition*))))
       (when (and (> (length ks) 1) (equal (aref ks 0) #\R))
	 (let ((n (parse-integer (subseq ks 1))))
;	   (invoke-restart-interactively (nth n (compute-restarts)))))
;	   (format t "[Invoking restart ~d (~a)]~%" n (nth n rs))
	   (invoke-restart-interactively (nth n rs))))
       (case value
	 (:b (debugger-backtrace (read-arg state)) t)
	 (:w (debugger-wacktrace (read-arg state)) t)
	 (:r (list-restarts rs) t)
	 (:s (debugger-show-source (read-arg state)) t)
	 (:l (debugger-show-locals (read-arg state)) t)
	 ((:h :help) (debugger-help) t)
	 (:z (debugger-snargle) t)
	 (:e (format *debug-io* "~a ~a~%"
		     (type-of *interceptor-condition*)
		     *interceptor-condition*) t)
	 (:a (format *debug-io* "Abort.~%")
	     ;; This is like find-restart, but omits the most recent abort
	     ;; which is this debugger's.
	     (let ((borty (find 'abort rs :key #'restart-name)))
	       (if (not borty)
		   (format *debug-io* "Can't find an abort restart!~%")
		   (invoke-restart-interactively borty))))
	 ((:q :quit) (format *debug-io* "We quit.~%") (nos:exit-lisp) t))))
    ;; Numbers invoke that numbered restart.
    ((typep value 'number)
     (let ((rs (cdr (compute-restarts *interceptor-condition*))))
       (if (and (>= value 0) (< value (length rs)))
	   (invoke-restart-interactively (nth value rs))
	   (format *debug-io* "~a is not a valid restart number.~%" value))))))

(defun try-to-reset-curses ()
  "If curses is loaded and active try to reset the terminal to a sane state so when we get in error in curses we can type at the debugger."
  (when (find-package :curses)
    (funcall (find-symbol (symbol-name '#:endwin) (find-package :curses)))))

;; @@@ It might be nice if we could avoid this duplication and just call the
;; one in ansiterm.
(defun try-to-reset-terminal ()
  "Try to reset the terminal to a sane state so when we get in error in some program that messes with the terminal, we can still type at the debugger."
  (flet ((out (s) (format *terminal-io* "~c~a" #\escape s)))
    ;; First reset the terminal driver to a sane state.
    (termios:sane)
    ;; Then try to reset the terminal itself to a sane state, assuming an ANSI
    ;; terminal (just like tiny-rl). We could just do ^[c, which is quite
    ;; effective, but it's pretty drastic, and usually clears the screen and
    ;; can even resize the window, which is so amazingly annoying. So let's
    ;; just try do individual things that need resetting.  This is pretty much
    ;; the idea of termcap/info reset string, usually the "rs2", since "rs"
    ;; usually just does ^[c.
    (mapcar
     #'out '(" F"    ;; 7 bit controls
	     "[0m"   ;; color and attributes
	     ">"     ;; normal keypad
	     "#@"    ;; default char set
	     "m"     ;; memory unlock
	     "[4l"   ;; replace mode (vs insert mode)
	     "[?4l"  ;; jump scroll (vs smooth scroll)
	     "[?25h" ;; show the cursor
	     "[?9l"  ;; Don't send position on mouse press
	     "[?47l" ;; Use normal screen buffer
	     ))
    (finish-output)))

(defun tiny-debug (c hook)
  "Entry point for the tiny debugger, used as the debugger hook."
  (declare (ignore hook))		;@@@ wrong
  (setf *saved-frame* (debugger-internal-frame))
  (unwind-protect
    (progn
      ;;(try-to-reset-curses)
      (try-to-reset-terminal)
      (when (> *repl-level* 20)
	(format t "Something has probably gone wrong, so I'm breaking.~%")
	;; Abort assumes a restart is active, which may not be the case.
	;; But break seems to work.
	(break))
      (format *debug-io* "Entering the TINY debugger.~%")
      (format *debug-io* "Condition: ~a~%" c)
      (list-restarts (compute-restarts c))
;      (invoke-restart-interactively nil)
      ;; @@@ how do i invoke a restart to resolve conflicts?
      ;; or set variables?
      (with-standard-io-syntax
	(let ((*interceptor-condition* c)
	      (*current-frame* 0)
	      ;; Reset reader vars to sane values:
	      ;; [probably uneeded since we use with-standard-io-syntax]
	      (*read-suppress* nil)
	      (*read-base* 10)
	      (*read-eval* t)
	      ;; printer vars
	      (*print-readably* nil)
	      (*print-circle* t))
	  (tiny-repl :interceptor #'debugger-interceptor
		     :prompt-func #'debug-prompt
		     :output *debug-io*
		     :debug t
		     :no-announce t))))
;    (Format *debug-io* "Exiting the debugger level ~d~%" *repl-level*)
    ))

; (defvar *repl-debug* nil
;   "True to invoke the debugger when a error occurs.")

(defvar *saved-debugger-hook* nil
  "The old value of *debugger-hook*, so we can restore it.")

(defun toggle ()
  "Toggle the ‘Tiny’ debugger on and off."
  (if (eq *debugger-hook* 'tiny-debug)
      (setf *debugger-hook* *saved-debugger-hook*)
      (setf *saved-debugger-hook* *debugger-hook*
	    *debugger-hook* 'tiny-debug)))

(defun active-p ()
  "Return true if the debugger is set to activate."
  (eq *debugger-hook* 'tiny-debug))

;; EOF
