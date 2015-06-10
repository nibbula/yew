;;
;; tiny-debug.lisp - Multi platform minimal debugger
;;

;; $Revision: 1.5 $

;;; - debugger improvements.
;;;   - we should probably investigate the feasibility of having the system
;;;     debugger use our readline on the various implementations, or for
;;;     that matter making the system repl use our readline.
;;; - how about using swank?

(defpackage :tiny-debug
  (:documentation "Multi-platform minimal debugger")
  (:use :cl :dlib :char-util :keymap :ansiterm :tiny-rl :tiny-repl
	#+sbcl :sb-introspect)
  (:export
   #:tiny-debug
   #:*default-interceptor*
   #:*interceptor-condition*
   #:*visual-mode*
   #:toggle
   #:active-p
   ))
(in-package :tiny-debug)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defvar *visual-mode* nil
  "True to use visual mode.")

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
(defun print-frame (f &optional (stream *debug-io*))
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
		       (format stream " ~s" vv))))
		  ((and (listp v) (eql (car v) :rest))
		   (format stream " &rest")
		   (print-lambda-list f (cdr v)))))))
    (let* ((loc     (sb-di:frame-code-location f))
	   (dbg-fun (sb-di:code-location-debug-fun loc))
	   (name    (sb-di:debug-fun-name dbg-fun)))
      (format stream "(")
      (if (symbolp name)
	  (let ((pkg (symbol-package name)))
	    (if (not (eql (find-package :cl) pkg))
		(format stream "~(~a~)::" (package-name pkg)))))
      (format stream "~(~a~)" name)
      (print-lambda-list f (sb-di::debug-fun-lambda-list dbg-fun))
      (format stream ")"))))


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

#+sbcl
(defun debugger-backtrace-lines (n)
  (loop 
;;;     :with f = *saved-frame* #| (sbcl-start-frame) |#
     :with f = *current-frame*
     :for i :from 1 :to n
     :collect
     (with-output-to-string (str)
       (format str "~3d " (sb-di:frame-number f))
       (print-frame f str))
     :do
     (setf f (sb-di:frame-down f))
     :while f))

#|
#+ccl
(defun debugger-backtrace-lines (n)
  "Our own backtrace for CCL."
  (let ((frames '()) (*print-readably* nil))
    (ccl:map-call-frames
     #'(lambda (frame-ptr context)
	 (push (list frame-ptr context) frames))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (setf frames (nreverse frames))
    (loop :with i = 0
       :for (f context) :in frames
       :collect
       (format nil "~3a (~a~{ ~s~})~%"
	       i (ccl:function-name (ccl:frame-function f context))
	       (ccl:frame-supplied-arguments f context))
       :do
       (incf i)
       :while (< i n))))
|#
#+ccl
(defun debugger-backtrace-lines (n)
  (loop :with i = 0
     :for b :in (ccl::backtrace-as-list)
     :if (>= i *current-frame*)
     :collect (format nil "~(~3d ~a~)" i b)
     :end
     :do
     (incf i)
     :while (< i (+ *current-frame* n))))

#+ccl
(defun debugger-wacktrace (n)
  "Our own backtrace for CCL."
  (let ((frames '()) (*print-readably* nil))
    (ccl:map-call-frames
     #'(lambda (frame-ptr context)
	 (push (list frame-ptr context) frames))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (setf frames (nreverse frames))
    (loop :with i = 0
       :for (f context) :in frames
       :do
       (format *debug-io* "~3a (~(~a~{ ~s~}~))~%" i
	       (or (ignore-errors
		     (ccl:function-name (ccl:frame-function f context))) "")
	       (or (ignore-errors
		     (ccl:frame-supplied-arguments f context)) '("")))
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
(defun debugger-source-note (n)
  (multiple-value-bind (pointer context) (get-frame n)
    (multiple-value-bind (func pc) (ccl:frame-function pointer context)
      (if pc
	  (or (ccl:find-source-note-at-pc func pc)
	      (ccl:function-source-note func))
	  (ccl:function-source-note func)))))
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


#|#+sbcl
(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
         (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
         (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

#+sbcl
(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun fallback-source-location (code-location)
  )

(defun source-file-source-location (code-location)
  )
|#

#|
#+sbcl
(defun debugger-source (frame-number)
  (when (not frame-number)
    (setf frame-number 0))
  (let ((frame (frame-number frame-number))
	(loc  (sb-di::frame-code-location frame))
	(src  (sb-di::code-location-debug-source loc))
	(filename (sb-c::debug-source-namestring src)))
    (if filename
	(if (code-location-has-debug-block-info-p code-location)
	    (source-file-source-location code-location)
	    (fallback-source-location code-location))
	(prin1-to-string
	 (sb-debug::code-location-source-form loc 100)))))
|#

#|
(with-open-file (str filename)
      (file-position str pos)
      (loop
	 :for i :from 1 :to 10
	 :collect
	 (read-line str nil nil)
	 ;;(format nil "~s~%" (sb-di:debug-fun-start-location dbg-fun))
	 ))))
|#

#+sbcl
(defun debugger-source-path (frame)
  (let* ((fun (sb-di:code-location-debug-fun
	       (sb-di:frame-code-location frame)))
	 (src (and fun (sb-introspect:find-definition-source
			(symbol-function
			 (sb-di:debug-fun-name fun))))))
    (if src
	(definition-source-pathname src)
	":Unknown")))

#+ccl
(defun debugger-source-path (frame &optional (window-size 10))
  (let ((note (debugger-source-note frame)))
    (cond
      ((not note)
       ":Unknown")
      ((ccl:source-note-filename note)
       (ccl:source-note-filename note))
      (t
       ":Internal"))))

#+sbcl
(defun debugger-source (frame &optional (window-size 10))
  (let* ((fun (sb-di:code-location-debug-fun
	       (sb-di:frame-code-location frame)))
	 (src (and fun (sb-introspect:find-definition-source
			(symbol-function
			 (sb-di:debug-fun-name fun))))))
    (if src
	(with-open-file (stream (definition-source-pathname src))
	  (file-position stream (definition-source-character-offset src))
	  (loop :with line
	     :for i :from 1 :to window-size
	     :while (setf line (read-line stream nil nil))
	     :collect line))
	(list "Sorry, I can't figure it out."))))

#+ccl
(defun debugger-source (frame &optional (window-size 10))
  (let ((note (debugger-source-note frame)))
    (if (not note)
	(list "Sorry, I can't figure it out.")
	(cond
	  ((ccl:source-note-text note)
	   (list (format nil "~a~%" (ccl:source-note-text note))))
	  ((ccl:source-note-filename note)
	   (with-open-file (stream (ccl:source-note-filename note))
	     (file-position stream (ccl:source-note-start-pos note))
	     #|(let* ((len (- (ccl:source-note-end-pos note)
			    (ccl:source-note-start-pos note)))
		    (str (make-array (list len) :element-type 'character)))
	       (read-sequence str stream)
	     (format nil "~a~%" str)))))))) |#
	     (loop :with line
		:for i :from 1 :to window-size
		:while (setf line (read-line stream nil nil))
		:collect line)))))))

#+sbcl
(defun debugger-show-source (n)
  (format t "~s~%" (debugger-source n)))

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
  #+cmu  (if n (debug:backtrace n) (debug:backtrace))
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
  #-(or sbcl ccl clisp lispworks ecl cmu)
  (debugger-sorry "backtrace"))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  #+sbcl (or sb-debug::*stack-top-hint* (sb-di::top-frame))
  ;;  #+ccl ccl:*top-error-frame*
  #+ccl 0
  ;; We don't want to be sorry here, so just be wrong.
  #-(or sbcl ccl) nil)

#+sbcl
(defun debugger-up-frame (&optional (count 1))
  (declare (ignore count))
  (let ((next (sb-di:frame-up *current-frame*)))
    (if next
	(setf *current-frame* next))))

#+ccl
(defun debugger-up-frame (&optional (count 1))
  (when (> *current-frame* 0)
    (decf *current-frame* (or count 1))))

#+sbcl
(defun debugger-down-frame (&optional (count 1))
  (declare (ignore count))
  (let ((next (sb-di:frame-down *current-frame*)))
    (if next
	(setf *current-frame* next))))

#+ccl
(defun debugger-down-frame (&optional (count 1))
  (incf *current-frame* (or count 1)))

#+sbcl
(defun debugger-set-frame (frame)
  (cond
    ((or (not frame) (and (numberp frame) (= frame 0)))
     (sb-di:top-frame))
    ((numberp frame)
     (setf *current-frame* (frame-number frame)))
    ((sb-di:frame-p frame)
     (setf *current-frame* frame))
    (t
     (format *debug-io* "No such frame ~s~%" frame))))

#+sbcl
(defun debugger-top-frame (count)
  (declare (ignore count))
  (setf *current-frame* *saved-frame*))

;; Stepping

#+sbcl
(progn
  ;; (defvar *step-form* nil)
  ;; (defvar *step-args* nil)
  (defun stepper (c)
    "Thing to set the stepper hook to."
    ;; (setf *step-form* (sb-ext::step-condition-form c)
    ;;  	  *step-args* (sb-ext::step-condition-args c))
    (format *debug-io* "-- TinY SteppeR --~%")
    (finish-output *debug-io*)
    (let ((sb-debug::*stack-top-hint* (sb-di::find-stepped-frame)))
      (invoke-debugger c))))

(defun activate-stepper ()
  "Activate the Tiny-DEBUG setpper."
  (format *debug-io* "Activating the TINY stepper.~%")
  #+sbcl (setf sb-ext::*stepper-hook* 'stepper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation independent functions

(defparameter *box_drawings_light_horizontal*
  #-ccl #\box_drawings_light_horizontal #+ccl #\U+2500)

(defparameter *box_drawings_light_vertical_and_left*
  #-ccl #\box_drawings_light_vertical_and_left #+ccl #\U+2524)

(defparameter *box_drawings_light_vertical_and_right*
  #-ccl #\box_drawings_light_vertical_and_right #+ccl #\U+251C)

(defun horizontal-line (tt &optional note)
  (tt-color tt :blue :black)
  (if note
      (progn
	(tt-write-string tt (s+
			     *box_drawings_light_horizontal*
			     *box_drawings_light_horizontal*
			     *box_drawings_light_vertical_and_left*))
	(tt-color tt :white :black)
	(tt-write-string tt (s+ " " note " "))
	(tt-color tt :blue :black)
	(tt-write-char tt *box_drawings_light_vertical_and_right*)
	(tt-format tt "~v,,,va"
		   (- (terminal-window-columns tt) (length (s+ note)) 6)
		   *box_drawings_light_horizontal*
		   *box_drawings_light_horizontal*)
	(tt-color tt :default :black)
	(tt-write-char tt #\newline))
      ;; no note, just a line
      (progn
      	(tt-color tt :blue :black)
	(tt-format tt "~v,,,va~%"
		   (1- (terminal-window-columns tt))
		   *box_drawings_light_horizontal*
		   *box_drawings_light_horizontal*)
	(tt-color tt :default :black))))

(defun visual ()
  (with-terminal (tt)
    (let* ((source-height (truncate (/ (terminal-window-rows tt) 3)))
	   (stack-height (min 10 source-height))
	   (command-height (- (terminal-window-rows tt)
			      (+ stack-height source-height 2)))
	   (command-top (- (terminal-window-rows tt) (1- command-height)))
	   (src (or (ignore-errors
		      (debugger-source *current-frame* source-height))
		    '("Sorry.")))
	   (path (or (ignore-errors
		       (debugger-source-path *current-frame*)
		       "Sorry")))
	   (stack (or (ignore-errors
			(debugger-backtrace-lines stack-height))
		      '("???"))))
      ;; Source area
      ;;(tt-clear tt)
      (tt-move-to tt (+ source-height stack-height) 0)
      (tt-erase-above tt)
      (tt-home tt)
      (loop :for i :from 0 :below source-height :do
	 (if (and (< i (length src)) (elt src i))
	     (tt-format tt "~a~%"
			(subseq (elt src i)
				0 (min (1- (terminal-window-columns tt))
				       (length (elt src i)))))
	     (tt-write-char tt #\newline)))
      (horizontal-line tt path)
      ;; Stack area
      (loop :for line :in stack :do
	 (tt-format tt "~a~%"
		    (subseq line
			    0 (min (1- (terminal-window-columns tt))
				   (length line)))))
      (horizontal-line tt)
      ;; Command area
      (tt-set-scrolling-region tt command-top (terminal-window-rows tt))
      (tt-move-to tt (1- (terminal-window-rows tt)) 0)
      (tt-finish-output tt))))

(defun reset-visual ()
  (with-terminal (tt)
    (tt-set-scrolling-region tt nil nil)
    (tt-move-to tt (1- (terminal-window-rows tt)) 0)
    (tt-finish-output tt)))

(defun debugger-up-frame-command (&optional foo)
  (declare (ignore foo))
  (debugger-up-frame)
  (visual))

(defun debugger-down-frame-command (&optional foo)
  (declare (ignore foo))
  (debugger-down-frame)
  (visual))

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
;;;  (format *debug-io* "Debug ~d~a" *repl-level* p)
  (when *visual-mode*
    (visual))
  (fresh-line *debug-io*)
  (tiny-rl::editor-write-string		; XXX
   e
   (format nil "Debug ~d~a" *repl-level* p))
;  (finish-output *debug-io*)
  nil)

;;;(declaim (special *interceptor-condition*))
(defvar *interceptor-condition* nil
  "The condition that happened.")

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

(defun toggle-visual-mode (state)
  (declare (ignore state))
  (setf *visual-mode* (not *visual-mode*)))

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
	 (:v (toggle-visual-mode (read-arg state)) t)
	 (:u (debugger-up-frame   (read-arg state)) t)
	 (:d (debugger-down-frame (read-arg state)) t)
	 (:f (debugger-set-frame  (read-arg state)) t)
	 (:t (debugger-top-frame  (read-arg state)) t)
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

(defvar *debugger-keymap* nil "Keymap for the debugger.")
(defvar *debugger-escape-keymap* nil "Escape key Keymap for the debugger.")

(defun setup-keymap ()
  (setf *debugger-keymap* (copy-keymap tiny-rl:*normal-keymap*))
  (define-key *debugger-keymap* (meta-char #\i) 'debugger-up-frame-commmand)
  (define-key *debugger-keymap* (meta-char #\o) 'debugger-down-frame-commmand)
  (define-key *debugger-escape-keymap* #\escape
    (build-escape-map *debugger-keymap*)))

(defun tiny-debug (c hook)
  "Entry point for the tiny debugger, used as the debugger hook."
  (declare (ignore hook))		;@@@ wrong
  (setf *saved-frame* (debugger-internal-frame))
  (when (not *debugger-keymap*)
    (setup-keymap))
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
	      (*current-frame* *saved-frame*)
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
		     :keymap *debugger-keymap*
		     :output *debug-io*
		     :debug t
		     :no-announce t))))
;;;    (Format *debug-io* "Exiting the debugger level ~d~%" *repl-level*)
    (reset-visual)))

; (defvar *repl-debug* nil
;   "True to invoke the debugger when a error occurs.")

(defun activate ()
  (format *debug-io* "Activating the TINY debugger.~%")
  (setf *debugger-hook* 'tiny-debug)
  (activate-stepper))

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
