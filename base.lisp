;;
;; base.lisp - Things that need to be defined before the system specific code.
;;

(in-package :deblarg)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; Variables

(defvar *visual-mode* nil
  "True to use visual mode.")

(defvar *visual-term* nil
  "Terminal for visual mode.")

(defvar *current-frame* nil
  "Current frame number. Frames are numbered from the top or innermost 0 to
the outermost. When entering the debugger the current frame is 0.")

(defvar *saved-frame* nil
  "Implementation handle to frame that the debugger started from.")

(defvar *interceptor-condition* nil
  "The condition that happened.")

(defvar *debug-term* nil
  "*debug-io* as a terminal.")

(defmacro with-new-debugger-io (() &body body)
  "Evaluate BODY with a new *terminal* redirected to *debug-io*."
  `(with-new-terminal ((pick-a-terminal-type) *debug-term*
			     :device-name (nos:file-handle-terminal-name
					   (nos:stream-system-handle *debug-io*))
			     :output-stream (make-broadcast-stream *debug-io*))
     ,@body))

(defmacro with-debugger-io (() &body body)
  "Evaluate BODY with *debug-term* set up, either existing or new."
  (with-unique-names (thunk)
    `(flet ((,thunk () ,@body))
       (if *debug-term*
	   (,thunk)
	   (with-new-debugger-io ()
	     (,thunk))))))

(defun debugger-sorry (x)
  "What to say when we can't do something."
  (format *debug-io* "~%Sorry, don't know how to ~a on ~a. ~
		       Snarf some slime!~%" x (lisp-implementation-type)))

;; @@@ Figure out some way to make these respect *debug-io*, even when not
;; in the debugger.

(defun debugger-print-string (string)
  (typecase string
    (string (princ string *debug-term*))
    (fatchar-string
     (render-fatchar-string string :terminal *debug-term*))
    (fat-string
     (render-fat-string string :terminal *debug-term*))))

(defun print-span (span)
  ;; This doesn't work since some implementations wrap our terminal stream
  ;; with something else before it gets to print-object.
  ;;(princ (span-to-fat-string span) *terminal*)
  (render-fatchar-string (span-to-fatchar-string span) :terminal *debug-term*))

(defun display-value (v stream)
  "Display V in a way which hopefully won't mess up the display. Also errors
are indicated instead of being signaled."
  (restart-case
      (typecase v
	;; Make strings with weird characters not screw up the display.
	(string
	 (prin1 (with-output-to-string (str)
		  (loop :for c :across v :do
		     (displayable-char c :stream str
				       :all-control t :show-meta nil)))
		stream))
	(t (prin1 v stream)))
    (error (c)
      (declare (ignore c))
      (return-from display-value
	(format nil "<<Error printing a ~a>>" (type-of v))))))

(defun print-stack-line (line &key width)
  "Print a stack LINE, which is a cons of (line-numbner . string)."
  (destructuring-bind (num . str) line
    (print-span `((:fg-yellow ,(format nil "~3d" num) " ")))
    (debugger-print-string
     (if width
	 (osubseq str 0 (min (olength str) (- width 4)))
	 str))
    ;;(terpri *terminal*)
    (terminal-write-char *debug-term* #\newline)
    ))

;; EOF
