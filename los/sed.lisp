;;;
;;; sed.lisp - Stream editor.
;;;

(defpackage :sed
  (:use :cl :dlib :collections :cl-ppcre)
  (:export
   #:edit-stream
   #:!sed
   ))
(in-package :sed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *doc*
"Scripts are a list of (address form). Forms are Lisp code to execute when the
line matches the address.

Addresses:
  Form                 Type      Description
  ──────────────────── ───────── ──────────────────────────────────────────────
  number               integer   Line ‘number’.
  :last                keyword   The last line.
  \"regexp\"             string    A line matching ‘regexp’.
  (first last)         cons      The range of line from ‘first’ address to
                                 ‘last’ address.
  (first :plus number) cons      ‘first’ address, and ‘number’ more lines.
  (first :mod number) cons       ‘first’ address, until the line number is
                                 evenly divisible by ‘number’.

It the form, the following can be used:

Variables:
  count               The current line count.
  line                The text of the current line.
  clip                A string for copying and pasting.

Special functions:
  Form                   Description
  ────────────────────── ──────────────────────────────────────────────
  (stop)                 Stop processing the stream.
  (next-line)            Read the next line into ‘line’.
  (append-next-line)     Append the next line to ‘line’.
  (next-file-line file)  Get the next line from ‘file’.

Whatever the command returns is converted to a string, like with princ-to-string,
and used as the line. If the command returns NIL, the line is deleted."))

(setf (documentation (find-package :sed) t)
      (s+ "Stream editor." #\newline *doc*))

;; We could also make an editing stream type.
;; See also filter-stream, which is probably more flexible, but requires more
;; code.
;;
;; We could make a posix sed out of this, but it would probably only be useful
;; for terseness. Almost every use of use sed, just uses the 's' command anyway.
;; This is actually a lot closer to awk. But to make it an awk, we would
;; probably want to provide a few more convenience functions, most notably
;; split, which is what most people really use awk for.

(defparameter +range-types+ '(:plus :mod))
(defparameter +symbol-typs+ '(:last))

(defclass address ()
  ((negated
    :initarg :negated :accessor address-negated :initform nil :type boolean
    :documentation "True if the address test should be negated."))
  (:documentation "A designator for a line or range of lines."))

(defclass null-address (address)
  ()
  (:documentation "An address that matches any line."))

(defclass single-value-address (address)
  ((value :initarg :value :accessor address-value :initform nil))
  (:documentation "An address with a single value."))

(defmethod print-object ((object single-value-address) stream)
  "Print a range-address to STREAM."
  (with-slots (value) object
    (print-unreadable-object (object stream :type t)
			     (format stream "~s" value))))

(defclass line-number-address (single-value-address)
  ()
  (:documentation "An address which is a line number."))

(defclass line-number-start-address (line-number-address)
  ()
  (:documentation "An address which is a starting line number."))

(defclass line-number-end-address (line-number-address)
  ()
  (:documentation "An address which is a ending line number."))

(defclass symbol-address (single-value-address)
  ()
  (:documentation "An address which is a symbol."))

(defclass regex-address (single-value-address)
  ((scanner
    :initarg :scanner :accessor address-scanner :initform nil
    :documentation "The ppcre scanner for the regexp in VALUE."))
  (:documentation "An address which is a regular expression."))

(defclass range-address (address)
  ((start :initarg :start :accessor address-start :initform nil)
   (end :initarg :end :accessor address-end :initform nil)
   (active
    :initarg :active :accessor address-active :initform nil :type boolean
    :documentation "True if the start was seen and the end wasn't."))
  (:documentation "An address that is a range of lines."))

(defmethod print-object ((object range-address) stream)
  "Print a range-address to STREAM."
  (with-slots (start end) object
    (print-unreadable-object (object stream :type t)
			     (format stream "~s-~s" start end))))

(defclass block-address (range-address)
  ((start-line
    :initarg :start-line :accessor address-start-line :initform 0 :type integer
    :documentation "Line which the start matched on."))
  (:documentation "A range address where the end is relative. End is a positive
integer offset of lines from the starting line."))

(defclass modular-address (range-address)
  ()
  (:documentation
   "A range address where the end is a modulus. The end of the is the first
line after the start line whose absolute line number evenly divisible by the
end number."))

(defstruct state
  "State for a stream editor."
  input				       ; The input stream
  output			       ; The output stream
  (count 0 :type integer)	       ; The number of the current line
  (line nil :type (or string null))    ; The current line
  (next nil :type (or string null))    ; The next line
  (clip nil :type (or string null))    ; Copy and paste area
  (eof-p nil :type boolean)	       ; True if we hit the end of the stream.
  (stop-flag nil :type boolean)	       ; True to end processing
  ;; A hash table of regexp scanners for the current script.
  (scanners (make-hash-table :test #'equal) :type hash-table)
  ;; A hash table of file names to open streams.
  files)

(defvar *state* nil
  "The dynamic stream editor state.")

(defvar *matches* nil
  "Current matches from a single regex address.")

(defun get-scanner (regex)
  "Get a cached regex scanner."
  (or (gethash regex (state-scanners *state*))
      (setf (gethash regex (state-scanners *state*))
	    (ppcre:create-scanner regex))))

(defgeneric address-matches (address line count eof-p)
  (:documentation
   "Return true if ‘address’ matches. ‘line’ is the text of the line. ‘count’ is
the currrent line number. ‘eof-p’ is true if we're at the last line."))

(defmethod address-matches ((address null-address) line count eof-p)
  t)

(defmethod address-matches ((address line-number-address) line count eof-p)
  (= count (address-value address)))

(defmethod address-matches ((address line-number-start-address) line count eof-p)
  (>= count (address-value address)))

(defmethod address-matches ((address line-number-end-address) line count eof-p)
  (<= count (address-value address)))

(defmethod address-matches ((address regex-address) line count eof-p)
  (multiple-value-bind (result strings)
      (ppcre:scan (address-scanner address) line)
    (when result
      (setf *matches* strings))
    (and result t)))

(defmethod address-matches ((address symbol-address) line count eof-p)
  (case address
    (:last eof-p)))

(defmethod address-matches ((address range-address) line count eof-p)
  (with-slots (start end active) address
    (and (or active (and (address-matches start line count eof-p)
			 (setf active t)))
	 (address-matches end line count eof-p))))

(defmethod address-matches ((address block-address) line count eof-p)
  (with-slots (start end active start-line) address
    (prog1 (and (or active (and (address-matches start line count eof-p)
				(setf active t
				      start-line count)))
		(< count (+ start-line end)))
      (when (and active
		 (>= count (+ start-line end)))
	(setf active nil)))))

(defmethod address-matches ((address modular-address) line count eof-p)
  (with-slots (start end active start-line) address
    (prog1 (and (or active (and (address-matches start line count eof-p)
				(setf active t
				      start-line count)))
		(plusp (mod count end)))
      (when (and active (zerop (mod count end)))
	(setf active nil)))))

(defun make-address (form)
  "Return an addres object from a form."
  (typecase form
    (null    (make-instance 'null-address))
    (integer (make-instance 'line-number-address :value form))
    (symbol
     (case form
       (:last
	(make-instance 'symbol-address :value :last))
       (otherwise
	(error "Unknown symbol address ~s" form))))
    (string (make-instance 'regex-address
			   :value form
			   :scanner (ppcre:create-scanner form)))
    (cons
     (let ((len (length form)))
       (case len
	 (0 (error "This shouldn't happen."))
	 (1 ;; Just pretend it's not a range.
	  (make-address (car form)))
	 (2
	  (flet ((make-range-x (n which)
		  (etypecase n
		    (integer
		     (ecase which
		       (:start
			(make-instance 'line-number-start-address :value n))
		       (:end
			(make-instance 'line-number-end-address :value n))))
		    (string
		     (make-address n)))))
	    (make-instance 'range-address
			   :start (make-range-x (first form) :start)
			   :end (make-range-x
				 (or (second form) (cdr form)) :end))))
	 (3
	  (when (not (member (second form) '(:plus :mod)))
	    (error "Second item in a range must be one of ~s" +range-types+))
	  (when (not (integerp (third form)))
	    (error "The third item in a ~a address must be an integer, ~
                    not the ~a ~s." (second form) (type-of (third form))
		    (third form)))
	  (case (second form)
	    (:plus (make-instance 'block-address
				  :start (first form)
				  :end (third form)))
	    (:mod (make-instance 'modular-address
				 :start (first form)
				 :end (third form))))))))
    (t
     (error "Address ~s is of an unknown type ~a." form (type-of form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for use in the command form.

(defun stop ()
  "Stop processing the stream."
  (setf (state-stop-flag *state*) t))

(defun next-line ()
  "Read the next line into ‘line’."
  (with-slots (input count line next eof-p) *state*
    (cond
     (eof-p (setf line nil))
     ((not line) ; first time
      (setf line (read-line input nil)
	    next (read-line input nil))
      (incf count))
     (next       ; subsequent lines
      (setf line next
	    next (read-line input nil))
      (when (not next)
	(setf eof-p t))
      (incf count)))))

(defun append-next-line ()
  "Append the next line to ‘line’."
  (with-slots (input count line next) *state*
    (setf line (+ line next)
	  next (read-line input nil))
    (incf count)))

(defun get-file-stream (file-name)
  (with-slots (files) *state*
    (or (gethash file-name files)
	(setf (gethash file-name files)
	      (open file-name :direction :input)))))

(defun next-file-line (file)
  "Get the next line from ‘file’."
  (with-slots (files) *state*
    (when (not files)
      (setf files (make-hash-table :test #'equal)))
    (read-line (get-file-stream file) nil)))

(defun command-result (result)
  ;; (s+ (princ-to-string result) #\newline)
  result
  )

(defun p (&optional thing)
  "Print a thing."
  (with-slots (output line) *state*
    (let ((result (or (and thing (princ-to-string thing)) line)))
      (write-string result output)
      (write-char #\newline)
      result)))

(defun s (regex replacement &key all)
  "Substitute ‘replacement’ for ‘regex’. If ‘all’ is true, substitue every
occurrence. ‘regex’ is a Perl style regular expression. ‘replacement’ can be a
string which may contain the special substrings \"\&\" for the whole match,
\"\`\" for the part of the line before the match, \"\'\" for the part of line
after the match, \"\N\" or \"\{N}\" for the Nth register where N is a positive
integer."
  (with-slots (line) *state*
    (multiple-value-bind (result matched-p)
	(if all
	    (ppcre:regex-replace-all regex line replacement)
	    (ppcre:regex-replace regex line replacement))
      ;; (format t "result ~s matched ~s~%" result matched-p)
      (if matched-p
	  result
	  line))))

;; @@@ This whole idea seems too heavy. But how else to do it?
#|
(defun make-code-package ()
  "Make a copy of the current package with a shadowing import of all the
symbols we need for a command form."
  (let ((result (copy-package *package*)))
    (shadowing-import '(count line next clip stop-flag
			stop next-line append-next-line next-file-line
			command-result) result)
    result))
|#

;; @@@ This is stupid, but I don't really want to switch *package* here.
(defun wrap-form (form)
  (flet ((flurb (s) (or (find-symbol (string-upcase s) *package*) s)))
    (let ((%count            (flurb 'count))
	  (%line             (flurb 'line))
	  (%next             (flurb 'next))
	  (%clip             (flurb 'clip))
	  (%stop-flag        (flurb 'stop-flag))
	  (%stop             (flurb 'stop))
	  (%next-line        (flurb 'next-line))
	  (%append-next-line (flurb 'append-next-line))
	  (%next-file-line   (flurb 'next-file-line))
	  (%p                (flurb 'p))
	  (%s                (flurb 's))
	  ;; (%command-result   (flurb 'command-result))
	  )
      `(lambda (state)
	 (with-slots
	     ((,%count sed::count)
	      (,%line sed::line)
	      (,%next sed::next)
	      (,%clip sed::clip)
	      (,%stop-flag sed::stop-flag))
	     state
	   (declare (ignorable ,%count ,%line ,%next ,%clip ,%stop-flag))
	   (flet ((,%stop () (funcall 'sed::stop))
		  (,%next-line () (funcall 'sed::next-line))
		  (,%append-next-line () (funcall 'sed::append-next-line))
		  (,%next-file-line (file) (funcall 'sed::next-file-line file))
		  (,%p (thing) (funcall 'sed::p thing))
		  (,%s (re rep &key all) (funcall 'sed::s re rep :all all))
		  ;; (,%command-result () (funcall 'sed::command-result))
		  )
	     (declare
	      (ignorable (function ,%stop)
			 (function ,%next-line)
			 (function ,%append-next-line)
			 (function ,%next-file-line)
			 (function ,%p)
			 (function ,%s)
			 ;; (function ,%command-result)
			 )
	      (optimize (speed 0) (safety 3) (debug 3)
			(space 0) (compilation-speed 0))
	      ;; (optimize (speed 3) (safety 0) (debug 0)
	      ;; 		(space 0) (compilation-speed 0))
	      #+sbcl (sb-ext:muffle-conditions
		      sb-ext:compiler-note))
	     (command-result ,form)))))))

(defun compile-form (form #|package |#)
  (let (func)
    (handler-case
	;; (let ((*package* package))
	;;   (setf func (compile nil (wrap-form form)))
	;;   func)
	(setf func (compile nil (wrap-form form)))
      (error (c)
        ;; @@@ Figure out a way to do reasonable form re-edit.
	(format t "~a: ~a when compiling the form:~&~s~%" (type-of c) c form)))
    func))

(defun read-script (file)
  "Read a stream editor script from ‘file’. Scripts consist of pairs of address
and command forms."
  (with-open-file (stream file :direction :input)
    (loop :with address :and command
      :do
      (setf address (read stream nil 'eof)
            command (read stream nil 'eof))
      :while (and (not (eq address 'eof)) (not (eq command 'eof)))
      :collect (list address command))))

(defun edit-stream (script &key
			   (input *standard-input*)
			   (output *standard-output*)
			   script-file quiet)
  #.(s+ "Edit a stream. Run the ‘script’ and/or the contents of ‘script-file’ on
‘input’. Write the results to ‘output’"	*doc*)
  (let* ((*state* (make-state :input input :output output))
	 ;; (code-package (make-code-package))
	 (prepared
	  (loop :for (addr form)
		:in (or (and script-file (append (read-script script-file)
						 script))
			script)
		:collect (cons (make-address addr)
			       (compile-form form #|code-package|#))))
	 close-output close-input)
    ;; (format t "prepared ~s~%" prepared)
    (unwind-protect
	(with-slots (count line next stop-flag eof-p) *state*
	  (when (or (stringp output) (pathnamep output))
	    (setf close-output t
		  output (open output :direction :output
			       :if-does-not-exist :create)
		  (state-output *state*) output))
	  (when (or (stringp input) (pathnamep input))
	    (setf close-input t
		  input (open input :direction :input)
		  (state-input *state*) input))
	  (loop :with result
	   :do
	   (next-line)
	   :while (and line (not stop-flag))
	   :do
	     (loop :for (address . func) :in prepared :do
	       (setf result line)
	       (when (address-matches address line count eof-p)
		 (setf result (funcall func *state*)))
	       (when (and result (not quiet))
		 (write-line result output)))))
      (when (and output close-output)
	(close output))
      (when (and input close-input)
	(close input))
      (when (state-files *state*)
	(omapk (_ (when (open-stream-p (oelt _ 1))
		    (close (oelt _ 1))))
	       (state-files *state*))))))

;; @@@ The macro version is probably better, but we need the non-macro version
;; for calling from the shell.
#|
(defmacro sed ((&key (input *standard-input*) (output *standard-output*))
	       &body body)
  `(loop :with count :and line
    :while (setf line (read-line input) nil)
    :do
    (loop 
)

|#

#+lish
(lish:defcommand sed
  ((quiet boolean :short-arg #\n :help "Don't automatically output lines.")
   ;;(expression object :short-arg #\e :optional nil :help "Script to run.")
   (expression object :optional nil :help "Script to run.")
   (files pathname :optional t :repeating t :help "Files to use as input."))
  "Stream editor."
  (if files
      (let (streams)
	(unwind-protect
	    (edit-stream expression
	     :input
	     (apply #'make-concatenated-stream
		    (mapcar (_ (let ((str (open _ :direction :input)))
				 (push str streams)
				 str))
			    files))
	     :quiet quiet)
	  (map nil (_ (close _)) streams)))
    (edit-stream expression :quiet quiet)))

;; End
