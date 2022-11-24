;;;
;;; sed.lisp - Stream editor.
;;;

(defpackage :sed
  (:use :cl :dlib :collections)
  (:export
   #:edit-stream
   #:!sed
   ))
(in-package :sed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *doc*
"Scripts are a list of (address form). Forms are Lisp code to execute when the
line matches the address. If no address matches, the line is printed. Whatever
the form returns is converted to a string, like with princ-to-string,
and used as the line. If the command returns NIL, the line is not output.

Addresses:
  Form                 Type      Description
  ──────────────────── ───────── ──────────────────────────────────────────────
  number               integer   Line ‘number’.
  :last                keyword   The last line.
  t                    symbol    Every line.
  nil                  symbol    No line. Done before reading lines.
  \"regexp\"             string    A line matching ‘regexp’.
  (first last)         cons      The range of lines from ‘first’ address to
                                 ‘last’ address. Either can be nil, to mean
                                 the start or end of the input.
  (first :plus number) cons      ‘first’ address, and ‘number’ more lines.
  (first :mod number) cons       ‘first’ address, until the line number is
                                 evenly divisible by ‘number’.

In the form, the following can be used:

Variables:
  count         The current line count.
  line          The text of the current line.
  clip          A string for copying and pasting.
  eof-p         True if we're at the last line. Same as :last address.

Functions:
  Form                           Description
  ────────────────────────────── ──────────────────────────────────────────────
  (stop)                         Stop processing the stream.
  (next-line)                    Read the next line into ‘line’.
  (append-next-line)             Append the next line to ‘line’.
  (next-file-line file)          Get the next line from ‘file’.
  (s regex replacement &key all) Substitute replacement for regex.
  (p &optional thing)            Print ‘thing’ or the current line.
  (split regex &optional thing)  Split ‘thing’ or the current line, by ‘regex’.
  (w &optional n)                After split, return the Nth thing.
"))

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
(defparameter +symbol-typs+ '(:last t))

(defclass address ()
  ((negated
    :initarg :negated :accessor address-negated :initform nil :type boolean
    :documentation "True if the address test should be negated."))
  (:documentation "A designator for a line or range of lines."))

(defclass null-address (address)
  ()
  (:documentation "An address that matches no line."))

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
  input				      ; The input stream
  output			      ; The output stream, or list.
  input-name			      ; The input file name
  output-name			      ; The output file name
  (count 0 :type integer)	      ; The number of the current line
  (line nil :type (or string null))   ; The current line
  (next nil :type (or string null))   ; The next line
  (clip nil :type (or string null))   ; Copy and paste area
  (eof-p nil :type boolean)	      ; True if we hit the end of the stream.
  (stop-flag nil :type boolean)	      ; True to end processing
  (missing-newline nil :type boolean) ; True if the newline was missing
  (collect nil :type boolean)	      ; True to collect output lines
  words			              ; An array of words from split.
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
  nil)

(defmethod address-matches ((address line-number-address) line count eof-p)
  (= count (address-value address)))

(defmethod address-matches ((address line-number-start-address) line count eof-p)
  (>= count (address-value address)))

(defmethod address-matches ((address line-number-end-address) line count eof-p)
  (or (null (address-value address))
      (<= count (address-value address))))

(defmethod address-matches ((address regex-address) line count eof-p)
  (multiple-value-bind (result strings)
      (ppcre:scan (address-scanner address) line)
    (when result
      (setf *matches* strings))
    (and result t)))

(defmethod address-matches ((address symbol-address) line count eof-p)
  (case (address-value address)
    (:last eof-p)
    ((t) t)))

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
       ((t)
	(make-instance 'symbol-address :value t))
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
		    (null
		     (ecase which
		       (:start
			(make-instance 'line-number-start-address :value 0))
		       (:end
			(make-instance 'line-number-end-address :value nil))))
		    (string
		     (make-address n)))))
	    (make-instance 'range-address
			   :start (make-range-x (first form) :start)
			   :end (make-range-x (second form) :end))))
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
  (with-slots (input count line next eof-p missing-newline) *state*
    (let (m-n)
      (cond
       (eof-p (setf line nil))
       ((not line)			; first time
	(setf (values line m-n) (read-line input nil))
	(when (and line m-n)
	  (setf missing-newline t))
	(setf (values next m-n) (read-line input nil))
	(when (and next m-n)
	  (setf missing-newline t))
	(incf count))
       (next				; subsequent lines
	(setf line next
	      (values next m-n) (read-line input nil))
	(when (and next m-n)
	  (setf missing-newline t))
	(when (not next)
	  (setf eof-p t))
	(incf count))))))

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

(defun put-string (string output)
  (if (state-collect *state*)
      (push string (state-output *state*))
      (write-string string output)))

(defun put-line (line output)
  (if (state-collect *state*)
      (push line (state-output *state*))
      (write-line line output)))

(defun put-newline (output)
  ;; We don't need to do this for collected output.
  (unless (state-collect *state*)
    (write-char #\newline output)))

(defun p (&optional thing)
  "Print a ‘thing’. If ‘thing’ isn't given or is NIL, print the current line."
  (with-slots (output line missing-newline eof-p) *state*
    (let ((result (or (and thing (princ-to-string thing)) line)))
      (put-string result output)
      (when (not (and missing-newline eof-p))
	(put-newline output))
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

(defun split (regex &optional thing)
  "Split ‘thing’ or the current line, by ‘regex’."
  (with-slots (line words) *state*
    (setf words (ppcre:split regex (or thing line)))))

(defun w (&optional n)
  "After calling split, return the Nth split element. If N is NIL, return the
 words vector."
  (with-slots (words) *state*
    (when words
      (if n (elt words n) words))))

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
	  (%eof-p            (flurb 'eof-p))
	  (%stop-flag        (flurb 'stop-flag))
	  (%stop             (flurb 'stop))
	  (%next-line        (flurb 'next-line))
	  (%append-next-line (flurb 'append-next-line))
	  (%next-file-line   (flurb 'next-file-line))
	  (%p                (flurb 'p))
	  (%s                (flurb 's))
	  (%split            (flurb 'split))
	  (%w                (flurb 'w))
	  ;; (%command-result   (flurb 'command-result))
	  )
      `(lambda (state)
	 (with-slots
	     ((,%count sed::count)
	      (,%line sed::line)
	      (,%next sed::next)
	      (,%clip sed::clip)
	      (,%eof-p sed::eof-p)
	      (,%stop-flag sed::stop-flag))
	     state
	   (declare (ignorable ,%count ,%line ,%next ,%clip ,%eof-p ,%stop-flag))
	   (flet ((,%stop () (funcall 'sed::stop))
		  (,%next-line () (funcall 'sed::next-line))
		  (,%append-next-line () (funcall 'sed::append-next-line))
		  (,%next-file-line (file) (funcall 'sed::next-file-line file))
		  (,%p (thing) (funcall 'sed::p thing))
		  (,%s (re rep &key all) (funcall 'sed::s re rep :all all))
		  (,%split (re &optional tt) (funcall 'sed::split re tt))
		  (,%w (&optional n) (funcall 'sed::w n))
		  ;; (,%command-result () (funcall 'sed::command-result))
		  )
	     (declare
	      (ignorable (function ,%stop)
			 (function ,%next-line)
			 (function ,%append-next-line)
			 (function ,%next-file-line)
			 (function ,%p)
			 (function ,%s)
			 (function ,%split)
			 (function ,%w)
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

(defun backup-file-name (original-file pattern &optional count)
  "Return a backup file name for ‘original-file’ using ‘pattern’. If ‘pattern’
has an asterisk * in it, the asterisk is replaced by ‘original-file’. If it
doens't have an asterisk, it is just appended to ‘original-file’. If ‘count’ is
given, it's appended to the result."
  (let* ((star-pos (position #\* pattern))
	 (result
	  (cond
	   (star-pos
	    (replace-subseq "*" original-file pattern))
	   (t
	    (s+ original-file pattern)))))
    (if count
	(s+ result "~" count)
        result)))

(defun temporary-file-name (original count)
  (s+ original ".sed-" count))

(defparameter *retry-limit* 10101)

(defun open-output (file-name in-place)
  "Open ‘file-name’ and return a stream. Create it if it doesn't exist.
 If ‘in-place’ is true, open a temporary file based on ‘file-name’."
  (cond
   (in-place
    (let* ((count 0) stream)
      (loop :do
        (setf stream
	      (open (temporary-file-name file-name count)
		    :direction :output :if-does-not-exist :create
		    :if-exists nil))
	(incf count)
	:while (and (not stream) (< count *retry-limit*)))
      (when (not stream)
	;; We could do a correctable error here, but I don't really want
	;; to make sed potentially interactive.
	(error "Failed to open a temporary file too many times."))
      stream))
   (t
    (open file-name :direction :output :if-does-not-exist :create))))

(defun copy-metadata (from to)
  #+unix
  (let* ((stat (uos:stat (nos:safe-namestring from)))
	 (to-name (nos:safe-namestring to)))
    (uos:chmod to-name (uos:file-status-mode stat))
    ;; Note there's things like xattrs and acls but they really suck. If we
    ;; finish the cp command that does everything properly, then switch to
    ;; using that.
    ;;
    ;; Use real-chown herebecause we want to ignore errors, becuase only root
    ;; can be setting uid.
    (when (= -1 (uos::real-chown to-name
				 (uos:file-status-uid stat)
				 (uos:file-status-gid stat)))
      ;; Try to just set the group.
      (uos::real-chown to-name -1 (uos:file-status-gid stat))))
  #-unix
  (declare (ignore from to)))

(defun close-output (in-place backup-pattern delete-backup)
  "Close ‘stream’. If ‘in-place’ is true, rename the stream according to
 ‘backup-pattern’ as described in backup-file-name. If ‘delete-backup’ is true,
delete the backup file at the end."
  (with-slots (output output-name) *state*
    (unwind-protect
	(when in-place
	  (let (fn)
	    ;; Pick a backup file name.
	    (loop :with count = 0
	      :do
	      (setf fn (backup-file-name output-name backup-pattern
					 (when (> count 0) count)))
	      (incf count)
	      :while (and (probe-file fn) (< count *retry-limit*)))
	    ;; Rename the the actual output file to a backup file.
	    (rename-file output-name fn)
	    ;; Rename the temopary output file to the real output file name.
	    (nos:os-rename-file (probe-file output) output-name)
	    (copy-metadata fn output-name)
	    (when delete-backup
	      (nos:os-delete-file fn))))
      (close output))))

(defun edit-stream (script &key
			   (input *standard-input*)
			   (output *standard-output*)
			   script-file quiet in-place backup-pattern
			   delete-backup
			   collect)
  #.(s+ "Edit a stream. Run the ‘script’ and/or the contents of ‘script-file’ on
‘input’. Write the results to ‘output’"	*doc*)
  (when (and in-place collect)
    (error "in-place doesn't make sense with collect."))
  (let* ((*state* (make-state :input input :output output :collect collect))
	 ;; (code-package (make-code-package))
	 (preface nil)
	 (prepared
	  (loop :for (addr form)
		:in (or (and script-file (append (read-script script-file)
						 script))
			script)
		:if (null addr)
                  :do (push (compile-form form) preface)
                :else
		  :collect (cons (make-address addr)
				 (compile-form form #|code-package|#))))
	 close-output close-input)
    (when preface
      (setf preface (nreverse preface)))
    ;; (format t "prepared ~s~%" prepared)
    (unwind-protect
	(with-slots (count line next stop-flag eof-p missing-newline) *state*
	  (flet ((maybe-print-result (result)
		   (when (and result (not quiet))
		     (if (and missing-newline eof-p)
			 (put-string result output)
			 (put-line result output)))))
	    (when (and (or (stringp output) (pathnamep output))
		       (not collect))
	      (setf close-output t
		    (state-output-name *state*) output
		    output (open-output output in-place)
		    (state-output *state*) output))
	    (when collect
	      (setf (state-output *state*) '()))
	    (when (or (stringp input) (pathnamep input))
	      (setf close-input t
		    (state-input-name *state*) input
		    input (open input :direction :input)
		    (state-input *state*) input))
	    (when preface
	      (loop :with result
		    :for func :in preface
		    :do
		       (setf result (funcall func *state*))
		       (maybe-print-result result)))
	    (loop :with result :and matches
		  :do
		     (next-line)
		  :while (and line (not stop-flag))
		  :do
		     (setf matches nil
			   result line)
		     (loop :for (address . func) :in prepared :do
		       (when (address-matches address line count eof-p)
			 (setf result (funcall func *state*)
			       matches t)
			 (maybe-print-result result)))
		     (when (not matches)
		       (maybe-print-result result)))))
      (when (and input close-input)
	(close input))
      (when (and output close-output)
	(close-output in-place backup-pattern delete-backup))
      (when (state-files *state*)
	(omapk (_ (when (open-stream-p (oelt _ 1))
		    (close (oelt _ 1))))
	       (state-files *state*))))
    (when collect
      (nreverse (state-output *state*)))))

;; @@@ The macro version can probably be better, but we need the non-macro
;; version for calling as shell command.
#|
(defmacro sed ((&key (input *standard-input*) (output *standard-output*))
	       &body body)
  `(loop :with count :and line
    :while (setf line (read-line input) nil)
    :do
    (loop
)
|#

#|

(defun parse-address ()
  )

(defun parse-command-letter ()
  (case (next-char)
    (#\a (read-un
    (#\b)
    (#\c)
    (#\d)
    (#\D)
    (#\e)
    (#\F)
    (#\g)
    (#\G)
    (#\h)
    (#\H)
    (#\i)
    (#\l)
    (#\n)
    (#\N)
    (#\p)
    (#\P)
    (#\q)
    (#\Q)
    (#\Q)
    (#\r)
    (#\R)
    (#\s)
    (#\t)
    (#\T)
    (#\v)
    (#\w)
    (#\W)
    (#\x)
    (#\y)
    (#\z)
    (#\#)
    (#\{)
    (#\=)
    (#\:)
  )

(defun parse-options ()
  )

(defun parse-sed-command ()
  (sequence-of
   (zero-or-more (in-sequence *whitespace*))
   (optional (parse-address))
   (parse-command-letter)
   (optional (parse-options))
   (zero-or-more (in-sequence *whitespace*))))

(defun parse-sed-program ()
  "Turn a unix style sed script into Lisp style."
  (with-parsing ()
    (one-or-more (parse-sed-command))))
|#

;; End
