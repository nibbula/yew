;;;
;;; dlib2.lisp - Utilities of redundant doom, second file.
;;;

;; This is for things which depend on features added in dlib1.lisp

#+debug-rc (progn (format t "+") (force-output *standard-output*))

(in-package :dlib)

;; @@@ except we could move safe-read* & fancy-read* back to dlib since
;; we moved clean-read* and package-robust* read to reader-ext.

;; Muffle the complaint about using &optional and &key.
;;#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(locally
    #+sbcl (declare (sb-ext:muffle-conditions style-warning))
(defun safe-read-from-string (string &optional (eof-error-p t) eof-value
			      &key (start 0) end preserve-whitespace)
  "Read from a string in a hopefully safe manner, such that the content
cannot cause evaluation."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string string eof-error-p eof-value
			:start start :end end
			:preserve-whitespace preserve-whitespace)))))

(defun safe-read (&optional (stream *standard-input*) (eof-error-p t)
		    eof-value recursive-p)
  "Read in a hopefully safe manner, such that the content cannot cause
evaluation."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read stream eof-error-p eof-value recursive-p))))

(defparameter *readtable-cases* '(:upcase :downcase :preserve :invert)
  "List of valid values for READTABLE-CASE.")

(defun fancy-read-from-string (string
			       &key
				 (eof-error-p t eof-error-p-supplied-p)
				 eof-value
				 (start 0) end
				 preserve-whitespace
				 safe
				 base
				 default-float-format
				 case)
  "Read from string with most of the bells and whistles. This doesn't include
things that need the *READ-INTERN* extension. This probably isn't efficient for
setting CASE. If you need do a lot with CASE, you should probably make your
readtable.

EOF-ERROR-P, EOF-VALUE, START, END, and PRESERVE-WHITESPACE, are all as in
standard read, except for being keywords arguments, and supplying EOF-VALUE,
sets EOF-ERROR-P to NIL if it's not supplied.

SAFE sets *read-eval* to NIL.
BASE sets *read-base*.
DEFAULT-FLOAT-FORMAT sets *READ-DEFAULT-FLOAT-FORMAT*.
CASE sets the temporarily sets the READTABLE-CASE, which should be one of:
  :UPCASE :DOWNCASE :PRESERVE :INVERT
"
  (let ((*read-eval* (if safe nil *read-eval*))
	(*read-base* (or base *read-base*))
	(*read-default-float-format*
	 (or default-float-format *read-default-float-format*))
	(*readtable* (if case
			 (progn
			   (when (not (member case *readtable-cases*))
			     (error "Case must be one of ~s" *readtable-cases*))
			   (let ((rt (copy-readtable *readtable*)))
			     (setf (readtable-case rt) case)
			     rt))
			 *readtable*)))
    (when (and (not eof-error-p-supplied-p)
	       eof-value)
      (setf eof-error-p nil))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace)))

;; Since defalias for the :mop package won't work until after dlib1,
;; I put these in here rather than be potentially slow with symbol-call.
;; In general, anything that uses the MOP might have to be in here.

(defun find-slot-name (class symbol)
  "Return the symbol which is the name of the slot in CLASS whose symbol-name
matches SYMBOL."
  (mop:slot-definition-name
   (find symbol (mop:class-slots (find-class class))
	 :key (_ (mop:slot-definition-name _))
	 :test (lambda (a b)
		 (search (symbol-name a) (symbol-name b) :test #'equalp)))))

#+excl (mop:finalize-inheritance (find-class 'simple-condition))

(defparameter +simple-condition-format-control-slot+
  (find-slot-name 'simple-condition
		  #-lispworks 'format-control
		  #+lispworks 'format-string
		  )
  "Name of the slot that simple-condition-format-control accesses.")

(defparameter +simple-condition-format-arguments-slot+
  (find-slot-name 'simple-condition 'format-arguments)
  "Name of the slot that simple-condition-format-arguments accesses.")

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

#+debug-rc (progn (format t "2") (force-output *standard-output*))

;; EOF
