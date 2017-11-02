;;
;; dlib2.lisp - Dan's utilities of redundant doom, second file.
;;

;; This is for things which depend on features added in dlib1.lisp

#+debug-rc (progn (format t " 2") (force-output *standard-output*))

(in-package :dlib)

;; Muffle the complaint about using &optional and &key.
#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(defun safe-read-from-string (string &optional (eof-error-p t) eof-value
			      &key (start 0) end preserve-whitespace)
  "Read from a string in a hopefully safe manner, such that the content
cannot cause evaluation."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string string eof-error-p eof-value
			:start start :end end
			:preserve-whitespace preserve-whitespace))))

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

(defun clean-read-from-string (string package
			       &optional (eof-error-p t) eof-value
			       &key (start 0) end preserve-whitespace)
  "Read from a string without interning unknown symbols in *package*, instead
returning them as uninterned symbols."
  ;; This is the good way, which uses the *read-intern* extension.
  #+has-read-intern
  (let ((*read-intern* #'(lambda (str pkg)
			   (interninator str pkg package))))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace))
  ;; This is a very inefficient way which makes a new package every time.
  #-has-read-intern
  (let (pkg obj pos)
    (unwind-protect
	 (progn
	   (setf pkg (or (and package (copy-package package))
			 (make-package (gensym "junkpak") :use '())))
	   (with-package pkg
	     (setf (values obj pos)
		   (read-from-string
		    string eof-error-p eof-value
		    :start start :end end
		    :preserve-whitespace preserve-whitespace))))
      (when pkg
	(delete-package pkg)))
    (values obj pos))
  )

(defun package-robust-intern (s p)
  "Return S interned in package P, or S interned in *PACKAGE*, or S as an
un-interned symbol."
  (let ((p (find-package p)))
    (if p
	(multiple-value-bind (sym status) (find-symbol s p)
	  (if status
	      sym
	      (multiple-value-bind (sym status)
		  (find-symbol s *package*)
		(if status sym (make-symbol s)))))
	(multiple-value-bind (sym status) (find-symbol s *package*)
	  (if status sym (make-symbol s))))))

(defun package-robust-read-from-string (string
					&optional (eof-error-p t) eof-value
					&key (start 0) end preserve-whitespace)
  "Read from a string treating unknown symbols or packages as uninterned."
  #+has-read-intern  
  (let ((*read-intern* #'package-robust-intern))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace))
  #-has-read-intern
  (declare (ignore string eof-error-p eof-value start end preserve-whitespace))
  #-has-read-intern
  (missing-implementation 'package-robust-read-from-string))

(defun package-robust-read (&optional (stream *standard-input*)
			      (eof-error-p t) (eof-value nil) (recursive-p nil))
  "Read treating unknown symbols or packages as uninterned."
  #+has-read-intern  
  (let ((*read-intern* #'package-robust-intern))
    (read stream eof-error-p eof-value recursive-p))
  #-has-read-intern
  (declare (ignore stream eof-error-p eof-value recursive-p))
  #-has-read-intern
  (missing-implementation 'package-robust-read-from-string))

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

#+debug-rc (progn (format t "]") (force-output *standard-output*))

;; EOF
