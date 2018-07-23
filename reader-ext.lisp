;;
;; reader-ext.lisp - Extensions to the Common Lisp reader.
;;

(defpackage :reader-ext
  (:documentation "Extensions to the Common Lisp reader.

Currently, there is only one conceptual extension which is *READ-INTERN*.
Based on this we provide two facilities: CLEAN-READ* and PACKAGE-ROBUST-READ*.

The CLEAN-READ* functions, read without interning unknown symbols *package*
instead returning them as uninterned symbols. This preserves the set of internal
symbols, and doesn't pollute the package with useless or possibly troublesome
symbols. This is very useful if you want to read partially complete or incorrect
code.

The PACKAGE-ROBUST-READ* functions, read treating unknown symbols or packages
as uninterned. This useful for reading code that is not loaded, so the packages
don't exist yet. Or reading s-expressions data from other images which may
include symbols from non-existant packages.

I think the best way to do this is to have the *READ-INTERN* extension to your
implementation. I have done this for SBCL and CCL. CLEAN-READ* has a fallback
using temporary packages, but it's slow and maybe doesn't even work. I haven't
come up with any fallback for PACKAGE-ROBUST-READ*.

Another way to do it is to have whole separate read implementation...

If you have *READ-INTERN*, :has-read-intern should be in features, before
loading this.
")
  (:use :cl :dlib)
  (:export
   #:clean-read-from-string
   #:package-robust-read-from-string
   #:package-robust-read
   ))
(in-package :reader-ext)

#+has-read-intern
(defun interninator (name package dirt-pile)
  "Return the symbol NAME from package if it exists, or from the DIRT-PILE
package if it doesn't. If DIRT-PILE is NIL, return a packageless symbol."
  (or (let ((pkg (find-package package)))
	(and pkg (find-symbol name pkg)))
      (if dirt-pile
	  (intern name dirt-pile)
	  (make-symbol name))))

;; Muffle the complaint about using &optional and &key.
#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
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
  (missing-implementation 'package-robust-read))

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

;; EOF
