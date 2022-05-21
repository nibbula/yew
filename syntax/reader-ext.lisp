;;;
;;; reader-ext.lisp - Extensions to the Common Lisp reader.
;;;

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
don't exist yet. Or reading s-expression data from other images which may
include symbols from non-existent packages.

I think the best way to do this is to have the *READ-INTERN* extension to your
implementation. I have done this for SBCL and CCL. CLEAN-READ* has a fallback
using temporary packages, but it's slow and maybe doesn't even work. I haven't
come up with any fallback for PACKAGE-ROBUST-READ*.

Another way to do it is to have a whole separate read implementation. For this
we currently use the Eclector library.

If you have *READ-INTERN*, :has-read-intern should be in features, before
loading this.
")
  (:use :cl :dlib
	;; extensions
	#+ccl :ccl
	#+sbcl :sb-ext
	#-has-read-intern :cl-unicode)
  #-has-read-intern
  (:shadowing-import-from :eclector.reader
			  #:read #:read-from-string)
  #-has-read-intern
  (:import-from :eclector.reader
		;;#:*read-intern*
		#:*client*
		#:interpret-symbol
		#:find-character
		#:make-structure-instance)
  (:export
   #:clean-read-from-string
   #:safe-clean-read-from-string
   #:safe-clean-read
   #:package-robust-read-from-string
   #:package-robust-read
   #:robust-read
   #:flexible-read
   #:flexible-read-from-string
   ))
(in-package :reader-ext)

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 2) (space 0) (compilation-speed 0)))

(defun interninator (name package dirt-pile)
  "Return the symbol NAME from package if it exists, or from the DIRT-PILE
package if it doesn't. If DIRT-PILE is NIL, return a packageless symbol."
  (or (let ((pkg (find-package package)))
	(and pkg (find-symbol name pkg)))
      (if dirt-pile
	  (intern name dirt-pile)
	  (make-symbol name))))

#-has-read-intern
(progn
  (defclass unicode-reader ()
     ()
    (:documentation "A reader that reads unicode character names."))

  (defmethod find-character ((client unicode-reader) name)
    (character-named (string name) :try-lisp-names-p t))

  (defclass robust-unicode-reader (unicode-reader)
    ()
    (:documentation
     "A reader that reads unicode character names, avoids errors by substituting
#\replacement_character for any unknown names."))

  (defmethod find-character ((client robust-unicode-reader) name)
    (or (character-named (string name) :try-lisp-names-p t)
	(code-char #XFFFD)))

  (defclass practical-reader ()
    ()
    (:documentation "A reader that can usually read structures."))

  ;; see make-structure.lisp
  #+(and (not has-read-intern) (or sbcl ccl ecl clisp))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defmacro %make-structure (s-name arglist)
      `(let* ((class (find-class ,s-name))
	      (struct (allocate-instance class))
	      ;; (package (symbol-package ,s-name))
	      )
	 (loop :for (slot value) :on ,arglist :by #'cddr
               :do (setf (slot-value struct
				     ;; (intern
				     ;;  (string
				     ;;   (find-slot-name class slot)
				     ;;  package)
				     (find-slot-name class slot))
			 value))
	 struct)))

  #+(and (not has-read-intern) (not (or sbcl ccl ecl clisp)))
  (error
   "Sorry, make-structure-instance hasn't been implemented yet for your Lisp.")

  (defmethod make-structure-instance ((client practical-reader)
				      name initargs)
  "Make an instance of structure ‘name’, with slots initialized by ‘initargs’,
‘initargs’ should be a plist like (slot-name value slot-name value ...).
‘name’ should be a symbol of which the package is probably ignored."
    (%make-structure name initargs))

  (defvar *dirt-pile* nil
    "A package to pile dirt into.")

  (defclass clean-reader (unicode-reader practical-reader)
    ()
    (:documentation "A reader that doesn't pollute packages."))

  ;; #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defmethod interpret-symbol ((client clean-reader) input-stream
			       package-name symbol-name internp)
    (declare (ignore client input-stream internp)
	     #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
    (interninator symbol-name package-name *dirt-pile*))
  ;; #+sbcl (declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

  (defparameter *clean-client* (make-instance 'clean-reader)
    "A ‘client’ for the clean reader."))

#|
(defun horrible-clean-read-from-string (string package
					&optional (eof-error-p t) eof-value
					&key (start 0) end preserve-whitespace)
  "This is a very inefficient way which makes a new package every time."
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
    (values obj pos)))
|#

;; Muffle the complaint about using &optional and &key.
#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(defun clean-read-from-string (string package
			       &optional (eof-error-p t) eof-value
			       &key (start 0) end preserve-whitespace)
  "Read from a string without interning unknown symbols in *package*, instead
interning them in PACKAGE, or if PACKAGE is NIL, returning them as uninterned
symbols."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (let (#+has-read-intern (*read-intern* #'(lambda (str pkg)
					     (interninator str pkg package)))
	#-has-read-intern (*client* *clean-client*)
	#-has-read-intern (*dirt-pile* package))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace)))

#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(defun safe-clean-read-from-string (string package
				    &optional (eof-error-p t) eof-value
				    &key (start 0) end preserve-whitespace)
  "Read from a string in a hopefully safe manner, such that the content cannot
cause evaluation, and without interning unknown symbols in *package*, instead
interning them in PACKAGE, or if PACKAGE is NIL, returning them as uninterned
symbols."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (let (#+has-read-intern (*read-intern* #'(lambda (str pkg)
					     (interninator str pkg package)))
	#-has-read-intern (*client* *clean-client*)
	#-has-read-intern (*dirt-pile* package))
    (with-standard-io-syntax
      (let ((*read-eval* nil))
	(read-from-string string eof-error-p eof-value
			  :start start :end end
			  :preserve-whitespace preserve-whitespace)))))

#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
(defun safe-clean-read (stream package &optional (eof-error-p t) eof-value)
  "Read from a stream in a hopefully safe manner, such that the content cannot
cause evaluation, and without interning unknown symbols in *package*, instead
interning them in PACKAGE, or if PACKAGE is NIL, returning them as uninterned
symbols."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (let (#+has-read-intern (*read-intern* #'(lambda (str pkg)
					     (interninator str pkg package)))
	#-has-read-intern (*client* *clean-client*)
	#-has-read-intern (*dirt-pile* package))
    (with-standard-io-syntax
      (let ((*read-eval* nil))
	(read stream eof-error-p eof-value)))))

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

#-has-read-intern
(progn
  (defclass robust-reader (robust-unicode-reader practical-reader)
    ()
    (:documentation "A reader that doesn't fail on unknown packages."))

  (defmethod interpret-symbol ((client robust-reader) input-stream
			       package-name symbol-name internp)
    (declare (ignore client input-stream internp))
    (package-robust-intern symbol-name package-name))

  (defparameter *robust-client* (make-instance 'robust-reader)
    "A ‘client’ for the robust reader."))

(defun package-robust-read-from-string (string
					&optional (eof-error-p t) eof-value
					&key (start 0) end preserve-whitespace)
  "Read from a string treating unknown symbols or packages as uninterned."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (let (#+has-read-intern (*read-intern* #'package-robust-intern)
	#-has-read-intern (*client* *robust-client*))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace)))

(defun package-robust-read (&optional (stream *standard-input*)
			      (eof-error-p t) (eof-value nil) (recursive-p nil))
  "Read treating unknown symbols or packages as uninterned."
  (let (#+has-read-intern (*read-intern* #'package-robust-intern)
	#-has-read-intern (*client* *robust-client*))
    (read stream eof-error-p eof-value recursive-p)))

;; Define a customizable reader interface which give you at least as much
;; control as *read-intern*.

#-has-read-intern
(progn
  (defclass flexible-reader (unicode-reader practical-reader)
    ()
    (:documentation "A reader with a customizable intern."))

  (defparameter *flexible-client* (make-instance 'flexible-reader)
    "A ‘client’ for the flexible reader.")

  (defvar *flexible-intern* nil
    "The dynmaic intern function for the flexible reader.")

  (defmethod interpret-symbol ((client flexible-reader) input-stream
			       package-name symbol-name internp)
    (declare (ignore client input-stream internp))
    (funcall *flexible-intern* symbol-name package-name)))

(defun flexible-read (&key stream eof-error-p eof-value recursive-p
			intern-function)
  "A read which can be customized by providing an ‘intern’ function to use
when the reader wants to call ‘intern’ a symbol. Note that it uses keywods
instead of the traditional optional arguments."
  (let (#+has-read-intern (*read-intern* intern-function)
	#-has-read-intern (*client* *flexible-client*)
	#-has-read-intern (*flexible-intern* intern-function))
    (read stream eof-error-p eof-value recursive-p)))

(defun flexible-read-from-string (&key string eof-error-p eof-value
				    (start 0) end preserve-whitespace
				    intern-function)
  "A read which can be customized by providing an ‘intern’ function to use
when the reader wants to call ‘intern’ a symbol. Note that it uses keywods
instead of the traditional optional arguments."
  (let (#+has-read-intern (*read-intern* intern-function)
	#-has-read-intern (*client* *flexible-client*)
	#-has-read-intern (*flexible-intern* intern-function))
    (read-from-string string eof-error-p eof-value
		      :start start :end end
		      :preserve-whitespace preserve-whitespace)))

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

;; EOF
