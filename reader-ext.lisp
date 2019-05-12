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

Another way to do it is to have whole separate read implementation. For this
we currently use the Eclector package.

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
		#:find-character)
  (:export
   #:clean-read-from-string
   #:safe-clean-read-from-string
   #:safe-clean-read
   #:package-robust-read-from-string
   #:package-robust-read
   #:robust-read
   ))
(in-package :reader-ext)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
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
    (character-named name :try-lisp-names-p t))

  (defclass robust-unicode-reader (unicode-reader)
    ()
    (:documentation
     "A reader that reads unicode character names, avoids errors by substituting
#\replacement_character for any unknown names."))

  (defmethod find-character ((client robust-unicode-reader) name)
    (or (character-named name :try-lisp-names-p t)
	(code-char #XFFFD)))

  ;; @@@ maybe use this when make-structure-instance is in the quicklisp version.
#|
  (defclass practical-reader ()
    ()
    (:documentation "A reader that can usually read structures."))

  ;; Making a structure of which all you know is the name, seems to be rife
  ;; with subtle problems. None of the solutions presented here are completely
  ;; satisfactory. Implementation specific code may function best, but is
  ;; susceptable to drift. Portable code can't be assured to do the job
  ;; perfectly.

  ;; Not the similarity of the implementation specific code. I think if Common
  ;; Lisp would have just included a way to retrieve the constructor, this
  ;; would be able to be portable. I know in the old days, for maximum
  ;; efficiency, you might want to toss everything about a struct, but since
  ;; structs are now classes too, you could only do that in very extreme cases.

  (defmethod make-structure-instance ((client practical-reader)
				      name initargs)
    "Make a structure of type NAME with constructor arguments INITARGS."
    (cond
      #+sbcl
      ((let ((classoid (sb-kernel:find-classoid name nil)))
	 (unless (typep classoid 'sb-kernel:structure-classoid)
	   (%reader-error stream 'sharp-s-not-structure-type :body body))
	 (let ((default-constructor (sb-kernel:dd-default-constructor
				     (sb-kernel:layout-info
				      (sb-kernel:classoid-layout classoid)))))
	   (unless default-constructor
	     (error "The ~s structure does not have a default constructor."
		    name))
	   (apply (fdefinition default-constructor) initargs))))
      #+ccl
      ((let ((structure-definition (gethash name ccl::%defstructs%)))
	 (unless structure-definition
	   (%reader-error stream 'sharp-s-not-structure-type :body body))
	 (let ((default-constructor (ccl::sd-constructor structure-definition)))
	   (unless default-constructor
	     (error "The ~s structure does not have a default constructor."
		    name))
	   (apply (fdefinition default-constructor) initargs))))
      ;; Some implementations will fail to make-instance for structs with a NIL
      ;; constructor. Some won't fail. I don't think it's specified. But also
      ;; the arguments could be incorrect. We really don't have any portable
      ;; way of knowing.
      ((catch 'some-error
	 (handler-case
	     ;; Since this will mask errors, we probably need to check the
	     ;; arguments ourselves with the MOP?
	     (apply #'make-instance class initargs)
	   (error (c)
	     (declare (ignore c))
	     (throw 'some-error nil)))))
      ;; See if we can just guess the constructor name.
      ;; This is probably a bad idea, since it could have been redefined to
      ;; anything.
      ((let ((maker (find-symbol (concatenate 'string "MAKE-"
					      (string-upcase name))
				 (symbol-package name))))
	 (and maker (fboundp maker)
	      (apply maker initargs))))
      ;; We have no other way of getting the constructor name or even knowing if
      ;; it has one. So we have to take the worst option: defer to the host
      ;; implementaion. Of course this means if you use this code for writing an
      ;; implementation, and don't change this, #S processing code will become
      ;; hidden in your image.
      (t
       (cl:read-from-string (format nil "#S~s" `(,name ,@initargs))))))
|#

  (defvar *dirt-pile* nil
    "A package to pile dirt into.")

  (defclass clean-reader (unicode-reader #| practical-working-reader |#)
    ()
    (:documentation "A reader that doesn't pollute packages."))

  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defmethod interpret-symbol ((client clean-reader) input-stream
			       package-name symbol-name internp)
    (declare (ignore client input-stream internp))
    (interninator symbol-name package-name *dirt-pile*))
  #+sbcl (declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

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
  (defclass robust-reader (robust-unicode-reader)
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

#+sbcl (declaim (sb-ext:unmuffle-conditions style-warning))

;; EOF
