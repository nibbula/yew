;;
;; opsys-base.lisp - Helper functions and setup which is not system specific.
;;

(defpackage :opsys-base
  (:documentation "Helper functions and setup which are not system specific.")
  (:use :cl :cffi)
  (:export
   #:config-feature
   #:function-defined
   #:missing-implementation
   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode
   #:size-t
   #:string-designator
   ))
(in-package :opsys-base)

;; Stuff to assist in feature frobbing and portability.

(defmacro config-feature (f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ,f *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-defined (sym pack)
    "True if SYM is an external function defined in package PACK."
    (multiple-value-bind (found-symbol status)
	(find-symbol (symbol-name sym) (find-package pack))
      (and found-symbol (eql status :external) (fboundp found-symbol)))))

;; @@@ I should probably really do this with an error type
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun missing-implementation (sym)
    "Complain that something is missing."
    (error "Somebody needs to provide an implementation for ~a on ~a~%"
	   sym (lisp-implementation-type))))

;; Now we depend on dlib. :(
;; I suppose we could use the one in alexandria, since but it's a dependency
;; of CFFI, but I'm a little nervous about that.
#| 
(defmacro define-constant (name value &optional doc)
  "Like defconstant but works with pendanticly anal SCBL."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))
|#

;; The comments about define-constant apply to this as well.
;; This has to be a macro so it can be used in read time expressions
;; in this file.
;; (defmacro featurep (symbol)
;;   "True if the SYMBOL is in *FEATURES*."
;;   `(not (null (find ,symbol *features*))))

;; This is so we can use the #_ reader macro on openmcl without it interfering
;; with other lisps. On other lisps we define it to do nothing.
#-openmcl (eval-when (:execute)
	    #. (set-dispatch-macro-character
		#\# #\_
		(flet ((pr (stream subchar arg)
			 (declare (ignore subchar arg))
			 (read stream t nil t)))
		  (setf (fdefinition '|#_-reader|) (function pr)))))

;; Generic things

;; Define :32-bit-target or :64-bit-target
#+(and (or darwin linux) x86-64) (config-feature :64-bit-target)
#+ecl (eval-when (:compile-toplevel :load-toplevel :execute)
	(when (= (cffi:foreign-type-size :long) 8)
	  (config-feature :64-bit-target)))
#+(and (not 64-bit-target) (or x86 ppc sparc arm))
  (config-feature :32-bit-target)

#+(and 32-bit-target 64-bit-target) (error "Can't be both 32 & 64 bits!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

;; Returned by read-directory.
(defstruct dir-entry
  "Filesystem directory entry, like unix dirent."
  (name  nil :type (or string null))
  (type  nil :type (or keyword null))
  (inode nil :type (or integer null)))

(defctype size-t :unsigned-long)

(deftype string-designator ()
  "A designator for a string; that is, an object that denotes a string and
that is one of: a character (denoting a string that has the character as its
only element), a symbol (denoting the string that is its name), or a
string (denoting itself)."
  '(or string character symbol))

;; EOF
