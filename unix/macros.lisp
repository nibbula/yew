;;
;; unix/macros.lisp - Macros for the unix interface
;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;; Macros for convenient defining of platform specific things.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *platform-index*
    (or
     #+darwin           1
     #+linux            2
     #+sunos            3
     #+freebsd          4
     #+openbsd          5
     nil))

  (defparameter *platform-bitsize-index*
    (or
     #+darwin                           1
     #+(and linux 32-bit-target)        2
     #+(and linux 64-bit-target x86-64) 3
     #+(and linux 64-bit-target arm64)  4
     #+sunos                            5
     #+(and freebsd 64-bit-target)      6
     #+(and openbsd 64-bit-target)      7
     nil))

  (when (not (and *platform-index* *platform-bitsize-index*))
    (error "We don't know about your platform."))

  (defmacro define-simple-types (types-array)
    "Define the appropriate types for the platform from the given TYPES-ARRAY."
    `(progn
       ,@(loop :for type :across types-array
	    :collect
	    `(defctype ,(aref type 0) ,(aref type *platform-bitsize-index*)))))

  (defmacro define-constants (constant-array)
    "Define the appropriate constant for the platform from the given
CONSTANT-ARRAY."
    `(progn
       ,@(loop :for type :across constant-array
	    :collect
	    `(defconstant ,(aref type 0) ,(aref type *platform-index*)))))

  (defmacro define-platform-constants (constant-array)
    "Define the appropriate constant for the platform, with distict bit sizes,
from the given CONSTANT-ARRAY."
    `(progn
       ,@(loop :for type :across constant-array
	    :collect
	      `(defconstant ,(aref type 0)
		 ,(aref type *platform-bitsize-index*)))))

  (defmacro define-constants-from (constant-array)
    "Define the appropriate constants for the platform from the array in the
given CONSTANT-ARRAY variable."
    `(progn
       ,@(loop :for type :across constant-array
	    :collect
	    `(defconstant ,(aref type 0) ,(aref type *platform-index*)))))

  (defmacro define-name-list-from (name constant-array &optional docstring)
    "Define a variable named NAME as a list of the appropriate platform entries
from the CONSTANT-ARRAY variable, that are non-NIL."
    `(defparameter ,name
       '(,@(loop :for v :across constant-array
	      :when (aref v *platform-index*)
	      :collect (aref v 0)))
       ,@(or (list docstring))))
  )

;; End
