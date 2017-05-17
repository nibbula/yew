;;
;; opsys-base.lisp - Helper functions, setup, and types which are not system
;;                   specific, and need to come before the system specific
;;		     package.

(defpackage :opsys-base
  (:documentation "Helper functions and setup which are not system specific.")
  (:use :cl :cffi :dlib)
  (:export
   #:config-feature
   #:function-defined
   #:missing-implementation
   #:quote-filename
   #:safe-namestring

   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode

   #:size-t
   #:string-designator

   #:user-info
   #:user-info-p
   #:make-user-info
   #:user-info-name
   #:user-info-id
   #:user-info-full-name
   #:user-info-home-directory
   #:user-info-shell
   #:user-info-primary-group-id
   #:user-info-guid
   #:user-info-picture

   #:terminal-mode
   #:terminal-mode-p
   #:make-terminal-mode
   #:terminal-mode-echo
   #:terminal-mode-line
   #:terminal-mode-raw
   #:terminal-mode-timeout

   #:derp-time
   #:derp-time-p
   #:make-derp-time
   #:derp-time-seconds
   #:derp-time-nanoseconds

   #:file-info
   #:file-info-p
   #:make-file-info
   #:file-info-creation-time
   #:file-info-access-time
   #:file-info-modification-time
   #:file-info-size
   #:file-info-type
   #:file-info-flags

   #:filesystem-info
   #:filesystem-info-p
   #:make-filesystem-info
   #:filesystem-info-device-name
   #:filesystem-info-mount-point
   #:filesystem-info-type
   #:filesystem-info-total-bytes
   #:filesystem-info-bytes-free
   #:filesystem-info-bytes-available

   #:os-process
   #:os-process-p
   #:make-os-process
   #:os-process-id
   #:os-process-parent-id
   #:os-process-group-id
   #:os-process-user-id
   #:os-process-terminal
   #:os-process-text-size
   #:os-process-resident-size
   #:os-process-percent-cpu
   #:os-process-nice-level
   #:os-process-usage
   #:os-process-command
   #:os-process-args

   #:opsys-error
   #:opsys-error-code
   #:opsys-resumed
   #:opsys-resized

   #:*directory-separator*
   #:*directory-separator-string*
   #:*path-separator*
   #:*path-variable*
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
#+(and (or darwin linux freebsd) (or x86_64 x86-64))
  (config-feature :64-bit-target)
#+ecl (eval-when (:compile-toplevel :load-toplevel :execute)
	(when (= (cffi:foreign-type-size :long) 8)
	  (config-feature :64-bit-target)))
#+(and (not 64-bit-target) (or x86 ppc sparc arm))
  (config-feature :32-bit-target)

#+(and 32-bit-target 64-bit-target) (error "Can't be both 32 & 64 bits!")

(defparameter *need-quoting* "[*?;:"
  "Characters that may need escaping in a pathname.")

;; I am probably unable to express how unfortunate this is.
(defun quote-filename (namestring)
  "Try to quote a file name so none of it's characters are noticed specially
by the Lisp pathname monster. This is useful just before passing strings to
standard functions that take a pathname designator, such as OPEN."
  (with-output-to-string (str)
    (loop :for c :across namestring :do
       (when (position c *need-quoting*)
	 (princ #\\ str))
       (princ c str))))

#|  (let ((result namestring))
      (flet ((possibly-quote (c)
	     (when (position c result)
	       ;; It's just not possible to write code this inefficient in C.
	       (setf result (join (split-sequence c result) (s+ #\\ c))))))
      (loop :for c :across "[*;:" :do
	 (possibly-quote c))
      result)))
|#

(defun safe-namestring (pathname)
  "Like NAMESTRING, but if pathname is a string, just return it. This is
useful for accepting pathnames or strings in case namestring would interpret
any characters in strings specially."
  (typecase pathname
    (pathname (namestring pathname))
    (string pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

;; Returned by read-directory.
(defstruct dir-entry
  "Filesystem directory entry, like unix dirent."
  (name  nil :type (or string null))
  (type  nil :type (or keyword null))
  (inode nil :type (or integer null)))

;; Needed for standard C library functions.
(defctype size-t :unsigned-long)

(deftype string-designator ()
  "A designator for a string; that is, an object that denotes a string and
that is one of: a character (denoting a string that has the character as its
only element), a symbol (denoting the string that is its name), or a
string (denoting itself)."
  '(or string character symbol))

(defstruct user-info
  "Minimal semi-compatible user data."
  name
  id
  full-name
  home-directory
  shell
  primary-group-id
  guid
  picture)

(defstruct terminal-mode
  "Terminal settings."
  (echo    nil :type boolean)
  (line    nil :type boolean)
  (raw     nil :type boolean)
  (timeout nil :type (or null integer)))

(defstruct derp-time
  "I can't tell you how much I dislike these units."
  seconds
  nanoseconds)

;; Whatever
(defstruct file-info
  "File information."
  ;; Type and flags should only have things which can be reliably detected
  ;; on all systems and have nearly the same meaning and are useful.
  (type nil  :type (member :regular :directory :link :device :other))
  (size 0    :type integer)		; in bytes
  (flags nil :type list)		; :hidden :immutable :compressed
  creation-time
  access-time
  modification-time)

(defstruct filesystem-info
  "File system information."
  device-name
  mount-point
  type
  (total-bytes     0 :type integer)
  (bytes-free      0 :type integer)
  (bytes-available 0 :type integer))

(defstruct os-process
  "Information about a system process."
  (id		   0 :type integer)
  (parent-id	   0 :type integer)
  (group-id	   0 :type integer)
  (user-id	   0 :type integer)
  terminal
  (text-size	   0 :type integer)
  (resident-size   0 :type integer)
  percent-cpu
  (nice-level	   0 :type integer)
  usage
  command
  (args #() :type vector))

(define-condition opsys-error (simple-error)
  ((code
    :accessor opsys-error-code
    :initarg :error-code
    :type (signed-byte 32)
    :documentation "The error code of the last error."))
  (:report (lambda (c s)
	     (if (and (slot-boundp c 'format-control)
		      (slot-value c 'format-control))
		 (format s "~? ~a"
			 (simple-condition-format-control c)
			 (simple-condition-format-arguments c)
			 (symbol-call :opsys :error-message
				      (opsys-error-code c)))
		 (format s "~a"
			 (symbol-call :opsys :error-message
				      (opsys-error-code c))))))
  (:documentation "An error from calling an operating system function."))

(define-condition opsys-resumed (simple-error)
  ()
  (:default-initargs
   :format-control "[Terminal Resumed]~%")
  (:documentation "The process was resumed from being suspended."))

(define-condition opsys-resized (simple-error)
  ()
  (:default-initargs
   :format-control "[Terminal Resized]~%")
  (:documentation "The window changed size."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defparameter *directory-separator*
  #-windows #\/
  #+(and windows (not cygwin)) #\\
  "Character that separates directories in a path.")

(defparameter *directory-separator-string* (string *directory-separator*)
  "The directory separator character as a string, for convenience or
efficiency.")

;; Like on windows this is #\; right? But not cygwin?
(defparameter *path-separator*		; @@@ defconstant?
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

(defparameter *path-variable*
  #-windows "PATH"
  #+windows "%PATH%"
  "The environment variable which stores the command search paths.")

;; EOF
