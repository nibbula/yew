;;
;; opsys-base.lisp - Helper functions, setup, and types which are not system
;;                   specific, and need to come before the system specific
;;		     package.

(defpackage :opsys-base
  (:documentation "Helper functions and setup which are not system specific.")
  (:use :cl :cffi :dlib)
  (:export
   ;; Stuff in this file:
   #:config-feature
   #:function-defined
   #:missing-implementation
   #:define-enum-list
   #:define-to-list
   #:quote-filename
   #:safe-namestring

   #:*directory-separator*
   #:*directory-separator-string*
   #:*path-separator*
   #:*path-variable*

   ;; Things in types.lisp:
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

   #:os-time
   #:os-time-p
   #:make-os-time
   #:os-time-seconds
   #:os-time-nanoseconds

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
   #:os-process-user
   #:os-process-size
   #:os-process-name

   #:process-handle
   #:process-handle-value

   #:event-set #:make-event-set #:event-set-list #:event-set-os-data
   #:*event-set*
   #:os-event #:os-event-triggered
   #:signal-event #:signal-event-number
   #:io-event #:io-event-handle
   #:input-available-event
   #:output-possible-event
   #:output-finished-event
   #:io-error-event
   #:network-event
   #:network-connection-available-event
   #:os-process-event #:os-process-event-handle
   #:child-died-event #:child-died-event-reason
   #:child-stopped-event
   #:terminal-event
   #:terminal-size-change-event
   #:terminal-size-change-event-width
   #:terminal-size-change-event-height
   #:timer-event #:timer-event-timer
   #:timer-expired-event
   #:timer-triggered-event
   #:system-message-event

   #:opsys-error
   #:opsys-error-code
   #:opsys-resumed
   #:opsys-resized
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

;; Constant defining macros.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-enum-list (list-var constant-array &key (start 0))
    "Define enumerated constants and put the names in LIST-VAR."
    (with-unique-names (offset)
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,offset ,start))
	     ,@(loop :with name :and doc :and i = 0
		  :for c :across constant-array
		  :do
		  (setf name  (aref c 0)
			doc   (aref c 1))
		  :collect
		  `(defconstant ,name (+ ,offset ,i) ,doc)
		  :do (incf i))))
	 ,@(loop :for c :across constant-array
	      :collect
	      `(push ',(aref c 0) ,list-var)))))

  (defmacro define-to-list (list-var constant-array)
    "Define constants and put the names in LIST-VAR."
    `(progn
       ,@(loop :with name :and value :and doc
	    :for c :across constant-array :do
	    (setf name  (aref c 0)
		  value (aref c 1)
		  doc   (if (>= (length c) 3) (aref c 2) nil))
	    :collect
	      (if doc
		  `(defconstant ,name ,value ,doc)
		  `(defconstant ,name ,value))
	    :collect
	    `(push ',name ,list-var)))))

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

(defparameter *need-quoting*
  #-windows "[*?;:"
  #+windows "[*?;"
  "Characters that may need escaping in a pathname.")

;; I am probably unable to express how unfortunate this is.
(defun quote-filename (namestring)
  "Try to quote a file name so none of it's characters are noticed specially
by the Lisp pathname monster. This is useful just before passing strings to
standard functions that take a pathname designator, such as OPEN."
  (typecase namestring
    (pathname namestring)
    (string
     (with-output-to-string (str)
       (loop :for c :across namestring :do
	  (when (position c *need-quoting*)
	    (princ #\\ str))
	  (princ c str))))))

#|  (let ((result namestring))
      (flet ((possibly-quote (c)
	     (when (position c result)
	       ;; It's just not possible to write code this inefficient in C.
	       (setf result (join-by-string (split-sequence c result)
					    (s+ #\\ c))))))
      (loop :for c :across "[*;:" :do
	 (possibly-quote c))
      result)))
|#

(declaim (ftype (function (t) string) safe-namestring))
(defun safe-namestring (pathname)
  "Like NAMESTRING, but if pathname is a string, just return it. This is
useful for accepting pathnames or strings in case namestring would interpret
any characters in strings specially."
  (etypecase pathname
    (pathname
     (let ((ns (namestring pathname)))
       (check-type ns string)
       ns))
    (string pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(declaim (type character *directory-separator*))
(defparameter *directory-separator*
  #-windows #\/
  #+(and windows (not cygwin)) #\\
  "Character that separates directories in a path.")

(defparameter *directory-separator-string* (string *directory-separator*)
  "The directory separator character as a string, for convenience or
efficiency.")

;; Like on windows this is #\; right? But not cygwin?
(declaim (type character *path-separator*))
(defparameter *path-separator*		; @@@ defconstant?
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

(defparameter *path-variable*
  #-windows "PATH"
  ;;#+windows "%PATH%"
  #+windows "PATH"
  "The environment variable which stores the command search paths.")

;; EOF
