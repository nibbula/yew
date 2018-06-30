;;
;; types.lisp - Operating system types
;;

(in-package :opsys-base)

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

;; @@@ Perhaps we should rename this to process-info or something.
(defstruct os-process
  "Information about a system process."
  (id		   0 :type integer)
  (parent-id	   0 :type integer)
  user
  (size            0 :type integer)
  name)

(defclass process-handle ()
  ((value
    :initarg :value :accessor process-handle-value  
    :documentation "The system specific value of the handle."))
  (:documentation
   "System identifier for a running process, usually one that we made."))

(define-condition opsys-error (simple-error)
  ((code
    :accessor opsys-error-code
    :initarg :error-code
    :type (signed-byte 32)
    :documentation "The error code of the last error."))
  (:report (lambda (c s)
	     (if (and (slot-boundp c +simple-condition-format-control-slot+)
		      (slot-value c +simple-condition-format-control-slot+))
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

;; EOF
