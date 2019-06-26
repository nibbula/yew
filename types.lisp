;;
;; types.lisp - Operating system types
;;

(in-package :opsys-base)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

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

(defstruct os-time
  "I can't tell you how much I dislike these units."
  seconds
  nanoseconds)

(deftype file-type ()
  "Most general category of a file in a file system."
  `(member :regular :directory :link :device :other))

;; Whatever
(defstruct file-info
  "File information."
  ;; Type and flags should only have things which can be reliably detected
  ;; on all systems and have nearly the same meaning and are useful.
  (type nil  :type file-type)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; events

;; What we're trying to do here is provide a uniform platform independent
;; interface to the lowest level of operating system events, which other,
;; perhaps more complicated, things can be built on. Application event
;; handling is a complex subject, and is tightly coupled to application
;; design. O/S's with GUI's have many many other events, which are beyond the
;; scope of what we're trying to do here. In particular, this should be able to
;; be reasonably used by a network server application, a command-line
;; application, or a GUI application.
;;
;; The goal with this is so you can say 'await-event' or similar, at the
;; lowest level and be sure you will get some kind of event for anything the
;; operating system can do. A higher level will likely have to disect or
;; process that event into an application level event.

(defstruct event-set
  list			; The list of events
  os-data)		; A place for the O/S specific code to put something.

(defvar *event-set* nil
  "The default event set.")

(defclass os-event ()
  ((triggered
    :initarg :triggered :accessor os-event-triggered :initform nil :type boolean
    :documentation
    "True if the event was triggered by your outrageous comment."))
  (:documentation "A generic event of any type."))

(defclass signal-event (os-event)
  ((number
    :initarg :number :accessor signal-event-number
    :documentation "A number indicating the 'type' of the signal."))
  (:documentation "An event that's some kind of stupid low level OS signal."))

(defclass io-event (os-event)
  ((handle
    :initarg :handle :accessor io-event-handle
    :documentation "A handle to the thing."))
  (:documentation "There's was some I/O on somthing."))

(defclass input-available-event (io-event) ()
  (:documentation "Input is available."))

(defclass output-possible-event (io-event) ()
  (:documentation "Output might be possible."))

(defclass output-finished-event (io-event) ()
  (:documentation "Output is probably done."))

(defclass io-error-event (io-event) ()
  (:documentation "The thing got some kind of error."))

(defclass network-event (io-event) ()
  (:documentation "Some kind of event on the network."))

(defclass network-connection-available-event (network-event
					      input-available-event) ()
  (:documentation "A connection is available."))

(defclass os-process-event (os-event)
  ((handle
    :initarg :handle :accessor os-process-event-handle
    :documentation "The handle of the process."))
  (:documentation "Something happend to a process."))

(defclass child-died-event (os-process-event)
  ((reason
    :initarg :reason :accessor child-died-event-reason
    :documentation "The reason the child died."))
  (:documentation "A child process died."))

(defclass child-stopped-event (os-process-event) ()
  (:documentation "A child process stopped."))

;; @@@ You would think this would be an io-event, but it's not :P
(defclass terminal-event (signal-event) ()
  (:documentation "Something happened to a terminal."))

(defclass terminal-size-change-event (terminal-event)
  ((width
    :initarg :width :accessor terminal-size-change-event-width
    :documentation "New width of the terminal.")
   (height
    :initarg :height :accessor terminal-size-change-event-height
    :documentation "New height of the terminal."))
  (:documentation "Something happened to a terminal."))

(defclass timer-event (os-event)
  ((timer
    :initarg :timer :accessor timer-event-timer
    :documentation "The timer which something happened to."))
  (:documentation "Something happened with a timer."))

(defclass timer-expired-event (timer-event) ()
  (:documentation "Your time is up."))

(defclass timer-triggered-event (timer-event) ()
  (:documentation "It's that time again."))

(defclass system-message-event (os-event)
  ((message
    :initarg :message :accessor system-message-event-message
    :documentation "The message from the system."))
  (:documentation "Some kind of message from the system."))

(defclass tex-boxing-event (os-event)
  ((badness
    :initarg :badness :accessor tex-boxing-event-badness
    :initform 10000 :type integer
    :documentation "It's over 9000!"))
  (:documentation "This will inevitably happen."))

;; @@@ There should probably be some more types like thread, memory, and IPC?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions

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
			 (if (and (slot-boundp c 'code)
				  (slot-value c 'code))
			     (symbol-call :opsys :error-message
					  (opsys-error-code c))
			     ""))
		 (if (and (slot-boundp c 'code)
			  (slot-value c 'code))
		     (format s "~a"
			     (symbol-call :opsys :error-message
					  (opsys-error-code c)))
		     (format s "Somebody probably forgot proper arguments ~
                                when signaling this OPSYS-ERROR.")))))
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
