;;;
;;; terminal-null.lisp - A terminal that doesn't do anything.
;;;

(defpackage :terminal-null
  (:documentation "A terminal that doesn't do anything.

This is useful for when you want terminal output silenced and suppressed.
Consider it the /dev/null of terminals.

For a practical example, see
terminal-crunch:with-terminal-output-to-fat-string.")
  (:use :cl :dlib :terminal :dgray)
  (:export
   #:terminal-null
   ))
(in-package :terminal-null)

(defclass terminal-null (terminal)
  ()
  (:default-initargs
   :output-stream nil
   :window-rows nil
   :window-columns nil
   :file-descriptor nil
   :device-name nil)
  (:documentation "A terminal that doesn't do anything."))

;; We mostly rely on the default methods.

(defmethod terminal-default-device-name ((type (eql 'terminal-null)))
  (declare (ignore type)))

(defmethod terminal-start ((tty terminal-null))
  (declare (ignore tty)))

(defmethod terminal-end ((tty terminal-null) &optional state)
  (declare (ignore tty state)))

(defmethod terminal-done ((tty terminal-null) &optional state)
  (declare (ignore tty state)))

(defmethod terminal-reinitialize ((tty terminal-null))
  (declare (ignore tty)))

(defmethod terminal-device-time ((tty terminal-null))
  (declare (ignore tty)))

(defmethod terminal-get-size ((tty terminal-null))
  (values nil nil))

(defmethod terminal-get-cursor-position ((tty terminal-null))
  (values 0 0))

(defmethod terminal-enable-event ((tty terminal-null) event)
  (declare (ignore tty event)))

(defmethod terminal-disable-event ((tty terminal-null) event)
  (declare (ignore tty event)))

(defmethod (setf terminal-input-mode) (mode (tty terminal-null))
  (declare (ignore mode tty)))

(defmethod (setf terminal-window-foreground) (color (tty terminal-null))
  (declare (ignore color tty)))

(defmethod (setf terminal-window-background) (color (tty terminal-null))
  (declare (ignore color tty)))

(defmethod (setf terminal-title) (title (tty terminal-null))
  (declare (ignore title tty)))

(defmethod (setf terminal-selection) (selection (tty terminal-null) &key type)
  (declare (ignore selection tty type)))

(defmethod terminal-has-attribute ((tty terminal-null) attribute)
  (declare (ignore tty attribute))
  t)

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Stream methods.
 ╰|#

(defmethod-quiet close ((stream terminal-null) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-finish-output ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-force-output ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-write-sequence ((stream terminal-null) seq start end
				  &key &allow-other-keys)
  (declare (ignore stream seq start end)))

;; character output stream methods

(defmethod stream-line-column ((stream terminal-null))
  (declare (ignore stream))
  0)

(defmethod stream-start-line-p ((stream terminal-null))
  (declare (ignore stream))
  nil)

(defmethod stream-advance-to-column ((stream terminal-null) column)
  (declare (ignore stream column))
  t)

(defmethod stream-write-char ((stream terminal-null) char
			     #| &optional start end |#)
  (declare (ignore stream char)))

(defmethod stream-write-string ((stream terminal-null) string
			       &optional start end)
  (declare (ignore stream string start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-null, which is also an input stream.

(defmethod stream-clear-input ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-read-sequence ((stream terminal-null) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore seq start end)))

(defmethod stream-read-char-no-hang ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-read-char ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-read-line ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-listen ((stream terminal-null))
  (declare (ignore stream)))

(defmethod stream-unread-char ((stream terminal-null) character)
  (declare (ignore stream character)))

#|──────────────────────────────────────────────────────────────────────────|#

(register-terminal-type :null 'terminal-null)

;; End
