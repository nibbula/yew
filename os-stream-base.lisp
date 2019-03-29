;;
;; os-stream-base.lisp - OS stream stuff that has to go in the base package.
;;

;; Things in here are used by the system specific os-stream implementation,
;; so must be defined before the system specific code is loaded. This means
;; mostly classes, and generic functions, and parameters, constants. The
;; code implementing the methods are in os-stream.lisp in the opsys package
;; to give them the opportunity to use anything in the full opsys.

(in-package :opsys-base)

(defconstant +buffer-size+ #.(* 8 1024) "Buffer sizes in bytes.")
(defconstant +input-buffer-size+  +buffer-size+ "Input buffer size in bytes.")
(defconstant +output-buffer-size+ +buffer-size+ "Output buffer size in bytes.")

(defclass os-stream (fundamental-stream)
  ((handle
   :initarg :handle :accessor os-stream-handle
   :documentation "Handle to the operating system stream.")
   ;; Isn't this in fundamental-streams?
   ;; (element-type
   ;;  :initarg :element-type :accessor os-stream-element-type
   ;;  :documentation "The type of data we operate on.")
   )
  (:documentation
   "A stream that provides facility for using it with lower level operating
system functions."))

(defgeneric os-stream-open (stream filename if-exists if-does-not-exist share)
  (:documentation "Open an os-stream for the FILENAME."))

(defgeneric fill-buffer (os-stream)
  (:documentation "Read into the input buffer. Return NIL on EOF."))

(defgeneric flush-buffer (os-stream &key force)
  (:documentation "Write the input buffer."))

(defclass os-input-stream (os-stream fundamental-input-stream)
  ((input-buffer
    :initarg :input-buffer :accessor os-stream-input-buffer
    :initform (cffi:make-shareable-byte-vector +input-buffer-size+)
    :type `(simple-array (unsigned-byte 8) ,+input-buffer-size+)
    :documentation "Store characters that have been read but not consumed.")
   (position
    :initarg :position :accessor os-input-stream-position
    :initform 0 :type fixnum
    :documentation "Read position in the input buffer.")
   (input-fill
    :initarg :input-fill :accessor os-input-stream-input-fill
    :initform 0 :type fixnum
    :documentation "Postion which in input buffer is filled to.")
   (unread-char
    :initarg :unread-char :accessor os-input-stream-unread-char
    :initform nil :type (or null character)
    :documentation "A character for the unusual unread.")
   (got-eof
    :initarg :got-eof :accessor os-input-stream-got-eof
    :initform nil :type boolean
    :documentation "True if we got an End Of File."))
  (:documentation
   "An os-stream that does input."))

(defclass os-output-stream (os-stream fundamental-output-stream)
  ((output-buffer
    :initarg :output-buffer :accessor os-stream-output-buffer
    :initform (cffi:make-shareable-byte-vector +output-buffer-size+)
    :type `(simple-array (unsigned-byte 8) ,+output-buffer-size+)
    :documentation "Store characters that have been written but not flushed.")
   (output-position
    :initarg :output-position :accessor os-output-stream-output-position
    :initform 0 :type fixnum
    :documentation "The end of the last written data in the buffer.")
   (output-fill
    :initarg :output-fill :accessor os-output-stream-output-fill
    :initform 0 :type fixnum
    :documentation "Position which the output buffer is filled to."))
  (:documentation
   "An os-stream that does output."))

(defclass os-io-stream (os-input-stream os-output-stream)
  ()
  (:documentation
   "A stream that provides facility for using it with lower level operating
system functions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary streams

(defclass os-binary-stream (os-stream fundamental-binary-stream)
  (
   ;; (element-type
   ;;  :initarg :element-type :accessor os-binary-stream-element-type
   ;;  :documentation "Supports UNSIGNED-BYTEs that are a multiple of 8.")
   )
  (:default-initargs
   :element-type '(unsigned-byte 8))
  (:documentation
   "An os-stream with an element type of byte."))

(defclass os-binary-input-stream (os-binary-stream os-input-stream
				  fundamental-binary-input-stream)
  ()
  (:documentation
   "An os-stream that does input of bytes."))

(defclass os-binary-output-stream (os-binary-stream os-output-stream
				   fundamental-binary-output-stream)
  ()
  (:documentation
   "An os-stream that does output of bytes."))

(defclass os-binary-io-stream (os-binary-input-stream
			       os-binary-output-stream)
  ()
  (:documentation
   "An os-stream that does input and output of bytes."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character streams

(defclass os-character-stream (os-stream fundamental-character-stream)
  ((encoding
    :initarg :encoding :accessor os-character-stream-encoding
    :documentation "Character encoding for the stream."))
  (:documentation
   "An os-stream with an element type of character and supports encoding."))

(defclass os-character-input-stream (os-character-stream
				     fundamental-character-input-stream)
  ()
  (:documentation
   "An os-stream that does input of characters."))

(defclass os-character-output-stream (os-character-stream
				      fundamental-character-output-stream)
  ((column
    :initarg :column :accessor os-character-output-stream-column
    :initform 0 :type integer
    :documentation "Output column."))
  (:documentation
   "An os-stream that does output of characters."))

(defclass os-character-io-stream (os-character-input-stram
				  os-character-output-stram)
  ()
  (:documentation
   "An os-stream that does input and output of characters."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous

(defgeneric notice-changes (stream) ;; @@@ Or maybe re-sync?
  (:documentation
   "This notifies the stream machinery that changes at the O/S level may have
happend since it's last operations. This may have to be done to allow the
upper level of the stream to resume working properly."))

;; EOF
