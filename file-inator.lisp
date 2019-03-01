;;
;; file-inator.lisp - Inators that use files.
;;

(defpackage :file-inator
  (:documentation "Inators that use files.")
  (:use :cl :dlib :inator :keymap :char-util)
  (:export
   ;; Classes
   #:file-inator
   #:file-inator-file-name
   ;; Generic functions
   #:next-file
   #:previous-file
   #:save-file
   #:save-as-file
   #:revert-file
   #:open-file
   ;; Macros
   #:with-file-list
   ;; Keymaps
   #:*default-file-inator-keymap*
   #:*default-file-inator-escape-keymap*
   #:*default-file-inator-ctrl-x-keymap*
   ))
(in-package :file-inator)

(defclass file-inator (inator)
  ((file-name
    :initarg :file-name :accessor file-inator-file-name :initform nil
    :documentation "The name of the current file being edited."))
  (:documentation "An inator that uses files."))

;; File operations

(defgeneric next-file (file-inator)
  (:documentation "Go to the next file."))
(defgeneric previous-file (file-inator)
  (:documentation "Go to the previous file."))
(defgeneric save-file (file-inator)
  (:documentation "Save the current file."))
(defgeneric save-as-file (file-inator)
  (:documentation "Save the current file as a different name."))
(defgeneric revert-file (file-inator)
  (:documentation "Revert the current buffer"))
(defgeneric open-file (file-inator)
  (:documentation "Open a file."))

;; Keymaps are copies of the default inator ones, with our additions.

(defkeymap *default-file-inator-keymap-additions*
  `((,(ctrl #\x) . *default-file-inator-ctrl-x-keymap*)
    (,(meta-char #\n) . next-file)
    (,(meta-char #\p) . previous-file)
    (#\escape    . *default-file-inator-escape-keymap*)))

(defparameter *default-file-inator-keymap*
  (copy-keymap *default-inator-keymap*))
(add-keymap *default-file-inator-keymap-additions*
	    *default-file-inator-keymap*)

(defparameter *default-file-inator-escape-keymap*
  (copy-keymap *default-inator-escape-keymap*))

(defparameter *default-file-inator-escape-keymap-additions*
  (build-escape-map *default-file-inator-keymap-additions*))
(add-keymap *default-file-inator-escape-keymap-additions*
	    *default-file-inator-escape-keymap*)

(defkeymap *default-file-inator-ctrl-x-keymap*
  `((,(ctrl #\S)	. save-file)
    (,(ctrl #\W)	. save-as-file)
    (,(ctrl #\R)	. revert-file)
    (,(ctrl #\F)	. open-file)))

;; This doesn't really have to be an inator specific thing, but it is useful
;; for them. The names of the restarts _do_ have to come from somewhere.

(defmacro with-file-list ((var list &key index) &body body)
  "Evaluate the BODY in a return-able loop with next-file and previous-file
restarts set up to move through the LIST, and FILE bind to the current file."
  (with-unique-names (i files len)
    (let ((ii (or index i)))
      `(let* ((,ii 0)
	      (,files (coerce ,list 'vector))
	      (,len (length ,files))
	      ,var)
	 (block nil
	   (loop :while (< ,ii ,len) :do
		(restart-case
		    (progn
		      (setf ,var (aref ,files ,ii))
		      ,@body)
		  (file-inator:next-file ()
		    :report "Go to the next file."
		    (when (< ,ii ,len)
		      (incf ,i)))
		  (file-inator:previous-file ()
		    :report "Go to the previous file."
		    (when (> ,ii 0)
		      (decf ,ii))))))))))

;; EOF
