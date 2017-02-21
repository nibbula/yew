;;
;; magic.lisp - Content sniffer.
;;

(defpackage :magic
  (:documentation "Content sniffer.")
  (:use :cl :dlib :opsys :stretchy)
  (:export
   #:*default-database-type*
   #:use-database
   #:guess-content-type
   #:guess-file-type
   ))
(in-package :magic)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;; These should probably be the same as what's in:
;;;  https://www.iana.org/assignments/media-types/media-types.xhtml
;;; or whatever the latest RFC says.

(defparameter *reasonable-categories*
  #(:application :audio :example :image :message :model :multipart :text
    :video :chemical)
  "Reasonable media categories.")

(defvar *content-types*
  (make-hash-table :test #'equal :size 128)
  "Table of content type name to content-type.")

(defstruct content-type
  name				  ; e.g. "png"
  category			  ; e.g. :image, in *reasonable-categories*
  description			  ; "Portable Network Graphics image"
				  ; or maybe "PNG image data"
  file-name-match		  ; e.g. "\\.png$"
  encoding
  properties
  )

(defstruct type-match
  "Data for trying guess the content type."
  system-type
  head
  tail
  file-name
  matcher)

(defstruct magic-data
  "Data from ‘magic’ format files."
  offset
  type
  test
  message
  apple
  mime
  strength)

(defparameter *fake-types*
  `(("x-directory"        :application "directory")
    ("x-symlink"          :application "symbolic link")
    ("x-device"           :application "device")
    ("x-character-device" :application "character special")
    ("x-block-device"     :application "block special")
    ("x-socket"           :application "socket")
    ("x-fifo"             :application "fifo")
    ("octet-stream"       :application "data")
    ))

(defun content-types ()
  (or *content-types*
      (progn
	(loop :for (name category desc) :in *fake-types* :do
	   (setf (gethash name *content-types*)
		 (make-content-type
		  :name name
		  :category category
		  :description desc
		  :file-name-match nil)))
	*content-types*)))

(defun get-content-type (name)
  "Get the CONTENT-TYPE record for NAME, or nil."
  (gethash name (content-types)))

;; (defun find-content-type (name)
;;   ""

(defstruct easy-tree
  )

;;;;;;;;;;;;;;

(defclass content ()
  ()
  (:documentation "The thing we are trying to guess about."))

(defgeneric seek-to (content position)
  (:documentation "Set to offset in octets in CONTENT to POSITION."))

(defgeneric finish (content)
  (:documentation "Finish using the CONTENT.")
  (:method ((content content)) #| Do nothing, but don't complain. |#))

;;;;;;;;;;;;;;

(defclass content-file (content)
  ((name
    :initarg :name :accessor content-file-name :type string
    :documentation "The name of the file.")
   (stream
    :accessor content-file-stream :initform nil :type stream
    :documentation "The stream to analyze."))
  (:documentation "Content which is in a seekable file."))

(defmethod initialize-instance
    :after ((o content-file) &rest initargs &key &allow-other-keys)
  "Initialize a content-file."
  (declare (ignore initargs))
  (when (slot-boundp o 'name)
    (error "Must have a file name."))
  (setf (slot-value o 'stream) (open (slot-value o 'name))))

(defmethod finish ((content content-file))
  (close (slot-value content 'stream)))

(defmethod seek-to ((content content-file) position)
  (file-position (content-file-stream content) position))

(defclass content-buffer (content)
  ((buffer
    :initarg :buffer :accessor content-buffer-buffer  :type vector
    :documentation "Stuff."))
  (:documentation "Content which is fully contained in main memory."))

(defclass content-stream (content)
  ((stream
    :initarg :stream :accessor content-stream-stream  :type stream
    :documentation "Stream to read from.")
   (buffer
    :initarg :buffer :accessor content-stream-buffer :type vector
    :documentation
    "Buffer to cache content so we can seek on non-seekable stream."))
  (:documentation "Content which is read from a non-seekable stream."))

(defmethod initialize-instance
    :after ((o content-stream) &rest initargs &key &allow-other-keys)
  "Initialize a content-stream."
  (declare (ignore initargs))
  (when (slot-boundp o 'stream)
    (error "Must supply a stream."))
  (setf (slot-value o 'buffer)
	(make-stretchy-vector
	 128 :element-type (stream-element-type (slot-value o 'stream)))))

  ;; (when (and (file-position stream)
  ;; 	     (file-position stream (file-position stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun guess-buffer-content (buffer)
  (declare (ignore buffer))
  ;; @@@ traverse nodes of the easy tree
  ;; stop if we got to a match, and return it
  
  )

(defun guess-stream-content (stream)
  (let (content-stream)
    (unwind-protect
       (progn
	 (setf content-stream (make-instance 'content-stream :stream stream)))
      (when content-stream
	(finish content-stream)))))

(defun guess-file-content (filename)
  (let (content-file)
    (unwind-protect
       (progn
	 (setf content-file (make-instance 'content-file :name filename))
	 #|
	 (let ()
	   (loop :with b
	      :while (setf b (read-byte stm nil nil))
	      :do (stretchy-append s b)
	      ;; @@@ traverse nodes of the easy tree
	      ;; stop if we got to a match, and return it
	      ))
	 |#
	 )
      (when content-file
	(finish content-file)))))

(defun guess-content (thing type)
  (ecase type
    (:buffer (guess-buffer-content thing))
    (:file (guess-file-content thing))))

(defun libmagic-guess-content (thing type)
  (let* ((mime (not-so-funcall :libmagic :guess-content thing type :mime))
	 (m (split-sequence #\/ mime))
	 (name (or (second m) mime))
	 (category (first m)))
    (make-content-type
     :name name
     :category category
     :description
     (not-so-funcall :libmagic :guess-content thing type :content)
     :encoding
     (not-so-funcall :libmagic :guess-content thing type :encoding))))

(defvar *guess-func* nil
  "Function to guess content.")

(defvar *default-database-type* :libmagic
  "Function to guess content.")

(defun ensure-database ()
  (or *guess-func* (use-database *default-database-type*))) 
  
(defun use-database (type)
  (ccase type
    (:internal (setf *guess-func* #'guess-content))
    (:libmagic
     (when (not (find-package :libmagic))
       (asdf:load-system :libmagic))
     (setf *guess-func* #'libmagic-guess-content))))

(defgeneric guess-content-type (thing)
  (:documentation "Guess the content type of something."))

(defmethod guess-content-type ((thing string))
  "Guess the content type of a string."
  (ensure-database)
  (funcall *guess-func* thing :buffer))

;; (defmethod guess-content-type ((thing stream))
;;   "Guess the content type of a string."
;;   (funcall *guess-func*
;; 	   (quote-filename (safe-namestring pathname)) :buffer))
;;   )

(defun guess-file-type (file-name)
  "Guess the content of the file FILENAME."
  (ensure-database)
  (let* ((filename (safe-namestring file-name))
	 (info (get-file-info filename)))
    (case (file-info-type info)
      (:directory (get-content-type "x-directory"))
      (:link      (get-content-type "x-symlink"))
      (:device
       #+unix (if (os-unix:is-character-device
		   (os-unix:file-status-mode (os-unix:stat filename)))
		  (get-content-type "x-character-device")
		  (get-content-type "x-block-device"))
       #-unix (get-content-type "x-device"))
      (:regular (funcall *guess-func* (quote-filename filename) :file))
      (t #| :other |#
       ;; @@@ should get more specific on unix
       (get-content-type "octet-stream")))))

(defmethod guess-content-type ((thing pathname))
  "Guess the content type of a pathname."
  (guess-file-type thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

android apple archive audio database elf fonts flash gcc gimp gnome gnu
gnumeric icc images javascript jpeg kde linux lisp luks mach macintosh macos
maple mathematica matroska mercurial metastore mips mozilla msdos msvc music
netbsd netware ocaml pbm pdf perl pgp pkgadd project pwsafe python qt riff
ruby sc scientific selinux sgi sketch sql ssh sun tcl terminfo tex timezone
tgif troff unicode vax vms vmware vorbis windows wireless wordprocessors wsdl
xenix xilinx xwindows zfs zyxel

|#

;; EOF
