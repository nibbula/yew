;;;
;;; snip.lisp - Snip the output
;;;

(defpackage :snip
  (:documentation "Snip the output.")
  (:use :cl :dlib :cl-ppcre :opsys :stretchy :lish :utf8b-stream)
  (:export
   #:!snip
   #:snip-bytes
   #:take-lines
   #:!head
   ))
(in-package :snip)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ This performance of this is horrible!
(defun snip-bytes (file-or-stream count direction)
  (declare (type integer count))
  (when (not (or (eq direction :before) (eq direction :after)))
    (error "Direction must be :BEFORE or :AFTER."))
  (with-open-file-or-stream (stream file-or-stream
				    :direction :input
				    :element-type '(unsigned-byte 8))
    (case direction
      (:before
       (loop :with b :and n :of-type integer = 0
	  :while (and (< n count)
		      (setf b (read-byte stream nil nil)))
	  :do (write-byte b *standard-output*)
	  (incf n)))
      (:after
       (let ((seekable (file-position stream 0)))
	 (if seekable
	     (progn
	       (when (not (file-position stream count))
		 (error "Can't position stream to ~a." count))
	       (loop :with b
		  :while (setf b (read-byte stream nil nil))
		  :do (write-byte b *standard-output*)))
	     (progn
	       ;; Read the non-output part
	       (loop :with n :of-type integer = 0
	       	  :while (and (< n count)
	       		      (read-byte stream nil nil))
	       	  :do (incf n))
	       ;; Read & output the rest
	       (loop :with b
		  :while (setf b (read-byte stream nil nil))
		  :do (write-byte b *standard-output*)))))))
    (finish-output *standard-output*)))

(defcommand snip
  ((pattern	string #| regexp |# :optional nil
    :help "A regular expression to search for or an integer byte offset.")
   (source	input-stream-or-filename :default '*standard-input*
    :help "Input file or stream to read from.")
   (before	boolean :short-arg #\b :long-arg "before":default nil
    :help "True to cut from before the pattern.")
   (after	boolean :short-arg #\a :long-arg "after" :default nil
    :help "True to cut from after the pattern."))
  "Snip output before or after a pattern."
  (when (not (or before after))
    (error "You probably should specify either BEFORE or AFTER."))
  (let ((saw-it nil))
    (cond
      (before
       (with-lines (line source)
	 (if saw-it
	     (progn (write-string line) (terpri))
	     (when (ppcre:all-matches pattern line)
	       (setf saw-it t)))))
      (after
       (with-lines (line source)
	 (progn (write-string line) (terpri))
	 (when (ppcre:all-matches pattern line)
	   (finish-output)
	   (return)))))
    (finish-output)))

(defun snip-lines-after (stream count)
  (let ((n 0))
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (write-line line)
	    (incf n))
	  (return)))))

(defun take-lines (stream count)
  (let ((n 0) result)
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (push line result)
	    (incf n))
	  (return)))
    result))

(defcommand head
  ((line-count integer :short-arg #\n :default 10 :help "Lines to show.")
   (byte-count integer :short-arg #\c :help "Bytes to show.")
   (use-encoding boolean :short-arg #\e
    :help "Use the default system encoding.") ; otherwise know as: get errors
   ;; ("count" integer :default 10
   ;;  :help "The number of units to show.")
   (files pathname :repeating t
    :help "Files to use as input."))
  "Output the first portion of input."
  (if byte-count
      (if files
	  (loop :for f :in files :do
	     (snip-bytes f byte-count :before))
	  (snip-bytes *standard-input* byte-count :before))
      (if files
	  (loop :for f :in files :do
	     (if use-encoding
		 (snip-lines-after f line-count)
		 (with-utf8b-input (str f :errorp nil)
		   (snip-lines-after str line-count))))
	  (if use-encoding
	      (snip-lines-after *standard-input* line-count)
	      (with-utf8b-input (str *standard-input* :errorp nil)
		(snip-lines-after str line-count))))))

;; EOF
