;;;
;;; snip.lisp - Snip the output
;;;

(defpackage :snip
  (:documentation "Snip the output.")
  (:use :cl :dlib :opsys :stretchy :lish :utf8b-stream)
  (:export
   #:!snip
   #:snip-bytes
   #:take-lines
   #:!head
   ))
(in-package :snip)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ This performance of this is horrible!
(defun snip-bytes (file-or-stream count direction &key collect quiet)
  (declare (type integer count))
  (when (not (or (eq direction :before) (eq direction :after)))
    (error "Direction must be :BEFORE or :AFTER."))
  (let ((result nil) (n 0))
    (declare (type integer n))
    (flet ((put-byte (b)
	     (unless quiet
	       (write-byte b *standard-output*))
	     (when collect
	       (setf (aref result n) b))))
      (with-open-file-or-stream (stream file-or-stream
					:direction :input
					:element-type '(unsigned-byte 8))
	(when collect
	  (setf result (make-array count :element-type '(unsigned-byte 8))))
	(case direction
	  (:before
	   (loop :with b
		 :while (and (< n count)
			     (setf b (read-byte stream nil nil)))
		 :do
		    (put-byte b)
		    (incf n)))
	  (:after
	   (let ((seekable (file-position stream 0)))
	     (if seekable
		 (progn
		   (when (not (file-position stream count))
		     (error "Can't position stream to ~a." count))
		   (loop :with b
			 :while (setf b (read-byte stream nil nil))
			 :do (put-byte b) (incf n)))
		 (progn
		   ;; Read the non-output part
		   (loop :while (and (< n count)
				     (read-byte stream nil nil))
			 :do (incf n))
		   (setf n 0)
		   ;; Read & output the rest
		   (loop :with b
			 :while (setf b (read-byte stream nil nil))
			 :do (put-byte b) (incf n)))))))
	(finish-output *standard-output*)))
    result))

(defcommand snip
  ((pattern	string #| regexp |# :optional nil
;;    :help "A regular expression to search for or an integer byte offset.")
    :help "A regular expression to search for.")
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
  "Write the first ‘count’ lines of ‘stream’ to *standard-output*."
  (let ((n 0))
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (write-line line)
	    (incf n))
	  (return)))))

(defun snip-and-collect (stream count)
  "Write the first ‘count’ lines of ‘stream’ to *standard-output*, and return
them."
  (let ((n 0) result)
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (write-line line)
	    (push line result)
	    (incf n))
	  (return)))
    (nreverse result)))

(defun take-lines (stream count)
  "Return the first ‘count’ lines of ‘stream’."
  (let ((n 0) result)
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (push line result)
	    (incf n))
	  (return)))
    (nreverse result)))

(defcommand head
  ((line-count integer :short-arg #\n :default 10 :help "Lines to show.")
   (byte-count integer :short-arg #\c :help "Bytes to show.")
   (collect boolean :short-arg #\C :default (accepts 'sequence)
    :help "Return a sequence.")
   (quiet boolean :short-arg #\q :help "Don't produce output.")
   (use-encoding boolean :short-arg #\e
    :help "Use the default system encoding.") ; otherwise known as: get errors
   ;; ("count" integer :default 10
   ;;  :help "The number of units to show.")
   (files pathname :repeating t
    :help "Files to use as input."))
  "Output the first portion of input."
  (flet ((do-snip (stream)
	   (if collect
	       (setf *output* (if quiet
				  (take-lines stream line-count)
				  (snip-and-collect stream line-count)))
	       (snip-lines-after stream line-count)))
	 (do-byte-snip (stream)
	   (if collect
	       (setf *output*
		     (snip-bytes stream byte-count :before :collect collect
				 :quiet quiet))
	       (snip-bytes stream byte-count :before :quiet quiet))))
    (if byte-count
	(if files
	    (loop :for f :in files :do
	      (do-byte-snip f))
	    (do-byte-snip *standard-input*))
	(if files
	    (loop :for f :in files :do
	      (if use-encoding
		  (do-snip f)
		  (with-utf8b-input (str f :errorp nil)
		    (do-snip str))))
	    (if use-encoding
		(do-snip *standard-input*)
		(with-utf8b-input (str *standard-input* :errorp nil)
		  (do-snip str)))))))

;; EOF
