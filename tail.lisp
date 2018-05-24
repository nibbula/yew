;;
;; tail.lisp - The back part of the animal.
;;

(defpackage :tail
  (:documentation "The back part of the animal.")
  (:use :cl :dlib :stretchy :snip)
  (:export
   #:tail-lines
   #:!tail
   ))
(in-package :tail)

(defun tail-forever (stream)
  (if (interactive-stream-p stream)
      (loop :with line
	 :do
	 (if (listen stream)
	     (progn
	       (setf line (read-line stream nil nil))
	       (when line
		 (write-line line *standard-output*)))
	     (progn
	       (sleep 1)
	       (setf line t)))
	 :while line))
      (loop :with line
	 :do
	 (if (setf line (read-line stream nil nil))
	     (write-line line *standard-output*)
	     (progn
	       (sleep 1)))))

(defun tail-lines (file-or-stream count forever)
  "Output the last COUNT lines of FILE-OR-STREAM."
  (with-open-file-or-stream (stream file-or-stream)
    (let* ((seekable (file-position stream 0))
	   (buf-len (nos:memory-page-size))
	   (buf (make-string buf-len))
	   (line-buf (make-stretchy-string 80))
	   lines
	   (line-count 0)
	   read-pos
	   look-pos
	   end)
      (if seekable
	  (progn
	    (setf end (file-length stream)
		  read-pos (max (- end buf-len) 0))
	    (loop :while (and (file-position stream read-pos)
			      (>= read-pos 0))
	       :do
	       (read-sequence buf stream)
	       (setf look-pos (1- buf-len))
	       (loop :while (and (>= look-pos 0)
				 (<= line-count count))
		  :do
		  (if (char= (aref buf look-pos) #\newline)
		      (progn
			(when (not (zerop line-count))
			  (push (reverse line-buf) lines))
			(stretchy-truncate line-buf)
			(incf line-count))
		      (stretchy-append line-buf (aref buf look-pos)))
		  (decf look-pos))
	       (when (>= line-count count)
		 (return))
	       (decf read-pos buf-len)
	       (when (< read-pos 0)
		 (setf read-pos 0)))
	    (mapcar #'write-line lines))
	  ;; Non-seekable streams
	  (when (not (interactive-stream-p stream))
	    (progn
	      (error "I didn't do non-seekable streams yet."))))
      (when forever
	(tail-forever stream))
      #| (finish-output *standard-output*) |#
      )))

#+lish
(lish:defcommand tail
  ((line-count integer :short-arg #\n :default 10
    :help "Lines to show.")
   (byte-count integer :short-arg #\c
    :help "Bytes to show.")
   ;; ("count" integer :default 10
   ;;  :help "The number of units to show.")
   (forever boolean :short-arg #\f
    :help "True to keep displaying lines added to the end of the file.")
   (files pathname :repeating t
    :help "Files to use as input."))
  "Output the last portion of input."
  (if byte-count
      (progn
	(if files
	    (loop :for f :in files :do
	       (snip-bytes f byte-count :before))
	    (snip-bytes *standard-input* byte-count :before))
	(when forever
	  (with-open-file-or-stream (stream (car (last files)))
	    (let ((seekable (file-position stream 0)))
	      (when (not seekable)
		(error "I didn't do non-seekable streams yet."))
	      (file-position stream (file-length stream))
	      (tail-forever stream)))))
      (if files
	  (loop :with i = 0 :and len = (length files)
	     :for f :in files :do
	     (tail-lines f line-count (and forever (= i (1- len))))
	     (incf i))
	  (tail-lines *standard-input* line-count forever))))

;; EOF
