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

(defun tail-forever (stream interval)
  (if (interactive-stream-p stream)
      (loop :with line
	 :do
	 (if (listen stream)
	     (progn
	       (setf line (read-line stream nil nil))
	       (when line
		 (write-line line *standard-output*)))
	     (progn
	       (sleep interval)
	       (setf line t)))
	 :while line))
      (loop :with line
	 :do
	 (if (setf line (read-line stream nil nil))
	     (write-line line *standard-output*)
	     (progn
	       (sleep interval)))))

(defun really-seekable (stream)
  "Return true if STREAM is really seekable."
  (and (file-position stream 0)
       (handler-case
	   (file-length stream)
	 (type-error (c)		; according to the standard
	   (declare (ignore c))
	   nil))))

(defun tail-lines (file-or-stream count &key forever)
  "Output the last COUNT lines of FILE-OR-STREAM. If FOREVER is true, use it as
the time to sleep between checking for output."
  (cond
    ((zerop count) (return-from tail-lines nil))
    ((minusp count)
     ;; Assume a negtaive count is actually positive.
     (setf count (- count))))
  (with-open-file-or-stream (stream file-or-stream)
    (let* ((seekable (really-seekable stream))
	   (buf-len (nos:memory-page-size))
	   read-pos
	   (lines (make-array count))
	   (i 0)) ;; line count & add position in lines
      (labels ((add-line (l)
		 "Add L to the ring buffer LINES."
		 (setf (aref lines (mod i count)) l)
		 (incf i))
	       (read-lines ()
		 "Just read everything until EOF"
		 (loop :with l
		    :while (setf l (read-line stream nil))
		    :do (add-line l))))
	;; Don't bother on non-seekable interactive streams.
	(when (or seekable (not (interactive-stream-p stream)))
	  ;; Read until EOF.
	  (if seekable
	      ;; Keep backing up until we read enough lines
	      (loop :with inc = 1
		 :do
		 (setf read-pos (max 0 (- (file-length stream)
					  (* buf-len inc))))
		 ;; (format t "read-pos = ~s~%" read-pos)
		 (file-position stream read-pos)
		 (setf i 0)
		 (read-lines)
		 (setf inc (* inc 2))
		 :while (and (> read-pos 0) (< i count)))
	      ;; Non-seekable, so just read all lines.
	      (read-lines))
	  ;; Output lines from the ring buffer.
	  (loop :with limit = (min count i)
	     :with j = (mod (- i limit) count)
	     :and output = 0
	     :while (< output limit)
	     :do
	     (write-line (aref lines (mod j count)))
	     (incf output)
	     (incf j))))
      (finish-output)
      (when forever
	(tail-forever stream forever))
      #| (finish-output *standard-output*) |#
      )))

;; @@@ I want to be able to support args like "-12" meaning "-n 12".
;; I could hack it in to the arg processing here, like with :keys-as, but I want
;; the automatic arg parsing to be able to support it.

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
   (sleep-interval number :short-arg #\s :default 1
    :help "Interval in seconds to sleep between checking for output when forever is true.")
   ;; (list boolean :short-arg #\l :help "True to return lines as a list.")
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
	    (let ((seekable (really-seekable stream)))
	      (when (not seekable)
		(error "I didn't implement non-seekable streams yet."))
	      (file-position stream (file-length stream))
	      (tail-forever stream sleep-interval)))))
      (if files
	  (loop :with i = 0 :and len = (length files)
	     :for f :in files :do
	     (tail-lines f line-count
			 :forever (and forever (= i (1- len)) sleep-interval))
	     (incf i))
	  (tail-lines *standard-input* line-count
		      :forever (and forever sleep-interval)))))

;; EOF
