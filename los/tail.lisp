;;;
;;; tail.lisp - The back part of the animal.
;;;

(defpackage :tail
  (:documentation "The back part of the animal.")
  (:use :cl :dlib :stretchy :snip)
  (:export
   #:tail-lines
   #:!tail
   ))
(in-package :tail)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

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
	 :while line)
      (loop :with line
	 :do
	 (if (setf line (read-line stream nil nil))
	     (write-line line *standard-output*)
	     (progn
	       (sleep interval))))))

(defun really-seekable (stream)
  "Return true if STREAM is really seekable."
  (and (file-position stream 0)
       (handler-case
	   (file-length stream)
	 (type-error (c)		; according to the standard
	   (declare (ignore c))
	   nil))))

(defun discard-lines (file-or-stream count &key forever)
  "Copy from `file-or-stream' to `*standard-output*' after discarding `count'
lines. If `forever' is true, keep displaying lines added to the end."
  (let ((discard-count (- count))
	line (i 0))
    (with-open-file-or-stream (stream file-or-stream)
      (loop :while (and (< i discard-count)
			(setf line (read-line stream nil)))
	   :do (incf i))
      (copy-stream stream *standard-output*)
      (finish-output)
      (when forever
	(tail-forever stream forever)))))

(defparameter *plus-p* nil
  "A stupid hack to support +1 without polluting the arguments of tail-lines.")

(defun tail-lines (file-or-stream count &key forever)
  "Output the last COUNT lines of FILE-OR-STREAM. If FOREVER is true, use it as
the time to sleep between checking for output."
  (cond
    ((or (minusp count) (and (zerop count) *plus-p*))
     (discard-lines file-or-stream count :forever forever)
     (return-from tail-lines nil))
    ((zerop count) (return-from tail-lines nil)))
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

;; EOF
