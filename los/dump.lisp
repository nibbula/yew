;;;
;;; dump.lisp - dump bytes
;;;

(defpackage :dump
  (:documentation "Dump bytes from byte dump.")
  (:use :cl :grout :opsys :terminal)
  (:export
   #:dump
   ))
(in-package :dump)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun dump (source &key line-length show-offset start end)
  "Hex dump of a file.
  SOURCE      - a stream or a pathname to input from. If SOURCE is NIL,
                use *STANDARD-INPUT*.
  SHOW-OFFSET - True to show the offset in bytes at the start of the line.
  START       - Byte offset to start from.
  END         - Byte offset to stop at."
  (with-grout ()
    (when (not line-length)
      (setf line-length (grout-width)))
    (let (stream
	  (pos 0)
	  (hex-len (- (truncate (* 3/4 line-length)) 4))
	  ;; (asc-len (truncate (* 1/4 line-length)))
	  (printable (make-string-output-stream))
	  (len 0))
      (unwind-protect
	(block main
	  (cond
	    ((not source)
	     (setf stream *standard-input*))
	    ((or (stringp source) (pathnamep source))
	     ;; (format t "Hi> ~s ~s~%" source (quote-filename source))
	     (setf stream (open (quote-filename source)
				:direction :input
				:element-type '(unsigned-byte 8))))
	    ((not (streamp source))
	     (error "source must be a stream or a file name."))
	    (t
	     (setf stream source)))
	  (when (and start (numberp start) (not (zerop start)))
	    (when (not (file-position stream start))
	      (cerror "Just keep going anyway."
		      "Couldn't set file position to start ~s." start))
	    (setf pos start))
	  (when (and end (not (numberp end)))
	    (cerror "Ignore it."
		    "END is not a number, but a ~a = ~a." (type-of end) end)
	    (setf end nil))
	  (loop :with b
	     :and offset-str
	     :while (setf b (read-byte stream nil nil))
	     :do
	     ;; Possibly show the offset
	     (when (and show-offset (= len 0))
	       (grout-set-color :yellow :black)
	       (setf offset-str (format nil "~5,'0d: " pos))
	       (grout-princ offset-str)
	       ;;(format t "~a" offset-str)
	       (incf len (length offset-str)))
	     ;; Show the hex byte
	     (grout-set-color :green :black)
	     (grout-format "~2,'0x " b)
	     (incf len 3)
	     (princ (if (graphic-char-p (code-char b))
			(code-char b)
			".")
		    printable)
	     ;; Show the printable version
	     (when (> len hex-len)
	       (grout-set-color :white :black)
	       ;;(grout-format " ~a~%" (get-output-stream-string printable))
	       (loop :for c :across (get-output-stream-string printable) :do
		  (grout-set-color (if (eql c #\.) :blue :white) :black)
		  (grout-princ c))
	       (grout-princ #\newline)
	       (file-position printable :start)
	       (setf len 0))
	     (incf pos)
	     (when (and end (> pos end))
	       (return-from main)))
	  (when (/= len 0)	    ; i.e. we're not a the beginning of a line
	    (loop :while (<= len (+ hex-len (- 3 (mod hex-len 3))))
	       :do (incf len) (grout-princ #\space))
	    (loop :for c :across (get-output-stream-string printable) :do
	       (grout-set-color (if (eql c #\.) :blue :white) :black)
	       (grout-princ c))
	    (grout-princ #\newline)
	    ;;(grout-format "~a~%" (get-output-stream-string printable))
	    ))
	(when (and stream (not (equal stream *standard-input*)))
	  (close stream))))))

#+lish
(lish:defcommand dump
  ((files	pathname :repeating t   :help "Files to dump.")
   (line-length	integer  :short-arg #\l :help "Length of lines.")
   (show-offset	boolean  :short-arg #\o :help "True to show byte offset.")
   (start	integer  :short-arg #\s :help "Byte offset to start at.")
   (end		integer  :short-arg #\e :help "Byte offset to stop at."))
  "Dump data bytes."
  (when (and (not line-length) *terminal*)
    (setf line-length (terminal-window-columns *terminal*)))
  (if (not files)
      (dump *standard-input*
	    :line-length line-length
	    :show-offset show-offset
	    :start start :end end)
      (loop :for f :in files :do
	 (dump f :line-length line-length
	       :show-offset show-offset
	       :start start :end end))))

;; EOF
