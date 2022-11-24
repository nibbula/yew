;;;
;;; dump.lisp - dump bytes
;;;

(defpackage :dump
  (:documentation "Dump bytes from byte dump.")
  (:use :cl :dlib :grout :opsys :terminal)
  (:export
   #:dump
   #:!dump
   ))
(in-package :dump)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defparameter *braille*
  #.(s+ " ⡀⢀⣀⠠⡠⢠⣠⠄⡄⢄⣄⠤⡤⢤⣤"
	"⠁⡁⢁⣁⠡⡡⢡⣡⠅⡅⢅⣅⠥⡥⢥⣥"
	"⠃⡃⢃⣃⠣⡣⢣⣣⠇⡇⢇⣇⠧⡧⢧⣧"
	"⠉⡉⢉⣉⠩⡩⢩⣩⠍⡍⢍⣍⠭⡭⢭⣭"
	"⠊⡊⢊⣊⠪⡪⢪⣪⠎⡎⢎⣎⠮⡮⢮⣮"
	"⠑⡑⢑⣑⠱⡱⢱⣱⠕⡕⢕⣕⠵⡵⢵⣵"
	"⠚⡚⢚⣚⠺⡺⢺⣺⠞⡞⢞⣞⠾⡾⢾⣾"
	"⠛⡛⢛⣛⠻⡻⢻⣻⠟⡟⢟⣟⠿⡿⢿⣿"))

(defun read-buf (stream buf)
  (let ((i 0) (pos 0))
    (loop
      :do
        (setf pos (read-sequence (aref buf i) stream))
        (incf i)
      :while (and (< i 4)
		  (= pos (length (aref buf i)))))
    (values i pos)))

(defun dump-as-braille (stream start end line-length show-offset)
  (let ((braille-len line-length)
	(len 0)
	(pos start)
	(buf (make-sequence 'vector 4))
	(end-x 0) (end-y 0))
    (loop :for i :from 0 :to 3
      :do (setf (aref buf i)
		(make-array braille-len :element-type '(unsigned-byte 8))))
    (loop
      :with b
      :and offset-str
      :while (progn
	       (setf (values end-y end-x) (read-buf stream buf))
	       (= end-y 4))
      :do
      ;; Possibly show the offset
      (when (and show-offset (= len 0))
	(grout-set-color :yellow :black)
	(setf offset-str (format nil "~5,'0d: " pos))
	(grout-princ offset-str)
	;;(format t "~a" offset-str)
	(incf len (length offset-str)))
      ;; Show the braille bits
      (grout-set-color :green :black)
      (loop :with value = 0
        :for x :from 0 :below braille-len :do
	  (loop :for bit :from 6 :downto 0 :by 2 :do
	    (setf value
		  (logior
		   (ldb (byte 2 bit) (aref (aref buf 0) x))
		   (ash (ldb (byte 2 bit) (aref (aref buf 1) x)) -2)
		   (ash (ldb (byte 2 bit) (aref (aref buf 2) x)) -4)
		   (ash (ldb (byte 2 bit) (aref (aref buf 3) x)) -6)))
	    ;; (format t "~c" (char *braille* value))
	    (format t "~b" value)
	    ))
      (incf len)
      ;; Show the printable version
      (when (> len braille-len)
	(grout-princ #\newline)
	(setf len 0))
      (incf pos)
      (when (and end (> pos end))
	(return-from dump-as-braille)))
    (when (/= len 0)	    ; i.e. we're not a the beginning of a line
      ;; @@@@
      #|
      (loop :while (<= len (+ hex-len (- 3 (mod hex-len 3))))
	    :do (incf len) (grout-princ #\space))
      (loop :for c :across (get-output-stream-string printable) :do
	(grout-set-color (if (eql c #\.) :blue :white) :black)
	(grout-princ c))
      |#
      (grout-princ #\newline)
      ;;(grout-format "~a~%" (get-output-stream-string printable))
      )))

(defun dump-as-hex (stream start end line-length show-offset)
  (let ((hex-len (- (truncate (* 3/4 line-length)) 4))
	(printable (make-string-output-stream))
	(len 0)
	(pos start))
    (loop
      :with b
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
	(return-from dump-as-hex)))
    (when (/= len 0)	    ; i.e. we're not a the beginning of a line
      (loop :while (<= len (+ hex-len (- 3 (mod hex-len 3))))
	    :do (incf len) (grout-princ #\space))
      (loop :for c :across (get-output-stream-string printable) :do
	(grout-set-color (if (eql c #\.) :blue :white) :black)
	(grout-princ c))
      (grout-princ #\newline)
      ;;(grout-format "~a~%" (get-output-stream-string printable))
      )))

(defun dump (source &key line-length show-offset start end (style :hex))
  "Hex dump of a file.
  SOURCE      - a stream or a pathname to input from. If SOURCE is NIL,
                use *STANDARD-INPUT*.
  SHOW-OFFSET - True to show the offset in bytes at the start of the line.
  START       - Byte offset to start from.
  END         - Byte offset to stop at.
  STYLE       - One of :hex :braille. Default is :hex."
  (with-grout ()
    (when (not line-length)
      (setf line-length (grout-width)))
    (let (stream
	  (pos 0)
	  ;; (hex-len (- (truncate (* 3/4 line-length)) 4))
	  ;; (asc-len (truncate (* 1/4 line-length)))
	  ;; (printable (make-string-output-stream))
	  ;; (len 0)
	  )
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
	  (case style
	    (:braille  (dump-as-braille stream pos end line-length show-offset))
	    (otherwise (dump-as-hex     stream pos end line-length show-offset)))
	  #|
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
	    )
	  |#
	)
	(when (and stream (not (equal stream *standard-input*)))
	  (close stream))))))

;; EOF
