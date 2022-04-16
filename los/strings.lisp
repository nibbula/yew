;;;
;;; strings.lisp - Try to extract human readable strings from data.
;;;

(defpackage :strings
  (:documentation "Try to extract human readable strings from data.")
  (:use :cl :dlib :stretchy)
  (:export
   #:strings-file
   #:!strings
   ))
(in-package :strings)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun ascii-graphic-char-p (c)
  "ASCII only version."
  (let ((cc (char-code c)))
    (and (>= cc 32) (< cc 126))))

;; I'm not really sure how good this is yet.
(defun strings-file-16 (file &key (minimum-string-length 4) ascii-only)
  (with-open-file-or-stream (stream file :direction :input
				    :element-type '(unsigned-byte 8))
    (let (#| (s (make-stretchy-vector 100 :element-type '(unsigned-byte 8))) |#
	  (us (make-stretchy-vector 100 :element-type '(unsigned-byte 16)))
	  (gcp (if ascii-only #'ascii-graphic-char-p #'graphic-char-p))
	  c cc c1 c2 (last-was-good t) (count 0))
      (flet ((read-16-char ()
	       (setf c nil cc nil
		     c1 (if last-was-good
			    (read-byte stream nil)
			    c2)
		     last-was-good nil)
	       (when c1
		 (setf c2 (read-byte stream nil))
		 (when c2
		   ;; (setf c (logior c2 (ash c1 8)))
		   (setf c (logior (ash c1 8) c2))
		   (when (< c char-code-limit)
		     (setf cc (code-char c))
		     (when (or (funcall gcp cc)
			       (char= cc #\newline))
		       (setf last-was-good t)
		       (stretchy-append us c)))))
	       ;; (format t "c1 = ~s c2 = ~s~%" c1 c2)
	       (and c1 c2)))
	(loop
	   :while (read-16-char)
	   :do
	   (when (not last-was-good)
	     (when (>= (length us) minimum-string-length)
	       (map nil (_ (princ (code-char _))) us)
	       (terpri))
	     (stretchy-truncate us))
	   (incf count))
	;; print out anything remaining
	(when (>= (length us) minimum-string-length)
	  (map nil (_ (princ (code-char _))) us))
	(fresh-line))
      ;; (format t "count = ~s~%" count)
      )))

(defun strings-file (file &key (minimum-string-length 4) ascii-only)
  (with-open-file-or-stream (stream file :direction :input
				    :element-type '(unsigned-byte 8))
    (let ((s (make-stretchy-vector 100 :element-type '(unsigned-byte 8)))
	  (gcp (if ascii-only #'ascii-graphic-char-p #'graphic-char-p))
	  c cc)
      (loop
	 ;;:and last-was-good
	 :while (setf c (read-byte stream nil))
	 :do
	 (setf cc (code-char c))
	 (cond
	   ((or (funcall gcp cc)
		(char= cc #\newline))
	    (stretchy-append s c)
	    ;;(format t "c = ~a~%" cc)
	    ;;(setf last-was-good t)
	    )
	   (t
	    (when (>= (length s) minimum-string-length)
	      (map nil (_ (princ (code-char _))) s)
	      (terpri))
	    ;;(format t "prounc ~a ~a~%" c (length s))
	    ;;(setf last-was-good nil)
	    (stretchy-truncate s))))
      (when (>= (length s) minimum-string-length)
	(map nil (_ (princ (code-char _))) s)
	;;(format t "prazoo ~a ~a~%" c (length s))
	)
      (fresh-line))))

#+lish
(lish:defcommand strings
  ((show-all boolean
    :short-arg	#\a
    :prompt	"Show All? "
    :help	"This option causes strings to look for strings in all sections
 of the object file (including the text section).")
   (show-offset	boolean
    :short-arg	#\o
    :prompt	"Show Offset"
    :help	"Preceded each string by its offset in the file (in decimal).")
   (offset-format choice
    :short-arg	#\t
    :default	:decimal
    :prompt	"Offset format"
    :help	"Base for byte offset."
    :choices	'("octal" "decimal" "hex"))
   (minimum-string-length integer
    :short-arg	#\n
    :default	4
    :help	"The minimum string length.")
   (utf-16 boolean
    :short-arg  #\u
    :help       "True to pretend strings might be in UTF-16.")
   (ascii-only boolean
    :short-arg  #\A
    :help       "Look for ASCII strings only.")
   (files	pathname
    :default	"-"
    :repeating	t
    :help	"An input file name."))
  "Try to extract human readable strings from data."
  (declare (ignore show-all show-offset offset-format))
  (loop :for f :in (or files (list *standard-input*))
     :do
     (if utf-16
       (strings-file-16 f :minimum-string-length minimum-string-length
			  :ascii-only ascii-only)
       (strings-file f :minimum-string-length minimum-string-length
		       :ascii-only ascii-only))))

;; EOF
