;;
;; stty.lisp - Terminal driver manipulator.
;;

(defpackage :stty
  (:documentation "Terminal driver modes manipulator.")
  (:use :cl :dlib :opsys #+unix :os-unix #+unix :termios :cffi :dlib-misc
	:char-util)
  (:export
   #:describe-tty
   #:set-tty
   #:!stty
   ))
(in-package :stty)

(defun demuff (s)
  "Remove the earmuffs."
  (trim (string s) #(#\+)))

#+unix
(defun describe-tty (&key (device "/dev/tty") (format :nice))
  "Describe the termios settings for DEVICE. Much like `stty -a`.
FORMAT defaults to :NICE but can also be :STTY."
  (let (tty desc)
    (unwind-protect
      (progn
	(setf tty (posix-open device (logior +O_RDWR+ +O_NONBLOCK+) 0)
	      desc (foreign-alloc '(:struct termios)))
	(error-check tty "Error opening ~a~%" device)
	(error-check (tcgetattr tty desc)
		     "Can't get terminal description for ~a." device)
	(with-foreign-slots ((c_iflag c_oflag c_cflag c_lflag c_cc
			      c_ispeed c_ospeed)
			     desc (:struct termios))
	  (multiple-value-bind (cols rows) (get-window-size tty)
	    (flet ((dump-char (sym c)
		     (cond
		       ((= c #xff) "undef")
		       ((or (eq sym '+VMIN+) (eq sym '+VTIME+)) c)
		       (t (nice-char (code-char c) :caret t))))
		   (s-dump-flags (flags var)
		     (justify-text
		      (join-by-string
		       (loop
			  :for f :in flags
			  :collect
			  (format nil "~a~(~a~)"
				  (if (= 0 (logand var (symbol-value f)))
				      #\- "")
				  (demuff f)))
		       " ")
		      :omit-first-prefix t
		      :cols cols :prefix "        "))
		   (n-dump-flags (flags var)
		     (print-columns
		      (loop
			 :for f :in flags
			 :collect
			 (format nil "~a~(~a~)"
				 (if (= 0 (logand var (symbol-value f))) #\- "")
				 (demuff f)))
		      :columns cols
		      :prefix "  ")))
	      (case format
		((:succinct :stty)
		 (if (= c_ispeed c_ospeed)
		     (format t "speed ~d baud; " c_ospeed)
		     (format t "speed input ~d output ~d baud;"
			     c_ispeed c_ospeed))
		 (format t "~d rows; ~d columns;~%" rows cols)
		 (format t "~&lflags: ") (s-dump-flags *lflags* c_lflag)
		 (format t "~&iflags: ") (s-dump-flags *iflags* c_iflag)
		 (format t "~&oflags: ") (s-dump-flags *oflags* c_oflag)
		 (format t "~&cflags: ") (s-dump-flags *cflags* c_cflag)
		 (format t "~&cchars: ")
		 (justify-text
		  (join-by-string
		   (loop :for c :in termios:*cchars*
		      :collect
		      (format nil "~(~a~) = ~a; " (demuff c)
			      (dump-char
			       c (mem-aref c_cc :unsigned-char
					   (symbol-value c)))))
		   " ")
		  :cols cols :prefix "        " :omit-first-prefix t)
		 (terpri))
		(:nice
		 (print-properties
		  `(,@(if (= c_ispeed c_ospeed)
			  `(("Speed" ,(format nil "~d baud" c_ospeed)))
			  `(("Input Speed"
			     ,(format nil "~d baud" c_ispeed))
			    ("Output Speed"
			     ,(format nil "~d baud" c_ospeed))))
		      ("Rows" ,rows) ("Columns" ,cols))
		  :right-justify t)
		 (format t "Local flags: #x~x~%" c_lflag)
		 (n-dump-flags *lflags* c_lflag)
		 (format t "Input flags: #x~x~%" c_iflag)
		 (n-dump-flags *iflags* c_iflag)
		 (format t "Output flags: #x~x~%" c_oflag)
		 (n-dump-flags *oflags* c_oflag)
		 (format t "Control flags: #x~x~%" c_cflag)
		 (n-dump-flags *cflags* c_cflag)
		 (format t "Control chars: ~d~%" (length *cchars*))
		 (print-columns
		  (loop :with cc
		     :for c :in *cchars*
		     :do (setf cc (mem-aref c_cc :unsigned-char
					    (symbol-value c)))
		     :collect
		     (format nil "~(~a~) = ~a"
			     (subseq (demuff c) 1)
			     (dump-char c cc)))
		  :columns cols
		  :prefix "  ")))))))
      ;; close the terminal
      (when (and tty (>= tty 0))
	(posix-close tty))
      ;; free C memory
      (when desc (foreign-free desc))))
  (values))

(defun set-tty (&key (device "/dev/tty") set-modes unset-modes)
  "Change the termios settings for DEVICE. Similar to `stty`."
  (declare (ignore device set-modes unset-modes))
#|  (let (tty desc)
    (unwind-protect
      (progn
	(setf tty (posix-open device (logior +O_RDWR+ +O_NONBLOCK+) 0)
	      desc (foreign-alloc '(:struct termios)))
	(error-check tty "Error opening ~a~%" device)
	(error-check (tcgetattr tty desc)
		     "Can't get terminal description for ~a." device)
	(with-foreign-slots ((c_iflag c_oflag c_cflag c_lflag c_cc
			      c_ispeed c_ospeed)
			     desc (:struct termios))
	  (loop :for i :in set-modes :do
	     (if (find i *iflags*)
	  (loop :for i :in modes :do
	     (if i
		 (find i *cchars*
	  ;; (make-symbol (string-upcase ))
	  ;; (multiple-value-bind (cols rows) (get-window-size tty)
	  ;;   modes
	  ;;   ))
	  )
      ;; close the terminal
      (when (and tty (>= tty 0))
	(posix-close tty))
      ;; free C memory
      (when desc (foreign-free desc))))
|#
  (values))

#+lish
(lish:defcommand stty
  ((all boolean :short-arg #\a
    :help "True to show all settings.")
   (device string :default *default-console-device-name*
    :help "Terminal device to operate on."))
  "Show and change terminal settings."
  (describe-tty :format (if all :stty :nice) :device device))

;; EOF
