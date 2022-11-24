;;;
;;; stty.lisp - Terminal driver manipulator.
;;;

(defpackage :stty
  (:documentation "Terminal driver modes manipulator.")
  (:use :cl :dlib :opsys #+unix :os-unix :cffi :dlib-misc
	:char-util :completion)
  (:shadow #:sane)
  (:export
   #:describe-tty
   #:set-tty
   #:!stty
   ))
(in-package :stty)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun demuff (s)
  "Remove the earmuffs."
  (trim (string s) #(#\+)))

(defun print-termios (desc cols rows format)
  "Print the termios struct in desc."
  (with-foreign-slots ((c_iflag c_oflag c_cflag c_lflag c_cc c_ispeed c_ospeed)
		       desc (:struct termios))
    (flet ((dump-char (sym c)
	     (cond
	       ((= c #xff) "undef")
	       ((or (eq sym '+VMIN+) (eq sym '+VTIME+)) c)
	       (t (nice-char (code-char c) :caret t))))
	   (s-dump-flags (name flags var)
	     (let* ((label (format nil "~a: " name))
		    (prefix (make-string (display-length label)
					 :initial-element #\space)))
	       (fresh-line)
	       (write-string label)
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
		:cols cols :prefix prefix
		:start-column (display-length label))))
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
	 (s-dump-flags "lflags" *lflags* c_lflag)
	 (s-dump-flags "iflags" *iflags* c_iflag)
	 (s-dump-flags "oflags" *oflags* c_oflag)
	 (s-dump-flags "cflags" *cflags* c_cflag)
	 (format t "~&cchars: ")
	 (justify-text
	  (join-by-string
	   (loop :for c :in *cchars*
		 :collect
		 (format nil "~(~a~) = ~a; " (demuff c)
			 (dump-char
			  c (mem-aref c_cc :unsigned-char
				      (symbol-value c)))))
	   " ")
	  :cols cols :prefix "        " :omit-first-prefix t
	  :start-column 8)
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
	  :prefix "  "))))))

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
	(multiple-value-bind (cols rows) (get-window-size tty)
	  (print-termios desc cols rows format)))
      ;; close the terminal
      (when (and tty (>= tty 0))
	(posix-close tty))
      ;; free C memory
      (when desc (foreign-free desc))))
  (values))

(defun set-tty (&key (device "/dev/tty") set-modes unset-modes control-chars)
  "Change the termios settings for DEVICE. Similar to `stty`."
  (let (tty desc)
    ;; Do function modes first
    (loop :for mode :in set-modes :do
      (when (fboundp mode)
	(funcall mode device)))

    (format t "set ~s~%unset ~s~%chars ~s~%" set-modes unset-modes control-chars)

    (unwind-protect
      (progn
        (setf tty (syscall (posix-open device (logior +O_RDWR+ +O_NONBLOCK+) 0))
	      desc (foreign-alloc '(:struct termios)))
	(syscall (tcgetattr tty desc))
	;; these slot names are stupid as fuck
	(with-foreign-slots ((c_iflag c_oflag c_cflag c_lflag c_cc
			      c_ispeed c_ospeed)
			     desc (:struct termios))
	  ;; set modes
	  (loop :for mode :in set-modes :do
	     (when (find mode uos::*iflags*)
	       (setf c_iflag (logior c_iflag (symbol-value mode))))
	     (when (find mode uos::*oflags*)
	       (setf c_oflag (logior c_oflag (symbol-value mode))))
	     (when (find mode uos::*cflags*)
	       (setf c_cflag (logior c_cflag (symbol-value mode))))
	     (when (find mode uos::*lflags*)
	       (setf c_lflag (logior c_lflag (symbol-value mode)))))

	  ;; unset modes
	  (loop :for mode :in unset-modes :do
	     (when (find mode uos::*iflags*)
	       (setf c_iflag (logand c_iflag (lognot (symbol-value mode)))))
	     (when (find mode uos::*oflags*)
	       (setf c_oflag (logand c_oflag (lognot (symbol-value mode)))))
	     (when (find mode uos::*cflags*)
	       (setf c_cflag (logand c_cflag (lognot (symbol-value mode)))))
	     (when (find mode uos::*lflags*)
	       (setf c_lflag (logand c_lflag (lognot (symbol-value mode))))))

	  ;; set chars
	  (loop :for (name . char) :in control-chars :do
	     (setf (mem-aref c_cc :unsigned-char (symbol-value name)) char))

	  (print-termios desc 80 24 :nice)

	  ;; actually do it now holmes
	  (syscall (tcsetattr tty +TCSANOW+ desc))
	  ))
      ;; Close the terminal
      (when (and tty (>= tty 0))
	(syscall (posix-close tty)))
      ;; free shabby old C memory
      (when desc (foreign-free desc)))
  (values)))

(defun sane (device)
  (uos:sane :device device))

(defparameter *all-settings*
  `(,@uos::*cchars*
    ,@uos::*iflags* ,@uos::*oflags* ,@uos::*cflags* ,@uos::*lflags*
    ,'sane))

(defun setting-name (symbol)
  "Return the string for the setting symbol."
  (cond
    ((fboundp symbol)
     (string-downcase (symbol-name symbol)))
    (t
     (string-downcase (trim (symbol-name symbol) "+")))))

(defparameter *all-setting-strings*
  (map 'list (_ (setting-name _)) *all-settings*))

(defparameter *settings-table*
  (let ((table (make-hash-table :test #'equal)))
    (loop :for s :in *all-settings*
       :do
       (setf (gethash (setting-name s) table) s))
    table))

(defun find-setting (name)
  (gethash (string-downcase name) *settings-table*))

(defun gather-settings (settings)
  "Convert the shell style stty settings into Lisp style."
  (let (set-modes unset-modes cchars)
    (loop :with sym :and s
       :for ss = settings :then (cdr ss)
       ;;:until (endp ss)
       :while (not (endp ss))
       :do
       (setf s (car ss))
       (cond
	 ((and (symbolp s)
	       (setf sym (gethash (setting-name s) *settings-table*)))
	  (push sym set-modes))
	((or (not (stringp s)) (zerop (length s)))
	  (error "~s isn't a valid setting" s))
	((and (char= (char s 0) #\-)
	      (setf sym (find-setting (subseq s 1))))
	 (push sym unset-modes))
	((setf sym (find-setting s))
	 (if (find sym uos::*cchars*)
	     (progn
	       (push (cons sym (cadr settings)) cchars)
	       (setf ss (cdr settings)))
	     (progn
	       (push sym set-modes))))))
    (values set-modes unset-modes cchars)))

;; EOF
