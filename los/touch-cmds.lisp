;;;
;;; touch-cmds.lisp - Commands for touch.
;;;

(in-package :touch)

(defun parse-time (string)
  "Parse ‘string’ as [[CC]YY]MMDDhhmm[.ss] and return an os-time."
  (let* ((seconds-pos (position #\. string))
	 (plus (if seconds-pos 3 0))
	 (len (length string))
	 (year (current-year))
	 month date hour (minute 0) (seconds 0))
    (labels ((as-int (thing string start &optional (len 2))
	       (let ((result (ignore-errors
			      (parse-integer string :start start
						    :end (+ start len)))))
		 (when (not result)
		   (error "Bad date format for ~a: ~a ~a." thing
			  string (subseq string start (+ start len))))
		 result))
	     (get-date (str start)
	       (let ((s (subseq str start)))
	         (setf month  (as-int "month"   s 0 2)
	               date   (as-int "day"     s 2 2)
	               hour   (as-int "hour"    s 4 2)
	               minute (as-int "minute"  s 6 2)))))
      (cond
	((= len (+ 12 plus))			; CCYYMMDDhhmm
	 (when (>= (current-year) 10000)	; y10k problem!
	   (error "Date format obsolete. Use --date."))
	 (setf year (as-int "year" string 0 4))
	 (get-date string 4))
	((= len (+ 10 plus))			; YYMMDDhhmm
	 (setf year (+ (* (truncate (current-year) 100) 100)
		       (as-int "year" string 0)))
	 (get-date string 2))
	((= len (+ 8 plus))			; MMDDhhmm
	 (get-date string 0))
	(t
	 (error "Bad date format: ~a." string)))
      (when seconds-pos
	(setf seconds (as-int "seconds" string (1+ seconds-pos) 2)))
      (make-os-time :seconds (encode-universal-time
			      seconds minute hour date month year)))))

(defun parse-date (string)
  "Parse ‘string’ as a fancy free form date and return an os-time."
  (declare (ignore string))
  ;; @@@ It should be added to the :dtime package.
  (error "Fancy date parsing isn't implemented yet."))

(lish:defcommand touch
  ((files pathname #|:optional nil|# :repeating t :help "Files to touch.")
   (no-create boolean :short-arg #\c :help "Don't create files.")
   (date string :short-arg #\d :optional t ;; @@@ make an arg type
    :help "Date to set as the file's time.")
   (access-only boolean :short-arg #\a
    :help "Only change the access time.")
   (modification-only boolean :short-arg #\m
    :help "Only change the modification time.")
   (time string :short-arg #\t
    :help "Date to set as the file's time, in the format [[CC]YY]MMDDhhmm[.ss]"))
  "Touch a thing in the file system. "
  (when (and time date)
    (error "Please provide only one of --date or --time."))
  (let* ((time-stamp (or (and date (parse-date date))
			(and time (parse-time time))
			(get-os-time)))
	 (mtime (and modification-only time-stamp))
	 (atime (and access-only time-stamp)))
    (lish:with-files-or-input (files)
      (loop :for f :in files
	    :do
	       (when (or (not no-create)
			 (and no-create (nos:file-exists f)))
		 (touch f :modification-time mtime
			  :access-time atime
			  :time time-stamp))))))

;; End
