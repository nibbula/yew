;;;
;;; cal.lisp - Command to print a calendar.
;;;

(defpackage :cal
  (:documentation "Calendar")
  (:nicknames :cal)
  (:use :cl :dlib :calendar :table :table-print :grout :fatchar :char-util
	:collections :dtime)
  (:export
   #:calendar-table
   #:print-calendar
   #:mini-strftime
   #:*default-date-format*
   #:!cal
   #:!date
   ))
(in-package :cal)

(defun default-date (now year month)
  "Return the YEAR and MONTH defaulting to the current."
  ;; Use the current date if not given.
  (when (or (null month) (null year))
    (setf month (or month (date-month now))
	  year  (or year (date-year now))))
  (when (or (<= month 0) (> month (calendar:months year)))
    (error "Month is not valid: ~d" month))
  (values year month))

(defun calendar-table (&key year month (now (current-date)))
  "Return calendar table for YEAR and MONTH, which default to the current if
not given."
  (setf (values year month) (default-date now year month))
  (let* ((start-weekday (calendar:day-of-week year month 1))
	 (days (calendar:days-per-month month year))
	 (tab) (table))
    ;; (format t "~s ~s~%" start-weekday days)
    (setf tab (loop
		:with day = 1
		:and weekday = start-weekday
		:while (<= day days)
		:collect
		(let ((week (make-array 7 :initial-element "")))
		  (loop :for d :from weekday :below 7
			:do
			   (setf (aref week d)
				 (cond
				   ((> day days) "")
				   ((and (= day (date-day now))
					 (= month (date-month now))
					 (= year (date-year now)))
				    (ß `(:inverse ,(format nil "~d" day))))
				   (t day)))
			   (incf day))
		  week)
		:do (setf weekday 0))
	  table (table:make-table-from
		 tab :columns
		 (loop :for i :from 1 :to 7
		       :collect
		       (list :name (calendar:weekday-name i :format :short)
			     :type 'number
			     :format "~v@/fatchar-io:print-string/"))))
    table))

(defun print-calendar (&key year month type (stream *standard-output*))
  (declare (ignore type)) ;; @@@
  (let ((now (current-date)))
    (setf (values year month) (default-date now year month))
    (let ((table (calendar-table :year year :month month :now now)))
      (with-grout ()
	(let* ((table-str (with-output-to-string (str)
			    (print-table table :stream str)))
	       (width (display-length
		       (subseq table-str 0 (oposition #\newline table-str))))
	       (month-name (calendar:month-name month year))
	       (mon-pos (round (- (/ width 2)
				  (/ (display-length month-name) 2)))))
	  (grout-format "~v,,,' a~a~%" mon-pos #\space month-name))
	(grout-print-table table :stream stream))
      table)))

;; The format unix date wants for setting the date:
;; (format-date "~2,'0d~2,'0d~2,'0d~2,'0d~4d.~2,'0d~%"
;; (:month :date :hour :min :year :sec #\return) :stream *standard-output*)

(defun hour-24-to-12 (hours)
  "Return 12 based hour from 24 based hour."
  (let ((p (mod hours 12))) (if (zerop p) 12 p)))

;; At least do: %a %d %b %Y %r %Z
(defun mini-strftime (format &optional (time (get-dtime)))
  "Fake strftime. Doesn't do modifiers, or every format."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time (dtime-seconds time))
    (declare (ignore dst tz))
    (with-output-to-string (str)
      (loop :with percent
        :for c :across format :do
	(cond
	  (percent
	   (case c
	     (#\a
	      (write-string
	       (calendar:weekday-name
		(dtime:lisp-to-calendar-weekday day)
		:context format :format :abbreviated) str))
	     (#\A
	      (write-string
	       (calendar:weekday-name
		(dtime:lisp-to-calendar-weekday day)) str))
	     ((#\b #\h)
	      (write-string
	       (calendar:month-name month year :format :abbreviated) str))
	     (#\B
	      (write-string
	       (calendar:month-name month year) str))
	     (#\c
	      (write-string
	       #+unix (mini-strftime (uos:nl-langinfo uos::+D-T-FMT+))
	       #-unix (mini-strftime "%a %d %b %Y %r %Z")
	       ))
	     (#\d
	      (format str "~2,'0d" date))
	     ;; Don't even fucking do %D
	     (#\e
	      (format str "~2d" date))
	     ;; %E is a modifier
	     (#\F
	      (format str "~d-~2,'0d-~2,'0d" year month date))
	     ;; %G %g week-based year?
	     (#\H
	      (format str "~2,'0d" hour))
	     (#\I
	      (format str "~2,'0d" (hour-24-to-12 hour)))
	     ;; #\j day of the year 1-366
	     (#\k
	      (format str "~2d" hour))
	     (#\l
	      (format str "~2d" (hour-24-to-12 hour)))
	     (#\m
	      (format str "~2,'0d" month))
	     (#\M
	      (format str "~2,'0d" minute))
	     (#\n
	      (write-char #\newline str))
	     ;; %O alternate format
	     (#\p
	      (write-string
	       (if (> hour 12)
		   (or 
		    #+unix (uos:nl-langinfo uos::+PM-STR+)
		    #+unix "PM"
		    #-unix "PM")
		   (or
		    #+unix (uos:nl-langinfo uos::+AM-STR+)
		    #+unix "AM"
		    #-unix "AM"))
	       str))
	     (#\r
	      (write-string (mini-strftime "%I:%M:%S %p" time) str))
	     (#\S
	      (format str "~2,'0d" second))
	     (#\Y
	      (format str "~d" year))
	     (#\Z
	      ;; (format str "~d" tz)) ;; @@@ XXX Wrong. look up the name
	      (format str "~a" (nos:timezone-name)))
	     (#\%
	      (write-char #\% str)))
	   (setf percent nil))
	  (t
	   (if (eql c #\%)
	       (setf percent t)
	       (write-char c str))))))))

(defvar *default-date-format* #+unix :unix #-unix :net
  "Default format for date command.")

;; End
