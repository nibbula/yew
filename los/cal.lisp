;;;
;;; cal.lisp - Command to print a calendar.
;;;

(defpackage :cal
  (:documentation "Calendar")
  (:use :cl :dlib :calendar :table :table-print :grout :fatchar :char-util
	:collections :dtime)
  (:export
   #:calendar-table
   #:print-calendar
   #:!cal
   #:!date
   ))
(in-package :cal)

#|
* Examples:

** Customized style:

cal 1 2021 | print-table -r (make-instance
                             'terminal-table:terminal-box-table-renderer
                              :box-style table-print::*fancy-box*
                              :box-color :magenta :title-color :cyan :x 0)

** Bigger:

(defun boo ()
  (let ((cal (cal::calendar-table)))
    (omap
      (_
       (loop for i from 0 below (olength _)
             do (setf (oelt _ i)
                        (s+  "   " (oelt _ i) "   "))))
      cal)
    (omap (_ (setf (table:column-align _) :wrap
                   (table:column-width _) 8))
          (table:table-columns cal))
    ;; (print (table:table-columns cal))
    (tt-newline)
    (table-print:output-table cal
     (make-instance 'terminal-table:terminal-box-table-renderer :box-style
                    table-print:*fancy-box* :box-color :magenta :title-color
                    :cyan)
     *terminal*)
    (tt-finish-output)))
|#

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
		:while (< day days)
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

(defun print-calendar (&key year month type #|(stream *standard-output*) |#)
  (declare (ignore type)) ;; @@@
  (let ((now (current-date)))
    (setf (values year month) (default-date now year month))
    (let ((table (calendar-table :year year :month month :now now)))
      ;; (format t "~s~%" tab)
      (with-grout ()
	(let* ((table-str (with-output-to-string (str)
			    (print-table table :stream str)))
	       (width (display-length
		       (subseq table-str 0 (oposition #\newline table-str))))
	       (month-name (calendar:month-name month year))
	       (mon-pos (round (- (/ width 2)
				  (/ (display-length month-name) 2)))))
	  (grout-format "~v,,,' a~a~%" mon-pos #\space month-name))
	(grout-print-table table #| :stream stream |#))
      table)))

#+lish
(lish:defcommand cal
  ((year integer :optional t :help "The year to show a calendar for.")
   (month integer :optional t :help "The month to show a calendar for."))
  :args-as args
  "Print a calendar."
  (let ((arg-count (/ (length args) 2)))
    (cond
      ((zerop arg-count)
       (setf year (current-year)))
      ((= 1 arg-count)
       (setf year (getf args :year)))
      ((= 2 arg-count)
       (psetf month (getf args :year)
	      year (getf args :month)))
      (t
       ;; @@@ Maybe there should be a way to do totally manual argument
       ;; processing?
       ))
    (setf lish:*output* (print-calendar :year year :month month))))

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
	       (mini-strftime (uos:nl-langinfo uos::+D-T-FMT+))))
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
		   (or (uos:nl-langinfo uos::+PM-STR+) "PM")
		   (or (uos:nl-langinfo uos::+PM-STR+) "AM"))
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

;; If you really want to specify a complex format like posix date, just use
;; format-date. If you want to set the date use, set-date.
#+lish
(lish:defcommand date
  ((format string :short-arg #\f :default #+unix :unix #-unix :net
    :help "Format for the date."))
  "Print the date."
  (write-line
   (case (keywordify format)
     (:unix
      #+unix (mini-strftime (uos:nl-langinfo uos::+D-T-FMT+))
      #-unix (mini-strftime "%a %d %b %Y %r %Z"))
     (:net
      (date-string :format :net))
     (otherwise
      (date-string)))))

#| @@@ finish simple-parse-date
(lish:defcommand set-date
  ((date string :optional t :help "String date representation to set.")
   (seconds integer :short-arg #\s :help "Lisp Universal time in seconds.")
   (nanoseconds integer :short-arg #\n :default 0
    :help "Nanosecond part of universal time.")
   (from string :short-arg #\f :help "Where to get the date from."))
  "Set the date."
  (when (and date from)
    (error "Only specify one of either date or from."))
  (if (or date from (and *input* (typep *input* (or string integer))))
      ;; Until we have our own network date thing
      (cond
	(date
	 (etypecase date
	   (integer
	    (when (not (typep nanosecond integer))
	      (error "Nanoseconds must be an integer."))
	    (nos:set-time seconds nanoseconds))
	   (string
	    (let ((time (simple-parse-date date)))
	      (nos:set-time time)))
	   (t
	    (error "Date should be an integer or string."))))
	(from
	 (error "We don't know how to do remote date setting yet.")))
      (format t "~a~%" (date-string :format :net)))
  (values))
|#

;; End
