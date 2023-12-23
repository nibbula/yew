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

(defvar *default-date-format* #+unix :unix #-unix :net
  "Default format for date command.")

;; End
