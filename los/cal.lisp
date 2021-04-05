;;;
;;; cal.lisp - Command to print a calendar.
;;;

(defpackage :cal
  (:documentation "Calendar")
  (:use :cl :calendar :table :table-print :grout :fatchar :char-util
	:collections)
  (:export
   #:calendar-table
   #:print-calendar
   #:!cal
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

;; End
