;;;
;;; ql-stats.lisp - Grab Quicklisp statistics files.
;;;

(defpackage :ql-stats
  (:documentation "Grab Quicklisp statistics files.")
  (:use :cl :dlib :collections :dlib-misc :glob :table-viewer)
  (:export
   #:get-month
   #:get-year
   #:get-all
   #:view-it
   #:chart-it
   #:!ql-stats
   ))
(in-package :ql-stats)

(defparameter *url*
  "https://www.quicklisp.org/stats/~4,'0D~:*/~4,'0D-~2,'0D.csv")

(defparameter *data-directory* nil
  "Where to put the data files. Otherwise it defaults to the user's cache 
directory.")

(defclass stats-viewer (table-viewer:table-viewer)
  ()
  (:documentation "Table viewer for Quicklisp statistics."))

(defmethod view-cell ((o stats-viewer))
  (cond
    ((symbol-call :lish :get-command "ql-origin")
     (symbol-call :lu :!ql-origin :open t :system (current-cell o) :popup t))
    (t
     (fui:show-text
      "I can't find the ql-origin command, so I'm sorry but you'll have ~
       to figure it out yourself."))))

(defun stat-url (year month)
  "Return the statistics URL name for ‘year’ and ‘month’."
  (format nil *url* year month))

(defun stat-file (year month)
  "Return the statistics file name for ‘year’ and ‘month’."
  (format nil (nos:path-append (or *data-directory*
				   (nos:cache-dir "ql-stats"))
			       "~4,'0D~:*" "~4,'0D-~2,'0D.csv")
	  year month))

#+(or) ;; Actually we don't need this.
(defun year-and-month-from-path (path)
  "Return the year and month from the ‘path’."
  (let* ((parsed-path (nos:os-pathname-path (nos:parse-path path)))
	 (ym (nthcdr (- (length p) 2) parsed-path))
	 (year (parse-integer (first ym)))
	 (month (parse-integer (second ym) :start 5 :junk-allowed t)))
    (values year month)))

(defun stat-files ()
  "Return all the statistics files in the cache."
  (glob (nos:path-append 
	 (or *data-directory* (nos:cache-dir "ql-stats"))
	 "*" "*.csv")))

(defun download-month (year month)
  "Download the data for ‘year’ ane ‘month’ into it's cache file."
  (let ((file-name (stat-file year month)))
    (nos:ensure-directory (nos:path-directory-name file-name))
    (format t "Getting ~s ..." (stat-url year month))
    (finish-output)
    (multiple-value-bind (content status)
	(drakma:http-request (stat-url year month))
      (case status
	(200
	 (format t "ok~%") (finish-output)
	 (spit file-name content))
	(404
	 (format t "missing~%") (finish-output))
	(otherwise
	 (cerror "Keep going anyway"
		 "Got an HTTP error ~s" status))))))

(defun ensure-month (year month)
  "Make sure the file for ‘year’ and ‘month’ is downloaded."
  (let ((file-name (stat-file year month)))
    (when (not (nos:file-exists file-name))
      (download-month year month))))

(defun current-month ()
  "Return the current month number."
  (fifth (multiple-value-list (decode-universal-time (get-universal-time)))))

(defun current-year ()
  "Return the current year number."
  (sixth (multiple-value-list (decode-universal-time (get-universal-time)))))

(defun read-table-file (file)
  "Read the table from ‘file’."
  (dtt:read-table file :style dtt:+pipe+ :guess-types t))

(defun read-month-table (year month)
  "Read the table for ‘year’ and ‘month’."
  (read-table-file (stat-file year month)))

(defun add-table (from to)
  "Add the download counts from table ‘from’ to ‘to’."
  (when (not (typep (container-data to) 'list))
    ;; We have to convert it to a list so we can add to it.
    ;; Hopefully it shouldn't be too long.
    (setf (container-data to) (coerce (container-data to) 'list)))
  (omapn (lambda (from-row)
	   (let ((to-row (ofind (oelt from-row 2) to
				:test #'equal :key (_ (oelt _ 2)))))
	     (when (not to-row)
	       (setf to-row (ocopy from-row))
	       (opush to to-row))
	     (incf (oelt to-row 1) (oelt from-row 1))))
	 from))

(defun get-month (&optional (month (1- (current-month))) (year (current-year)))
  "Return statistics for ‘month’ in the current year. ‘month’ defaults to the
current month."
  (ensure-month year month)
  (read-month-table year month))

(defun get-year (&optional (year (current-year)))
  "Return a full year of statistics."
  (let (year-table)
    (loop :with month = 0
      :do
      (with-simple-restart (continue "Skip this month.")
	(incf month)
	(when (<= month 12)
	  (ensure-month year month)
	  (cond
	    ((not year-table)
	     (setf year-table
		   (read-month-table year month)))
	    (t
	     (add-table (read-month-table year month) year-table)))))
      :while (<= month 12))
    year-table))

;; This doesn't really get all, but just what you've already downloaded, which
;; might be nothing.
(defun get-all ()
  "Return all statistics that are already downloaded."
  (let (full-table)
    (loop :with year :and month
      :for file :in (stat-files) :do
      ;; (setf (values year month) (year-and-month-from-path file))
      (cond
	((not full-table)
	 (setf full-table
	       (read-table-file file)))
	(t
	 (add-table (read-table-file file) full-table))))
    full-table))

(defun view-it (table)
  "View the ‘table’ in a custom stats-viewer."
  (table-viewer:view-table table :type 'stats-viewer))

#+lish
(lish:defcommand ql-stats
  ((month number :short-arg #\m :help "View statistics for a month.")
   (year number :short-arg #\y :help "View statistics for a year.")
   (all boolean :short-arg #\a :help "View statistics for all cached files.")
   (collect boolean :short-arg #\c
    :help "Just collect the data and return it as a table. Don't run the viewer.")
   (system quicklisp-system-designator :short-arg #\s :repeating t
    :help "System to report on."))
  "Show Quicklisp download statistics."
  (let ((table
	  (cond
	    ((and all (or month year))
	     (error
	      "Sorry, I can't do a specific month (-m) or year (-y) with all ~
               (-a)."))
	    (all
	     (get-all))
	    ((and month year)
	     (get-month month year))
	    (year
	     (get-year year))
	    (month
	     (get-month month))
	    (t
	     (get-month)))))
    (when (not collect)
      (view-it table))
    (setf lish:*output* table)))

(defun chart-it (&key (dir :horizontal) (type :total) system)
  "Show a chart of counts of Quicklisp downloads."
  (flet ((month-total (month year)
	   (reduce '+ (omap-as 'list (_ (oitem 1 _))
			       (ql-stats:!ql-stats :collect t
						   :month month
						   :year year))))
	 (top (month year)
	   (oitem 1 (oitem 0 (ql-stats:!ql-stats :collect t
						 :month month :year year))))
	 (one (month year)
	   (let ((s (ofind system
			   (ql-stats:!ql-stats :collect t
					       :month month :year year)
			   :test #'equal :key (_ (oitem 2 _)))))
	     ;; If we can't find the system, pretend it's zero.
	     (if s
		 (oitem 1 s)
		 0))))
    (let* ((func (cond
		   (system #'one)
		   ((eq type :total) #'month-total)
		   ((eq type :top) #'top)
		   (t (error "Unknown chart type ~s" type))))
	   (chart
	     (loop :for year :from 2020 :to (calendar:current-year) :append
	       (loop :for month :from 1 :below
	         (if (= year (calendar:current-year))
		     (oitem 'month (calendar:current-date))
                     12)
		 :collect (cons (s+ (calendar:month-name
				     month year :format :abbreviated)
				    " " year)
				(funcall func month year))))))
      (chart:view-chart (chart:make-chart-from
			 (if (member dir '(:h :horizontal))
			     'chart::horizontal-bar
			     'chart::vertical-bar)
			 chart)))))

;; End
