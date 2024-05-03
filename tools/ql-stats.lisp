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
   #:*ql-projects-dir*
   #:quicklisp-hosts
   #:!ql-origin
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

;; quicklisp-projects utilities
;; @@@ maybe we should chage the name of this package to ql-utils or something

(defparameter *ql-projects-dir*
  (load-time-value
   (or (maybe-refer-to :cl-user '#:*ql-projects-dir*)
       (maybe-refer-to :lish-user '#:*ql-projects-dir*)
       (nos:env "QL_PROJECTS_DIR")))
  "Directory with a checked out quicklisp-projects. Defaults from
‘*ql-projects-dir*’ in ‘cl-user’ or ‘lish-user’ so you can just set it in
your .*rc file, or an environment variable QL_PROJECTS_DIR.")

(defun ql-projects-dir ()
  (unless (nos:directory-p (lish:expand-single-file-name *ql-projects-dir*))
    (error "Please set *ql-projects-dir* to a quicklisp-projects directory."))
  *ql-projects-dir*)

(defun extract-host (line)
  "Return the host from a Quicklisp source.txt line."
  (multiple-value-bind (m mm)
      (ppcre:scan-to-strings "http[s]*://([^/]+)/" line)
    (declare (ignore m))
    (ofirst mm)))

;; (defun quicklisp-projects-count-tree (func)
;;   "Return a list of (count result) for all Quicklisp projects, where ‘result’
;; is the result of calling ‘func’ on each source.txt line."
;;   (sort
;;     (loop :for site :in
;;       (group-by-alist
;;        #'first
;;        (loop :with site :and project
;;              :for r :in (loop :for f :in (glob
;;                                           (nos:path-append
;;                                            (ql-projects-dir)
;;                                            "projects" "*" "source.txt"))
;; 			      :do (setf project
;; 					(nos:basename (nos:path-parent f)))
;;                               :collect (first (file-lines f)))
;;              :do (setf site (funcall func r))
;;              :when site
;;              :collect (list site project)))
;;        :collect (list (length (cdr site)) (first site)
;;     #'> :key #'car))

(defun quicklisp-projects-count (func)
  "Return a list of (count result) for all Quicklisp projects, where ‘result’
is the result of calling ‘func’ on each source.txt line."
  (sort
    (loop :for site :in
      (group-by-alist
       #'identity
       (loop :with site
             :for r :in (loop :for f :in (glob
                                          (nos:path-append
                                           (ql-projects-dir)
                                           "projects" "*" "source.txt"))
                              :collect (first (file-lines f)))
             :do (setf site (funcall func r))
             :when site
             :collect site))
       :collect (list (length (cdr site)) (first site)))
    #'> :key #'car))

(defun quicklisp-hosts ()
  "Return a list of (count host) for all Quicklisp projects."
  (quicklisp-projects-count #'extract-host))

(defun extract-method (line)
  "Return the method from a Quicklisp source.txt line."
  (multiple-value-bind (m mm)
      (ppcre:scan-to-strings "^([^ ]+) " line)
    (declare (ignore m))
    (string-downcase (ofirst mm))))

(defun quicklisp-methods ()
  "Return a list of (count host) for all Quicklisp projects."
  (quicklisp-projects-count #'extract-method))

#+(or)
(defun quicklisp-hosts ()
  "Return a list of (count host) for all Quicklisp projects."
  (sort
    (loop :for site :in
      (group-by-alist
       #'identity
       (loop :with site
             :for r :in (loop :for f :in (glob
                                          (nos:path-append
                                           (ql-projects-dir)
                                           "projects" "*" "source.txt"))
                              :collect (first (file-lines f)))
             :do (multiple-value-bind (m mm)
                     (ppcre:scan-to-strings "http[s]*://([^/]+)/" r)
                   (declare (ignore m))
                   (setf site (ofirst mm)))
             :when site
             :collect site))
       :collect (list (length (cdr site)) (first site)))
    #'> :key #'car))

;; @@@ This is so stupid. We should fix "view".
(defun open-url (url)
  (lish:!=
   #+(or linux bsd) "xdg-open"
   #+darwin "open"
   #+windows "start" ;; This will probably start edge since M$ is still fuck
   #-(or linux darwin windows) (error "I don't know how to open a URL.")
   url))

#+quicklisp
(lish:defcommand ql-origin
  ((open boolean :short-arg #\o :help "Try to open it.")
   (clone boolean :short-arg #\c :help "Try to git clone it.")
   (popup boolean :short-arg #\p :help "Pop up stuff.")
   (system quicklisp-system-designator
    :help "System to show the origin of."))
  :accepts '(arg-quicklisp-system-designator)
  (when (not (or system lish:*input*))
    (error "Please supply a system."))
  (when (not system)
    (setf system (ql-dist:system-file-name lish:*input*)))
  (let* ((project-name (string-downcase system))
	 (ql-system (ql-dist:find-system project-name))
	 (project-file (glob:expand-tilde
			(nos:path-append
			 (ql-projects-dir)
			 "projects"
			 (if ql-system
			     (ql-dist:project-name (ql-dist:release ql-system))
			     project-name)
			 "source.txt"))))
  (when (not (nos:file-exists project-file))
    (error "The system isn't in quicklisp. Maybe you should update ~
            quicklisp-projects?"))
  (setf lish:*output*
	(write-string
	 (slurp project-file)))
  (let ((url (cadr (split-sequence #\space (trim lish:*output*)))))
    (when open
      (when (if popup
		(fui:popup-y-or-n-p (s+ "Open " url "?"))
		(y-or-n-p "Open ~s ?" url))
	(open-url url)))
    (when clone
      (lish:!= "git" "clone" url)))))

;; End
