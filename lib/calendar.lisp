;;;
;;; calendar.lisp - Naming and counting time.
;;;

(defpackage :calendar
  (:documentation "Naming and counting time on planets.")
  (:use :cl :locale :dlib)
  (:export
   #:calendar
   #:calendar-months
   #:calendar-month-names
   #:calendar-month-name
   #:calendar-days-per-month
   #:calendar-week-days
   #:calendar-week-day-names
   #:calendar-day-of-week
   #:calendar-leap-year-p
   #:calendar-current-year
   #:calendar-current-date
   #:date #:typical-date #:date-year #:date-month #:date-day
   #:julian
   #:gregorian
   ;; current calendar
   #:day-of-week #:date-day-of-week
   #:days-per-month #:date-days-per-month
   #:weekday-name
   #:months
   #:month-name #:date-month-name
   #:current-year
   #:current-date
   ))
(in-package :calendar)

(defclass calendar ()
  ((name
    :initarg :name :accessor calendar-name
    :documentation "What you would call this calendar system."))
  (:documentation "A naming system for time passing."))

(defclass date ()
  ()
  (:documentation "Some kind of date."))

;; @@@ The units of months, years, days is specific to common planetary
;; calendars, but some time in the future we should probably support arbitrary
;; named divisions. But that would add quite a bit of complexity.

(defgeneric calendar-months (calendar year)
  (:documentation "Number of months for a year."))

(defgeneric calendar-month-names (calendar year context format)
  (:documentation "Return a vector of month names."))

(defgeneric calendar-month-name (calendar month year context format)
  (:documentation "Return the name of the numbered MONTH."))

(defgeneric calendar-days-per-month (calendar month year)
  (:documentation "Number of days in a specific month."))

(defgeneric calendar-week-days (calendar)
  (:documentation "Return the number of days in a week."))

(defgeneric calendar-weekday-names (calendar context format)
  (:documentation "Return a vector of weekday names."))

(defgeneric calendar-weekday-name (calendar weekday context format)
  (:documentation "Return the name of weekday numbered WEEKDAY."))

(defgeneric calendar-day-of-week (calendar year month day)
  (:documentation "Return the day of the week of the given date."))

(defgeneric calendar-current-year (calendar)
  (:documentation "Return the current year of CALENDAR."))

(defgeneric calendar-current-date (calendar)
  (:documentation "Return the current date for CALENDAR."))

(defclass typical-date (date)
  ((year  :initarg :year  :accessor date-year)
   (month :initarg :month :accessor date-month)
   (day   :initarg :day   :accessor date-day))
  (:documentation "A typical Earth date, with year, month, and day."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julian

(defclass julian (calendar)
  ()
  (:default-initargs
   :name "Julian")
  (:documentation "The Julian calendar system for Earth."))

(defmethod calendar-months ((calendar julian) year) 12)

(defmethod calendar-month-names ((calendar julian) year context format)
  (declare (ignore year))
  (language-month-names calendar (keywordify (language-for-time))
			format context))

(defmethod calendar-month-name ((calendar julian) month year context format)
  (declare (ignore year))
  (let ((names (language-month-names calendar (keywordify (language-for-time))
				     format context)))
    (if (and (plusp month) (< month (length names)))
	(aref names (1- month))
	(error "Invalid month number ~s" month))))

(defgeneric calendar-leap-year-p (calendar year)
  (:documentation "Return true if YEAR is a leap year."))

(defmethod calendar-leap-year-p ((calendar julian) year)
  "Return true if YEAR is a leap year."
  (zerop (mod year 4)))

(defmethod calendar-days-per-month ((calendar julian) month year)
  (ecase month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2
     (if (calendar-leap-year-p calendar year)
	 29
	 28))))

(defmethod calendar-week-days ((calendar julian)) 7)

(defmethod calendar-weekday-names ((calendar julian) context format)
  (language-weekday-names calendar (keywordify (language-for-time))
			  format context))

(defmethod calendar-weekday-name ((calendar julian) weekday context format)
  (when (or (< weekday 1) (> weekday (calendar-week-days calendar)))
    (error "Invalid week day number: ~s" weekday))
  (aref (calendar-weekday-names calendar) (1- weekday)))

;; See
;; https://www.tondering.dk/claus/cal/chrweek.php

(defmethod calendar-day-of-week ((calendar julian) year month day)
  "Return the day of the week for a Gregorian date."
  (let* ((a (truncate (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (- (* 12 a) 2))))
    (mod (+ 5 day y (truncate year 4) (truncate (* 31 m) 12)) 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gregorian

(defclass gregorian (calendar)
  ()
  (:default-initargs
   :name "Gregorian")
  (:documentation "The Gregorian calendar system for Earth. "))

(defmethod calendar-months ((calendar gregorian) year) 12)

(defgeneric calendar-leap-year-p (calendar year)
  (:documentation "Return true if YEAR is a leap year."))

(defmethod calendar-leap-year-p ((calendar gregorian) year)
  "Return true if YEAR is a leap year."
  (and (zerop (mod year 4))
       (not (zerop (mod year 100)))
       (not (zerop (mod year 400)))))

;; In ~/stuff/misc/cldr/common/main/<lang>.xml
;;
;; ja.xml ldml dates calendar calendar type='gregorian' days
;;   daycontext type='format' daywidth type='narrow' day type='sun'

(defmethod calendar-days-per-month ((calendar gregorian) month year)
  ;; @@@ This is duplicated in the julian code. It it worth making a common
  ;; superclass?
  (ecase month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2
     (if (calendar-leap-year-p calendar year)
	 29
	 28))))

(defmethod calendar-month-names ((calendar gregorian) year context format)
  (declare (ignore year))
  (language-month-names calendar (keywordify (language-for-time))
			format context))

(defmethod calendar-month-name ((calendar gregorian) month year context format)
  (declare (ignore year))
  (let ((names (language-month-names calendar (keywordify (language-for-time))
				     format context)))
    (if (and (plusp month) (<= month (length names)))
	(aref names (1- month))
	(error "Invalid month number: ~s" month))))

(defmethod calendar-week-days ((calendar gregorian)) 7)

(defmethod calendar-weekday-names ((calendar gregorian) context format)
  (language-weekday-names calendar (keywordify (language-for-time))
			  format context))

(defmethod calendar-weekday-name ((calendar gregorian) weekday context format)
  (when (or (< weekday 1) (> weekday (calendar-week-days calendar)))
    (error "Invalid week day number: ~s" weekday))
  (aref (calendar-weekday-names calendar context format) (1- weekday)))

#|
Sakamoto's method for calculating the day of the week.

See:
https://en.wikipedia.org/wiki/Calculating_the_day_of_the_week#Sakamoto's_methods
|#

(define-constant +month-offset+ #(0 3 2 5 0 3 5 1 4 6 2 4)
  "Month offset for day-of-week calculations based on Zeller's method, where
the year starts with March." 'vector-equal)

(defmethod calendar-day-of-week ((calendar gregorian) year month day)
  "Return the day of the week for a Gregorian date."
  (when (< month 3)
    (decf year))
  (mod (+ year
	  (- (truncate year 4) (truncate year 100))
	  (truncate year 400)
	  (aref +month-offset+ (1- month))
	  day)
       7))

;; Or from Tøndering (same URL from above):
#+(or)
(defmethod calendar-day-of-week ((calendar gregorian) year month day)
  "Return the day of the week for a Gregorian date."
  (let* ((a (truncate (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (- (* 12 a) 2))))
    (mod (+ -1 day y
	    (truncate year 4)
	    (truncate year 100)
	    (truncate year 400)
	    (truncate (* 31 m) 12))
	 7)))

(defmethod calendar-current-year ((calendar gregorian))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour date month))
    year))

(defmethod calendar-current-date ((calendar gregorian))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (make-instance 'typical-date :year year :month month :day date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for the current calendar.

(defun day-of-week (year month day)
  "Return the day of the week for the given date in the default calendar for the
current locale."
  (calendar-day-of-week (locale-calendar (ensure-locale))
			year month day))

(defun date-day-of-week (date)
  "Return the day of the week for the given date in the default calendar for the
current locale."
     (calendar-day-of-week (locale-calendar (ensure-locale))
			   (date-year  date)
			   (date-month date)
			   (date-day   date)))

(defun months (year)
  (calendar-months (locale-calendar (ensure-locale)) year))

(defun days-per-month (month year)
  "Number of days in a specific month."
  (calendar-days-per-month (locale-calendar (ensure-locale))
			   month year))

(defun date-days-per-month (date)
  "Number of days in a specific month."
  (calendar-days-per-month (locale-calendar (ensure-locale))
			   (date-month date)
			   (date-year date)))

(defun weekday-name (weekday &key (context :format) format)
  (calendar-weekday-name (locale-calendar (ensure-locale))
			 weekday context format))

(defun month-name (month year &key (context :format) format)
  (calendar-month-name (locale-calendar (ensure-locale)) month year
		       context format))

(defun date-month-name (date &key (context :format) format)
  (calendar-month-name (locale-calendar (ensure-locale))
		       (date-month date) (date-year date)
		       context format))

(defun month-names (year &key (context :format) format)
  (calendar-month-names (locale-calendar (ensure-locale)) year context format))

(defun current-year ()
  (calendar-current-year (locale-calendar (ensure-locale))))

(defun current-date ()
  (calendar-current-date (locale-calendar (ensure-locale))))

;; End
