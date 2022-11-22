;;;
;;; dtime.lisp - Time library.
;;;

(defpackage :dtime
  (:documentation
   "Things to deal with simple counting of time for stupid meat creatures that
live on Earth very recently only!")
  (:use :cl :dlib :opsys :calendar)
  (:export
   ;; dtime object
   #:dtime #:dtime-seconds #:dtime-nanoseconds #:make-dtime #:copy-dtime
   #:dtime-p #:get-dtime #:dtime-round #:make-dtime-as #:dtime-to
   #:dtime= #:dtime/= #:dtime< #:dtime> #:dtime<= #:dtime>= #:dtime+ #:dtime-
   #:dtime-zerop #:dtime-plusp #:dtime-minusp #:dtime-min #:dtime-max
   #:dtime-normalize #:dtime-normalize!
   ;; other stuff
   #:date-string
   #:format-date
   #:simple-parse-time
   #:describe-duration
   #:millennia-to-time #:centuries-to-time #:decades-to-time #:years-to-time
   #:weeks-to-time #:days-to-time #:hours-to-time #:minutes-to-time
   #:time-to-millennia #:time-to-centuries #:time-to-decades #:time-to-years
   #:time-to-weeks #:time-to-days #:time-to-hours #:time-to-minutes
   #:lisp-to-calendar-weekday #:lisp-weekday-name
   ))
(in-package :dtime)

(declaim (optimize (debug 2)))

;; It is very very rudimentary and should be someday redesigned for
;; universality (see universal_time.txt).

;; These, for lack of a better thing, these operate on the dual time from opsys.
;; To do better we probably need arbitrary precision floats.
;; [Could use mpfr or bfloats from maxima] (see wip/units.lisp)

(defstruct dtime
  (seconds 0 :type integer)
  (nanoseconds 0 :type integer))

(defun get-dtime ()
  "Return the current time as a new DTIME."
  (multiple-value-bind (s n) (get-time)
    (make-dtime :seconds s :nanoseconds n)))

(defconstant +ns-per-sec+ (expt 10 9)
  "The number of nanoseconds in a second.")

(defun make-dtime-as (value unit)
  (let ((divvy (dtime-divisor unit)))
    (when (not divvy)
      (error "Unknown unit ~s~%" unit))
    (multiple-value-bind (s leftover) (truncate value divvy)
      (make-dtime :seconds s
		  :nanoseconds
		  (truncate (* leftover (/ +ns-per-sec+ divvy)))))))

(defun dtime-to (dtime unit)
  (let ((multy (dtime-divisor unit)))
    (when (not multy)
      (error "Unknown unit ~s~%" unit))
    (+ (* multy (dtime-seconds dtime))
       (/ (dtime-nanoseconds dtime) (/ +ns-per-sec+ multy)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The stupid base unit of time is seconds.
  ;; Anything after weeks is bogus because years are variable and poorly defined!
  ;; But for this bullcrap, we use the Jullian year which is exactly 365.25.
  ;; Just more of the long painful future history of time.
  (defparameter *time-super-units*
    #(((:millennia :millennium)      #.(* 60 60 24 (+ 365 1/4) 1000))
      ((:centuries :century)         #.(* 60 60 24 (+ 365 1/4) 100))
      ((:decades   :decade)          #.(* 60 60 24 (+ 365 1/4) 10))
      ((:years     :year   :yr)      #.(* 60 60 24 (+ 365 1/4)))
      ((:weeks     :week   :wk)      #.(* 60 60 24 7))
      ((:days      :day    :d)       #.(* 60 60 24))
      ((:hours     :hour   :h :hr)   #.(* 60 60))
      ((:minutes   :minute :m :min)  60)
      ((:seconds   :second :s)       1)))

  (defun dtime-factor (unit)
    (second (find-if (_ (member unit _)) *time-super-units* :key #'first)))

  (defparameter *time-sub-units*
    #(
      ((:seconds      :second        :s)  #.(expt 10 0))
      ((:deciseconds  :decisecond    :ds) #.(expt 10 1))
      ((:centiseconds :centisecond   :cs) #.(expt 10 2))
      ((:milliseconds :millisecond   :ms) #.(expt 10 3))
      ((:microseconds :microsecond   :µs) #.(expt 10 6))
      ((:nanoseconds  :nanosecond    :ns) #.(expt 10 9))
      ((:picoseconds  :picosecond    :ps) #.(expt 10 12))
      ((:femtoseconds :femtosecond   :fs) #.(expt 10 15))
      ((:attoseconds  :attosecond    :as) #.(expt 10 18))
      ((:zeptoseconds :zeptosecond   :zs) #.(expt 10 21))
      ((:yoctoseconds :yoctosecond   :ys) #.(expt 10 24))
      ((:rontoseconds :rontosecond   :rs) #.(expt 10 27))
      ((:quectoseconds :quectosecond :qs) #.(expt 10 30))
      ))

  (defun dtime-divisor (unit)
    (second (find-if (_ (member unit _)) *time-sub-units* :key #'first))))

(defun millennia-to-time (millennia) (* millennia #.(dtime-factor :millennia)))
(defun centuries-to-time (centuries) (* centuries #.(dtime-factor :centuries)))
(defun decades-to-time   (decades)   (* decades   #.(dtime-factor :decades)))
(defun years-to-time     (year)      (* year      #.(dtime-factor :years)))
(defun weeks-to-time     (weeks)     (* weeks     #.(dtime-factor :weeks)))
(defun days-to-time      (days)      (* days      #.(dtime-factor :days)))
(defun hours-to-time     (hours)     (* hours     #.(dtime-factor :hours)))
(defun minutes-to-time   (minutes)   (* minutes   #.(dtime-factor :minutes)))

(defun time-to-millennia (seconds) (/ seconds #.(dtime-factor :millennia)))
(defun time-to-centuries (seconds) (/ seconds #.(dtime-factor :centuries)))
(defun time-to-decades   (seconds) (/ seconds #.(dtime-factor :decades)))
(defun time-to-years     (seconds) (/ seconds #.(dtime-factor :year)))
(defun time-to-weeks     (seconds) (/ seconds #.(dtime-factor :weeks)))
(defun time-to-days      (seconds) (/ seconds #.(dtime-factor :days)))
(defun time-to-hours     (seconds) (/ seconds #.(dtime-factor :hours)))
(defun time-to-minutes   (seconds) (/ seconds #.(dtime-factor :minutes)))

#|
(defun millennia-to-time (millennia) (* millennia (* 60 60 24 (+ 365 1/4) 1000)))
(defun centuries-to-time (centuries) (* centuries (* 60 60 24 (+ 365 1/4) 100)))
(defun decades-to-time   (decades)   (* decades   (* 60 60 24 (+ 365 1/4) 10)))
(defun years-to-time     (year)      (* year      (* 60 60 24 (+ 365 1/4))))
(defun weeks-to-time     (weeks)     (* weeks     (* 60 60 24 7)))
(defun days-to-time      (days)      (* days      (* 60 60 24)))
(defun hours-to-time     (hours)     (* hours     (* 60 60)))
(defun minutes-to-time   (minutes)   (* minutes   60))

(defun time-to-millennia (seconds) (/ seconds (* 60 60 24 (+ 365 1/4) 1000)))
(defun time-to-centuries (seconds) (/ seconds (* 60 60 24 (+ 365 1/4) 100)))
(defun time-to-decades   (seconds) (/ seconds (* 60 60 24 (+ 365 1/4) 10)))
(defun time-to-years     (seconds) (/ seconds (* 60 60 24 (+ 365 1/4))))
(defun time-to-weeks     (seconds) (/ seconds (* 60 60 24 7)))
(defun time-to-days      (seconds) (/ seconds (* 60 60 24)))
(defun time-to-hours     (seconds) (/ seconds (* 60 60)))
(defun time-to-minutes   (seconds) (/ seconds 60))
|#

(defun dtime-round (time unit)
  "Round off DTIME to UNIT units."
  (ecase unit
    (:millennia
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :millennia))))
    (:centuries
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :centuries))))
    (:decades
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :decades))))
    ((:yr :years)
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :years))))
    ((:wk :weeks)
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :weeks))))
    ((:d :days)
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :days))))
    ((:h :hr :hours)
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :hours))))
    ((:m :min :minutes)
     (make-dtime
      :seconds (truncate (dtime-seconds time) #.(dtime-factor :minutes))))
    ((:s :seconds)
     (make-dtime :seconds (dtime-seconds time)))
    ;; @@@ This isn't right. It's not rounding?
    ((:ds :decisecond :deciseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (truncate (dtime-nanoseconds time) 100000000)))
    ((:cs :centisecond :centiseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (truncate (dtime-nanoseconds time) 10000000)))
    ((:ms :millisecond :milliseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (truncate (dtime-nanoseconds time) 1000000)))
    ((:µs :microsecond :microseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (truncate (dtime-nanoseconds time) 1000)))
    ((:ns :nanosecond :nanoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (dtime-nanoseconds time)))
    ((:ps :picosecond :picoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) 1000)))
    ((:fs :femtosecond :femtoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 6))))
    ((:as :attosecond :attoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 9))))
    ((:zs :zeptosecond :zeptoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 12))))
    ((:ys :yoctosecond :yoctoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 15))))
    ((:rs :rontosecond :rontoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 18))))
    ((:qs :quectosecond :quectoseconds)
     (make-dtime :seconds (dtime-seconds time)
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 21))))))

(defun dtime-normalize (dtime)
  "Return a new ‘dtime’ with excess whole seconds moved from the nanaoseconds
to the seconds component."
  (check-type dtime dtime)
  (dtime-normalize! (copy-dtime dtime)))

(defun dtime-normalize! (dtime)
  "Modify ‘dtime’ to move excess whole seconds from the nanaoseconds to the
seconds component."
  (check-type dtime dtime)
  (when (>= (dtime-nanoseconds dtime) +ns-per-sec+)
    (let ((secs-over (truncate (dtime-nanoseconds dtime) +ns-per-sec+)))
      (incf (dtime-seconds dtime) secs-over)
      (decf (dtime-nanoseconds dtime) (* secs-over +ns-per-sec+))))
  ;; @@@ Maybe we should also convert from fractional to integer components?
  dtime)

;; This demonstrates the extent to which I don't like to write repetitive code.
(eval-when (:compile-toplevel)
  (defun define-time-comparison-operator (op)
    (let ((name (symbolify (s+ "DTIME" op)))
	  (doc (s+ "Return true if DTIME1 " op " DTIME2, which should both be "
		   "a struct dtime.")))
      `(defun ,name (time1 time2)
	 ,doc
	 (or (,op (dtime-seconds time1) (dtime-seconds time2))
	     (and
	      (= (dtime-seconds time1) (dtime-seconds time2))
	      (,op (dtime-nanoseconds time1) (dtime-nanoseconds time2)))))))

  (defparameter %time-comp-op '(< <= > >=))

  (defmacro def-comp-ops ()
    (let ((forms
	   (loop :for o :in %time-comp-op
	      :collect (define-time-comparison-operator o))))
      `(progn ,@forms))))

(def-comp-ops)

(defgeneric dtime= (time1 time2)
  (:documentation
   "Return true if ‘time1’ = ‘time2’."))

(defmethod dtime= ((time1 dtime) (time2 dtime))
  "Return true if ‘time1’ = ‘time2’, which should both be a dtime."
  (and (= (dtime-seconds time1) (dtime-seconds time2))
       (= (dtime-nanoseconds time2) (dtime-nanoseconds time2))))

(defgeneric dtime-equal (time1 time2)
  (:documentation
   "Return true if ‘time1’ = ‘time2’."))

(defmethod dtime-equal ((time1 dtime) (time2 dtime))
  "Return true if ‘time1’ = ‘time2’, which should both be a dtime. The times are
normalized before comparison."
  (let ((t1 (dtime-normalize time1))
	(t2 (dtime-normalize time2)))
    (and (= (dtime-seconds t1) (dtime-seconds t2))
	 (= (dtime-nanoseconds t2) (dtime-nanoseconds t2)))))

(defun dtime/= (time1 time2)
  "Return true if TIME1 = TIME2, which should both be a struct time."
  (or (/= (dtime-seconds time1) (dtime-seconds time2))
      (/= (dtime-nanoseconds time1) (dtime-nanoseconds time2))))

(defun dtime+ (time1 time2)
  "Return the sum of TIME1 and TIME2, as a dtime."
  (let ((s (+ (dtime-seconds time1) (dtime-seconds time2)))
	(n (+ (dtime-nanoseconds time1) (dtime-nanoseconds time2))))
    (cond
      ((> n +ns-per-sec+)
       (incf s)
       (decf n +ns-per-sec+))
      ((= n +ns-per-sec+)
       (incf s)
       (setf n 0)))			; perhaps just saving a subtraction
    (make-dtime :seconds s
		:nanoseconds n)))

(defun dtime- (time1 time2)
  "Return the difference of TIME1 and TIME2, as a dtime."
  (let ((n (- (dtime-nanoseconds time1) (dtime-nanoseconds time2)))
	s)
    (cond
      ((minusp n)
       (incf n +ns-per-sec+)
       (setf s (- (dtime-seconds time1) 1 (dtime-seconds time2))))
      (t
       (setf s (- (dtime-seconds time1) (dtime-seconds time2)))))
    (make-dtime :seconds s
		:nanoseconds n)))

(defun dtime-zerop (dtime)
  "Return true if time is zero."
  (and (zerop (dtime-seconds dtime))
       (zerop (dtime-nanoseconds dtime))))

(defun dtime-minusp (dtime)
  "Return true if the time is less than zero."
  (or (minusp (dtime-seconds dtime))
      (and (zerop (dtime-seconds dtime))
	   (minusp (dtime-nanoseconds dtime)))))

(defun dtime-plusp (dtime)
  "Return true if the time is positive."
  (or (plusp (dtime-seconds dtime))
      (and (zerop (dtime-seconds dtime))
	   (plusp (dtime-nanoseconds dtime)))))

(defun dtime-min (t1 t2)
  "Return the minimum of T1 and T1."
  (if (dtime< t1 t2) t1 t2))

(defun dtime-max (t1 t2)
  "Return the maximum of T1 and T1."
  (if (dtime> t1 t2) t1 t2))

(defun tz-minutes (tz)
  (* 60 (nth-value 1 (truncate tz))))

(defun tz-hours (tz)
  (truncate tz))

(defun lisp-to-calendar-weekday (day)
  "Return the calendar day number for the Lisp day number."
  ;; Calendar days start from Sunday = 1, Lisp days start from Monday = 0
  (if (= day 6) 1 (+ day 2)))

(defun lisp-weekday-name (day &key abbrev)
  "Return the weekday name, given the Lisp decoded time DAY."
  (calendar:weekday-name (lisp-to-calendar-weekday day)
			 :format (if abbrev :abbreviated t)))

(defun date-string (&key (time (get-universal-time)) format
			 (gmt-p nil gmt-p-set) now)
  "Return a formated date string. A universal time can be provided with the
TIME keyword. FORMAT can be one of:
  :net          - an RFC822 formatted date.
  :iso          - an ISO 8601 date.
  :filename     - a format that works well for a user readable file name.
  :relative     - a relative time, sensible for people
  anything else - some format that Nibby likes.

If GMT-P is true, the date is in Grenwich Mean Time, otherwise it's in the 
current time zone. NOW is a universal time to base relative times off of, which
defaults to the current time."
; This makes a format default to GMT:
;  (when (and (not gmt-p-set) (find format '(:rfc822 :rfc :net)))
;    (setf gmt-p t))
  (declare (ignore gmt-p-set))
  (when (dtime-p time)
    (setf time (dtime-seconds time)))
  (multiple-value-bind (seconds minutes hours date month year day
				daylight-p zone)
      (if gmt-p
	  (decode-universal-time time 0)
	  (decode-universal-time time))
    (declare (ignore daylight-p))
    (case format
      ((:rfc822 :rfc :net)
       (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~c~2,'0d~2,'0d"
	       (lisp-weekday-name day)
	       date (calendar:month-name month year) year
	       hours minutes seconds
	       (if (< zone 0) #\+ #\-) (tz-hours zone) (tz-minutes zone)))
      ((:filename :file)
       (format nil "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"
	       year month date hours minutes seconds))
      ((:iso :iso8601)
       (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
	       year month date hours minutes seconds))
      (:relative
       ;; @@@ This of course has language issues, as well as precision issues.
       (let* ((now-ish (or now (get-universal-time)))
	      (Δ (- now-ish time))
	      (change (abs Δ))
	      (dir (cond ((> Δ 0) "ago") ((< Δ 0) "from now") (t "now")))
	      integer-change
	      units)
	 (cond
	   ((< change 90)
	    (setf units "second~:P"))
	   ((< (time-to-minutes change)   90)
	    (setf units "minute~:P"
		  change (time-to-minutes change)))
	   ((< (time-to-hours change)     36)
	    (setf units "hour~:P"
		  change (time-to-hours change)))
	   ((< (time-to-days change)      14)
	    (setf units "day~:P"
		  change (time-to-days change)))
	   ((< (time-to-weeks change)     10)
	    (setf units "week~:P"
		  change (time-to-weeks change)))
	   ((< (time-to-years change)     1)
	    (setf units "month~:P"
		  ;;change (time-to-months change)))
		  change (/ (time-to-days change) 30)))
	   ((< (time-to-decades change)   1)
	    (setf units "year~:P"
		  change (time-to-years change)))
	   ((< (time-to-centuries change) 1)
	    (setf units "decade~:P"
		  change (time-to-decades change)))
	   ((< (time-to-millennia change) 1)
	    (setf units "centur~:@P"
		  change (time-to-centuries change)))
	   ((< (time-to-millennia change) 100)
	    (setf units "~1:*millenni~[a~;um~:;a~]"
		  change (time-to-millennia change)))
	   (t
	    (setf units "long long")))
	 (if (zerop Δ)
	     "now"
	     (progn
	       (setf integer-change (round change))
	       (format nil (s+ "~d " units " ~a")
		       integer-change dir)))))
      (otherwise
       (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	       year month date hours minutes seconds)))))

(defun %format-date (format values &key (time nil)
				     (stream nil)
				     (gmt-p nil))
  "Call #'format with FORMAT and the given date fields in VALUES. 
VALUES is a sequence of any of the following keywords:
  :seconds :minutes :hours :date :month :year :day :daylight-p :zone
  :day-abbrev :month-abbrev :year-abbrev :12-hours :am :pm :weekday :day-name
Some abbreviations of the keywords are accepted, like :hrs :min :sec.
Note that :day is the day of the week number and :date is the day of the month."
  (setf time
	(typecase time
	  (null	 (get-universal-time))
	  (dtime (dtime-seconds time))
	  (t	 time)))
  (multiple-value-bind (seconds minutes hours date month year day
			daylight-p zone)
      (if gmt-p
	  (decode-universal-time time 0)
	  (decode-universal-time time))
    (let* ((args
	     (loop :for v :in values
	       :collect
	       (etypecase v
		 (keyword
		  (case v
		    (:day-abbrev
		     (lisp-weekday-name day :abbrev t))
		    ((:weekday :day-name)
		     (lisp-weekday-name day))
		    ((:month-name)
		     (calendar:month-name month year))
		    ((:month-abbrev :mon-abbrev)
		     (calendar:month-name month year :format :abbreviated))
		    ((:year-abbrev :yr-abbrev)
		     (format nil "~2,'0d" (mod year 100)))
		    (:std-zone
		     (format nil "~c~2,'0d~2,'0d"
			     (if (< zone 0) #\+ #\-)
			     (tz-hours zone) (tz-minutes zone)))
		    ((:12-hours :12-hour :12-hrs :12-hr
		      :12hours :12hour :12hrs :12hr)
		     (let ((p (mod hours 12)))
		       (if (zerop p) 12 p)))
		    ((:am :pm :am/pm :am-pm)
		     (if (> hours 12) "PM" "AM"))
		    (otherwise
		     (case v
		       ((:seconds :second :sec) seconds)
		       ((:minutes :minute :min) minutes)
		       ((:hours :hour :hrs :hr) hours)
		       (:date date)
		       ((:month :mon) month)
		       ((:year :yr) year)
		       (:day day)	; @@@ really easy to mistake for :date
		       (:zone zone)
		       (:daylight-p daylight-p)
		       (otherwise
			(error "Unknown format-date keyword ~s." v))))))
		 (t v)))))
      ;; @@@ Is this really right? or should we only do it for :std-zone?
      (when daylight-p (decf zone))
      (apply #'format stream format args))))

;; (ulet (s1 s2 s3) body) ->
;; (let ((s1 (gensym)) (s2 (gensym)) (s3 (gensym))) body)

;; @@@ should we get rid of this and replace with the function version?
(defmacro format-date (format (&rest values)
		       &key (time nil)
			 (stream nil)
			 (gmt-p nil))
  "Call #'format with FORMAT and the given date fields in VALUES. 
VALUES is a sequence of any of the following keywords:
  :seconds :minutes :hours :date :month :year :day :daylight-p :zone
  :day-abbrev :month-abbrev :year-abbrev :12-hours :am :pm :weekday :day-name
Some abbreviations of the keywords are accepted, like :hrs :min :sec.
Note that :day is the day of the week number and :date is the day of the month."
  ;; (dlib::with-unique-names
  ;;     (seconds minutes hours date month year day daylight-p zone)
  (let* ((seconds (gensym "SECONDS"))
	 (minutes (gensym "MINUTES"))
	 (hours (gensym "HOURS"))
	 (date (gensym "DATE"))
	 (month (gensym "MONTH"))
	 (year (gensym "YEAR"))
	 (day (gensym "DAY"))
	 (daylight-p (gensym "DAYLIGHT-P"))
	 (zone (gensym "ZONE"))
	 (t1 (gensym "TIME-1"))
	 (t2 (gensym "TIME-2"))
	 (args (loop :for v :in values
		  :collect
		  (etypecase v
		    (keyword
		     (case v
		       (:day-abbrev `(lisp-weekday-name ,day :abbrev t))
		       ((:weekday :day-name)
			`(lisp-weekday-name ,day))
		       ((:month-name)
			`(calendar:month-name ,month ,year))
		       ((:month-abbrev :mon-abbrev)
			`(calendar:month-name ,month ,year :format :abbreviated))
		       ((:year-abbrev :yr-abbrev)
			`(format nil "~2,'0d" (mod ,year 100)))
		       (:std-zone
		       ;; 	`(format nil "~c~2,'0d~2,'0d"
		       ;; 		 (if (< ,zone 0) #\+ #\-)
			;; 		 (tz-hours ,zone) (tz-minutes ,zone)))
			`(format nil "~c~2,'0d~2,'0d"
				 (if (< ,zone 0) #\+ #\-)
				 (tz-hours ,zone) (tz-minutes ,zone)))
		       ((:12-hours :12-hour :12-hrs :12-hr
				   :12hours :12hour :12hrs :12hr)
			`(let ((p (mod ,hours 12))) (if (zerop p) 12 p)))
		       ((:am :pm :am/pm :am-pm)
			`(if (> ,hours 12) "PM" "AM"))
		       (otherwise
			(case v
			  ((:seconds :second :sec) seconds)
			  ((:minutes :minute :min) minutes)
			  ((:hours :hour :hrs :hr) hours)
			  (:date date)
			  ((:month :mon) month)
			  ((:year :yr) year)
			  (:day day)	; @@@ really easy to mistake for :date
			  (:zone zone)
			  (:daylight-p daylight-p)
			  (otherwise
			   (error "Unknown format-date keyword ~s." v))))))
		    (t v)))))
    `(let* ((,t1 ,time)
	    (,t2 (if (dtime-p ,t1) (dtime-seconds ,t1) ,t1)))
       (multiple-value-bind (,seconds ,minutes ,hours ,date ,month ,year ,day
			     ,daylight-p ,zone)
	 ;; One of the branches of gmt-p will be unreachable.
	 (locally 
	   #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	   (if ,gmt-p
	       (decode-universal-time (or ,t2 (get-universal-time)) 0)
	       (decode-universal-time (or ,t2 (get-universal-time)))))
	 (declare (ignorable ,seconds ,minutes ,hours ,date ,month ,year ,day
			     ,daylight-p ,zone))
	 ;; @@@ Is this really right? or should we only do it for :std-zone?
	 (when ,daylight-p (decf ,zone))
	 (format ,stream ,format ,@args)))))

(defun simple-parse-time (str)
  "Parse a string into a universal-time. Format is:
HH [ ':' MM [ ':' SS ] ] [ PM | AM ]
The date part is considered to be the current date."
  (let (hour (min 0) (sec 0) (i 0) am-pm (len (length str)))
    (flet ((done ()
	     (multiple-value-bind (seconds minutes hours date month year day
					   daylight-p zone) (get-decoded-time)
	       (declare (ignore seconds minutes hours day daylight-p zone))
	       (encode-universal-time
		sec min hour date month year))))
      (multiple-value-setq (hour i) (parse-integer str :junk-allowed t))
      (when (not hour)
	(error "Time must start with a number."))
      (when (or (> hour 23) (< hour 0))	; fuck 24 as midnight
	(error "Hour must be in the range 0 to 23."))
      (if (>= i len)
	  (return-from simple-parse-time (done)) ;; only gave the hour
	  (progn
	    (cond
	      ((eql (aref str i) #\:)	; gonna give mintues
	       (incf i)
	       (multiple-value-setq (min i)
		 (parse-integer str :junk-allowed t :start i))
	       (when (not min)
		 (error "Minutes must be a number."))
	       (when (or (> min 59) (< min 0))
		 (error "Minutes must be in the range 0 to 59.")))
	      ;; @@@ This doesn't handle space after the hour, eg "1 am"
	      ((and (< (1+ i) len)	; give just am/pm
		    (position (aref str i) '(#\a #\p) :test #'equalp)
		    (equalp (aref str (1+ i)) #\m))
	       (setf am-pm (char-downcase (aref str i))))
	      (t
	       ;;(format t "i=~a len=~a~%" i len)
	       (error "Hour must be followed by either :MM or AM/PM.")))))
      (cond
	((and (< i len) (eql (aref str i) #\:))	; seconds
	 (incf i)
	 (multiple-value-setq (sec i)
	   (parse-integer str :junk-allowed t :start i))
	 (when (not sec)
	   (error "Second colon must be followed by number of seconds.")))
	((and (< (1+ i) len)
	      (position (aref str i) '(#\a #\p) :test #'equalp)
	      (equalp (aref str (1+ i)) #\m))
	 (setf am-pm (char-downcase (aref str i)))))
      (when (or (< sec 0) (> sec 59))
	(error "Seconds must be in the range 0 to 59."))
      (when (eql am-pm #\p)
	(when (> hour 12)
	  (error "Hour must be less than 13 with AM/PM."))
	(incf hour 12))
      (done))))

#|
(defun tokenize-date (string)
  ""
  (let* ((len (length string))
	 (i 0)
	 (result)
	 (words (osplit
    (loop
      :do
      whe
      :while (< i len))


(defun simple-parse-date (string)
  ""
  (
|#

(defun describe-duration (dtime &key stream)
  "Describe a duration in time units to ‘stream’. If stream is NIL (the default),
return the description as a string."
  (let* ((out (or stream (make-string-output-stream)))
	 (norm (dtime-normalize dtime))
	 (seconds (dtime-seconds norm))
	 (nanos (dtime-nanoseconds norm))
	 (once nil))
    ;; Cut the time up into big units.
    (loop :with rounded :and factor
      :for u :across *time-super-units*
      :do (setf factor (dtime-factor (caar u))
		rounded (truncate seconds factor))
      :when (plusp rounded)
      :do
	 (if once
	       (write-char #\space out)
	       (setf once t))
	 (format out "~d ~(~a~)" rounded (if (> rounded 1) (caar u) (cadar u)))
	 (decf seconds (* rounded factor)))
    ;; Report in the largest integer sub-unit down to ns.
    (when (not (zerop nanos))
      (loop :with rounded :and divisor
        :for u :across (subseq *time-sub-units* 0 6) :do
	(when (plusp (truncate nanos
			       (setf divisor
				     (/ +ns-per-sec+ (dtime-divisor (caar u))))))
	  (when once
	    (write-char #\space out))
	  (format out "~d ~(~a~)"
		  (setf rounded (truncate nanos divisor))
		  (if (> rounded 1) (caar u) (cadar u)))
	  (decf nanos (* rounded divisor)))))
    (when (not stream)
      (get-output-stream-string out))))

;; End
