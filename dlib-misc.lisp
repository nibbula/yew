;;
;; dlib-misc.lisp - Library of miscellaneous useful functions.
;;

;; See dlib.lisp for the most essential stuff.
;; This is for things that are nice, but not essential.
;; Things for mostly interactive use are in dlib-interactive.
;;
;; This is mostly separate from dlib so I can depend on other stuff like OPSYS,
;; and to keep dlib minimal.
;;
;; I don't like the name “MISC”. Let’s think of something better.

(defpackage :dlib-misc
  (:use :cl :dlib :opsys :char-util :glob :collections
	#+use-re :re #-use-re :ppcre)
  ;; Also has an inplicit dependency on ASDF.
  (:documentation "More generally dubious miscellaneous functions.")
  (:export
   ;; general
   #:randomize-vector
   #:parse-integer-with-radix
   #:tree-ify
   #:un-tree-ify
   #:oquote-format

   ;; time
   #:date-string
   #:format-date
   #:simple-parse-time
   #:millennia-to-time #:centuries-to-time #:decades-to-time #:years-to-time
   #:weeks-to-time #:days-to-time #:hours-to-time #:minutes-to-time
   #:time-to-millennia #:time-to-centuries #:time-to-decades #:time-to-years
   #:time-to-weeks #:time-to-days #:time-to-hours #:time-to-minutes
   #:dtime #:dtime-seconds #:dtime-nanoseconds #:make-dtime #:dtime-p
   #:get-dtime #:dtime-round #:make-dtime-as #:dtime-to
   #:dtime= #:dtime/= #:dtime< #:dtime> #:dtime<= #:dtime>= #:dtime+ #:dtime-
   #:dtime-zerop #:dtime-plusp #:dtime-minusp #:dtime-min #:dtime-max

   ;; hooks
   #:add-hook
   #:remove-hook
   #:run-hooks

   ;; printing
   #:untabify
   #:justify-text
   #:print-properties
   #:print-values
   #:print-values*
   #:print-values-of
   #:print-columns-sizer
   #:print-columns-array
   #:print-columns
   #:print-size

   ;; spin
   #:*default-spin-string*
   #:*spin-strings*
   #:*plain-spin-string*
   #:*unicode-disk-spin-string*
   #:*unicode-scan-spin-string*
   #:*unicode-digit-spin-string*
   #:*unicode-sparkle-spin-string*
   #:*unicode-braille-spin-string*
   #:*unicode-square-spin-string*
   #:*emoji-spin-string*
   #:spin
   #:with-spin

   ;; packages
   #:*loadable-packages*
   #:loadable-packages
   #:*quickloadable-systems*
   #:quickloadable-systems
   #:clear-loadable-package-cache
   #:ensure-package
   #:unintern-conflicts
   #:d-autoload

   ;; I/O
   #:safe-file-length
   #:slurp
   #:confirm
   #:get-file-local-variables
   )
)
(in-package :dlib-misc)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1)
		   (compilation-speed 2)))

#+clisp
(defun load-again (module &rest other-args &key &allow-other-keys)
  "Load without complaining about constant redefinitions."
  (let ((custom:*suppress-check-redefinition* t))
    (apply 'load module other-args)))

;; Maybe this should be put somewhere else, since it's seldom used.
(defun randomize-vector (vector &key (factor 3))
  "Randomize the order of elements in an vector. FACTOR is how many random
swaps to do times the length of the vector."
  (when (not (vectorp vector))
    (error "VECTOR must be a vector, not a ~a." (type-of vector)))
  (let* ((len (length vector))
	 (swaps (* factor len)))
    (loop :for i :from 0 :to swaps
       :do (let* ((a (random len))
		  (b (random len)))
	     (rotatef (aref vector a) (aref vector b)))))
  vector)

;; @@@ This should probably support the same args as PARSE-INTEGER.
(defun parse-integer-with-radix (str)
  "Parse an integer from a string, allowing for a Lisp radix prefix."
  (cond
    ((and str (> (length str) 2)
	  (char= (char str 0) #\#))
     (cond
       ((position (char str 1) #(#\b #\o #\x) :test #'char-equal)
	(parse-integer str :junk-allowed nil :start 2
		       :radix (cond
				((char-equal (char str 1) #\b) 2)
				((char-equal (char str 1) #\o) 8)
				((char-equal (char str 1) #\x) 16)
				(t 10))))
       ((digit-char-p (char str 1))
	(let (radix start)
	  (cond
	    ((char-equal #\r (char str 2))
	     (setf radix (parse-integer str :start 1 :end 2 :junk-allowed nil)
		   start 3))
	    ((and (digit-char-p (char str 2))
		  (char-equal #\r (char str 3)))
	     (setf radix (parse-integer str :start 1 :end 3 :junk-allowed nil)
		   start 4))
	    (t
	     (error "Malformed radix in integer ~a." str)))
	  (parse-integer str :junk-allowed nil :start start :radix radix)))
       (t
	(error "Malformed integer ~a." str))))
    (t
     (parse-integer str :junk-allowed nil))))

;;
;; foo bar quux
;; (("foo" ()) ("bar" ()) ("quux" ()))
;;
;; foo foobar foobarquux
;; (("foo" ()) ("bar" ()) ("quux" ()))

(defun seqify (s) (or (and (typep s 'sequence) s) (princ-to-string s)))

(defun put-in-tree (s sub-tree)
  (let (added thing new spot (ss (seqify s)))
    (loop :for node :in sub-tree :do
       (setf thing (car node)
	     spot (mismatch ss thing))
       (when (not (zerop spot))
	 (if (> (length thing) (length ss))
	     (setf (car node) ss
		   new (subseq thing spot))
	     (setf new (subseq ss spot)))
	 (setf (cdr node)
	       (if (not (cdr node))
		   (list new ())
		   (put-in-tree new (cdr node))))
	 (format t "zerp ~s~%" (cdr node))
	 (setf added t)))
    (if (not added)
	(progn
	  (format t "plonk ~s~%" ss)
	  (if (equal sub-tree '(nil))
	      (setf sub-tree (list (list ss ())))
	      (push (list ss ()) sub-tree)))
	sub-tree)))

;; (defun put-in-tree (string sub-tree)
;;   (loop :for c :across string
;;      :do

(defun tree-ify (list)
  (let ((tree '()))
    (loop :for s :in list
       :do (setf tree (put-in-tree s tree))
       (format t "tree: ~s~%" tree)
       )
    (nreverse tree)))

(defun un-tree-ify (list &optional (prefix ""))
  (if list
      (if (and (consp list) (not (equal '(nil) list)))
	  (loop :for i :in list :do
	     (if (consp i)
		 (progn
		   (when (consp (cdr i))
		     (format t "-> ~s~%" (s+ prefix (car i)))
		     (un-tree-ify (cdr i) (s+ prefix (car i)))))
		 (format t "<- ~s~%" (s+ prefix i))))
	  ;;(format t "<. ~s~%" (s+ prefix list))
	  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time

;; @@@ So where would we get this from for other languages?
;; I suppose we could mine them from strftime.
(defparameter *day-abbrevs* #("Mon" "Tue" "Wed"
			      "Thu" "Fri" "Sat" "Sun")) ; @@@ i18n

(defparameter *weekday* #("Monday" "Tuesday" "Wednesday"
			  "Thursday" "Friday" "Saturday" "Sunday")) ; @@@ i18n

(defparameter *month-abbrevs* #("Jan" "Feb" "Mar" "Apr"
				"May" "Jun" "Jul" "Aug"
				"Sep" "Oct" "Nov" "Dec")) ; @@@ i18n

(defparameter *month* #("January" "February" "March" "April"
			"May" "June" "July" "August"
			"September" "October" "November" "December")) ; @@@ i18n

(defun tz-minutes (tz)
  (* 60 (nth-value 1 (truncate tz))))

(defun tz-hours (tz)
  (truncate tz))

(defun date-string (&key (time (get-universal-time)) format
			 (gmt-p nil gmt-p-set) now)
  "Return a formated date string. A universal time can be provided with the
TIME keyword. FORMAT can be one of:
  :net          - an RFC822 formatted date.
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
  (multiple-value-bind (seconds minutes hours date month year day
				daylight-p zone)
      (if gmt-p
	  (decode-universal-time time 0)
	  (decode-universal-time time))
    (declare (ignore daylight-p))
    (case format
      ((:rfc822 :rfc :net)
       (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~c~2,'0d~2,'0d"
	       (aref *day-abbrevs* day)
	       date (aref *month-abbrevs* (1- month)) year
	       hours minutes seconds
	       (if (< zone 0) #\+ #\-) (tz-hours zone) (tz-minutes zone)))
      (:filename
       (format nil "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"
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

;; (ulet (s1 s2 s3) body) ->
;; (let ((s1 (gensym)) (s2 (gensym)) (s3 (gensym))) body)

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
  (dlib::with-unique-names
      (seconds minutes hours date month year day daylight-p zone)
    (let ((args (loop :for v :in values
		   :collect
		   (etypecase v
		     (keyword
		      (case v
			(:day-abbrev `(aref *day-abbrevs* ,day))
			((:weekday :day-name)
			 `(aref *weekday* ,day))
			((:month-name)
			 `(aref *month* ,month))
			((:month-abbrev :mon-abbrev)
			 `(aref *month-abbrevs* (1- ,month)))
			((:year-abbrev :yr-abbrev)
			 `(format nil "~2,'0d" (mod ,year 100)))
			(:std-zone
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
			    (error "Unknown format-date keyword ~s." v))))))))))
      `(multiple-value-bind (,seconds ,minutes ,hours ,date ,month ,year ,day
				      ,daylight-p ,zone)
	   ;; One of the branches of gmt-p will be unreachable.
	   (locally 
	       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	       (if ,gmt-p
		   (decode-universal-time (or ,time (get-universal-time)) 0)
		   (decode-universal-time (or ,time (get-universal-time)))))
	 (declare (ignorable ,seconds ,minutes ,hours ,date ,month ,year ,day
			     ,daylight-p ,zone))
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

;; The stupid base unit of time is seconds.
;; Anything after weeks is bogus because years are variable and poorly defined!
;; But for this bullcrap, we use the Jullian year which is exactly 365.25.
;; I know I'm gonna have to end up writing that time lib (which should of
;; course be properly integrated with a units system).
(defun millennia-to-time (millennia) (* millennia (* 60 60 24 (+ 365 1/4) 1000)))
(defun centuries-to-time (centuries) (* centuries (* 60 60 24 (+ 365 1/4) 100)))
(defun decades-to-time   (decades)   (* decades   (* 60 60 24 (+ 365 1/4) 10)))
(defun years-to-time     (year)      (* year      (* 60 60 24 (+ 365 1/4))))
(defun weeks-to-time     (weeks)     (* weeks     (* 60 60 24 7)))
(defun days-to-time      (days)      (* days      (* 60 60 24)))
(defun hours-to-time     (hours)     (* hours     (* 60 60)))
(defun minutes-to-time   (minutes)   (* minutes   60))

(defun time-to-millennia (millennia) (/ millennia (* 60 60 24 (+ 365 1/4) 1000)))
(defun time-to-centuries (centuries) (/ centuries (* 60 60 24 (+ 365 1/4) 100)))
(defun time-to-decades   (decades)   (/ decades   (* 60 60 24 (+ 365 1/4) 10)))
(defun time-to-years     (years)     (/ years     (* 60 60 24 (+ 365 1/4))))
(defun time-to-weeks     (weeks)     (/ weeks     (* 60 60 24 7)))
(defun time-to-days      (days)      (/ days      (* 60 60 24)))
(defun time-to-hours     (hours)     (/ hours     (* 60 60)))
(defun time-to-minutes   (minutes)   (/ minutes   60))

;; These, for lack of a better thing, these operate on the dual time from opsys.
;; To do better we probably need arbitrary precision floats.
;; [Could use mpfr or bfloats from maxima] (see wip/units.lisp)

;; @@@ Revise the above functions to handle a dtime too.

(defstruct dtime
  (seconds 0 :type integer)
  (nanoseconds 0 :type integer))

(defun get-dtime ()
  "Return the current time as a new DTIME."
  (multiple-value-bind (s n) (get-time)
    (make-dtime :seconds s :nanoseconds n)))

(defparameter *time-units*
  #(((:yoctoseconds  :ys :yoctosecond) #.(expt 10 24))
    ((:zeptoseconds  :zs :zeptosecond) #.(expt 10 21))
    ((:attoseconds   :as :attosecond)  #.(expt 10 18))
    ((:femtoseconds  :fs :femtosecond) #.(expt 10 15))
    ((:picoseconds   :ps :picosecond)  #.(expt 10 12))
    ((:nanoseconds   :ns :nanosecond)  #.(expt 10 9))
    ((:microseconds  :µs :microsecond) #.(expt 10 6))
    ((:milliseconds  :ms :millisecond) #.(expt 10 3))
    ((:centiseconds  :cs :centisecond) #.(expt 10 2))
    ((:deciseconds   :ds :decisecond)  #.(expt 10 1))
    ((:seconds       :s  :second)      #.(expt 10 0))))

(defconstant +ns-per-sec+ (expt 10 9)
  "The number of nanoseconds in a second.")

(defun dtime-unit-divisor (unit)
  (second (find-if (_ (member unit _)) *time-units* :key #'first)))

(defun make-dtime-as (value unit)
  (let ((divvy (dtime-unit-divisor unit)))
    (when (not divvy)
      (error "Unknown unit ~s~%" unit))
    (multiple-value-bind (s leftover) (truncate value divvy)
      (make-dtime :seconds s
		  :nanoseconds
		  (truncate (* leftover (/ +ns-per-sec+ divvy)))))))

(defun dtime-to (dtime unit)
  (let ((multy (dtime-unit-divisor unit)))
    (when (not multy)
      (error "Unknown unit ~s~%" unit))
    (+ (* multy (dtime-seconds dtime))
       (/ (dtime-nanoseconds dtime) (/ +ns-per-sec+ multy)))))

(defun dtime-round (time unit)
  "Round off DTIME to UNIT units."
  (ecase unit
    (:millennia
     (make-dtime :seconds (time-to-millennia (dtime-seconds time))))
    (:centuries
     (make-dtime :seconds (time-to-centuries (dtime-seconds time))))
    (:decades
     (make-dtime :seconds (time-to-decades   (dtime-seconds time))))
    ((:yr :years)
     (make-dtime :seconds (time-to-years     (dtime-seconds time))))
    ((:wk :weeks)
     (make-dtime :seconds (time-to-weeks     (dtime-seconds time))))
    ((:d :days)
     (make-dtime :seconds (time-to-days      (dtime-seconds time))))
    ((:h :hr :hours)
     (make-dtime :seconds (time-to-hours     (dtime-seconds time))))
    ((:m :min :minutes)
     (make-dtime :seconds (time-to-minutes   (dtime-seconds time))))
    ((:s :seconds)
     (make-dtime :seconds (dtime-seconds time)))
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
		 :nanoseconds (* (dtime-nanoseconds time) (expt 10 15))))))

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

(defun dtime= (time1 time2)
  "Return true if TIME1 = TIME2, which should both be a struct time."
  (and (= (dtime-seconds time1) (dtime-seconds time2))
       (= (dtime-nanoseconds time1) (dtime-nanoseconds time2))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks

;; Hooks - a simple, old-fashioned convention.

(defmacro add-hook (var func)
  "Add a hook function FUNC to the hook variable VAR."
  `(pushnew ,func ,var))

(defmacro remove-hook (var func)
  "Remove hook function FUNC from the hook variable VAR."
  `(setf ,var (delete ,func ,var)))

(defun run-hooks (var &rest args)
  (loop :for f :in var
     :do (apply f args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing

(defun untabify (line &optional (col 0))
  "Return a new string with all tabs in the LINE to the appropriate number
of spaces, preserving columns. Assumes that LINE is starting in column COL,
which defaults to zero."
  (with-output-to-string (str)
    (loop
       :for c :across line
       :do
       (if (eql c #\tab)
           (loop :for i :from 0 :below (- 8 (rem col 8))
              :do
              (write-char #\space str)
              (incf col))
           (progn
             (write-char c str)
             (incf col))))))

;; This slow and very consing, but remarkably succinct. It has the benefit of
;; knowing the stream's current column by way of the implementation's stream
;; machinery. It might be nice if it got the COLS from the same place.

(defun old-justify-text (s &key (cols 80) (stream *standard-output*)
			 (prefix "") (separator #\space)
			 omit-first-prefix)
  "Print the string S right justified by words, into COLS characters wide,
on the stream STREAM.
SEPARATOR is a character or string which separates the input words.
PREFIX is printed in front of each line.
If OMIT-FIRST-PREFIX is true, don't print the first prefix."
  (format stream (format nil "~a~a~a~d~a"
			 "~a~{~<~%" prefix "~1," cols ":;~a~> ~}")
	  (if omit-first-prefix "" prefix)
	  (split-sequence separator s :omit-empty t)))

;; This is much faster.

;; @@@ Problems:
;; It doesn't know what column it's starting at (unlike the old version),
;; and therefore wrongly assumes zero.
;; Assumes fixed width characters, without all the unicode nonsense.

;; Keep this around in case we need a faster simple character only version.
#|
(defun %justify-text-OLD (text &key (cols 80) (stream *standard-output*)
			     (prefix "") (separator #\space) omit-first-prefix
			     (start-column 0))
  (declare (optimize (speed 3) (safety 0) (debug 3) (space 0)
		     (compilation-speed 0))
	   (type simple-string text prefix)
	   (type fixnum cols))
  ;;(check-type text simple-string)
  (let ((len             (length text))
	(i               0)
	(last-word-start 0)
	;;(line-start	 0)
	(column          start-column))
    (declare (type fixnum len i last-word-start column))
    (flet ((write-word (final)
	     ;;(dbugf :justify-text "col ~d " column)
	     (if (< column cols)
		 (progn
		   ;;(dbugf :justify-text "space")
		   (write-string text stream :start last-word-start :end i)
		   (when (< column (- cols 2))
		     (when (not final)
		       (write-char separator stream))
		     (incf column)))
		 (progn
		   ;;(dbugf :justify-text "newline")
		   (write-char #\newline stream)
		   (if prefix
		       (progn
			 (write-string prefix stream)
			 (setf column (length prefix)))
		       (setf column 0))
		   (write-string text stream :start last-word-start :end i)
		   (write-char separator stream)
		   (incf column (+ (- i last-word-start) 2))))
	     ;;(dbugf :justify-text "~%")
	     ))
      (when (and prefix (not omit-first-prefix))
	(write-string prefix stream)
	(setf column (length prefix)))
      (loop
	 :while (< i len)
	 :do
	 (cond
	   ((char= (aref text i) #\Newline)
	    ;; (when (not (zerop last-word-start))
	    ;;   (write-char #\space stream))
	    ;; (write-string text stream :start last-word-start :end i)
	    (write-word nil)
	    (write-char #\newline stream)
	    (if prefix
		(progn
		  (write-string prefix stream)
		  (setf column (length prefix)))
		(setf column 0))
	    (incf i)
	    (setf last-word-start i
		  ;; line-start i
		  ))
	   ((char= (aref text i) separator)
	    (write-word nil)
	    (incf i)
	    ;; eat multiple spaces
	    (loop :while (and (< i len) (char= (aref text i) separator))
	       :do (incf i))
	    (setf last-word-start i)
	    ;;(when (zerop column)
	    ;;  (setf line-start i))
	    )
	   (t
	    (incf i)
	    (incf column))))
      (when (< last-word-start i)
	(write-word t)))))
|#

;; This can handle some wide and fatchars, but it only gives reasonable output
;; for the the subset of unicode scripts that have a non-context-dependant
;; word separator character (a.k.a. western style). This is certainly not an
;; implementation of the unicode line breaking algorithm. Perhaps someday we
;; can make it work in a more universal text rendering system, but it would be
;; nice to at least have it support anything that we can reasonably render in
;; a character grid system (i.e. terminals).
(defun %justify-text (text &key (cols 80) (stream *standard-output*)
			     (prefix "") (separator #\space) omit-first-prefix
			     (start-column 0))
  ;; (declare (optimize (speed 3) (safety 0) (debug 3) (space 0)
  ;; 		     (compilation-speed 0))
  (declare (optimize (speed 0) (safety 3) (debug 3) (space 0)
   		     (compilation-speed 0))
	   (type fixnum cols))
  (let ((len             (olength text))
	(i               0)
	(last-word-start 0)
	(first-word      t)
	(column          start-column)
	(sep-len	 (display-length separator))
	(prefix-len      (display-length prefix)))
    (declare (type fixnum len i last-word-start column))
    (flet ((write-word ()
	     (dbugf :justify-text "col ~d " column)
	     (if (< (+ column (if first-word 0 sep-len)) cols)
		 (progn
		   ;;(dbugf :justify-text "space")
		   (when (not first-word)
		     (princ separator stream)
		     (incf column sep-len))
		   (setf first-word nil)
		   (princ (osubseq text last-word-start (min i len)) stream))
		 (progn
		   (dbugf :justify-text "newline")
		   (princ #\newline stream)
		   (if prefix
		       (progn
			 (princ prefix stream)
			 (setf column prefix-len))
		       (setf column 0))
		   (princ (osubseq text last-word-start i) stream)
		   (incf column (+ (display-length
				    (osubseq text last-word-start i))))))
	     (dbugf :justify-text "~%")
	     ))
      (when (and prefix (not omit-first-prefix))
	(princ prefix stream)
	(setf column (+ prefix-len start-column)))
      (loop
	 :while (< i len)
	 :do
	 (dbugf :justify-text "~s: " i)
	 (cond
	   ((char= (simplify-char (oelt text i)) #\Newline)
	    (write-word)
	    (write-char #\newline stream)
	    (if prefix
		(progn
		  (princ prefix stream)
		  (setf column prefix-len))
		(setf column 0))
	    (setf first-word t)
	    (incf i)
	    (setf last-word-start i))
	   ((char= (simplify-char (oelt text i)) separator)
	    (write-word)
	    (incf i)
	    ;; eat multiple spaces
	    (loop :while (and (< i len)
			      (char= (simplify-char (oelt text i)) separator))
	       :do (incf i))
	    (setf last-word-start i))
	   (t
	    (incf column (display-length (oelt text i)))
	    (incf i))))
      (when (< last-word-start i)
	(dbugf :justify-text "last word ~s ~s~%" i column)
	(write-word)))))

(defun justify-text (text &key (cols 80) (stream *standard-output*)
			    (prefix "") (separator #\space) omit-first-prefix
			    (start-column 0))
  "Try to output substrings of TEXT to STREAM, separated by SEPARATOR, so that
they fit in COLS columns of fixed sized characters. Output PREFIX before each
line. If OMIT-FIRST-PREFIX is true, don't output the prefix on the first line.
If STREAM is nil, return a string of the output."
  (if stream
      (%justify-text text :cols cols :stream stream
		     :prefix prefix :separator separator
		     :omit-first-prefix omit-first-prefix
		     :start-column start-column)
      (with-output-to-string (output)
	(%justify-text text :cols cols :stream output
		       :prefix prefix :separator separator
		       :omit-first-prefix omit-first-prefix
		       :start-column start-column))))

(defun print-properties (prop-list &key (right-justify nil) (de-lispify t)
				     (stream t))
  "Print a set of names and values nicely in two vertical columns."
  (let ((label-length (loop :for p :in prop-list
			 :maximize (length (princ-to-string (car p))))))
    (flet ((niceify (s)
	     (string-capitalize
		    (substitute #\space #\_
				(substitute #\space #\- s)))))
      (loop :with name :and value
	 :for p :in prop-list :do
	 (setf name (car p)
	       value (if (and (cdr p) (listp (cdr p))) (cadr p) (cdr p)))
	 (format stream (if right-justify "~v@a: ~a~%" "~va: ~a~%")
		   label-length
		   (if de-lispify
		       (niceify (princ-to-string name))
		       (string-downcase (princ-to-string name)))
		   value)))))

(defun print-values (value-list &optional (stream t))
  "Print a vertical list of values. VALUE-LIST is a list of symbols whose
values are printed. Symbols in the VALUE-LIST must either be dynamic variables
of fbound to a function, which called with no arguments to get the value.
Use PRINT-VALUES* if you want to print lexical variables."
  (let ((max-len (loop :for f :in value-list
		       :maximize (length (string f)))))
    (loop :for f :in value-list :do
       (format stream "~va  : ~s~%"
	       max-len (string-capitalize f)
	       (if (fboundp f)
		   (apply f nil)
		   (symbol-value f))))))

;; This can do everything the unstarred version can do, but it causes
;; potential over-abundant code generation, which is why I'm keeping both
;; versions.
(defmacro print-values* (value-list &optional (stream t))
  "Print a vertical list of values. VALUE-LIST is an unquoted list of symbols
whose values are printed. If the symbol is FBOUND to a function it is called
with no arguments to get the value. Unlike PRINT-VALUES, this can print
lexical variables."
  (let* ((max-len (loop :for f :in value-list
		     :maximize (length (string f))))
	 (spudgers
	  (loop :with snork
	     :for f :in value-list
	     :do (setf snork (if (fboundp f) `(apply ',f nil) f))
	     :collect
	     `(format ,stream "~va  : ~s~%" ,max-len (string-capitalize ',f)
		      ,snork))))
    `(progn ,@spudgers)))

(defun print-values-of (value-list object &key (stream t) prefix
					    (value-format "~S"))
  "Print a vertical list of results of applying functions to OBJECT.
VALUE-LIST is a list of symbols who are functions of one argument, which can
be OBJECT. PREFIX is optionally a prefix to remove from symbols in the value
list before printing. VALUE-FORMAT is a format string with which the value
is printed. This is useful for printing, e.g. slots of a structure or class."
  (let* ((fixed-list (if prefix
			 (mapcar (_ (remove-prefix (string _) (string prefix)))
				 value-list)
			 value-list))
	 (max-len (loop :for f :in fixed-list
		     :maximize (length (string f))))
	 (format-string (if value-format
			    (s+ "~va  : " value-format "~%")
			    "~va  : ~s~%")))
    (loop :for f :in value-list :do
       (format stream format-string
	       max-len (string-capitalize
			(if prefix
			    (remove-prefix (string f) (string prefix))
			    (string f)))
	       (if (fboundp f)
		   (apply f (list object))
		   (symbol-value f))))))

;; @@@ Is this really necessary or maybe should it be a constant?
(defparameter *inter-space* 2)

(defun format-length (object format-char)
  "Return the length in characters to print OBJECT with FORMAT-CHAR."
  (cond
    ;; ((and (stringp object) (eql format-char #\a))
    ((and (typep object 'standard-class)
	  (find-method #'display-length nil
		       (list (find-class (type-of object)))))
     (display-length object))
    (t
     (display-length (format nil (s+ "~" format-char) object)))))

(defun smush-output (list cols height format-char stream row-limit screen-width)
  "Output LIST to STREAM in column major COLS and HEIGHT lines, limited to
ROW-LIMIT. Items are printed with FORMAT-CHAR."
  (loop
     :with len = (length list)
     :and format-str      = (format nil "~~v~a" format-char)
     :and format-str-last = (format nil "~~~a" format-char)
     :and a = (make-array (length list) :initial-contents list)
     :and limit = (if row-limit (min height row-limit) height)
     :and last-col = (1- (length cols))
     :and x
     :for i :from 0 :below limit :do
     (setf x 0)
     (loop
	:with n
	:for c :in cols
	:for j = 0 :then (1+ j)
	:do
	(setf n (+ (* j height) i))
	(when (< n len)
	  (if (= j last-col)
	      (progn
		(format stream format-str-last (aref a n))
		(incf x (format-length (aref a n) format-char)))
	      (progn
		(format stream format-str c (aref a n))
		(incf x c)))))
     (when (/= x screen-width)
       (terpri stream))))

(defun smush-columns (list cols rows lengths)
  "Return a list of the smallest column sizes for the putting the LIST in COLS
and ROWS. Return nil if the list can't fit. Second value is the extra space in
the last column, or the reason it didn't fit, either :TOO-NARROW or :TOO-WIDE."
  (let (col-list extra max-len (l list) (col 0) row)
    (loop :with i = 0
       :do
       (setf max-len 0 row 0)
       ;; Go down the column.
       (loop
	  :do
	  ;; Add space between cols except for the last row.
	  (setf max-len (max max-len
			     (+ ;(format-length (car l) format-char)
			      (aref lengths i)
			      (if (= col (1- cols)) 0 *inter-space*))))
	  (setf l (cdr l))
	  (incf i)
	  (incf row)
	  :while (and l (< row rows)))
       ;; Save the amount of blank space left in the last column.
       (when (and (not l) (< row rows))
	 (setf extra (- rows row)))
       (push max-len col-list)
       (incf col)
       :while (and l (< col cols)))
    (cond
      ((not (null l))			; not all items fit
       (values nil :too-narrow))
      ((and (< col (1- cols))		; didn't fill up all columns
	    (> rows 1))			; but multiple rows
       (values nil :too-wide))
      (t
       (values (nreverse col-list) extra)))))

;; @@@ This is linear, so we should be able to calculate the minimal
;; @@@ intersection of the rows and cols directly without iterating.
(defun print-columns-smush (list &key (columns 80) (stream *standard-output*)
				   (format-char #\a) prefix suffix
				   smush row-limit)
  (declare (ignore prefix suffix smush)) ;; @@@
  (when (not list)
    (return-from print-columns-smush 0))
  (let ((screen-width columns)
	;; (terminal:with-terminal (tty 'terminal-ansi:terminal-ansi)
	;;    (terminal:terminal-window-columns tty)))
	(len 0)
	(max-len 0)
	(min-len most-positive-fixnum)
	(area 0)
	max-area
	cols rows
	new-rows new-cols
	col-list
	last-col-list last-new-rows
	extra not-fit-count
	(cached-lengths (make-array (length list)
				    :element-type 'fixnum
				    :initial-element 0))
	)
    ;; Compute the length, maximum item length, and minimum area.
    (loop :with l
       :for i :in list
       :for n = 0 :then (1+ n)
       :do
       (setf l (+ (setf (aref cached-lengths n)
			(format-length i format-char)) *inter-space*)
	     max-len (max max-len l)
	     min-len (min min-len l))
       (incf area l)
       (incf len))
    (setf max-area (* max-len len))
    (dbug "List length:  ~d~%" len)
    (dbug "Screen width:  ~d~%" screen-width)
    (dbug "Minimum area: ~d ~d~%" area (ceiling area screen-width))
    (dbug "Max item len: ~d~%" max-len)
    (dbug "Min item len: ~d~%" min-len)
    (dbug "Maxium area : ~d ~d~%" max-area
    	    (floor max-area screen-width))

    (setf cols (max 1 (floor screen-width max-len))
	  rows (ceiling len cols)
	  new-rows rows
	  new-cols cols
	  col-list (make-list cols :initial-element max-len)
	  last-col-list col-list
	  last-new-rows new-rows
	  not-fit-count 0)

    (if (= new-rows 1)
	(progn
	  (multiple-value-setq (last-col-list extra)
	    (smush-columns list new-cols new-rows cached-lengths))
	  (setf last-new-rows 1))
	(loop
	   :while (and (< not-fit-count 4)
		       (<= (apply #'+ col-list)
			   #| (- screen-width min-len) |#
			   screen-width)
		       #| (and extra (> extra 2)) |#
		       (> new-rows 1))
	   :do
	   (if col-list
	       ;; save the previous results
	       (setf last-col-list col-list
		     last-new-rows new-rows
		     not-fit-count 0)
	       (incf not-fit-count))

	   #|
	   (if (and extra #| (>= extra (1- new-cols)) |# (> extra 1))
	       ;;(decf new-rows #| (max (truncate extra 2) 1) |# )
	       (if (= not-fit-count 0)
		   (incf new-cols)
		   (decf new-rows)))
	   |#

	   (cond
	     ((eq extra :too-narrow)
	      (incf new-cols))
	     ((eq extra :too-wide)
	      (decf new-rows))
	     (t
	      (decf new-rows)))
	   
	   (multiple-value-setq (col-list extra)
	     (smush-columns list new-cols new-rows cached-lengths))
	   (dbug "Squish: ~d x ~d (~d)~25t~a~%"
		 new-cols new-rows extra col-list)))

    ;; Output the last good setup
    (dbug "Squish Final: ~d x ~d (~d ~d)~25t~a~%"
	  (length last-col-list) last-new-rows
	  extra not-fit-count last-col-list)
    (smush-output list last-col-list last-new-rows format-char stream
		  row-limit screen-width)
    last-new-rows))

(defun print-columns-sizer (list &key (columns 80) (stream *standard-output*)
			     (format-char #\a) prefix suffix smush row-limit)
  "Return how many rows it might take to print list. Also returns the number of
columns and the maximum width of a column."
  (declare (ignore stream smush row-limit))
  (let* ((len (length list))
	 (format-str (format nil "~~~a" format-char))
	 (max-len
	  ;; This, although terse, may be inefficient w/big lists (@@@ test!):
	  (loop :with m = 0
	     :for c :in list :do
	     (setf m (max m (length (format nil format-str c))))
	     :finally (return (1+ m))))
	 (width (- #| (1- columns) |# columns
		   (if prefix (length prefix) 0)
		   (if suffix (length suffix) 0)))
	 (ccc   (floor width max-len))
	 (cols  (if (zerop ccc) 1 ccc))
	 (rows  (if (zerop cols) len (ceiling len cols))))
    (when (> max-len width) (setf max-len width))
    (values rows cols max-len)))

(defun print-columns-array (list rows cols)
  "Return an array with the size given by ROWS and COLS, filled in with items
from LIST in column-major order."
  (let ((a (make-array `(,cols ,rows) :initial-element nil)))
    (loop :with col = 0 :and row = 0
       :for c :in list
       :do
       (setf (aref a col row) c)
       (incf row)
       (when (>= row rows)
	 (incf col)
	 (setf row 0)))
    a))

;; @@@ consider:
;; (defun print-columns-output (array rows cols max-len
;; 			     &key prefix suffix)
;;   )

(defun print-columns (list &rest keys
		      &key (columns 80) (stream *standard-output*)
			(format-char #\a) prefix suffix smush row-limit)
  "Print the LIST on STREAM with as many columns as will fit in COLUMNS fixed
width character cells. Items are sorted down the columns, then across. Return
the number of rows.
FORMAT-CHAR is used to print the items, as with FORMAT.
PREFIX is a string to prepend to each row.
SUFFIX is a string to append to each row."
  (declare (ignorable columns))
  (when smush
    (return-from print-columns (apply #'print-columns-smush list keys)))
  (multiple-value-bind (rows cols max-len)
      (apply #'print-columns-sizer list keys)
    (let* ((format-str (format nil "~~v~a" format-char))
	   (a (print-columns-array list rows cols))
	   (limit (if row-limit (min rows row-limit) rows)))
      ;; output the array
      (loop :for r :from 0 :below limit
	 :do
	 (when prefix
	   (write-string prefix stream))
	 (loop :for c :from 0 :below cols
	    :do
	    (if (aref a c r)
		(progn
		  ;; (write-string ;; @@@ Why the superfluous write-string?
		  ;;  (format nil format-str max-len (aref a c r))
		  ;;  stream))
		  (format stream format-str max-len (aref a c r)))
		;; If we have a suffix, fill out blank space
		(if suffix
		    ;; (write-string ;; @@@ Why the superfluous write-string?
		    ;;  (format nil "~va" max-len #\space)
		    ;;  stream))))
		    (format stream "~va" max-len #\space))))
	 (when suffix
	   (write-string suffix stream))
	 (terpri stream)))
    rows))

(defparameter *binary-size-prefixes*
  #(nil "kibi" "mebi" "gibi" "tebi" "pebi" "exbi" "zebi" "yobi" "buttload"))

(defparameter *traditional-size-prefixes*
  #(nil "kilo" "mega" "giga" "tera" "peta" "exa" "zetta" "yotta" "buttload"))

(defparameter *binary-size-abbreviations*
  #(nil "Ki" "Mi" "Gi" "Ti" "Pi" "Ei" "Zi" "Yi" "**"))

(defparameter *traditional-size-abbreviations*
  #(nil "k" "M" "G" "T" "P" "E" "Z" "Y" "*"))

(defparameter *binary-sizes*
  (make-array '(11) :initial-contents
	      (loop :for i :from 0 :to 10 :collect (expt 1024 i))))

(defparameter *decimal-sizes*
  (make-array '(11) :initial-contents
	      (loop :for i :from 0 :to 10 :collect (expt 1000 i))))

(defun print-size (size &key (stream t) long unit abbrevs
			  (traditional t)
			  (binary t)
			  (format "~:[~3,1f~;~d~]~@[ ~a~]~@[~a~]"))
  "Print a size with standard binary units.
If LONG is true, print the long unit name.
If UNIT is supplied, it should be a string of the unit to be prefixed.
UNIT defaults to ‘B’ or “byte” depending on LONG.
ABBREVS is a custom list of size abbreviations.
If TRADITIONAL is true, use traditional units for binary units, e.g.
kilobyte instead of the kibibyte.
If BINARY is true (the default), use powers of two sizes, otherwise use powers
of ten sizes. If BINARY is false, TRADITIONAL defaults to true.
FORMAT is the format to print the number with, which gets passed 4 values:
  1 - a boolen indicating if the number is an integer or not
  2 - the number
  3 - the prefix
  3 - the unit
FORMAT defaults to \"~:[~3,1f~;~d~]~@[ ~a~]~@[~a~]\""
  (setf unit (or unit (or (and long "byte") "B")))
  (let ((prefixes (if traditional
		      *traditional-size-prefixes*
		      *binary-size-prefixes*))
	(sizes (if binary *binary-sizes* *decimal-sizes*)))
    (setf abbrevs (or abbrevs
		      (if traditional *traditional-size-abbreviations*
			  *binary-size-abbreviations*)))
    (flet ((pr (i)
	     (let* ((divisor (svref sizes i))
		    (n (/ size divisor))
		    (rem (rem size divisor)))
	       (format stream format
		       (zerop rem) n
		       (svref (if long
				  prefixes
				  abbrevs) i)
		       (when (svref abbrevs i) unit)))))
      (loop :for i :from 0 :to 9
	 :do
	 (when (< size (svref sizes (1+ i)))
	   (return-from print-size (pr i))))
      (pr 9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spin

(defvar *spin* nil
  "Index into the spinner string.")

(defvar *spin-string* nil
  "The string of characters to animate.")

(defvar *spin-length* nil
  "The pre-calculated length of the spin string.")

(defvar *spin-spun* nil
  "True if the spinner was spun at least once.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *spin-strings* nil
    "List of spin strings.")

  (defmacro defspin (name string doc)
    `(progn
       (defparameter ,name ,string ,doc)
       (push (list ',name ,string) dlib-misc:*spin-strings*)))
  
  (defspin *plain-spin-string* "|/-\\"
    "Simple ASCII baton.")

  (defspin *unicode-disk-spin-string* "◒◐◓◑"
    "Spin string using common unicode characters.")

  (defspin *unicode-scan-spin-string* "▏▎▍▌▋▊▉█"
    "Spin string using common unicode characters.")

  (defspin *unicode-digit-spin-string* "➊➋➌➍➎➏➐➑➒➓"
    "Spin string using common unicode characters.")

  (defspin *unicode-sparkle-spin-string* "❋❊❈❇❇··"
    "Spin string using common unicode characters.")

  (defspin *unicode-braille-spin-string* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    "Spin string using common unicode characters.")

  (defspin *unicode-square-spin-string* "◰◳◲◱"
    "Spin string using common unicode characters.")

  (defspin *emoji-spin-string* "🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚🕛"
    "Spin string with fancy emoji clock face characters.")

  (defvar *default-spin-string* *plain-spin-string*
    "The default spin string.")
  )

(defun spin (&optional (stream *standard-output*))
  "Do one iteration of a spin animation."
  (write-char (char *spin-string* *spin*) stream)
  (write-char #\backspace stream)
  (finish-output stream)
  (incf *spin*)
  (setf *spin-spun* t)
  (when (>= *spin* *spin-length*)
    (setf *spin* 0)))

(defun unspin (&optional (stream *standard-output*))
  "Hopefully remove the spinning character."
  (write-char #\space stream)
  (write-char #\backspace stream)
  (finish-output stream))

(defmacro with-spin ((&key spin-string)
		     &body body)
  "Evaluate the BODY with things set up so that you can call (SPIN)
repeatedly to get the next frame of the spinner animation displayed.
SPIN-STRING can be given and defaults to the value of *DEFAULT-SPIN-STRING*."
  `(let* ((*spin-string* ,(or spin-string
			      '(symbol-value '*default-spin-string*)))
	  (*spin-length* (length *spin-string*))
	  (*spin* 0)
	  (*spin-spun* nil))
     (prog1 (progn ,@body)
       (when *spin-spun*
	 (unspin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages

(defvar *loadable-packages* nil
  "Cached list of ASDF loadable packages. Set to NIL to recompute.")

;; This is an horrible hack. I wish we could ask ASDF and Quicklisp.
(defun loadable-packages (&key as-strings)
  "List of potentially ASDF loadable packages."
  (labels ((place-dir (p)
	     "Resolve place into a directory."
	     (with-output-to-string (s)
	       (if (listp p)
		   (loop :for e :in p
		      :if (eq e :home)
		      :do (write-string
			   (namestring (user-homedir-pathname)) s)
		      :else
		      :do (write-string e s))
		   (write-string p s)))))
    (or (and *loadable-packages*
	     (or (and (and as-strings (stringp (car *loadable-packages*)))
		      *loadable-packages*)
		 (and (not as-strings) (keywordp (car *loadable-packages*))
		      *loadable-packages*)))
	(setf *loadable-packages*
	      (with-spin ()
		(let ((s-dirs (loop :for e in asdf:*source-registry-parameter*
				 :if (and (listp e) (eq (car e) :directory))
				 :collect (place-dir (cadr e))))
		      (c-dirs (mapcar #'namestring asdf:*central-registry*)))
		  (append
		   (loop :for d :in (concatenate 'list s-dirs c-dirs)
		      :append
		      (loop :with base :and result
			 :for f :in (glob (path-append d "*.[Aa][Ss][Dd]"))
			 :do (spin)
			 :collect
			 (progn
			   (setf base (path-file-name f)
				 result (subseq base 0 (- (length base) 4)))
			   (if as-strings
			       result
			       (keywordify result)))))
		   #+quicklisp
		   ;; Quicklisp
		   (loop :for d :in (ql-dist:all-dists)
		      :append
		      (loop :for s :in (ql-dist:installed-systems d)
			 :do (spin)
			 :collect
			 (if as-strings
			     (ql-dist:name s)
			     (keywordify (ql-dist:name s))))))))))))

(defvar *quickloadable-systems* nil
  "Cached list of Quickload loadable systems. Set to NIL to recompute.")

(defun quickloadable-systems (&key as-strings)
  "List of packages the quickload can maybe load, if it can download them."
  (or *quickloadable-systems*
      #+quicklisp
      (setf *quickloadable-systems*
	    (with-spin ()
	      (loop :for d :in (ql-dist:all-dists)
		 :append
		 (loop :for s :in (ql-dist:provided-systems d)
		    :do (spin)
		    :collect
		    (if as-strings
			(string-downcase (ql-dist:name s))
			(keywordify (ql-dist:name s)))))))))

(defun clear-loadable-package-cache ()
  "This should be done whenever packages are added or removed or the search
configuration is changed."
  (setf *loadable-packages* nil
	*quickloadable-systems* nil))

(defun ensure-package (package)
  "Try to load PACKAGE as an ASDF system if we can't find it as a package."
  (when (not (find-package package))
    (asdf:load-system package)))

(defun unintern-conflicts (package conflicting-package)
  "Unintern all symbols in PACKAGE that conflict with CONFLICTING-PACKAGE."
  (when (not (packagep package))
    (setf package (find-package package)))
  (when (not (packagep conflicting-package))
    (setf conflicting-package (find-package conflicting-package)))
  (do-external-symbols (sym conflicting-package)
    (let ((old-sym (find-symbol (string sym) package)))
      (when old-sym
	(unintern old-sym package)
;	(format t "Uninterning ~a from ~a~%" old-sym package)
	))))

;; Simple mindless ASDF autoloader.
(defmacro d-autoload (symbol system doc-string &optional macro)
  "Define a function to load an ASDF system which contains a redefinition of
that function, and call it. In other words, define a stub function which
automatically loads the real function from a package and then calls it. The
actual package should be the same name as the ASDF package and define function
with the same name. If it's a macro, pass MACRO as true, mmkay?"
  (let ((doit
	 (if macro
	     `(eval (list* (intern (string ',symbol) ,system) args))
	     `(apply (intern (string ',symbol) ,system) args)))
	;;(symbol-string (string symbol))
	)
    `(,(if macro 'defmacro 'defun) ,symbol (&rest args)
       ,doc-string
       (asdf:load-system ,system)
       (unintern-conflicts *package* ,system)
       ;;(unintern (find-symbol ,symbol-string) ,*package*)
       ;;(unintern ,symbol ,*package*)
       ;;(unintern (make-symbol ,symbol-string) *package*)
       (use-package ,system)
       ;(import (find-symbol ,symbol-string ,system))
       (when (not (fboundp ',symbol))
	 (error "Autoload of ~a didn't define ~a." ,system ',symbol))
       ,doit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O

#|
(defun read-text ()
  "A very simplistic text reader. Mostly useful if you just want to paste some
text into lisp and have it be stored as lines of words."
  (loop :with s
     :while (string/= "" (setf s (read-line)))
     :collect (split-sequence #(#\space #\tab) s :omit-empty t :by-group t)))
|#

(defun dprobe-file (literal-filename)
  "Like probe-filename, but treat strings as literal."
  (probe-file (if (stringp literal-filename)
		  (quote-filename literal-filename)
		  literal-filename)))

(defun safe-file-length (stream)
  "Like FILE-LENGTH, but don't error, just return NIL if we can't figure it out."
  (handler-case
      (file-length stream)
    (type-error (c)
      (declare (ignore c)))))

(defparameter *slurp-buffer-size* 4096)

;; This is kind of some duplication from stuff in lish/piping.lisp.
;;
;; I don't think this should ever a slurp a URL, because security?.
;; Anyway Drakma does it better.

(defun slurp (file-or-stream &key
			       (external-format :default)
			       (element-type 'character))
  "Return a string (well actually an array of stream-element-type, which
defaults to character) with the contents of FILE-OR-STREAM."
  (let (stream (close-me nil) buffer pos len result)
    (unwind-protect
	 (progn
	   (setf stream
		 (etypecase file-or-stream
		   (null *standard-input*)
		   (stream file-or-stream) ; already a stream
		   ((or string pathname) ; a file name
		    (setf close-me t)
		    (open file-or-stream :external-format external-format
			  :element-type element-type))))
	   (if (and (typep stream 'file-stream)
		    ;; The length can be quite wrong, since it's probably in
		    ;; octets and we read it in characters. But hopefully it
		    ;; shouldn't be *less* than we need. 
		    (setf len (safe-file-length stream))
		    len)
	       (progn
		 ;; Read the whole thing at once.
		 (setf buffer (make-array len
					  :element-type
					  (stream-element-type stream)
					  :adjustable t))
		 (setf pos (read-sequence buffer stream))
		 (adjust-array buffer pos)
		 (setf result buffer))
	       (progn
		 ;; Read in chunks and write to a string.
		 (setf len *slurp-buffer-size*
		       buffer (make-array len
					  :element-type
					  (stream-element-type stream))
		       result
		       (with-output-to-string
			   (str nil
				:element-type (stream-element-type stream))
			 (loop :do
			    (setf pos (read-sequence buffer stream))
			    (when (> pos 0)
			      (write-sequence buffer str :end pos))
			    :while (= pos len)))))))
      (when (and close-me stream)
	(close stream)))
    result))

(defun confirm (action &key (output *standard-output*)
			 (input *standard-input*)
			 (confirming-input #\y)
			 (eof-confirms t))
  "A general confirmer using streams. ACTION is a description of the action
you want to confirm. CONFIRMING-INPUT can be either a character or a string,
whic defaults to #\y. If EOF-CONFIRMS is true (the default), then and end of
file is accepted as confirmation."
  (assert (or (stringp confirming-input) (characterp confirming-input)))
  (format output "~%Do you really want to ~a? " action)
  (finish-output output)
  (let ((l (read-line input nil nil)))
    (or (and eof-confirms (not l))	; EOF = confirm (i.e. hit ^D)
	(and (stringp l)
	     (etypecase confirming-input
	       (string (equalp l confirming-input))
	       (character (and (> (length l) 0)
			       (equalp (aref l 0) confirming-input))))))))

(defun get-file-local-variables (file-or-stream)
  "Return an alist of file local variables, like Emacs, e.g. variables that
are in the first couple of lines that are like:
  -*- MODE-NAME -*-
or
  '-*-' [ <variable> ':' <value> ';' ]* '-*-'
with the last ';' being optional.
Return NIL if we can't find local variables or if the format is messed up.
"
  (with-open-file-or-stream (in file-or-stream)
    (let ((line (read-line in nil))
	  start end var-list-string result)
      ;; If it has an interpreter spec, check the next line.
      (when (begins-with "#!" line)
	(setf line (read-line in nil)))
      (setf start (search "-*-" line))
      (when start
	(incf start 3)
	(setf end (search "-*-" line :start2 start)))
      (when end
	(setf var-list-string (subseq line start end)))
      (and var-list-string
	   (multiple-value-bind (a b)
	       (scan-to-strings "^[ \\t]*([^ \\t:;]+)[ \\t]*$"
				      var-list-string)
	     (if a
		 `(("mode" ,b))
		 (progn
		   (loop :for v :in (split ";[ \\t]*" var-list-string)
		      :do
		      (register-groups-bind
		       (name value)
		       ("[ \\t]*([^ :]+)[ \\t]*:[ \\t]*([^ ;]+)" v)
		       (push (list name value) result)))
		   result)))))))

;; Objectible version of one in dlib.
(defun oquote-format (s)
  "Quote a thing to send to format, so that any possible format directives
are printed rather than interpreted as directives, which really just means:
repleace a single tilde with double tidles."
  (oreplace-subseq "~" "~~" s))

;; End
