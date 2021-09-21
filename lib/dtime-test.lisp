;;;
;;; dtime-test.lisp - Tests for dtime.
;;;

(defpackage :dtime-test
  (:documentation "Tests for dtime.")
  (:use :cl :test :dtime)
  (:export
   #:run
   ))
(in-package :dtime-test)

(deftests (dtime-1 :doc "Basic tests of dtime functions.")
  "Test the basic object."
  (let ((tt (make-dtime)))
    (and (zerop (dtime-seconds tt)) (dtime-nanoseconds tt)))
  (let ((tt (make-dtime :seconds 1 :nanoseconds 1)))
    (and (= 1 (dtime-seconds tt)) (= (dtime-nanoseconds tt) 1)))
  "Test comparisons."
  (let ((t1 (get-dtime)) (t2 (get-dtime)))
    (or (dtime< t1 t2) (dtime> t1 t2) (dtime= t1 t2)))
  (let ((t1 (get-dtime)) (t2 (progn (sleep .1) (get-dtime))))
    (or (dtime< t1 t2) (dtime> t1 t2)))
  (let* ((t1 (get-dtime))
	 (t2 (make-dtime :seconds (dtime-seconds t1)
			 :nanoseconds (dtime-nanoseconds t1))))
    (dtime= t1 t2))
  (let ((t1 (make-dtime :seconds #x1111 :nanoseconds #x2222))
	(t2 (make-dtime :seconds #x3333 :nanoseconds #x4444)))
    (dtime/= t1 t2))
  (let ((t1 (make-dtime :seconds #x1111 :nanoseconds #x2222))
	(t2 (make-dtime :seconds #x3333 :nanoseconds #x4444)))
    (and (dtime< t1 t2)
	 (dtime> t2 t1)))
  "These have the remote possibility of failing if the system clock is ajusted."
  (let* ((t1 (get-dtime)) (t2 (progn (sleep .1) (get-dtime))))
    (and (dtime< t1 t2)
	 (dtime> t2 t1)))
  (let* ((t1 (get-dtime)) (t2 (progn (sleep .1) (get-dtime))))
    (and (dtime<= t1 t2)
	 (dtime>= t2 t1)))
  (let ((t1 (make-dtime :seconds #x1111 :nanoseconds #x2222))
	(t2 (make-dtime :seconds #x3333 :nanoseconds #x4444)))
    (progn
      (dtime> (dtime+ t1 t2) t1)))
  (let ((t1 (make-dtime :seconds #x1111 :nanoseconds #x2222))
	(t2 (make-dtime :seconds #x3333 :nanoseconds #x4444)))
    (progn
      (dtime> t2 (dtime- t2 t1))))
  (let ((t1 (make-dtime :seconds #x1111 :nanoseconds #x2222))
	(t2 (make-dtime :seconds #x3333 :nanoseconds #x4444)))
    (progn
      (dtime= (dtime+ t1 t2) (dtime+ t2 t1))))
  (let ((t1 (make-dtime :seconds 1111 :nanoseconds 77777))
	(t2 (make-dtime :seconds 3333 :nanoseconds 88888)))
    (progn
      (dtime= (dtime- t1 t1) (dtime- t2 t2))))
  (let ((t1 (make-dtime :seconds 1111 :nanoseconds 77777))
	(t2 (make-dtime :seconds 3333 :nanoseconds 88888)))
    (progn
      (dtime= (dtime- t2 t1) (dtime- t2 (dtime- (dtime+ t1 t2) t2)))))
  (let ((t1 (make-dtime :seconds 27 :nanoseconds 1111111111))
	(t2 (make-dtime :seconds 27 :nanoseconds 1111000000)))
    (progn
      (dtime= (dtime-round t1 :milliseconds)
	      (dtime-round t2 :milliseconds))))
  (let ((t1 (make-dtime :seconds 27 :nanoseconds 1111111111))
	(t2 (make-dtime :seconds 27 :nanoseconds 1111000000)))
    (progn
      (dtime/= (dtime-round t1 :microseconds)
	       (dtime-round t2 :microseconds))))
  )

(deftests (dtime-2 :doc "Test more stuff.")
  "Test describe-duration."
  (equal (describe-duration (make-dtime :seconds 1)) "1 second")
  (equal (describe-duration (make-dtime :seconds 2)) "2 seconds")
  (equal (describe-duration (make-dtime :seconds 60)) "1 minute")
  (equal (describe-duration (make-dtime :seconds 120)) "2 minutes")
  (equal (describe-duration (make-dtime :seconds 119))
	 "1 minute 59 seconds")
  (equal (dtime::describe-duration
	 (make-dtime
	  :seconds (+ 1 60
                      (* 60 60)
                      (* 24 60 60)
                      (* 7 24 60 60)
                      (* (+ 365 1/4) 24 60 60)
                      (* 10 (+ 365 1/4) 24 60 60)
                      (* 100 (+ 365 1/4) 24 60 60)
                      (* 1000 (+ 365 1/4) 24 60 60))))
  "1 millennium 1 century 1 decade 1 year 1 week 1 day 1 hour 1 minute 1 second")
  (equal
   (describe-duration (make-dtime :seconds 1 :nanoseconds 1))
   "1 second 1 nanosecond")
  (equal
   (describe-duration (make-dtime :seconds 2 :nanoseconds 2))
   "2 seconds 2 nanoseconds")
  (equal
   (describe-duration (make-dtime :seconds 2 :nanoseconds 20002))
   "2 seconds 20 microseconds 2 nanoseconds")
  (equal
   (describe-duration (make-dtime :seconds 2 :nanoseconds 20002002))
   "2 seconds 2 centiseconds 2 microseconds 2 nanoseconds")
  (equal
   (describe-duration (make-dtime :seconds 2 :nanoseconds 2002020020))
   "4 seconds 2 milliseconds 20 microseconds 20 nanoseconds")
  (equal
   (describe-duration (make-dtime :seconds 2 :nanoseconds 33000000000001))
   "9 hours 10 minutes 2 seconds 1 nanosecond")
  (equal
   (describe-duration (make-dtime :seconds 1 :nanoseconds 200030100))
   "1 second 2 deciseconds 30 microseconds 100 nanoseconds")
  (equal
   (describe-duration (make-dtime :seconds 12392835611))
   "3 centuries 9 decades 2 years 36 weeks 5 days 14 hours 20 minutes 11 seconds")
  )

(deftests (dtime-all :doc "Test :dtime.")
  dtime-1
  dtime-2)

(defun run ()
  (run-group-name 'dtime-all :verbose t))

;; End
