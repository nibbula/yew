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

(deftests (dtime-all :doc "Test :dtime.")
  dtime-1)

(defun run ()
  (run-group-name 'dtime-all :verbose t))

;; End
