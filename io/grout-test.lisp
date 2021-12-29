;;;
;;; grout-test.lisp - Tests for GROUT package.
;;;

(defpackage :grout-test
  (:documentation "Tests for GROUT package.")
  (:use :cl :test :grout :terminal)
  (:export
   #:run
   ))
(in-package :grout-test)

(deftests (grout-1 :doc "Test basics")
  (let (g)
    (unwind-protect
	 (progn
	   (setf g (make-grout))
	   (and (typep g 'grout)
		(streamp (grout-stream g))))
      (grout::%grout-done g)))
  (with-grout ()
    (and (typep *grout* 'grout)
	 (streamp (grout-stream *grout*))))
  ;; Make sure *terminal* is set.
  (with-terminal ()
    (with-grout ()
      (eq *terminal* (grout::generic-term *grout*))))
  ;; Make sure we get the same *grout* recursively
  (with-grout ()
    (let ((first-grout *grout*))
      (with-grout ()
	(eq first-grout *grout*))))
  ;; Make sure we get a different *grout* when we provide a stream
  (with-grout ()
    (let ((first-grout *grout*))
      (with-output-to-string (str)
	(let ((string-stream str))
	  (with-grout (*grout* str)
	    (and (not (eq first-grout *grout*))
		 (eq *grout* string-stream)))))))
  ;; Make sure nesting with a provided stream works
  (with-output-to-string (str)
    (let ((string-stream str))
      (with-grout (*grout* str)
	(with-grout ()
	  (eq (grout-stream *grout*) string-stream)))))
  ;; Even more nesting
  (with-output-to-string (str)
    (let ((string-stream str))
      (with-grout (*grout* str)
	(with-grout ()
	  (with-grout ()
	    (with-grout ()
	      (eq (grout-stream *grout*) string-stream)))))))
  ;; test nested output to string streams
  (equal "foo"
	 (with-output-to-string (str)
	   (let ((string-stream str))
	     (with-grout (*grout* str)
	       (with-grout ()
		 (eq (grout-stream *grout*) string-stream)
		 (grout-princ "foo")))))))

(deftests (grout-all :doc "Test grout.")
  grout-1)

(defun run ()
  (run-group-name 'grout-all :verbose t))

;; End
