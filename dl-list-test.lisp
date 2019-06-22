;;
;; dl-list-test.lisp - Tests for dl-list.
;;

(defpackage :dl-list-test
  (:documentation "Tests for dl-list.")
  (:use :cl :dl-list :test)
  (:export
   #:run
   ))
(in-package :dl-list-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(deftests (dl-list-1 :doc "Basic functionality.")
  "Push one element."
  (let (l)
    (dl-push l 1)
    (= (dl-length l) 1))
  "Check the content."
  (let (l)
    (dl-push l 1)
    (= (dl-content l) 1))
  "Push more elements."
  (let (l)
    (dl-push l 1)
    (dl-push l 2)
    (dl-push l 3)
    (= (dl-length l) 3))
  "Check more content."
  (let (l)
    (dl-push l 1)
    (dl-push l 2)
    (dl-push l 3)
    (and (= (dl-content l) 3)
  	 (= (dl-content (dl-next l)) 2)
  	 (= (dl-content (dl-next (dl-next l))) 1)))
  "Check more content by popping."
  (let (l)
    (dl-push l 1)
    (dl-push l 2)
    (dl-push l 3)
    (and (eql (dl-pop l) 3)
  	 (eql (dl-pop l) 2)
  	 (eql (dl-pop l) 1)))
  "Make a list and nth"
  (let ((l (make-dl-list '(0 1 2 3 4 5))))
    (and (= (dl-length l) 6)
	 (= (dl-nth 0 l) 0)
	 (= (dl-nth 3 l) 3)
	 (= (dl-nth 5 l) 5)))
  "Last"
  (let ((l (make-dl-list '(0 1 2 3 4 5))))
    (= (dl-content (dl-last l)) 5))
  "Last n"
  (let* ((l (make-dl-list '(0 1 2 3 4 5)))
	 (sub-l (dl-last l 3)))
    (and (= (dl-nth 0 sub-l) 3)
	 (= (dl-nth 1 sub-l) 4)
	 (= (dl-nth 2 sub-l) 5)))
  )

(deftests (dl-list-all :doc "All the tests.")
  dl-list-1)

(defun run ()
  "Run all the dl-list tests."
  (run-group-name 'dl-list-all :verbose t))

;; EOF
