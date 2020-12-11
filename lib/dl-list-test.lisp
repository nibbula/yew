;;
;; dl-list-test.lisp - Tests for dl-list.
;;

(defpackage :dl-list-test
  (:documentation "Tests for dl-list.")
  (:use :cl :dl-list :test :collections)
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
  "Making and unmaking a list"
  (let* ((l1 (loop :for i :from 1 :to 77 :collect i))
	 (l (make-dl-list l1))
	 (l2 (dl-list-to-list l)))
    (equal l1 l2))
  )

(deftests (dl-list-2 :doc "Iterating")
  "do iteration"
  (let* ((n 100)
	 (l (make-dl-list (loop :for i :from 1 :to n :collect i)))
	 (sum 0))
    (dl-list-do l (lambda (x) (incf sum x)))
    (= sum (+ (* (/ n 2) n) (/ n 2))))
  "iterate backward"
  (let* ((l1 '(0 1 2 3 4 5))
	 (l2 nil)
	 (l3 (make-dl-list l1)))
    (dl-list-do-backward (dl-last l3) (lambda (x) (push x l2)))
    (equal l1 l2))
   )

(deftests (dl-list-printing :doc "Printing")
  "printing"
  (equal "#<DL-LIST (1 2 3 4 5)>"
	 (prin1-to-string (dl-list:make-dl-list '(1 2 3 4 5))))
  "elided printing"
  (let ((*print-length* 3))
    (equal "#<DL-LIST (1 2 3 ...)>"
	   (prin1-to-string (dl-list:make-dl-list '(1 2 3 4 5 6)))))
  "pretty printing"
  (let ((*print-right-margin* 17))
    (equal (format nil
		   (concatenate 'string
				;;012345678901234567
				"#<DL-LIST (A B C~%"
				"            D E F~%"
				"            G H~%"
				"            I)>"))
	   (princ-to-string
	    (dl-list:make-dl-list '(a b c d e f g h i)))))
  )

(deftests (dl-list-collections :doc "Collection methods")
  "emptyness"
  (emptyp (make-dl-list '()))
  "elt"
  (let* ((l1 '(0 1 2 3 4 5 6 7 8))
	 (l (make-dl-list l1)))
    (every (lambda (x) (= (oelt l x) x)) l1))
  "length"
  (= (olength (make-dl-list '())) 0)
  (= (olength (make-dl-list '(1))) 1)
  (= (olength (make-dl-list (loop :for i :from 1 :to 88 :collect i))) 88)
  ;; "map"
  ;; (let ((l (make-dl-list
  )

(deftests (dl-list-all :doc "All the tests.")
  dl-list-1 dl-list-2 dl-list-printing dl-list-collections)

(defun run ()
  "Run all the dl-list tests."
  (run-group-name 'dl-list-all :verbose t))

;; EOF
