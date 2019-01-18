;;
;; dlib-misc-test.lisp - Tests for dlib-misc.
;;

(defpackage :dlib-misc-test
  (:documentation "Tests for DLIB-MISC.")
  (:use :cl :test :dlib-misc :fatchar)
  (:export
   #:run
   ))
(in-package :dlib-misc-test)

(defparameter *letter-vector* "abcdefghijklmnopqrstuvwxyz")

(deftests (dlib-misc-1 :doc "Test general functions.")
  "Technically this could fail and still be working, but it is hugely unlikely."
  (not (equal (randomize-vector (copy-seq *letter-vector*)) *letter-vector*))
  "Test that it modifies the vector."
  (let ((lv (copy-seq *letter-vector*)))
    (equal (randomize-vector lv) lv))
  "Pointless statistical test."
  (let ((vec (make-array 100)))
    (loop :for i :from 0 :below (length vec) :do (setf (aref vec i) i))
    (flet ((simil (a b)
	     (loop :for aa :across a :and bb :across b
		:sum (if (= aa bb) 1 0))))
      (< (simil vec (randomize-vector (copy-seq vec)))
	 50 #| I know this is entirely ridiculous |#)))
  "No radix"
  (eql (parse-integer-with-radix "0") 0)
  (eql (parse-integer-with-radix "-1") -1)
  ""
  "Tree-ification."
  )

(deftests (dlib-misc-dtime-1 :doc "Basic tests of dtime functions.")
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

(defparameter *fill-column* 60)
(defparameter *text* "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam semper risus mauris, et dignissim lorem lacinia non. Sed vitae lacus nisi. Fusce vitae lectus non quam dictum luctus et at mauris.")

(deftests (dlib-misc-justify-1 :doc "Basic tests of justify-text.")
  (equal (justify-text *text* :stream nil)
"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam semper risus
mauris, et dignissim lorem lacinia non. Sed vitae lacus nisi. Fusce vitae
lectus non quam dictum luctus et at mauris.")
  (equal (justify-text *text* :stream nil :cols 40)
"Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Etiam semper risus
mauris, et dignissim lorem lacinia non.
Sed vitae lacus nisi. Fusce vitae
lectus non quam dictum luctus et at
mauris.")
  (equal (justify-text *text* :stream nil :cols 20)
  "Lorem ipsum dolor
sit amet,
consectetur
adipiscing elit.
Etiam semper risus
mauris, et
dignissim lorem
lacinia non. Sed
vitae lacus nisi.
Fusce vitae lectus
non quam dictum
luctus et at
mauris.")
  (equal (justify-text *text* :stream nil :cols 30 :prefix "-->")
  "-->Lorem ipsum dolor sit
-->amet, consectetur
-->adipiscing elit. Etiam
-->semper risus mauris, et
-->dignissim lorem lacinia
-->non. Sed vitae lacus nisi.
-->Fusce vitae lectus non
-->quam dictum luctus et at
-->mauris.")
  (equal (justify-text *text* :stream nil :cols 30 :prefix "[ ] "
		       :omit-first-prefix t)
"Lorem ipsum dolor sit amet,
[ ] consectetur adipiscing
[ ] elit. Etiam semper risus
[ ] mauris, et dignissim
[ ] lorem lacinia non. Sed
[ ] vitae lacus nisi. Fusce
[ ] vitae lectus non quam
[ ] dictum luctus et at
[ ] mauris.")
  (equal (justify-text *text* :stream nil :cols 80 :prefix "| "
		       :omit-first-prefix t :start-column 40)
"Lorem ipsum dolor sit amet, consectetur
| adipiscing elit. Etiam semper risus mauris, et dignissim lorem lacinia non.
| Sed vitae lacus nisi. Fusce vitae lectus non quam dictum luctus et at mauris.")
  (equal (justify-text *text* :stream nil :separator #\.)
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit
 Etiam semper risus mauris, et dignissim lorem lacinia non
 Sed vitae lacus nisi. Fusce vitae lectus non quam dictum luctus et at mauris")
  )

(defparameter *fat-text*
  (span-to-fat-string
   '((:red "Lorem ipsum dolor sit amet")
     (:blue ", consectetur adipiscing elit. ")
     (:green "Etiam semper risus mauris, ")
     (:magenta "et dignissim lorem lacinia non. ")
     (:cyan "Sed vitae lacus nisi. ")
     (:yellow "Fusce vitae lectus non quam ")
     (:white (:bg-black "dictum luctus et at mauris.")))))

(deftests (dlib-misc-justify-fat-1 :doc "Justify-text on fat-strings.")
  (fat-string-equal
   (justify-text *fat-text* :stream nil :cols 40)
   (span-to-fat-string
    '((:red "Lorem ipsum dolor sit amet") (:blue ", consectetur") #\newline
      (:blue "adipiscing elit. ") (:green "Etiam semper risus") #\newline
      (:green "mauris, ") (:magenta "et dignissim lorem lacinia non.") #\newline
      (:cyan "Sed vitae lacus nisi. ") (:yellow "Fusce vitae") #\newline
      (:yellow "lectus non quam ")
      (:white (:bg-black "dictum luctus et at")) #\newline
      (:white (:bg-black"mauris."))))))

(defparameter *fat-text*
  (span-to-fat-string
   '((:red "Lorem ipsum dolor sit amet")
     (:blue ", consectetur adipiscing elit. ")
     (:green "Etiam semper risus mauris, ")
     (:magenta "et dignissim lorem lacinia non. ")
     (:cyan "Sed vitae lacus nisi. ")
     (:yellow "Fusce vitae lectus non quam ")
     (:white (:bg-black "dictum luctus et at mauris.")))))

(defparameter *wide-text*
  "鹽官云：「扇子既破，還我犀牛兒來。」且道他要犀牛兒作什麼？也只要驗人知得落處也無。投子云：「不辭將出，恐頭角不全。」雪竇云：「我要不全的頭角。」亦向句下便投機。")

(deftests (dlib-misc-all :doc "Test :dlib-misc.")
  dlib-misc-1 dlib-misc-dtime-1 dlib-misc-justify-1 dlib-misc-justify-fat-1)

(defun run ()
  (run-group-name 'dlib-misc-all :verbose t))

;; EOF
