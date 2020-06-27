;;;
;;; dlib-misc-test.lisp - Tests for dlib-misc.
;;;

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
