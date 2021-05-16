;;;								-*- Lisp -*-
;;; dlib-misc-test.asd - System definition for dlib-misc-test
;;;

(defsystem dlib-misc-test
    :name               "dlib-misc-test"
    :description        "Tests for DLIB-MISC"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "If perchance you have any interest in the testing of the DLIB-MISC, you may
very well want to look into this very package. I wouldst heartily recommend that
thou tryeth to invoke that petulant scamp DLIB-MISC-TEST:RUN forthwith."
    :depends-on (:test :dlib-misc :fatchar)
    :components
    ((:file "dlib-misc-test")))
