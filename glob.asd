;;;								-*- Lisp -*-
;;; glob.asd -- System definition for glob
;;;

(defpackage :glob-system
    (:use :common-lisp :asdf))

(in-package :glob-system)

(defsystem glob
    :name               "glob"
    :description        "Glops of globby blobby blob glob."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description   "Another great example of how to poorly reinvent something stupid. Including, among others, fnmatch, glob, and wordexp."
    :depends-on (:opsys :dlib)
    :components
    ((:file "glob"))
    :in-order-to ((asdf:test-op (asdf:test-op :glob-test))))
