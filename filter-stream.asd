;;;								-*- Lisp -*-
;;; filter-stream.asd -- System definition for filter-stream
;;;

(defpackage :filter-stream-system
    (:use :common-lisp :asdf))

(in-package :filter-stream-system)

(defsystem filter-stream
    :name               "filter-stream"
    :description        "Filter a stream by a function."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Streams that filter an underlying stream with a function."
    :depends-on (:trivial-gray-streams :stretchy)
    :components
    ((:file "filter-stream")))
