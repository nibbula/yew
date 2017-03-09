;;;								-*- Lisp -*-
;;; filter-stream.asd -- System definition for filter-stream
;;;

(defsystem filter-stream
    :name               "filter-stream"
    :description        "Filter a stream by a function."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Streams that filter an underlying stream with a function."
    :depends-on (:trivial-gray-streams :stretchy)
    :components
    ((:file "filter-stream")))
