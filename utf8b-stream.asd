;;;								-*- Lisp -*-
;;; utf8b-stream.asd - System definition for utf8b-stream
;;;

(defsystem utf8b-stream
    :name               "utf8b-stream"
    :description        "Yutifaitbi streams for your slacking enjoyment."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "something something something something etc."
    :depends-on (:dlib :char-util :trivial-gray-streams)
    :components
    ((:file "utf8b-stream")))
