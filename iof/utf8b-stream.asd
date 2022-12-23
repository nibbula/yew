;;;								-*- Lisp -*-
;;; utf8b-stream.asd - System definition for utf8b-stream
;;;

(defsystem utf8b-stream
    :name               "utf8b-stream"
    :description        "Yutifaitbi streams for your slacking enjoyment."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "something something something something etc."
    :depends-on (:dlib :char-util :unicode :dgray)
    :components
    ((:file "utf8b-stream")))
