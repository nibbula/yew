;;;								-*- Lisp -*-
;;; dtime.asd - System definition for dtime
;;;

(defsystem dtime
    :name               "dtime"
    :description        "Time library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Things to deal with simple counting of time for stupid meat creatures that
live on Earth only!"
    :depends-on (:dlib :opsys :calendar)
    :components
    ((:file "dtime")))
