;;;								-*- Lisp -*-
;;; calendar.asd - System definition for calendar
;;;

(defsystem calendar
    :name               "calendar"
    :description        "Naming and counting time."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Naming and counting time."
    :depends-on (:locale :dlib)
    :components
    ((:file "calendar")))
