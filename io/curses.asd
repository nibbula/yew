;;;								-*- Lisp -*-
;;; curses.asd -- System definition for CURSES package
;;;

(defsystem curses
    :name               "curses"
    :description        "Interface to curses library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "Not even close to being complete."
    :depends-on (:cffi)
    :components
    ((:file "curses")))
