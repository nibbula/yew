;;;								-*- Lisp -*-
;;; ldirs.asd - System definition for ldirs
;;;

(defsystem ldirs
    :name               "ldirs"
    :description        "List directories where things are loaded from."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "List directories where things are loaded from."
    :depends-on (:dlib :opsys :lish :grout)
    :components
    ((:file "ldirs")))
