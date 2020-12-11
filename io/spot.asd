;;;								-*- Lisp -*-
;;; spot.asd - System definition for spot
;;;

(defsystem spot
    :name               "spot"
    :description        "Locations of objects for inators."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Provide a generic position called a spot to encapsulate editing location
for inators."
    ;; :depends-on ()
    :components
    ((:file "spot")))
