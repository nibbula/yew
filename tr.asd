;;;								-*- Lisp -*-
;;; tr.asd - System definition for tr
;;;

(defsystem tr
    :name               "tr"
    :description        "Translate characters."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Translate characters, something like the traditional Unix command."
    :depends-on (:dlib :glob :lish)
    :components
    ((:file "tr")))
