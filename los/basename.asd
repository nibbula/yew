;;;								-*- Lisp -*-
;;; basename.asd - System definition for basename
;;;

(defsystem basename
    :name               "basename"
    :description        "Print the file name without the directory."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print the file name without the directory."
    :depends-on (:dlib :opsys :collections :lish)
    :components
    ((:file "basename")))
