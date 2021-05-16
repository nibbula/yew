;;;								-*- Lisp -*-
;;; source-path.asd - System definition for source-path
;;;

(defsystem source-path
    :name               "source-path"
    :description        "Source paths"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Source paths, for CMU derived Lisps."
    :depends-on (:reader-ext)
    :components
    ((:file "source-path")))
