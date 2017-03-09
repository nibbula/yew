;;;								-*- Lisp -*-
;;; unipose.asd -- System definition for unipose
;;;

(defsystem unipose
    :name               "unipose"
    :description        "Compose unicode characters"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Compose unicode characters."
    :depends-on (:glob)
    :components
    ((:file "unipose")))
