;;;								-*- Lisp -*-
;;; unipose.asd -- System definition for unipose
;;;

(defsystem unipose
    :name               "unipose"
    :description        "Compose unicode characters"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Compose unicode characters."
    :depends-on (:dlib :glob :dtime)
    :components
    ((:file "unipose")))
