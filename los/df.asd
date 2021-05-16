;;;								-*- Lisp -*-
;;; df.asd -- System definition for df
;;;

(defsystem df
    :name               "df"
    :description        "Show how much disk is free."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Just like in the good old times."
    :depends-on (:opsys :dlib :dlib-misc :table :grout :fatchar-io :los-config)
    :components
    ((:file "df")))
