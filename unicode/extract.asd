;;;								-*- Lisp -*-
;;; extract.asd - System definition for extract
;;;

(defsystem extract
    :name               "extract"
    :description        "Extract character data."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "This possibly downloads and extracts character data from the offical
Unicode character data files. It transforms the data into a form that
can be loaded and used by the unicode package."
    :depends-on (:dlib :opsys :dlib-misc :table :puri :drakma :ironclad)
    :components
    ((:file "extract")))
