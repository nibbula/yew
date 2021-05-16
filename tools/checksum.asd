;;;								-*- Lisp -*-
;;; checksum.asd - System definition for checksum
;;;

(defsystem checksum
    :name               "checksum"
    :description        "Print checksums."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print checksums."
    :depends-on (:dlib :grout :ironclad)
    :components
    ((:file "checksum")))
