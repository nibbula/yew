;;;								-*- Lisp -*-
;;; md5sum.asd - System definition for md5sum
;;;

(defsystem md5sum
    :name               "md5sum"
    :description        "MD5 message digest checksums."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "MD5 message digest checksums."
    :depends-on (:dlib :opsys :md5)
    :components
    ((:file "md5sum")))
