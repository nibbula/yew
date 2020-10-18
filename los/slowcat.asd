;;;								-*- Lisp -*-
;;; slowcat.asd -- System definition for slowcat
;;;

(defsystem slowcat
    :name               "slowcat"
    :description        "Cats who take their good old time."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Like in the old days, if you happen to need it."
    :depends-on (:opsys :los-config)
    :components
    ((:file "slowcat")))
