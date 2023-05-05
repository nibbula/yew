;;;								-*- Lisp -*-
;;; slowcat.asd -- System definition for slowcat
;;;

(defsystem slowcat
    :name               "slowcat"
    :description        "Cats who take their good old time."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Like in the old days, if you happen to need it."
    :depends-on (:opsys :los-config)
    :components
    ((:file "slowcat")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "slowcat-cmds"))
      :depends-on ("slowcat"))))
