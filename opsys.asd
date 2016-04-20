;;;								-*- Lisp -*-
;;; opsys.asd -- System definition for OPSYS package
;;;

(defpackage :opsys-system
    (:use :common-lisp :asdf))

(in-package :opsys-system)

(defsystem opsys
    :name               "opsys"
    :description        "Interface to the operating system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "The cessation of the repetition of ‘Never Again’."
    :depends-on (:cffi :dlib)
    :components
    ((:file "opsys-base")
     #+(or unix linux darwin sunos bsd)
     (:file "unix" :depends-on ("opsys-base"))
     #+(or unix linux darwin sunos bsd)
     (:file "termios" :depends-on ("unix"))
     #+(and windows (not unix))
     (:file "ms" :depends-on ("opsys-base"))
     (:file "opsys"
	    :depends-on (#+unix "unix"
			 #+(and windows (not unix)) "ms"))))
