;;;								-*- Lisp -*-
;;; opsys.asd -- System definition for OPSYS package
;;;

(defsystem opsys
    :name               "opsys"
    :description        "Interface to the operating system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "The cessation of the repetition of ‘Never Again’."
    :depends-on (:cffi 
		 ;; How can I avoid using reader conditionals here?
		 #+(and (or windows win32) (not unix) (not ccl)) :cffi-libffi
		 :dlib)
    :components
    ((:module "base"
      :pathname ""
      :serial t
      :components ((:file "base")
		   (:file "types")
		   (:file "os-stream")))
     (:module "unix"
      :depends-on ("base")
      :serial t
      :if-feature (:or :unix :linux :darwin :sunos :bsd)
      :components ((:file "package")
		   (:file "macros")
		   (:file "types")
		   (:file "errors")
		   (:file "environmental")
		   (:file "time")
		   (:file "users")
		   (:file "filesystem")
		   (:file "memory")
		   (:file "signals")
		   (:file "processes")
		   (:file "events")
		   (:file "terminals")
		   (:file "communication")
		   (:file "inspection")
		   (:file "unix")
		   (:file "unix-stream")))
     (:file "ms" :if-feature (:and :windows (:not :unix))
	    :depends-on ("base"))
     (:module "platform-dependant"
	      :components
	      ((:module "unix"
			:if-feature (:or :unix :linux :darwin :sunos :bsd))
	       (:module "ms"
			:if-feature (:and :windows (:not :unix)))))
     (:file "package" :depends-on ("base"))
     (:file "libc" :depends-on ("package"))
     (:file "opsys" :depends-on ("platform-dependant"))))
