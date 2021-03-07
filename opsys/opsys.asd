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
    :defsystem-depends-on (:opsys-config)
    :depends-on ((:feature (:not :mezzano) :cffi)
		 :opsys-config :unicode
		 (:feature (:and (:or :windows :win32) (:not :unix) (:not :ccl))
			   :cffi-libffi)
		 :trivial-gray-streams
		 (:feature :dlib :dlib)
		 (:feature (:not :dlib) :fake-dlib))
    :components
    ((:module "base"
      :pathname ""
      :serial t
      :components ((:file "base")
		   (:file "types")
		   (:file "os-stream-base")))
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
		   (:file "unix")
		   (:file "inspection")
		   (:file "unix-stream")))
     ;; (:file "ms" :if-feature (:and :windows (:not :unix))
     ;; 	    :depends-on ("base"))
     (:module "ms"
      :depends-on ("base")
      :serial t
      :if-feature (:and :windows (:not :unix))
      :components ((:file "package")
		   (:file "types")
		   (:file "ms")
		   (:file "errors")
		   (:file "environmental")
		   (:file "time")
		   (:file "users")
		   (:file "filesystem")
		   (:file "processes")
		   (:file "events")
		   (:file "terminals")
		   (:file "communication")
		   (:file "ms-stream")
		   ))
     (:module "mezzano"
      :depends-on ("base")
      :serial t
      :if-feature :mezzano
      :components ((:file "mezzano")))
     (:module "platform-dependant"
	      :components
	      ((:module "unix"
			:if-feature (:or :unix :linux :darwin :sunos :bsd))
	       (:module "ms"
			:if-feature (:and :windows (:not :unix)))
	       (:module "mezzano"
			:if-feature :mezzano)))
     (:file "package" :depends-on ("base"))
     (:file "libc" :depends-on ("package")
	    :if-feature (:not :mezzano))
     (:file "opsys" :depends-on ("platform-dependant"))
     (:file "os-stream" :depends-on ("opsys"))))
