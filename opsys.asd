;;;								-*- Lisp -*-
;;; opsys.asd -- System definition for OPSYS package
;;;

;; I don't like the way :if-feature works. How can I use it to make the same
;; file depend on different files? Also it makes me repeat the long ugly
;; feature conditionals. Why can't I just say something like:
;;
;; (defvar *system-specific-module*
;;   #+(or unix linux darwin sunos bsd) "unix"
;;   #+(and windows (not unix)) "ms")
;;
;; and then use that in the appropriate places? I know evaluation, but..
;; As it is, I'm forced to use reader conditionals anyway.
;; Or am I missing something?

(defsystem opsys
    :name               "opsys"
    :description        "Interface to the operating system."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "The cessation of the repetition of ‘Never Again’."
    :depends-on (:cffi 
		 #+(and (or windows win32) (not unix)) :cffi-libffi
		 :dlib)
    :components
    ((:file "base")
     ;; (:file "unix" :if-feature (:or :unix :linux :darwin :sunos :bsd)
     ;; 	    :depends-on ("base"))
     (:module "unix" :if-feature (:or :unix :linux :darwin :sunos :bsd)
      :depends-on ("base")
      :serial t
      :components ((:file "package")
		   (:file "macros")
		   (:file "types")
		   (:file "errors")
		   (:file "environmental")
		   (:file "users")
		   (:file "filesystem")
		   (:file "memory")
		   (:file "signals")
		   (:file "time")
		   (:file "processes")
		   (:file "events")
		   (:file "terminals")
		   (:file "communication")
		   (:file "inspection")
		   (:file "unix")))
     (:file "ms" :if-feature (:and :windows (:not :unix))
	    :depends-on ("base"))
     (:file "termios" :if-feature (:or :unix :linux :darwin :sunos :bsd)
	    :depends-on ("unix"))
     (:file "package" :depends-on ("base"))
     (:file "libc" :depends-on ("package"))
     (:file "opsys" :depends-on
	    #+(or unix linux darwin sunos bsd) ("unix")
	    #+(and (or windows win32) (not unix)) ("ms"))))
