;;
;; rl-config.lisp - Configuration options for RL.
;;

(defpackage :rl-config
  (:documentation "Configuration options for RL.")
  (:use :cl :config)
  (:export))
(in-package :rl-config)

;; (declaim (optimize (debug 2)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; (defvar *configuration* nil)
;; (setf *configuration* nil)

;; (defconfig use-sqlite boolean-feature
;;    "True to use CLSQL-SQLITE for command history storage."
;;    (library-loadable-p '((t "libsqlite3.so"))))

(defconfiguration
  ((use-sqlite boolean-feature
    "True to use CLSQL-SQLITE for command history storage."
    (library-loadable-p '((t "libsqlite3.so"))))))

;;(configure :verbose t)
(configure)

;; (let ((*print-length* nil))
;;   (format t "~s~%~%*configuration* ~s~%~%*config* ~s~%"
;; 	  *features* *configuration* *config*))

;; EOF
