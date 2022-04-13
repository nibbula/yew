;;;
;;; rl-config.lisp - Configuration options for RL.
;;;

(defpackage :rl-config
  (:documentation "Configuration options for RL.")
  (:use :cl :config)
  (:export))
(in-package :rl-config)

;; (declaim (optimize (debug 2)))
(declaim (optimize (speed 1) (safety 3) (debug 3) (space 1) (compilation-speed 1)))

;; (defvar *configuration* nil)
;; (setf *configuration* nil)

;; (defconfig use-sqlite boolean-feature
;;    "True to use CLSQL-SQLITE for command history storage."
;;    (library-loadable-p '((t "libsqlite3.so"))))

(defconfiguration
  ((use-sqlite boolean-feature
    "True to use CLSQL-SQLITE for command history storage."
    ;; (library-loadable-p '((t "libsqlite3.so")))
    nil
    )
   (optimization-settings list
    "Default optimization settings for each file/compilation unit?."
    ;; If we don't have at least debug 2, then most compilers won't save
    ;; the function arguments.
    ;; `((debug 2)))))
    '((speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))))


;;(configure :verbose t)
(configure)

;; (let ((*print-length* nil))
;;   (format t "~s~%~%*configuration* ~s~%~%*config* ~s~%"
;; 	  *features* *configuration* *config*))

;; EOF
