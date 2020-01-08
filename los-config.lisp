;;
;; los-config.lisp - Configuration for LOS
;;

(defpackage :los-config
  (:documentation "Configuration for LOS")
  (:use :cl :config)
  ;; (:export)
  )
(in-package :los-config)

(declaim (optimize (debug 2)))

(defconfiguration
  ((optimization-settings list
    "Default optimization settings for each file/compilation unit?."
    ;; If we don't have at least debug 2, then most compilers won't save
    ;; the function arguments.
    ;; `((debug 2)))))
    '((speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))))
    ;; '((speed 3) (safety 0) (debug 2) (space 0) (compilation-speed 0)))))

(configure)

;; EOF
