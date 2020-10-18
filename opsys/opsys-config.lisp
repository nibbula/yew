;;
;; opsys-config.lisp - Configuration options for OPSYS
;;

(defpackage :opsys-config
  (:documentation "Configuration options for OPSYS")
  (:use :cl #| :config |#)
  (:export
   #:*config*
   ;; #:*configuration*
   ))
(in-package :opsys-config)

#|
(defconfiguration
  ((optimization-settings list
    "Default optimization settings for each file/compilation unit?."
    ;; If we don't have at least debug 2, then most compilers won't save
    ;; the function arguments.
    `((debug 2)))))

(configure)
|#

;; Since we want people to be able to use this thing without depending on our
;; sprawling monorepo, where the "config" package currently resides,
;; FAKE IT:

(defvar *config* '(:optimization-settings ((debug 2))))

;; EOF
