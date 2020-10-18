;;
;; terminal-config.lisp - Build configuration for the TERMINAL pacakge.
;;

(defpackage :terminal-config
  (:documentation "Build configuration for terminal package.")
  (:use :cl :config)
  (:export
   #:*config*
   #:*configuration*
   ))
(in-package :terminal-config)

(defconfiguration
  ((optimization-settings list
    "Default optimization settings for each file/compilation unit?."
    ;; If we don't have at least debug 2, then most compilers won't save
    ;; the function arguments.
    `((debug 2))
    ;; `((safety 3) (debug 3)))))
    ;; `((speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0))
    )))

(configure)

;; EOF
