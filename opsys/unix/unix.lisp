;;;
;;; unix/unix.lisp - Miscellanous unix interface pieces
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;; Weird/simulation/emulation

(defcfun ("syscall" real-syscall) :long
  "Invoke a kernel system call with the NUMBER."
  (number :long) &rest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Go thru *features* and get rid of all our temporary configuration.
; (setf *features*
;       (delete-if #'(lambda (x)
; 		     (let ((s (string x)))
; 		       (string= s "OS-T-" :end1 (min 5 (length s)))))
; 		 *features*))

;; @@@ Should get rid of temporary features :os-t-*

;; End
