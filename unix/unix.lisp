;;
;; unix/unix.lisp - Miscellanous unix interface pieces
;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-like

;; @@@ Just use bordeaux-threads!
;; locks (mutexes)
;; create thread
;; join thread

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration???
;; is this even a good idea

;; reboot
;; acct

;; Weird/simulation/emulation

(defcfun ("syscall" real-syscall) :long
  "Invoke a kernel system call with the NUMBER."
  (number :long) &rest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(defcfun wcwidth :int (wc wchar-t))

#+clisp (shadowing-import 'ext:char-width)
#+clisp (export 'char-width)
#-clisp
(defun char-width (char)
  "Return the column width of CHAR. If it's not working as expected, you ~
   probably have to call setlocale first."
  (if (graphic-char-p char)		; assume this is equivalent to iswprint
      (wcwidth (char-code char))
      (error "Can't determine the width of a non-graphic character: ~s" char)))

(defun language ()
  "Return a the system language."
  (let (lang)
    ;; This is lame. I'd like to turn these into something more readable
    ;; to humans, but that seems likely to bitrot and error.
    (cond
      ((setf lang (env "LC_MESSAGES"))
       lang)
      ((setf lang (env "LC_ALL"))
       lang)
      ((setf lang (env "LANG"))
       lang)
      ;; But this is really just for look messages.
      ;; On one hand, the user having more control over what language messages
      ;; are in is good. On the other hand, then we don't have a concept of
      ;; what the system default language is set to.
      ;; Anyway we just take the first one.
      ((setf lang (env "LANGUAGE"))
       (setf lang (initial-span lang ":")))
      ;; (t "Unknown") ;; Or should we just return NIL?
      )))

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
