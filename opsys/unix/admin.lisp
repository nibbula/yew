;;;
;;; admin.lisp - System administration.
;;;

(in-package :opsys-unix)

;; acct

(defparameter *linux-reboot-commands* nil "Commands for Linux reboot.")

#+linux
(define-to-list *linux-reboot-commands*
  #(#(+LINUX-REBOOT-CMD-RESTART+     #x01234567
      "Perform a hard reset now.")
    #(+LINUX-REBOOT-CMD-RESTART2+    #xa1b2c3d4
      "Restart the system with the command string in ‘arg’.")
    #(+LINUX-REBOOT-CMD-HALT+        #xcdef0123
      "Halt the system.")
    #(+LINUX-REBOOT-CMD-CAD-ON+      #x89abcdef
      "Enable reboot using Ctrl-Alt-Delete keystroke.")
    #(+LINUX-REBOOT-CMD-CAD-OFF+     #x00000000
      "Disable reboot using Ctrl-Alt-Delete keystroke.")
    #(+LINUX-REBOOT-CMD-POWER-OFF+   #x4321fedc
      "Stop system and switch power off if possible. ")
    #(+LINUX-REBOOT-CMD-SW-SUSPEND+  #xd000fce2
      "Suspend system using software suspend.")
    #(+LINUX-REBOOT-CMD-KEXEC+       #x45584543
      "Reboot system into new kernel.")))

#+linux
(defcfun reboot :int
  "Reboot the system according to ‘command’, which should be one of
*linux-reboot-commands*."
  (command :int))

(defun system-power-off ()
  (sync)
  #+linux (syscall (reboot +LINUX-REBOOT-CMD-POWER-OFF+))
  #-linux (missing-implementation 'system-power-off))

(defun system-restart ()
  (sync)
  #+linux (syscall (reboot +LINUX-REBOOT-CMD-RESTART+))
  #-linux (missing-implementation 'system-restart))

(defun system-suspend ()
  (sync)
  #+linux (syscall (reboot +LINUX-REBOOT-CMD-SW-SUSPEND+))
  #-linux (missing-implementation 'system-suspend))

;; End
