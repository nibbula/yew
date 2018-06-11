;;
;; unix/terminals.lisp - Unix interface to terminals
;;

(in-package :opsys-unix)

;; @@@ ../termios.lisp should be merged in here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ttys

;; The tty also stores the process group to know who to send job control
;; signals to.
(defcfun tcsetpgrp :int (fd :int) (pgid pid-t))
(defcfun tcgetpgrp pid-t (fd :int))

(defcfun isatty  :int (fd :int))
(defcfun ttyname :string (fd :int))

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (= (isatty fd) 1))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
;;;  (let ((ttn (ttyname fd)))
;;;  (and (not (null-pointer-p ttn)) ttn)))
  ;; @@@ XXX We should probably use ttyname_r
  (ttyname fd))

(defvar *default-console-device-name* "/dev/tty"
  "Name of the default console device.")

(defun open-terminal (device-name direction)
  "Open a terminal. Return the system file handle."
  (ecase direction
    (:output
     (open device-name
	   :direction :output
	   #-(or clisp abcl) :if-exists
	   #-(or clisp abcl) :append))
    (:input
     (syscall (posix-open device-name +O_RDWR+ 0)))))

(defun close-terminal (terminal-handle)
  "Close a terminal."
  (cond
    ((streamp terminal-handle)
     (close terminal-handle))
    ((integerp terminal-handle)
     (syscall (posix-close terminal-handle)))
    (t
     (error "Unrecognized type of terminal handle."))))

(defmacro with-terminal-signals (() &body body)
  "Evaluate the BODY with signal handlers set appropriately for reading from
a terminal."
  `(with-signal-handlers ((+SIGWINCH+ . sigwinch-handler)
			  (+SIGTSTP+  . tstp-handler))
     ,@body))

;; End
