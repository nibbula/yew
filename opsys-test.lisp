;;
;; opsys-test.lisp - Tests for OPSYS.
;;

(defpackage :opsys-test
  (:documentation "Tests for OPSYS.")
  (:use :cl :test :opsys)
  (:export
   #:run
   ))
(in-package :opsys-test)

(defvar *tty* nil)

(deftests (opsys-terminal-1 :doc "Basic tests")
  "Make sure there's a default device name."
  (not (null *default-console-device-name*))
  "Test opening and closing the default device."
  (and (not (null (setf *tty* (open-terminal *default-console-device-name*))))
       (equal 23 (progn (ignore-errors (close-terminal *tty*)) 23))))

(defun terminal-mode-sane-p (mode)
  "Test that the terminal mode has sane values."
  (and (member (terminal-mode-echo mode) '(t nil))
       (member (terminal-mode-line mode) '(t nil))
       (member (terminal-mode-raw mode) '(t nil))
       (or (null (terminal-mode-timeout mode))
	   (and (integerp (terminal-mode-timeout mode))
		(not (minusp (terminal-mode-timeout mode)))))))

(deftests (opsys-terminal-2
	   :doc "Test the terminal interface."
	   :setup (setf *tty* (open-terminal *default-console-device-name*))
	   :takedown (close-terminal *tty*))
  "Make sure the default device is a terminal."
  (file-handle-terminal-p *tty*)
  "Make sure it has a name."
  (and (not (null (file-handle-terminal-name *tty*)))
       (or (stringp (file-handle-terminal-name *tty*))
	   (pathnamep (file-handle-terminal-name *tty*))))
  "Get terminal mode"
  (not (null (get-terminal-mode *tty*)))
  "Terminal mode sanity"
  (terminal-mode-sane-p (get-terminal-mode *tty*))
  "Fresh terminal mode sanity"
  (terminal-mode-sane-p (make-terminal-mode))
  "Window size"
  (multiple-value-bind (cols rows) (get-window-size *tty*)
    (and (and (integerp rows) (not (minusp rows)))
	 (and (integerp cols) (not (minusp cols))))))

(defvar *saved-mode* nil)
(defvar *new-mode* nil)

(defun tty-setup ()
  (setf *tty* (open-terminal *default-console-device-name*)
	*saved-mode* (get-terminal-mode *tty*))
  (setf *new-mode* (get-terminal-mode *tty*)))

(defun tty-takedown ()
  (set-terminal-mode *tty* :mode *saved-mode*)
  (close-terminal *tty*))

(deftests (opsys-terminal-3 :doc "Setting terminal modes")
  :setup tty-setup
  :takedown tty-takedown
  "Set individual values"
  (progn
    (set-terminal-mode *tty* :echo nil :line nil :raw nil :timeout nil)
    (setf *new-mode* (get-terminal-mode *tty*))
    (terminal-mode-sane-p *new-mode*))
  (eq (terminal-mode-echo *new-mode*) nil)
  (eq (terminal-mode-line *new-mode*) nil)
  (eq (terminal-mode-raw *new-mode*) nil)
  (eq (terminal-mode-timeout *new-mode*) nil))

(deftests (opsys-all :doc "All tests for OPSYS.")
  opsys-terminal-1
  opsys-terminal-2
  opsys-terminal-3)

(defun run ()
  (run-group-name 'opsys-all :verbose t))

;; EOF
