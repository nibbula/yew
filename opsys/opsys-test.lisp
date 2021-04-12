;;;
;;; opsys-test.lisp - Tests for OPSYS.
;;;

(defpackage :opsys-test
  (:documentation "Tests for OPSYS.")
  (:use :cl :test :opsys)
  (:export
   #:run
   ))
(in-package :opsys-test)

(deftests (opsys-path-1 :doc "Test path manipulation.")
  (equal "foo" (path-file-name "foo"))
  (equal "bar" (path-file-name "foo/bar"))
  (equal "baz" (path-file-name "foo/bar/baz"))
  (equal "bar" (path-file-name "foo////bar"))
  (equal "baz" (path-file-name "foo////bar////baz"))
  (equal "foo" (path-file-name "/foo"))
  (equal "bar" (path-file-name "/foo/bar"))
  (equal ""    (path-file-name "/"))
  (equal ""           (path-directory-name "foo"))
  (equal "foo"        (path-directory-name "foo/bar"))
  (equal "foo/bar"    (path-directory-name "foo/bar/baz"))
  (equal "foo"        (path-directory-name "foo////bar"))
  (equal "foo////bar" (path-directory-name "foo////bar////baz"))
  (equal "/"          (path-directory-name "/foo"))
  (equal "/foo"       (path-directory-name "/foo/bar"))
  (equal "/"          (path-directory-name "/"))
  )

;; (deftests (opsys-dirs-1 :doc "Test directory functions.")
;;   ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminals

(defvar *tty* nil)

(deftests (opsys-terminal-1 :doc "Basic tests")
  "Make sure there's a default device name."
  (not (null *default-console-device-name*))
  "Test opening and closing the default device."
  (and (not (null (setf *tty* (open-terminal *default-console-device-name*
					     :output))))
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
	   :setup (setf *tty* (open-terminal *default-console-device-name*
					     :input))
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
  (setf *tty* (open-terminal *default-console-device-name* :input)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; os-streams

(defvar *test-file-num* nil)

(defun test-file-name (str)
  (format nil "~a-~d" str *test-file-num*))

(defun stream-setup ()
  (setf *test-file-num* (random (expt 2 30)))
  ;; file with 5 lines
  (with-open-file (str (test-file-name "fizz") :direction :output)
    (loop :for i :from 1 :to 5 :do
       (format str "line ~d~%" i)))

  ;; file with 5 lines the last of which doesn't have a trailing newline
  (with-open-file (str (test-file-name "fizz-nonl") :direction :output)
    (loop :for i :from 1 :to 4 :do
       (format str "line ~d~%" i))
    (format str "line ~d" 5))

  (with-open-file (str (test-file-name "fizz-chars") :direction :output)
    (format str "12345"))

  (with-open-file (str (test-file-name "fizz-blank") :direction :output)
    (format str "    x")))

(defun stream-takedown ()
  (dolist (x '("fizz" "fizz-nonl" "fizz-chars" "fizz-blank")) 
    (delete-file (test-file-name x))))

(defun lines-test (filename)
  (let (ss (okay t) (i 1))
    (unwind-protect
	 (progn
	   (setf ss (make-os-stream (test-file-name filename)))
	   ;; (setf ss (open (test-file-name filename)))
	   (loop :with line
	      :while (setf line (read-line ss nil nil))
	      :do
	      (when (not (equal line (format nil "line ~d" i)))
		(format t "line ~d is not equal~%" i)
		(setf okay nil))
	      (incf i))
	   (when (/= i 6)
	     (format t "last line is not 6 ~d~%" i)
	     (setf okay nil)))
      (close ss))
    okay))

(defun chars-test (filename)
  (let (ss (okay t) (i 1) c)
    (unwind-protect
	 (progn
	   (setf ss (make-os-stream (test-file-name filename)))
	   ;; (setf ss (open (test-file-name filename)))
	   (loop
	      :while (setf c (read-char ss nil))
	      :do
	      (when (not (char= c (digit-char i)))
		(format t "char ~d is not right~%" i)
		(setf okay nil))
	      (incf i))
	   (when (/= i 6)
	     (format t "last char is not 6 ~d~%" i)
	     (setf okay nil)))
      (close ss))
    okay))

(defmacro with-test-stream ((var file) &body body)
  `(let (,var (okay t))
     (unwind-protect
	  (progn
	    (setf ,var (make-os-stream (test-file-name ,file)))
	    ,@body)
       (close ,var))
     okay))

(deftests (opsys-stream-1 :doc "OS streams")
  :setup stream-setup
  :takedown stream-takedown
  "Test reading lines."
  (lines-test "fizz")
  "Test reading lines without a trailing newline."
  (lines-test "fizz-nonl")
  "Test reading by characters."
  (chars-test "fizz-chars")
  (with-test-stream (ss "fizz-chars")
    (char= #\1 (peek-char nil ss)))
  (with-test-stream (ss "fizz-chars")
    (char= #\2 (peek-char #\1 ss)))
  (with-test-stream (ss "fizz-blank")
    (char= #\x (peek-char t ss))))

(deftests (opsys-all :doc "All tests for OPSYS.")
  opsys-path-1
  opsys-terminal-1
  opsys-terminal-2
  opsys-terminal-3
  opsys-stream-1
  )

(defun run ()
  (run-group-name 'opsys-all :verbose t))

;; EOF
