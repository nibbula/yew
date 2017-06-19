;;
;; rl-test.lisp - Tests for the RL line editor.
;;

(defpackage :rl-test
  (:documentation "Tests for the RL line editor.")
  (:use :cl :test :rl)
  (:export
   #:run
   ))
(in-package :rl-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(deftests (rl-1 :doc "Test things that are external requirements.")
  "I'm not really sure what to do for non-interactive tests."
  )

(deftests (rl-all :doc "Test the RL line editor.")
  rl-1)

(defun run ()
  (run-group-name 'rl-all :verbose t))

;; EOF
