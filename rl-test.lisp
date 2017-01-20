;;
;; rl-test.lisp - Tests for the RL line editor.
;;

;; Copyright © 2007-2017 Nibby Nebbulous
;; Licensed under the GPL (See file LICENSE for details).

(defpackage :rl-test
  (:documentation "Tests for the RL line editor.")
  (:use :cl :test :rl)
  (:export
   #:run
   ))
(in-package :rl-test)

(deftests (rl-1 :doc "Test things that are external requirements.")
  "I'm not really sure what to do for non-interactive tests."
  )

(deftests (rl-all :doc "Test the RL line editor.")
  rl-1)

(defun run ()
  (run-group-name 'rl-all :verbose t))

;; EOF
