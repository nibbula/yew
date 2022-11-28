;;;
;;; syntax-lisp-test.lisp - Tests for syntax-lisp.
;;;

(defpackage :syntax-lisp-test
  (:documentation "Tests for syntax-lisp.")
  (:use :cl :test :syntax-lisp :terminal)
  (:export
   #:run
   ))
(in-package :syntax-lisp-test)

(defun flc (c)
  (with-terminal-output-to-string (:dumb)
    (format-lisp-comment c *terminal*)))

(deftests (syntax-lisp-format-comment :doc "Comment formatting.")
  (equal "" (flc "" ))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'mpp) #'matching-paren-position))

(deftests (syntax-lisp-matching-paren-position :doc "Paren matching.")
  (null (mpp ""))
  (zerop (mpp "("))
  (eql 2 (mpp "((("))
  (eql 1 (mpp "((()"))
  (eql 6 (mpp "(foo #(1 2"))
  "escaping"
  (null (mpp "#\\("))
  (eql 0 (mpp "(#\\)"))
  (null (mpp "(#\\"))
  "strings"
  (null (mpp "\"(\""))
  (eql 0 (mpp "(\"(\""))
  (eql 0 (mpp "(\"(\""))
  (null (mpp "\"(\\\"(\""))
  (eql 0 (mpp "(\"(\\\"(\""))
  "vectors"
  (eql 1 (mpp "#("))
  (eql 2 (mpp "#(("))
  (eql 1 (mpp "#(()"))
  "comments"
  (null (mpp ";("))
  (null (mpp "(;("))
  (eql 0 (mpp (format nil "(;(~%")))
  (eql 0 (mpp "(#|(|#"))
  (eql 0 (mpp "(#|((((|#"))
  "given position"
  (null (mpp "" :position 0))
  (zerop (mpp "(" :position 1))
  (eql 2 (mpp "(((" :position 3))
  (eql 1 (mpp "((()" :position 4))
  (eql 6 (mpp "(foo #(1 2" :position 10))
  "given position, implicit forward"
  "given char"
  (eql 0 (mpp "[" :char #\]))
  (null (mpp "(" :char #\]))
  (null (mpp "[(" :char #\]))
  "given char, implicit forward"
  "given pairs"
  )

(deftests (syntax-lisp-all :doc "All the tests.")
  syntax-lisp-format-comment
  syntax-lisp-matching-paren-position)

(defun run ()
  "Run all the syntax-lisp tests."
  (run-group-name 'syntax-lisp-all :verbose t))

;; End
