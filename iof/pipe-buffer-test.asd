;;;								-*- Lisp -*-
;;; pipe-buffer-test.asd -- System definition for pipe-buffer-test
;;;

(defsystem pipe-buffer-test
    :name               "pipe-buffer-test"
    :description        "Test pipe-buffers"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Test pipe-buffers"
    :depends-on (:pipe-buffer :test)
    :components
    ((:file "pipe-buffer-test"))
    ;; :in-order-to ((asdf:test-op
    :perform (test-op (o c)
	       (uiop:symbol-call :pipe-buffer-test :run-tests)))

;; (defmethod perform ((o test-op) (c (eql (find-system :pipe-buffer-test))))
;;   (funcall (intern (string '#:run-tests) '#:pipe-buffer-test)))

;; (defmethod operation-done-p ((o test-op)
;; 			     (c (eql (find-system :pipe-buffer-test))))
;;   nil)
