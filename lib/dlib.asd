;;;								-*- Lisp -*-
;;; dlib.asd -- System definition for DLIB
;;;

(defsystem dlib
    :name               "dlib"
    :description        "A crusty old shed of oddly shaped Lisp tools."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A pile of old tires."
    ;; Don't ever add :depends-on
    :components
    ((:file "dlib1")
     (:file "dlib2"))
    ;; :in-order-to ((test-op (load-op dlib-test)))
    ;;  :perform (test-op :after (op c)
    ;; 		      (funcall
    ;; 		       (intern (symbol-name (read-from-string "test-dlib"))
    ;; 			       :dlib-test)))
    )

;; (defmethod operation-done-p ((op test-op)
;;                              (c (eql (find-system :dlib))))
;;   (values nil))
;;
;; (defsystem dlib-test
;;   :depends-on (:dlib :dlib-misc :lift)
;;   :components ((:file "dlib-test")))
