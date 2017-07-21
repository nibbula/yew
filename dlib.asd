;;;								-*- Lisp -*-
;;; dlib.asd -- System definition for DLIB
;;;

(defsystem dlib
    :name               "dlib"
    :description        "Dan's lisp library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "A new pile of old tires."
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
