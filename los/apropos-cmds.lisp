;;;
;;; apropos-cmds.lisp - Shell commands for apropos.
;;;

(in-package :apropos)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

#+lish
(lish:defcommand apropos
  ((os-only boolean :short-arg #\o
    :help "Search for operating system commands only.")
   (lish-only boolean :short-arg #\L
    :help "Search for Lish commands only.")
   (lisp-only boolean :short-arg #\l
    :help "Search for Common Lisp symbols only.")
   (quicklisp-only boolean :short-arg #\q
    :help "Search for Quicklisp systems only.")
   (external-only boolean :short-arg #\e
    :help "Limit search to only external symbols in packages.")
   (package package :short-arg #\p
    :help "Limit search to this Lisp package.")
   (type symbol :short-arg #\t
    :help "Limit search to this type of thing.")
   (collect boolean :short-arg #\c :help "Collect results into lists.")
   (compound boolean :short-arg #\C
    :help "Use compound prefix matching for Lisp symbols, e.g. w-o-t-s.")
   (thing object :help "Like what?"))
  :args-as args
  "Words given but not taken."
  (let ((types *types*))
    (when os-only        (setf types '(:os)))
    (when lish-only      (setf types '(:lish)))
    (when lisp-only      (setf types '(:lisp)))
    (when quicklisp-only (setf types '(:quicklisp)))
    (when type (setf types (list type)))
    (if collect
	(setf *output*
	      (mondo-apropos :thing thing :types types :package package
			     :external-only external-only :collect collect
			     :compound compound))
	(mondo-apropos :thing thing :types types :package package
		       :external-only external-only :collect collect
		       :compound compound))))

;; End
