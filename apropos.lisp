;;
;; apropos.lisp - What can I say?
;;

(defpackage :apropos
  (:documentation "Dans lequel nous échouons preuve d'à propos.")
  (:use :cl :cl-ppcre :dlib :collections :char-util :table :grout :syntax
	:syntax-lisp :lish)
  (:export
   #:mondo-apropos #:!apropos
   ))
(in-package :apropos)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defgeneric print-info (thing)
  (:documentation "Print information about something we found.")
  (:method ((thing symbol))
    (let ((token (make-instance 'lisp-symbol-token :object thing))
	  (s (string thing)))
      (grout-format "~a~%" (stylize-token token
					  :case (string-character-case s)
					  :package-p t))))
  (:method ((thing t))
    (grout-format "~s~%" thing)))

(defun symbol-apropos (thing &key package external-only)
  (declare (ignore package external-only)) ;; @@@
  (let* ((thing-string (princ-to-string thing))
	 (scanner
	  (create-scanner
	   ;; thing-string :case-insensitive-mode (not (functionp thing))))
	   thing-string :case-insensitive-mode t))
	 (table (make-hash-table))
	 matches did-one)
    ;; We use a hash table since symbols are frequently duplicated in packages,
    ;; and we only want each once.
    ;; Presumably with-package-iterator is faster than do-
    (do-all-symbols (symbol)
      (when (not (gethash symbol table))
	(setf (gethash symbol table) t)))
    (omapk (_ (let ((str (string (aref _ 0))))
		;;(when (funcall scanner str 0 (length str))
		(when (scan scanner str)
		  (push (aref _ 0) matches))))
	   table)
    (loop :for match :in matches :do
       ;;(loop :for pkg :in (list-all-packages) :do
       (multiple-value-bind (symbol status)
	   (ignore-errors (find-symbol (string match)
				       (symbol-package match)))
	 (when (and symbol (or (eq status :internal)
			       (eq status :external)))
	   ;; (grout-format "~a:~%" (package-name pkg))
	   (when (not did-one)
	     (grout-format "Lisp symbols:~%"))
	   (setf did-one t)
	   (print-info symbol))))
    did-one))

(defun command-apropos (thing)
  (let* ((scanner
	  (create-scanner (princ-to-string thing) :case-insensitive-mode t))
	 (tab
	  (loop :with cmd :and doc
	     :for c :in lish::*command-list*
	     :do
	     (setf cmd (lish:get-command c)
		   doc (documentation (lish:command-function cmd) 'function))
	     :when (or (scan scanner c)
		       (scan scanner doc))
	     :collect (list (lish:command-name cmd) doc))))
    (when tab
      (grout-format "Lish commands:~%")
      (grout-print-table (make-table-from
			  tab :column-names '("Command" "Description")))
      t)))

(defun system-apropos (thing)
  (let (did-one)
    (when (find-package :quicklisp)
      (loop :for system :in (symbol-call :ql :system-apropos-list thing)
	 :do 
	 (when (not did-one)
	   (grout-format "Quicklisp systems:~%"))
	 (grout-format "~a~%"
		       ;; I wish there was a real description.
		       (symbol-call :ql-dist :short-description system))
	 (setf did-one t)))
    did-one))

(defun os-apropos (thing)
  ;; /var/cache/man/index.db
  ;; GNU dbm 1.x or ndbm database, little endian, 64-bit
  ;; Ugh. For now just run the dang thing.
  (let ((full (nos:command-pathname "apropos"))
	table)
    (when full
      (setf table
	    (loop :with pos
	       :for line :in (!_= full thing)
	       :when (setf pos (search "- " line))
	       :collect (list (rtrim (subseq line 0 pos))
			      (ltrim (subseq line (1+ pos))))))
      (when (and table (olength table))
	(grout-format "System commands:~%")
	(grout-print-table
	 (make-table-from table :column-names '("Command" "Description")))
	t))))

(defun mondo-apropos (&key thing type package
			os-only lish-only lisp-only external-only)
  "Look for stuff you can do."
  (declare (ignore type))
  (with-grout ()
    (let ((did-one nil))
      (when (not (or os-only lish-only))
	(setf did-one
	      (symbol-apropos thing :package package
			      :external-only external-only)))
      (when (not (or lish-only lisp-only))
	(when did-one (grout-princ #\newline))
	(setf did-one (system-apropos thing)))
      (when (not (or os-only lisp-only))
	(when did-one (grout-princ #\newline))
	(setf did-one (command-apropos thing)))
      (when (not (or lish-only lisp-only))
	(when did-one (grout-princ #\newline))
	(setf did-one (os-apropos thing))))))

#|
;; test the speed of 3 different ways

(defun way1 ()
  (with-package-iterator (foo (list-all-packages) :external :internal)
    (loop :with s :and duh
       :do (multiple-value-setq (duh s) (foo))
       :while duh
       :do
       ;; (format t "~s~%" s)
       (+ 2 3)
       )))

(defun way2 ()
  (loop :for p :in (list-all-packages)
     :do (do-symbols (s p)
	   ;; (format t "~s~%" s)
	   (+ 2 3)
	   )))

(defun way3 ()
  (do-all-symbols (s)
    ;; (format t "~s~%" s)
    (+ 2 3)
    ))

(defun test-speed (&optional (n 100))
  (time (dotimes (i n) (apropos::way1)))
  (time (dotimes (i n) (apropos::way2)))
  (time (dotimes (i n) (apropos::way3))))

|#
    
#+lish
(lish:defcommand apropos
  ((os-only boolean :short-arg #\o
    :help "True to search for operating system commands only.")
   (lish-only boolean :short-arg #\l
    :help "True to search for Lish commands only.")
   (lisp-only boolean :short-arg #\L
    :help "True to search for Common Lisp symbols only.")
   (external-only boolean :short-arg #\e
    :help "Limit search to only external symbols in packages.")
   (package package :short-arg #\p
    :help "Limit search to this Lisp package.")
   (type symbol :short-arg #\t
    :help "Limit search to this type of thing.")
   (thing object :help "Like what?"))
  :args-as args
  "Words given but not taken."
  (when (not thing)
    (error "But apropos what?"))
  (apply #'mondo-apropos args))

;; EOF
