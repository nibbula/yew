;;;
;;; apropos.lisp - What can I say?
;;;

(defpackage :apropos
  (:documentation "Dans lequel nous échouons preuve d'à propos.")
  (:use :cl :dlib :collections :char-util :table :grout :syntax :syntax-lisp
	:lish)
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

(defun compound-scanner (regexp)
  (let* ((pos 0)
	 (expanded-regexp
	   (with-output-to-string (str)
	     ;; case insensitve and at the start or after a -
	     (write-string "(^|[-*+%!])" str)
	     (loop :with found
	       :do
	       (setf found nil)
	       (multiple-value-bind (s e #| ss ee |#)
		   (ppcre:scan "(\\S)-(\\S)" regexp :start pos)
		 (when s
		   (write-string (subseq regexp pos (1+ s)) str)
		   (write-string "[^-]*-" str)
		   (setf pos (1- e)
			 found t)))
	       :while (and found (< pos (1- (length regexp)))))
	     (write-string (subseq regexp pos) str))))
    (ppcre:create-scanner expanded-regexp :case-insensitive-mode t)))

(defun symbol-apropos (thing &key package external-only collect compound)
  (let* ((thing-string (princ-to-string thing))
	 (scanner
	   (cond
	     (compound
	      (compound-scanner thing-string))
	     (t
	      (ppcre:create-scanner
	       ;; thing-string :case-insensitive-mode (not (functionp thing))))
	       thing-string :case-insensitive-mode t))))
	 (table (make-hash-table))
	 matches did-one results)
    ;; We use a hash table since symbols are frequently duplicated in packages,
    ;; and we only want each once.
    (if package
	(do-symbols (symbol package)
	  (when (not (gethash symbol table))
	    (setf (gethash symbol table) t)))
	(do-all-symbols (symbol)
	  (when (not (gethash symbol table))
	    (setf (gethash symbol table) t))))
    (omapk (_ (let ((str (string (aref _ 0))))
		;;(when (funcall scanner str 0 (length str))
		(when (ppcre:scan scanner str)
		  (push (aref _ 0) matches))))
	   table)
    (loop :for match :in matches :do
       ;;(loop :for pkg :in (list-all-packages) :do
       (multiple-value-bind (symbol status)
	   (ignore-errors (find-symbol (string match)
				       (symbol-package match)))
	 (when (and symbol (or (and (eq status :internal) (not external-only))
			       (eq status :external)))
	   ;; (grout-format "~a:~%" (package-name pkg))
	   (when (not did-one)
	     (grout-format "Lisp symbols:~%"))
	   (setf did-one t)
	   (when collect
	     (push symbol results))
	   (print-info symbol))))
    (or (and collect results) did-one)))

(defun command-apropos (thing collect)
  (let* ((scanner
	  (ppcre:create-scanner (princ-to-string thing)
				:case-insensitive-mode t))
	 results
	 (tab
	  (loop :with cmd :and doc
	     :for c :in lish::*command-list*
	     :do
	     (setf cmd (lish:get-command c)
		   doc (and (typep cmd 'lish:command)
			    (documentation (lish:command-function cmd)
					   'function)))
	     :when (and doc
			(or (ppcre:scan scanner c)
			    (ppcre:scan scanner doc)))
	     :collect (list (lish:command-name cmd) doc)
	     ::and
	     :when collect
	     :do (push cmd results))))
    (when tab
      (grout-format "Lish commands:~%")
      (grout-print-table (make-table-from
			  tab :column-names '("Command" "Description")))
      (or (and collect results)
	  t))))

(defun system-apropos (thing &optional collect)
  (let (results)
    (when (find-package :quicklisp)
      (loop :for system :in (symbol-call :ql :system-apropos-list
					 (princ-to-string thing))
	 :do 
	 (when (not results)
	   (grout-format "Quicklisp systems:~%"))
	 (grout-format "~a~%"
		       ;; I wish there was a real description.
		       (symbol-call :ql-dist :short-description system))
	 (setf results (or (and collect (cons system results)) t))))
    results))

(defun os-apropos (thing &optional collect)
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
	(or (and collect table) t)))))

(defparameter *types* '(:lisp :lish :quicklisp :os))

;; Maybe it would be cool to have a tree browser output version?

(defun mondo-apropos (&key thing types package external-only collect compound)
  "Look for stuff you can do."
  (when (not thing)
    (error "But apropos what?"))
  (with-grout ()
    (let ((results nil) result)
      (when (find :lisp types)
	(when (and (setf result
			 (symbol-apropos thing :package package
					 :external-only external-only
					 :collect collect
					 :compound compound))
		   collect)
	  (push `(:lisp . ,result) results)))

      (when (find :quicklisp types)
	(when result
	  (grout-princ #\newline))
	(when (and (setf result (system-apropos thing collect)) collect)
	  (push `(:quicklisp . ,result) results)))

      (when (find :lish types)
	(when result
	  (grout-princ #\newline))
	(when (and (setf result (command-apropos thing collect)) collect)
	  (push `(:lish . ,result) results)))

      (when (find :os types)
	(when result (grout-princ #\newline))
	(when (and (setf result (os-apropos thing collect)) collect)
	  (push `(:os . ,result) results)))

      (if collect
	  results
	  (and result t)))))

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

;; EOF
