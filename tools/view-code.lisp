;;;
;;; view-code.lisp - View Lisp code with the tree viewer.
;;;

(defpackage :view-code
  (:documentation "View Lisp code with the tree viewer.

This is a somewhat weird experiment, and not very useful yet.
")
  (:use :cl :terminal :tree-viewer :reader-ext :tree-editor)
  (:export
   #:view-code
   #:view-project
   ))
(in-package :view-code)

(defclass code-node (object-node)
  ())

;; @@@ It might be cool if we could use the pretty-printer to figure out
;; how to group expressions into lines, but of course then they would lose
;; collapsability unless we mutated the viewer somehow.
;; @@@ see lab/pprint-color.lisp for why we would probably need a modified
;; pretty printer.

(defmethod display-node ((node code-node) level)
  (let ((prefix (display-prefix node level)))
    (tt-write-string prefix)
    (setf (tb::current-left tb::*viewer*)
	  (second (multiple-value-list
		   (terminal:terminal-get-cursor-position *terminal*)))))
  (flet ((get-name (n) (node-object (first (node-branches n))))
	 (get-args (n)
	   (let ((arg-node (second (node-branches n))))
	     (when arg-node
	       (append (list (node-object arg-node))
		       (loop :for s :in (node-branches arg-node)
			     :when (node-object s)
			     :collect (node-object s)))))))
    (cond
      ((symbolp (node-object node))
       (case (node-object node)
	 ((defun defmacro defgeneric defmethod)
	     (let* ((name (get-name node))
		    (args (get-args node)))
	       (tt-color :magenta :default)
	       (tt-format "~(~s~) " (node-object node))
	       (tt-color :green :default)
	       (tt-format "~(~a~)" name)
	       (tt-format " (~{~(~w~)~^ ~})~%" args)))
	 ((defstruct defclass deftype defvar defparameter defconstant)
	  (tt-color :magenta :default)
	  (tt-format "~(~s~)" (node-object node))
	  (tt-color :green :default)
	  (tt-format " ~(~a~)~%" (get-name node)))
      	 (otherwise
	  (let* ((pkg (symbol-package (node-object node)))
		 (pkg-name (and pkg (package-name pkg))))
	    (cond
	      ((equal pkg-name "COMMON-LISP")
	       (tt-color :magenta :default)
	       (tt-format "~(~s~)~%" (node-object node))
	       (tt-color :green :default))
	      ((equal pkg-name "KEYWORD")
	       (tt-color :blue :default)
	       (tt-format "~(~s~)~%" (node-object node))
	       (tt-color :green :default))
	      (t
	       (tt-format "~(~a~)~%" (node-object node))))))))
      ((stringp (node-object node))
       (tt-color :cyan :default)
       (tt-format "~s~%" (node-object node))
       (tt-color :green :default))
      (t
       (let ((*print-case* :downcase))
	 (tt-format "~s~%" (node-object node))))))
  (values))

(defun fake-reader (stream subchar arg)
  "A reader macro which should have no effect on the following form."
  (declare (ignore subchar arg))
  (read stream t nil t))

(defvar *safer-readtable* nil)

(defun safer-read (stream)
  (with-simple-restart (abort "Don't bother reading this thing.")
    (when (not *safer-readtable*)
      (setf *safer-readtable* (copy-readtable))
      ;; This should make it so that #. neither evals or errors but just
      ;; reads the thing, unlike setting *read-eval* false.
      (set-dispatch-macro-character #\# #\. #'fake-reader *safer-readtable*))
    (let ((*readtable* *safer-readtable*))
      (package-robust-read stream nil nil))))

(defun view-code (&optional (file (pick-list:pick-file)))
  "This shows why s-exps are cool."
  (with-simple-restart (abort "Give up viewing this file.")
    (with-open-file (stm file)
      (let ((editor (make-instance 'tree-editor :node-type 'code-node)))
	(edit-tree
	 (convert-tree
	  (append (list file)
		  (loop :with exp
			:while (setf exp (safer-read stm))
			:collect exp))
	  :type 'code-node)
	 :editor editor)))))

#+lish
(lish:defcommand view-code
  ((file pathname :optional nil :help "File to view."))
  "View Lisp code with the tree viewer."
  (view-code file))

;; (defun fake-code-view-dir (&optional (directory "."))
;;   "This shows why s-exps are cool."
;;   (view-tree
;;    (convert-tree
;;     (loop :for file :in (glob:glob (s+ directory "/*.lisp"))
;;        :collect
;;        (with-open-file (stm file)
;; 	 (append (list file)
;; 		 (loop :with exp
;; 		    :while
;; 		    (setf exp (safer-read stm))
;; 		    :collect exp))))
;;     :type 'code-node)))

(defun view-code-files (&optional files)
  (view-tree
   (convert-tree
    (append (list "Files")
	    (loop :for file :in (or files (glob:glob "*.lisp"))
	       :collect
	       ;; @@@ (handler-case
	       (with-open-file (stm file)
		 (append (list file)
			 (loop :with exp
			    :while
			    (setf exp (safer-read stm))
			    :collect exp)))))
    :type 'code-node)))

;; This doesn't really work, because we need a reader that can handle
;; unknown package prefixes and other errors.
(defun view-project (&rest files)
  (let* ((ff (or files '("*.lisp" "*.asd")))
	 (pp (or (loop :for f :in ff :appending (glob:glob f))
		 (glob:glob "*"))))
    (view-code-files (pick-list:pick-list pp :multiple t))))

;; EOF
