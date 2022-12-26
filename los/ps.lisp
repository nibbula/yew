;;;
;;; ps.lisp - Process status listing
;;;

(defpackage :ps
  (:documentation "Process status listing")
  (:use :cl :dlib :dlib-misc :opsys #+unix :os-unix #+windows :os-ms
        :table :table-print :grout :collections :los-config :los-util
        :dtime)
  (:export
   #:!ps
   #:ps-tree
   #:describe-processes
   #:ps
   ))
(in-package :ps)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))
;; (declaim
;;  (optimize (speed 3) (safety 0) (debug 3) (space 2) (compilation-speed 0)))

;; @@@ I really need to re-think this whole rigmarole.
;;
;; collect
;; select
;; process
;; print
;; struct/class

(defstruct short-process
   pid
   parent-pid
   user
   size
   name)

(defvar *ps-args*
  #+solaris
  '("ps" ("-Ay" "-o" "user=" "-o" "pid=" "-o" "ppid=" "-o" "vsz=" "-o" "args="))
  #+(or darwin linux freebsd)
  '("ps" ("-A" "-o" "user=" "-o" "pid=" "-o" "ppid=" "-o" "vsz=" "-o" "args="))
  #-(or solaris darwin linux freebsd)
  nil)

(defun process-list-from-ps ()
  "Returns a list of lists of process data, consisting of:
user, pid, ppid, size, command."
  (nos:with-process-output (s (first *ps-args*) (second *ps-args*))
    (loop
       :with l = nil :and z = nil
       :while (setf l (read-line s nil nil))
       :collect (progn (setf z (split-sequence " " l :omit-empty t))
		       (make-short-process
			:pid (parse-integer (second z))
			:parent-pid (parse-integer (third z))
			:user (first z)
			;; because ps is in KiB
			:size (* (parse-integer (fourth z)) 1024)
			:name (cddddr z))))))

(defun list-processes (&key (show-kernel-processes t))
  #-linux (declare (ignore show-kernel-processes))
  #+darwin (process-list-from-ps)
  #+linux
  ;; We don't have to use ps.
  (loop :for p :in (if show-kernel-processes
		       (system-process-list)
		       (remove 0 (system-process-list)
			       :key #'unix-process-text-size))
     :collect
     (make-short-process
      :user (user-name (unix-process-user-id p))
      :pid (unix-process-id p)
      :parent-pid (unix-process-parent-id p)
      :size (unix-process-text-size p)
      :name (if (zerop (length (unix-process-args p)))
		;;(list (unix-process-command p))
		(unix-process-command p)
		;;(map 'list #'identity (unix-process-args p))
		(join-by 'string (unix-process-args p) #\space)
		)))
  #+windows
  ;; (loop :for p :in (if show-kernel-processes
  ;; 		       (process-list)
  ;; 		       (remove 0 (process-list)
  ;; 			       :key #'nos:os-process-text-size))
  (loop :for p :in (process-list)
     :collect
     (make-short-process
      :user       (os-process-user p)
      :pid        (os-process-id p)
      :parent-pid (os-process-parent-id p)
      :size       (os-process-size p)
      :name       (os-process-name p))))

(defun ps-print-size (n)
  "Print the size in our prefered style."
  (print-size n :traditional t :stream nil
	      :format "~:[~3,1f~;~d~]~@[~a~]~@[~a~]"))

(defun ps-print-size-with-width (n width)
  "Print the size in our prefered style."
  (if (numberp n)
      (if width
	  (format nil "~v@a" width
		  (print-size n :traditional t :stream nil
			      :format "~:[~3,1f~;~d~]~@[~a~]~@[~a~]"))
	  (ps-print-size n))
      (if width
	  (format nil "~v@a" width n)
	  (format nil "~@a" n))))

;; (defun print-proc (p)
;;   (format t "~8a ~6d ~8@a ~{~a ~}~%"
;; 	  (first p) (second p)
;; 	  (ps-print-size (fourth p))
;; 	  (fifth p)))

(defun find-node (pid tree)
  (declare (type integer pid))
  (loop :for p :on tree :do
     (typecase (car p)
       (list
	(let ((n (find-node pid (car p))))
	  (if n (return-from find-node n))))
       (integer
	(when (= (car p) pid)
	  (return-from find-node p))))))
     ;; (cond
     ;;   ((listp (car p))
     ;; 	(let ((n (find-node pid (car p))))
     ;; 	  (if n (return-from find-node n))))
     ;;   ((and (integerp (car p)) (= (car p) pid))
     ;; 	(return-from find-node p)))))

(defun add-node (ppid pid tree)
  (if tree
      (let ((n (find-node ppid tree)))
	(if n
	    (progn
	      (if (and (cadr n) (listp (cadr n)))
		  (nconc (cadr n) (list pid))
		  (rplacd n (cons (list pid) (cdr n)))))
	    (nconc tree (list ppid (list pid))))
	tree)
      (if (= ppid pid)
	  (list ppid)
	  (list ppid (list pid)))))

;; @@@ need to add all parents first?
(defun make-process-tree (proc-list)
  (let (tree)
    (loop :for p :in proc-list :do
       ;; (setf tree (add-node (third p) (second p) tree)))
       (setf tree (add-node (short-process-parent-pid p)
			    (short-process-pid p) tree)))
    tree))

(defun tree-print-proc (p level prefix)
  (declare (ignore prefix))
  (when (> level 0)
    (format t "~v@a" level "├──"))
  (when p
    (format t "~d~15t~6d ~8a ~8@a ~va ~a ~%"
	    ;; (second p) (third p) (first p) (ps-print-size (fourth p))
	    ;; level "" (fifth p))))
	    (short-process-pid p) (short-process-parent-pid p)
	    (short-process-user p) (ps-print-size (short-process-size p))
	    level "" (short-process-name p))))

(defun print-tree (tree plist &key (level 0) prefix)
  (when tree
    (loop :for x :on tree :do
       (if (consp (first x))
	   (print-tree (first x) plist :level (1+ level))
	   (progn 
;	     (format t "~s ~a~%" x (find x plist :key #'short-process-pid))
	     (tree-print-proc (find (first x) plist :key #'short-process-pid)
			      level prefix))))))

(defun zprint-tree (tree &optional (level 0))
  (when tree
    (loop :for x :in tree :do
       (if (consp x)
	   (zprint-tree x (1+ level))
	   (progn 
	     (when (> level 0)
	       (format t "~va" level ""))
	     (format t "~s~%" x))))))

(defun ps-tree ()
  (format t "~6d ~8a ~8@a ~{~a ~}~%" "PID" "User" "Size" '("Command"))
  (let* ((plist (sort-muffled (copy-list (list-processes)) #'<
			      :key #'short-process-pid))
	 (tree (make-process-tree plist)))
    (print-tree tree plist)
    tree))

;;;;;;;;;;;

(defclass ps-node (tree-viewer:object-node)
  ()
  (:documentation "Process tree node."))

(defmethod print-object ((object ps-node) stream)
  "Print a process tree node to STREAM."
  (if (or *print-readably* *print-escape*)
      (print-unreadable-object (object stream)
       	(format stream "ps-node ~s"
       		(nos:os-process-id (tree-viewer:node-object object))))
      (let ((p (tree-viewer:node-object object)))
	(format stream "~d ~a" (nos:os-process-id p)
		(nos:os-process-name p))))
  object)

;; (defmethod tb:display-node ((node ps-node) level)
;;   (let* ((str (princ-to-string node)))
;;     (when (eql (char str (1- (length str))) #\newline)
;;       (setf (char str (1- (length str))) #\space))
;;     (tb:display-object node str level)))

(defun ps-view-tree ()
  (let* ((plist (nos:process-list))
	 (tree (tb:make-tree
		(make-instance 'ps-node
		 :object (find 1 plist :key #'nos:os-process-id))
		(lambda (p)
		   (mapcar (_ (make-instance 'ps-node :object _))
			   (remove-if
			    (_ (not (eql (nos:os-process-parent-id _)
					 (nos:os-process-id (tb:node-object p)))))
			    plist)))
		:type 'ps-node)))
    (print tree)
    ;; (tree-viewer:view-tree (tb:convert-tree tree :type 'ps-node))
    ;; (tree-viewer:view-tree tree)
    ))

(defun fake-ps ()
  (with-grout ()
    (let ((proc-list (sort-muffled
		      (nos:process-list) #'< :key #'os-process-id)))
      (grout-print-table
       (make-table-from
	(loop :for p :in proc-list
	   :collect (list
		     (os-process-id p)
		     (os-process-parent-id p)
		     ;;(os-process-percent-cpu p)
		     ;;(os-process-resident-size p)
		     ;;(os-process-text-size p)
		     (os-process-size p)
		     ;;(os-process-command p)
		     (os-process-name p)
		     ))
	;;:column-names '("PID" "PPID" "CPU" "Size" "T Size" "Command"))
	:column-names '("PID" "PPID" "Size" "Command"))
       :trailing-spaces nil)))
  (values))

;; @@@ Maybe fix opsys to use classes. This is very bogus.

(defun process-id (p)
  (etypecase p
    (short-process (short-process-pid p))
    #+unix (unix-process (uos:unix-process-id p))
    #+windows (ms-process (wos:ms-process-pid p))))

(defun process-name (p)
  (etypecase p
    (short-process (short-process-name p))
    #+unix (unix-process (uos:unix-process-command p))
    #+windows (ms-process (wos:ms-process-name p))))

(defun process-uid (p)
  (etypecase p
    (short-process (user-id :name (short-process-user p)))
    #+unix (unix-process (uos:unix-process-user-id p))
    #+windows (ms-process 0)))

(defun process-user (p)
  (etypecase p
    (short-process (short-process-user p))
    #+unix (unix-process (user-name (uos:unix-process-id p)))
    #+windows (ms-process "")))

(defun filter (list matching user)
  "Filter out processes from ‘list’ which don't match ‘matching’ and are not
owned by ‘user’."
  #+windows (declare (ignore user)) ;; @@@ fix opsys to get the user
  (labels ((matching-one (p match)
	     (typecase match
	       (integer
		(= match (process-id p)))
	       (string
		(some (_ (search match _ :test #'equalp))
		      (append (list (process-user p))
			      (list (process-name p)))))))
	   (matching-p (p)
	     (typecase matching
	       (list
		(some (_ (matching-one p _)) matching))
	       ((or string integer)
		(matching-one p matching))
	       (t
		(matching-one p (princ-to-string matching))))))
      #+unix
      (when user
	(let ((uid (if (numberp user) user (user-id :name user))))
	  (setf list (remove-if-not (_ (= uid _)) list :key #'process-uid))))

      (if matching
	  (progn
	    (let ((as-int
		    (typecase matching
		      (string (ignore-errors (parse-integer matching)))
		      (list
		       (let ((m (mapcar
				 (_ (ignore-errors (parse-integer _)))
				 matching)))
			 (and (every #'integerp m) m))))))
	      (when as-int
		(setf matching as-int))
	      (remove-if-not #'matching-p list)))
	  list)))

(defun ps-short (&key matching show-kernel-processes (print t) user)
  "Process status: Reformat the output of the \"ps\" command."
  (with-grout ()
    (let* ((proc-list (sort-muffled
		       (copy-list
			(list-processes
			 :show-kernel-processes show-kernel-processes))
		       #'> :key #'short-process-size))
	   (out-list (loop :for p :in (filter proc-list matching user)
			   :collect (list (short-process-user p)
					  (short-process-pid p)
					  (short-process-size p)
					  (short-process-name p))))
	   table)
      (setf table (make-table-from
		   out-list
		   :columns
		   `((:name "User")
		     (:name "PID"  :align :right :type number)
		     (:name "Size" :align :right :type number
		      :format ,#'ps-print-size-with-width)
		     (:name "Command"))))
      (when print
	(grout-print-table table :trailing-spaces nil))
      table)))

(defun ps-long (&key matching show-kernel-processes (print t) user)
  "Process status: Reformat the output of the \"ps\" command."
  (declare (ignore #-linux show-kernel-processes))
  (with-grout ()
    (let* ((proc-list
	    #-linux (system-process-list)
	    #+linux (if show-kernel-processes
			(system-process-list)
			(remove 0 (system-process-list)
				:key #'unix-process-text-size)))
	   (out-list (filter proc-list matching user))
	   table)
      (setf table (make-table-from
		   #-linux out-list
		   #+linux
		   (progn
		     (omapn (_ (setf (uos:unix-process-args _)
				     (coerce (uos:unix-process-args _) 'list)))
			    out-list)
		     out-list)
		   #+linux
		   :columns
		   `((:name "PID"  :type number)
		     (:name "PPID" :type number)
		     (:name "GID"  :type number)
		     (:name "UID"  :type number)
		     (:name "TTY")
		     (:name "Text" :type number
		      :format ,#'ps-print-size-with-width)
		     (:name "RSS" :type number
		      :format ,#'ps-print-size-with-width)
		     (:name "CPU%" :type number)
		     (:name "Nice" :type number)
		     (:name "Usage")
		     (:name "Command")
		     (:name "Args" :format "~*~{~a~^ ~}"))))
      #+unix
      (progn
	(let ((i 0))
	  (omap (_ (when (typep _ 'number)
		     (table-set-column-type table i 'number))
		   (incf i))
		(oelt (container-data table) 0))))
      ;; @@@ make a workaround for printing the os-time on windows
      ;; The print-object methood should do something different based
      ;; on a dynamic variable. Or it would be nice if there was a way
      ;; to do it with the table renderer?
      ;;(make-method)
      ;;(add-method #'print-object (os-time))
      ;;(remove-method (find-method ))
      #+windows ;; or how about this bullcrap?
      (progn
	(macrolet ((fix-time (x)
		     `(cond
			((not (,x p)) (setf (,x p) ""))
			((integerp (,x p))
			 (setf (,x p)
			       (format-date
				"~2,'0d:~2,'0d:~2,'0d"
				(:hrs :min :sec)
				:time (truncate (,x p) (expt 10 7))))))))
	  (loop :for p :in out-list :do
	       (fix-time wos::ms-process-creation-time)
	       (fix-time wos::ms-process-exit-time)
	       (fix-time wos::ms-process-kernel-time)
	       (fix-time wos::ms-process-user-time)))
	(loop
	   :for col :in (table:table-columns table)
	   :for (name align) :in '(("PID" :right)
				   ("PPID" :right)
				   ("Thread" :right)
				   ("Priority" :right)
				   ("Create")
				   ("Exit")
				   ("Kernel")
				   ("User")
				   ("GUI" :right)
				   ("Handles" :right)
				   ("Max WS" :right)
				   ("Min WS" :right)
				   ("Name")
				   ("File"))
	   :do (setf (table:column-name col) name
		     (table:column-align col) align)))
      (when print
	(grout-print-table
	 table :trailing-spaces nil :long-titles t :max-width nil))
      table)))

;; (defun user-name-list ()
;;   (mapcar #'nos:user-info-name (nos:user-list)))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defargtype user (arg-lenient-choice)
;;     "User name."
;;     ()
;;     (:default-initargs
;;      :choice-func #'user-name-list))
;;   ;; (defclass arg-user (arg-lenient-choice)
;;   ;;   ()
;;   ;;   (:default-initargs
;;   ;;    :choice-func #'user-name-list)
;;   ;;   (:documentation "User name."))
;;   )

(defun describe-processes (&key matching show-kernel-processes user long quiet)
  (if long
      (ps-long :matching matching
	       :show-kernel-processes show-kernel-processes
	       :user user :print (not quiet))
      (ps-short :matching matching
		:show-kernel-processes show-kernel-processes
		:user user :print (not quiet))))

(defalias 'ps 'describe-processes)

;; EOF
