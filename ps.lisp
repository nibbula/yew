;;
;; ps.lisp - Process status listing
;;

(defpackage :ps
  (:documentation "Process status listing")
  (:use :cl :dlib :dlib-misc :opsys #+unix :os-unix :table :table-print :grout
	:lish)
  (:export
   #:ps
   #:ps-tree
   ))
(in-package :ps)

;; (declaim
;;  (optimize (speed 3) (safety 0) (debug 3) (space 2) (compilation-speed 0)))

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 2) (compilation-speed 0)))

;; @@@ I really need to re-think this whole rigmarole.

(defstruct short-process
   pid
   parent-pid
   user
   size
   name)

;; (defun process-list ()
;;   "Returns a list of lists of process data, consisting of:
;;user, pid, ppid, size, command."
;;   (sysctl

#|
     %cpu       percentage CPU usage (alias pcpu)
     %mem       percentage memory usage (alias pmem)
     acflag     accounting flag (alias acflg)
     args       command and arguments
     comm       command
     command    command and arguments
     cpu        short-term CPU usage factor (for scheduling)
     etime      elapsed running time
     flags      the process flags, in hexadecimal (alias f)
     gid        processes group id (alias group)
     inblk      total blocks read (alias inblock)
     jobc       job control count
     ktrace     tracing flags
     ktracep    tracing vnode
     lim        memoryuse limit
     logname    login name of user who started the session
     lstart     time started
     majflt     total page faults
     minflt     total page reclaims
     msgrcv     total messages received (reads from pipes/sockets)
     msgsnd     total messages sent (writes on pipes/sockets)
     nice       nice value (alias ni)
     nivcsw     total involuntary context switches
     nsigs      total signals taken (alias nsignals)
     nswap      total swaps in/out
     nvcsw      total voluntary context switches
     nwchan     wait channel (as an address)
     oublk      total blocks written (alias oublock)
     p_ru       resource usage (valid only for zombie)
     paddr      swap address
     pagein     pageins (same as majflt)
     pgid       process group number
     pid        process ID
     ppid       parent process ID
     pri        scheduling priority
     re         core residency time (in seconds; 127 = infinity)
     rgid       real group ID
     rss        resident set size
     ruid       real user ID
     ruser      user name (from ruid)
     sess       session ID
     sig        pending signals (alias pending)
     sigmask    blocked signals (alias blocked)
     sl         sleep time (in seconds; 127 = infinity)
     start      time started
     state      symbolic process state (alias stat)
     svgid      saved gid from a setgid executable
     svuid      saved UID from a setuid executable
     tdev       control terminal device number
     time       accumulated CPU time, user + system (alias cputime)
     tpgid      control terminal process group ID
     tsess      control terminal session ID
     tsiz       text size (in Kbytes)
     tt         control terminal name (two letter abbreviation)
     tty        full name of control terminal
     ucomm      name to be used for accounting
     uid        effective user ID
     upr        scheduling priority on return from system call (alias usrpri)
     user       user name (from UID)
     utime      user CPU time (alias putime)
     vsz        virtual size in Kbytes (alias vsize)
     wchan      wait channel (as a symbolic name)
     wq         total number of workqueue threads
     wqb        number of blocked workqueue threads
     wqr        number of running workqueue threads
     wql        workqueue limit status (C = constrained thread limit, T = total thread limit)
     xstat      exit or stop status (valid only for stopped or zombie process)

|#

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
  #+darwin (declare (ignore show-kernel-processes))
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
		(list (unix-process-command p))
		;;(map 'list #'identity (unix-process-args p))
		(join-by 'string (unix-process-args p) #\space)
		)))
  #+windows (declare (ignore show-kernel-processes))
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
(defun make-tree (proc-list)
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
    (format t "~d~15t~6d ~8a ~8@a ~va~{~a ~}~%"
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
;	     (format t "~s ~a~%" x (find x plist :key #'second))
	     (tree-print-proc (find (first x) plist :key #'second)
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

;; (x 0 1 x)
;; (x 1 2 x)
;; (x 1 3 x)
;; (x 1 6 x)
;; (x 3 4 x)
;; (x 3 5 x)
;; (x 0 7 x)
;;
;; ((x 0 1 x) (x 1 2 x) (x 1 3 x) (x 1 6 x) (x 3 4 x) (x 3 5 x) (x 0 7 x))
;;
;; (0 (1 (2 3 (4 5) 6) 7))
;; 0
;;  1
;;   2
;;   3
;;    4
;;    5
;;   6
;;  7

;; This is just for a particularly complaintive implementation.
;; (declaim (inline sort-muffled))
;; (defun sort-muffled (seq pred &rest args &key key)
;;   (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
;; 	   (ignorable key))
;;   (apply #'sort seq pred args))

(defun ps-tree ()
  (format t "~6d ~8a ~8@a ~{~a ~}~%" "PID" "User" "Size" '("Command"))
  (let* ((plist (sort-muffled (copy-list (list-processes)) #'<
			      :key #'short-process-pid))
	 (tree (make-tree plist)))
    (print-tree tree plist)))

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

;; (defun user-filter (user list)
;;   (loop :for p :in list
;;      :when (
;;   )

#+lish
(lish:defcommand ps-tree ()
  "Show a tree of processes."
  (ps-tree))

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

(defun ps-short (&key matching show-kernel-processes (print t) user)
  "Process status: Reformat the output of the \"ps\" command."
  (with-grout ()
    (let* ((proc-list (sort-muffled
		       (copy-list
			(list-processes
			 :show-kernel-processes show-kernel-processes))
		       #'> :key #'short-process-size))
	   (matching-num (or (and (integerp matching) matching)
			     (ignore-errors (parse-integer matching))))
	   (out-list
	    (if matching
		(loop :for p :in proc-list
		   :if (or (and matching-num
				(= matching-num (short-process-pid p)))
			   (and (stringp matching)
				(some
				 (_ (search matching _ :test #'equalp))
				 (append (list (short-process-user p))
					 (short-process-name p)))))
		   :collect (list (short-process-user p) (short-process-pid p)
				  (ps-print-size (short-process-size p))
				  (short-process-name p)))
		(loop :for p :in proc-list
		   :collect (list (short-process-user p) (short-process-pid p)
				  (ps-print-size (short-process-size p))
				  (short-process-name p)))))
	   table)
      (when user
	(setf out-list (delete-if (_ (not (equalp (short-process-user _) user)))
				  out-list)))
      (setf table (make-table-from
		   out-list
		   :column-names '("User" "PID" ("Size" :right) "Command")))
      (when print
	(grout-print-table table :trailing-spaces nil))
      table)))

(defun ps-long (&key matching show-kernel-processes (print t) user)
  "Process status: Reformat the output of the \"ps\" command."
  (declare (ignore matching show-kernel-processes user))
  (with-grout ()
    (let* ((proc-list (system-process-list))
	   table)
      (setf table (make-table-from proc-list))
      (when print
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
	    (loop :for p :in proc-list :do
	       (fix-time wos::ms-process-creation-time)
	       (fix-time wos::ms-process-exit-time)
	       (fix-time wos::ms-process-kernel-time)
	       (fix-time wos::ms-process-user-time)))
	  (loop
	     :for col :in (table:table-columns table)
	     :for name :in '(("PID" :right)
			     ("PPID" :right)
			     ("Thread" :right)
			     ("Priority" :right)
			     "Create"
			     "Exit"
			     "Kernel"
			     "User"
			     ("GUI" :right)
			     ("Handles" :right)
			     ("Max WS" :right)
			     ("Min WS" :right)
			     "Name"
			     "File")
	     ;; :for type :in '(number number number number string string
	     ;; 		     string string number number number number
	     ;; 		     string string)
	     :do (setf (table:column-name col) name)))
	(grout-print-table
	 table :trailing-spaces nil :long-titles t :max-width nil))
      table)))

(defun user-name-list ()
  (mapcar #'nos:user-info-name (nos:user-list)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass arg-user (arg-lenient-choice)
    ()
    (:default-initargs
     :choice-func #'user-name-list)
    (:documentation "User name.")))

#+lish
(lish:defcommand ps
  ((matching string :help "Only show processes matching this.")
   (show-kernel-processes boolean :short-arg #\k :default nil
    :help "True to show kernel processes.")
   (user user :short-arg #\u :help "User to show processes for.")
   ;; (sort-by choice :short-arg #\s :default "size" :help "Field to sort by."
   ;; 	    #| :choice-func #'process-columns |# )
   (long boolean :short-arg #\l :help "True to show the long output."))
  "Process status."
  (setf *output*
	(if long
	    (ps-long :matching matching
		     :show-kernel-processes show-kernel-processes
		     :user user)
	    (ps-short :matching matching
		      :show-kernel-processes show-kernel-processes
		      :user user))))

;; EOF
