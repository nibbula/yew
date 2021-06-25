;;;
;;; misc.lisp - Miscellaneous small commands.
;;;

(defpackage :misc
  (:documentation "Miscellaneous small commands.")
  (:use :cl :dlib :opsys :terminal :collections :table :lish :grout)
  (:export
   #:!basename
   #:!clear
   #:!dirname
   #:!tty
   #:!uname
   #:!groups
   #:!id
   ))
(in-package :misc)

;; (defcommand za ((a string :rest t))
;;   (apply #'!= "/usr/bin/basename" a) (apply #'!= "basename" a))

;; @@@ some of the differences here are things in path-file-name that I think
;; I probably should fix.
(defcommand basename
  ((all boolean :short-arg #\a :long-arg "multiple"
    :help "All arguments are filenames.")
   (suffix string :short-arg #\s :help "Suffix to remove.")
   (collect boolean :short-arg #\c :help "Collect output as a sequence.")
   (files pathname :help "Path(s) to get the base name of." :rest t))
  :accepts (or sequence string pathname)
  "Print the file name without the directory name."
  (cond
    (suffix
     (setf all t))
    ((and (not suffix) (not all) (olength-at-least-p 2 files))
     (setf suffix (oelt files 1))))
  (when (and (not files) *input*)
    (typecase *input*
      ((or string pathname)
       (setf files (list *input*)))
      (sequence
       (setf files *input*))))
  (let (results)
    (labels ((snip (f)
	       (if suffix (remove-suffix f suffix) f))
	     (action (f)
	       (if collect
		   (push (path-file-name (snip f)) results)
		   (format t "~a~%" (path-file-name (snip f))))))
      (cond
	(all
	 (omapn (_ (action _)) files))
	(t
	 (action (first files)))))
    (when collect
      (setf *output* (nreverse results)))))

(defcommand clear ()
  "Clear the screen. You can just press Ctrl-L."
  (with-terminal ()
    (tt-home)
    (tt-clear)
    (tt-finish-output)))

(defcommand dirname
  ((collect boolean :short-arg #\c :help "Collect output as a sequence.")
   (files pathname :help "Path(s) to get the directory name of." :rest t))
  :accepts (or sequence string pathname)
  "Print the directory name of a file."
  (when (and (not files) *input*)
    (typecase *input*
      ((or string pathname)
       (setf files (list *input*)))
      (sequence
       (setf files *input*))))
  (let (results)
    (flet ((action (f)
	     (if collect
		 (push (path-directory-name f) results)
		 (format t "~a~%" (path-directory-name f)))))
      (omapn (_ (action _)) files))
    (when collect
      (setf *output* (nreverse results)))))

(defcommand tty
  ((lisp boolean :short-arg #\l :help "Use Lisp *standard-input*.")
   (type boolean :short-arg #\t :help "Print the name and type of *terminal*."))
  "Print the file name of the terminal on standard input."
  (let (fd name)
    (if type
	(if *terminal*
	    (progn
	      (let ((real-term
		     (or (terminal-wrapped-terminal *terminal*) *terminal*)))
		(format t "*terminal* is a ~a on ~a.~%"
			(type-of *terminal*)
			(setf name (terminal-device-name real-term)))
		(setf fd (terminal-file-descriptor real-term))))
	    (progn
	      (setf name "unknown")
	      (format t "*terminal* is not set.~%")))
	(progn
	  (when (not fd)
	    (setf fd (or (and lisp (stream-system-handle *standard-input*))
			 0)))
	  (setf name
		#+unix (if (uos:isatty fd) (uos:ttyname fd) "not a tty")
		#-unix "unknown")
	  (format t "~a~%" name)
	  (setf *output* name)))))

(defcommand uname
  ((system boolean :short-arg #\s :help "Print the operating system name.")
   (network boolean :short-arg #\n :help "Print the network name.")
   (release boolean :short-arg #\r :help "Print the kernel release.")
   (version boolean :short-arg #\v :help "Print the kernel version.")
   (machine boolean :short-arg #\m :help "Print the machine name.")
   (all boolean :short-arg #\a :help "Print all the infomation."))
  "Print system type information."
  (declare (ignorable system network version release machine))
  (when (not (or system network version release machine))
    (setf system t))
  (let ((first t))
    (macrolet ((foo (name func)
		 `(progn
		    (when (or ,name all)
		      (if first
			  (setf first nil)
			  (write-char #\space))
		      (princ (,func))))))
      (foo system  os-software-type)
      (foo network os-machine-instance)
      (foo release os-software-release)
      (foo version os-software-version)
      (foo machine os-machine-type)))
  (terpri)
  (values))

(defcommand groups ()
  "Print a list of groups that the current user is in."
  #+unix
  (let ((result (map 'list #'uos:group-name (uos:get-groups))))
    (format t "~{~a~^ ~}~%" result)
    (setf *output* result))
  #-unix
  (format t "I don't know how to show your groups on ~s.~%" *os*))

(defcommand id
  ((user user :default (nos:user-id) :optional t
    :help "The user to print information about.")
   (format choice :short-arg #\f :default :los :choices '("los" "unix")
    :help "Format for output."))
  "Print user information."
  (let ((id (if (and user (numberp user)) user (nos:user-id :name user))))
    (flet ((los-format ()
	     (with-grout ()
	       (let ((table
		      (make-table-from
		       (let (l)
			 (omapk (_ (push
				    (list
				     (name-to-title
				      (princ-to-string (oelt _ 0)))
				     (princ-to-string (oelt _ 1))) l))
				(nos:get-user-info :id user))
			 (setf l (nreverse l))
			 #+unix
			 (progn
			   (setf l
			     (append l
			       (list
			         (list
				  "Groups"
				  (format nil "~{~{~d(~(~a~))~}~^ ~}"
					  (map 'list
					       (_ (list _ (uos:group-name _)))
					       (uos:get-groups))))))))
			 l)
		       :columns '((:name "Name" :type string)
				  (:name "Value" :align :wrap)))))
		 (grout-print-table table)
		 (setf *output* table)))))
      (case (keywordify format)
	(:unix
	 (format t "uid=~d(~a) gid=~d(~a)" id (nos:user-name id)
		 (nos:group-id) (nos:group-name (nos:group-id)))
	 #+unix
	 (format t " groups=~{~{~d(~(~a~))~}~^,~}"
		 (map 'list (_ (list _ (uos:group-name _))) (uos:get-groups)))
	 (terpri))
	(:los (los-format))
	(otherwise (los-format))))))

(defun coerce-to-uid (thing what)
  (typecase thing
    (string
     (multiple-value-bind (n pos) (parse-integer thing :junk-allowed t)
       (if n
	   (if (= pos (length thing))
	       n
	       (nos:user-id :name thing))
	   (nos:user-id :name thing))))

    (integer thing)
    (t
     (error "~a should be string or integer, not a ~s." what (type-of thing)))))

(defun check-priority (priority)
  "Error of the priority is out of range."
  (let ((high (max nos:*os-process-most-favorable-priority*
		   nos:*os-process-least-favorable-priority*))
	(low (min nos:*os-process-most-favorable-priority*
		  nos:*os-process-least-favorable-priority*)))
    (when (or (> priority high) (< priority low))
      (error "priority should be between ~s (most favorable) and ~s ~
              (least favorable)."
	     nos:*os-process-most-favorable-priority*
	     nos:*os-process-least-favorable-priority*))))

;; @@@ arg dependent completion for id?
(defcommand renice
  ((priority integer :optional nil :help "The priority to set.")
   (group boolean :short-arg #\g :help "Set priority for the process group.")
   (pid   boolean :short-arg #\p :help "Set priority for the process ID.")
   (user  boolean :short-arg #\u :help "Set priority for the user.")
   (id object :help "What to set the priority of."))
  "Alter scheduling priority."
  (when user
    (setf id (coerce-to-uid id "ID")))
  (check-priority priority)
  (let ((key (cond
	      (group :group)
	      (user :user)
	      (pid :pid))))
    (setf (nos:os-process-priority key id) priority)))

(defmacro with-adjusted-priority ((original adjustment) &body body)
  #+unix
  `(let ((uos:*post-fork-hook*
	  (append uos:*post-fork-hook*
		  (list
		   (lambda ()
		     (setf (nos:os-process-priority :pid (uos:getpid))
			   (+ ,adjustment ,original)))))))
     ,@body)
  #-unix
  `(progn ,@body))

(defcommand nice
  ((adjustment integer :short-arg #\n :default 10
    :help
    "Integer to add to the process priority, making it more nice (lower scheduling priority).")
   (arguments string :rest t :help "Like the arguments to env."))
  "Run a system command with modified scheduling priority. With no arguments,
just print the current process priority."
  (if arguments
      (let* ((pid (nos:current-process-id))
	     (original (nos:os-process-priority :pid pid)))
	(check-priority (+ original adjustment))
	(with-adjusted-priority (original adjustment)
	  (lish:!env :arguments arguments)))
      (let ((result (nos:os-process-priority :pid (nos:current-process-id))))
	(format t "~d~%" result)
	(setf *output* result))))

(defun free-memory-table (&key in-bytes)
  (flet ((format-the-size (n width)
	   (if in-bytes
	       (format nil "~vd" width n)
	       (format nil "~v@a" width
		       (remove #\space
			       (dlib-misc:print-size n :traditional t
						     :stream nil :unit ""))))))
    (let* ((memory-unit-bytes   (nos:get-system-info :memory-unit-bytes))
	   (total-memory
	    (* (nos:get-system-info :total-memory) memory-unit-bytes))
	   (free-memory
	    (* (nos:get-system-info :free-memory) memory-unit-bytes))
	   (shared-memory
	    (* (nos:get-system-info :shared-memory) memory-unit-bytes))
	   (buffer-memory
	    (* (nos:get-system-info :buffer-memory) memory-unit-bytes))
	   (total-swap
	    (* (nos:get-system-info :total-swap) memory-unit-bytes))
	   (free-swap
	    (* (nos:get-system-info :free-swap) memory-unit-bytes))
	   ;; (processes           (nos:get-system-info :processes))
	   ;; (total-high-memory   (nos:get-system-info :total-high-memory))
	   ;; (free-high-memory    (nos:get-system-info :free-high-memory))
	   (table
	    (make-table-from
	     `(("Mem" ,total-memory ,(- total-memory free-memory) ,free-memory
		,shared-memory ,buffer-memory
		,(- total-memory free-memory shared-memory buffer-memory))
	       ("Swap" ,total-swap ,(- total-swap free-swap) ,free-swap  0 0
		,(- total-memory free-memory)))
	     :columns
	     `((:name "")
	       (:name "Total"      :align :right :type number
		      :format ,#'format-the-size)
	       (:name "Used"       :align :right :type number
		      :format ,#'format-the-size)
	       (:name "Free"       :align :right :type number
		      :format ,#'format-the-size)
	       (:name "Shared"     :align :right :type number
		      :format ,#'format-the-size)
	       (:name "Buff/Cache" :align :right :type number
		      :format ,#'format-the-size)
	       (:name "Available"  :align :right :type number
		      :format ,#'format-the-size)))))
      table)))

(defcommand free
  ((bytes boolean :short-arg #\b :help "Show the sizes in bytes.")
   (table boolean :short-arg #\t :help "Show as a table."))
  "Describe free memory."
  (declare (ignorable table))
  (let ((table (free-memory-table :in-bytes bytes)))
    (with-grout ()
      (grout-print-table table))
    (setf *output* table)))

;; EOF
