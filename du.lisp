;;
;; du.lisp - disk usage
;;

;; TODO
;; - add top node directory to save/load format
;; - make it harder to exit, ask for confirmation?
;; - update a directory?
;; - improved progress indication
;; - old fashioned du compatibility

(defpackage :du
  (:documentation "Disk usage")
  (:use :cl :dlib :dlib-misc :opsys :tree-viewer :inator :terminal :char-util
	:keymap)
  (:export
   ;; Main entry point
   #:du
   #:!du
   #:*last-tree*
   ))
(in-package :du)

;; (declaim (optimize (speed 2) (safety 0) (debug 0) (space 3)
;; 		   (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
 		   (compilation-speed 0)))

;; (defvar *show-progress* nil
;;   "True to show a progress bar during the creation of the usage tree.")

(defvar *default-ouput-file* "du.out"
  "The default file name to save data in.")

(defvar *last-tree* nil "The last tree that was made.")

;; @@@ should probably make a state obj, bro
(defstruct du-app
  tree					; The currently viewed tree.
  viewer
  show-all				; True to include files in the tree.
  show-progress
  exclude				; Paths to exclude.
  reload				; True to reload.
  (total 0)				; Current total space.
  (max 0)				; Maximum space.
  (track-hard-links t)
  inodes)

(defvar *du* nil)

;(declaim (ftype (function (du-node) integer) du-node-size))

(defclass du-node (tree-viewer:node)
  ((name
    :initarg :name :accessor du-node-name :type string
    :documentation "Name of the node.")
   (parent
    :initarg :parent :accessor du-node-parent :initform nil
    :documentation
    "Back pointer to parent node so we can construct the whole path.")
   (size
    :initarg :size :accessor du-node-size :initform 0 :type integer
    :documentation "Size of this node and all its branches."))
  (:documentation "Tree node for keeping file system sizes."))

(defmethod initialize-instance
    :after ((o du-node) &rest initargs &key &allow-other-keys)
  "Initialize a du-dir-node."
  (declare (ignore initargs))
  (when (not (slot-boundp o 'name))
    (error "du-node must be created with a name."))
  (when (not (slot-boundp o 'parent))
    (error "du-node must be created with a parent.")))

(defmethod node-object ((o du-node))
  (du-node-name o))

(defgeneric output (object stream)
  (:documentation "Output an object to a stream."))

(defmethod output ((node du-node) stream)
  "Output a du-node."
  (declare (type stream stream))
  (with-slots (name size) node
    (format stream "(~w ~d" name size)
    (when (node-branches node)
      (write-string " (" stream)
      (loop :for n :in (node-branches node)
	 :do (output n stream))
      (write-string ")" stream))
    (write-string ")" stream)))

(defgeneric input (type stream parent)
  (:documentation "Input an object from a stream."))

;; The point of this, instead of using read, is that I am hoping that
;; it will use significantly less memory.
(declaim (type (or null integer) *level*))
(defvar *level* nil)
(defmethod input ((type (eql 'du-node)) stream parent)
  "Input a du-node."
  (flet ((msg (fmt &rest args)
	   ;; 1+ to match emacs
	   ;;(format t "~2d ~3d " *level* (1+ (file-position stream)))
	   ;;(apply #'format t fmt args)
	   (declare (ignore fmt args))
	   ))
    (incf *level*)
    (let (result name size c)
      (when (not (eql (read-char stream) #\())
	(error "Expecting an open paren"))
      (setf name (read stream))
      (msg "name ~a~%" name)
      (when (not (stringp name))
	(error "name must be a string"))
      (setf size (read stream))
      (msg "size ~a~%" size)
      (when (not (integerp size))
	(error "size must be an integer"))
      (setf result (make-instance 'du-node
				  :name name :size size :parent parent))
      (if (char= (setf c (read-char stream)) #\))
	  (msg "end of subtree ~c~%" c)
	  (progn
	    (msg "got ~a~%" c)
	    (when (char/= c #\()
	      (error "Expecting an open paren for node list"))
	    ;; (when (char/= (read-char stream) #\()
	    ;; 	(error "Expecting an open paren for node list"))
	    (loop :while (char= (peek-char nil stream) #\()
	       :do
	       (msg "-> ~%")
	       (push (input 'du-node stream result) (node-branches result)))
	    (setf (node-branches result) (nreverse (node-branches result)))
	    (if (not (eql (setf c (read-char stream)) #\)))
		(progn
		  (format t "instead ~s" c)
		  (error "Expecting an close paren for node list"))
		(progn
		  (msg "end of nodes ~c~%" c)
		  ;; read the closing ) of the object
		  (when (char/= (setf c (read-char stream)) #\))
		    (error "Expecting an close paren for node"))))))
      (decf *level*)
      result)))

(defun save (&optional (filename *default-ouput-file*) (tree *last-tree*))
  (with-open-file (stream (quote-filename filename)
			  :direction :output :if-exists :rename)
    (output tree stream)))

(defun load-tree (&optional (filename *default-ouput-file*))
  (with-open-file (stream (quote-filename filename) :direction :input)
    (let ((*level* 0))
      (input 'du-node stream nil))))

(defclass du-file-node (du-node)
  ()
  (:documentation "A du-node for non directory files."))

(defclass du-dir-node (du-node)
  ()
  (:documentation "A du-node for directories."))

(defclass du-top-node (du-dir-node)
  ((directory
    :initarg :directory :accessor du-top-node-directory  
    :documentation
    "Directory which du was run from and all children are relative to."))
  (:documentation
   "A du-node for the top of a tree. Saves the current directory."))

(defun shrink-pathname (path &optional (to 70) (ellipsis "..."))
  "Make a path name fit in the given width, shrinking in a way to preserve
useful information."
  (let* ((str (namestring (quote-filename path)))
	 (len (length str)))
    (declare (string str ellipsis) (fixnum to))
    (if (> len to)
	(let* ((ellipsis-length (length ellipsis))
	       (half (- (truncate to 2) ellipsis-length)))
	  (declare (type fixnum ellipsis-length half))
	  (s+ (subseq str 0 half) ellipsis
	      (subseq str (- len (+ half ellipsis-length 1)))))
	str)))

(defun get-path (node)
  "Return the full pathname of NODE by walking up the tree."
  (let (path)
    (loop
       :for n = node :then (du-node-parent n)
       :if n :do (push (du-node-name n) path) :end
       :while n)
    (apply #'nos:path-append path)))

(defmethod initialize-instance
    :after ((o du-dir-node) &rest initargs &key &allow-other-keys)
  "Initialize a du-dir-node."
  (declare (ignore initargs))
  (with-slots (name parent size) o
    (with-slots (show-all exclude show-progress) *du*
      (let* (path
	     (full-name (get-path o))
	     (dirs (handler-case
		       (read-directory :dir full-name :full t)
		     (opsys-error (c)
		       (format t "~a: ~a~%" full-name c)
		       nil)))
	     info)
	;;(format t "full-name = ~s~%" full-name)
	;;(read-line)
	(labels ((get-info (path)
		   (or info
		       (setf info
			     (handler-case
				 (get-file-info path :follow-links nil)
			       (opsys-error (c)
				 (princ c) (terpri)
				 nil))))))
	  (when dirs
	    (loop :with new :and type
	       :for f :in dirs :do
	       (setf path (nos:path-append full-name (nos:dir-entry-name f))
		     info nil
		     type (dir-entry-type f))
	       ;; We have to handle some filesystems which fail to return
	       ;; the type.
	       (when (eq type :unknown)
		 (setf info (get-info path)
		       type (file-info-type info)))
	       (cond 
		 ((or (eq type :regular)
		      (eq type :link)
		      (and #| (eq (dir-entry-type f) :directory) |#
		       (equal (dir-entry-name f) ".")))
		  (when (setf info (get-info path))
		    (let ((my-size (file-info-size info)))
		      (declare (integer my-size))
		      (incf size my-size)
		      (when show-all
			(push (make-instance 'du-file-node
					     :name (dir-entry-name f)
					     :parent o
					     :size my-size)
			      (node-branches o))))))
		 ((and (eq type :directory)
		       (not (equal (dir-entry-name f) ".."))
		       (or (not exclude)
			   (not (equal path exclude))))
		  ;;(format t "--> ~a ~a~%" path exclude)
		  (setf new (make-instance 'du-dir-node
					   :name (dir-entry-name f)
					   :parent o))
		  (push new (node-branches o))
		  (incf size (du-node-size new)))))
	    (setf (node-branches o)
		  (sort-muffled (node-branches o) #'> :key #'du-node-size)))
	  (when show-progress
	    (tt-move-to-col 0)
	    (tt-write-string (shrink-pathname
			      full-name
			      (1- (terminal-window-columns *terminal*))))
	    (tt-erase-to-eol)
	    ;;(tt-finish-output)
	    ;;(tt-get-char)
	    ))))))

(defmethod display-node ((node du-node) level)
  (declare (ignorable level))
  (with-slots (name size) node
    (display-node-line node (s+ (display-prefix node level)
				(format nil "~10a ~a~%"
					(print-size size :stream nil
						    :traditional t)
					name)))))
(defun save-file (o)
  "Save the du tree in a file."
  (save)
  (tree-viewer::message o "Saved in ~a" *default-ouput-file*))

(defun load-file (o)
  "Load the du tree from a file."
  (with-slots (tree reload) *du*
    (setf tree (load-tree)
	  reload t)
    (message o "Loaded from ~a" *default-ouput-file*)
    (quit o)))

(defun view-file (o)
  "View the file with the pager."
  (terminal-end *terminal*)
  (handler-case
      (view:view (get-path (tree-viewer::current o)))
    (simple-error (c)
      (message o "Error: ~a" c)))
  (terminal-start *terminal*)
  ;;(refresh)
  )

(defkeymap *du-ctrl-x-keymap*
  `((,(ctrl #\s)	. save-file)
    (,(ctrl #\f)	. load-file)))

(defkeymap *du-keymap*
  `((,(ctrl #\x)	. *du-ctrl-x-keymap*)
    (#\v		. view-file)))

(defun du (&key (directory ".") #|follow-links tree |#
	     (show-progress t) (all t) resume file exclude)
  "Show disk usage.
DIRECTORY     - Directory to start from, which defaults to the current
                directory.
TREE          - A previously constructed tree to use.
SHOW-PROGRESS - True to show the progress while gathering data. Defaults to T.
ALL           - True to show sizes for all files. Otherwise it just shows
                directory totals. This can take much more memory.
RESUME        - True to resume from the last time it was run.
FILE          - Is a file to load data from.
EXCLUDE       - Sub-tree(s) to exclude.
"  
  (let* ((*du*
	  (make-du-app
	   :show-progress show-progress
	   :show-all all
	   :exclude exclude)))
    ;;(format t "*exclude* = ~s~%" *exclude*)
    (with-slots (tree viewer show-progress reload) *du*
      (when resume
	(if *last-tree*
	    (setf tree *last-tree*)
	    (format t "Nothing to resume.~%")))
      (if file
	  (setf tree (load-tree file))
	  (if show-progress
	      (with-terminal ()
	        (terminal-get-size *terminal*)
		(setf tree
		      (or tree (make-instance
				'du-top-node
				:name directory :parent nil
				:directory (current-directory)))))
	      (setf tree
		    (or tree (make-instance
			      'du-top-node
			      :name directory :parent nil
			      :directory (current-directory))))))
      (setf *last-tree* tree)
      (loop :do
	 (setf reload nil
	       (node-open tree) t)
	 (setf viewer (make-instance 'tree-viewer
				     :root tree
				     :bottom (- (tt-height) 2)))
	 (push *du-keymap* (inator-keymap viewer))
	 (view-tree tree :viewer viewer)
	 :while reload))))

#+lish
(lish:defcommand du
  ((show-progress boolean :short-arg #\p :default t
    :help "True to show progress while gathering data.")
   (all boolean :short-arg #\a :default t
    :help "True to include files as well as directories in the output.")
   (exclude pathname :short-arg #\x #| :repeating t |#
    :help "Exclude the sub-tree starting at this directory.")
   (resume boolean :short-arg #\r
    :help "True to resume viewing the last tree.")
   (file pathname :short-arg #\f
    :help "Load a tree from a file.")
   (directory directory :default "."
    :help "Directory to show usage for."))
  :keys-as keys
  "Show disk usage."
  (apply #'du keys))

;; EOF
