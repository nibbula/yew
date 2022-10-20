;;;
;;; du.lisp - disk usage
;;;

(defpackage :du
  (:documentation "Disk usage")
  (:use :cl :dlib :dlib-misc :opsys :char-util :inator :terminal
	:keymap :view-generic :unicode :tree-viewer :table-viewer)
  (:export
   ;; Main entry point
   #:du
   #:!du
   #:*last-tree*
   ))
(in-package :du)

;; TODO
;; - add top node directory to save/load format
;; - make it harder to exit, ask for confirmation?
;; - update a directory?
;; - improved progress indication
;; - old fashioned du compatibility

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defvar *default-ouput-file* "du.out"
  "The default file name to save data in.")

(defvar *last-tree* nil "The last tree that was made.")

(defclass data-container ()
  ((data
    :initarg :data :accessor data-container-data :initform nil
    :documentation "The contained data."))
  (:documentation "Container for file size data."))

(defgeneric gather-data (directories data)
  (:documentation "Gather size ‘data' for ‘directories’."))

(defgeneric dispose-data (data)
  (:documentation "Make data storage potentially collectible."))

(defgeneric save (filename data)
  (:documentation "Saved size ‘data’ to ‘filename’."))

(defgeneric load-data (filename data)
  (:documentation "Load saved size ‘data’ from ‘filename’."))

(defclass du-viewer ()
  ()
  (:documentation "A viewer for disk usage data."))

(defgeneric point-item (viewer)
  (:documentation "Return the directory the point is in."))

(defgeneric reload-current-item (viewer)
  (:documentation "Reload the current item in the viewer."))

(defclass du-app ()
  ((data
    :initarg :data :accessor data :initform nil
    :documentation "The currently viewed file size data.")
   (viewer
    :initarg :viewer :accessor viewer :initform nil
    :documentation "The viewer inator.")
   (directories
    :initarg :directories :accessor directories :initform nil :type list
    :documentation "The list of directories we're showing usage for.")
   (show-all
    :initarg :show-all :accessor show-all :initform t :type boolean
    :documentation "True to include files in the tree.")
   (show-progress
    :initarg :show-progress :accessor show-progress :initform nil
    :type boolean
    :documentation "True to show proress while collecting data.")
   (exclude
    :initarg :exclude :accessor exclude :initform '() :type list
    :documentation "Paths to exclude.")
   (one-file-system
    :initarg :one-file-system :accessor one-file-system
    :initform nil :type boolean
    :documentation
    "True to skip traversing into directories on different file systems than
the starting one.")
   (starting-device
    :initarg :starting-device :accessor starting-device :initform nil
    :documentation "The device we started on.")
   (reload
    :initarg :reload :accessor reload :initform nil :type boolean
    :documentation "True to reload.")
   (total
    :initarg :total :accessor total :initform 0 :type number
    :documentation "Current total space in octets.")
   (max-space
    :initarg :max-space :accessor max-space :initform 0 :type number
    :documentation "Maximum space in octets.")
   (track-hard-links
    :initarg :track-hard-links :accessor track-hard-links
    :initform t :type boolean
    :documentation "True to track hard links.")
   (inodes
    :initarg :inodes :accessor inodes
    :documentation "File system identity table.")
   (size-format
    :initarg :size-format :accessor du-app-size-format :initform :human
    :type (member :human :bytes)
    :documentation "The format for file sizes. Of of :human or :bytes."))
  (:documentation "Disk usage viewer."))

(defvar *du* nil "The current disk us inator.")

(deftype small-string () `(simple-array (unsigned-byte 8)))

(defun to-small-string (string)
  (string-to-utf8-bytes string))

(defun from-small-string (small-string)
  (utf8-bytes-to-string small-string))

;(declaim (ftype (function (du-node) integer) du-node-size))

(defclass du-node (tree-viewer:node)
  ((name
    :initarg :name :accessor du-node-name :type small-string
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
  (from-small-string (du-node-name o)))

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
				  :name (to-small-string name)
				  :size size
				  :parent parent))
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

(defclass tree-container (data-container)
  ()
  (:documentation "Disk usage tree data."))

(defmethod save (filename (data tree-container))
  "Saved size ‘data’ to ‘filename’. If ‘filename’ is NIL, use
‘*default-ouput-file*’. If ‘data’ is empty, use ‘*last-tree*’."
  (with-open-file (stream (quote-filename (or filename *default-ouput-file*))
			  :direction :output :if-exists :rename)
    (output (or data *last-tree*) stream)))

(defmethod load-data (filename (data tree-container))
  "Load saved size data from ‘filename’ into ‘data’. If ‘filename’ is NIL,
use ‘*default-ouput-file*’."
  (when (not filename)
    (setf filename *default-ouput-file*))
  (with-open-file (stream (quote-filename filename) :direction :input)
    (let ((*level* 0))
      (setf (data-container-data data)
	    (input 'du-node stream nil)))))

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

(defclass du-glom-node (du-dir-node)
  ((directories
    :initarg :directories :accessor du-top-node-directories
    :documentation
    "Set fo directories which du was run for."))
  (:documentation
   "A du-node for the top of a tree. Saves the set of directories."))

(defmethod initialize-instance
    :after ((o du-glom-node) &rest initargs &key &allow-other-keys)
  "Initialize a du-glom-node."
  (declare (ignore initargs))
  (if (and (slot-boundp o 'directories)
	   (slot-value  o 'directories))
      (progn
	(loop :with i
	   :for dir :in (slot-value o 'directories)
	   :do
	   (setf i (make-instance 'du-top-node
				  :name (to-small-string dir)
				  :directory dir))
	   (push i (node-branches o))
	   (incf (du-node-size o) (du-node-size i)))
	(setf (node-branches o)
	      (sort-muffled (node-branches o) #'> :key #'du-node-size)))
      (make-instance 'du-top-node :name (to-small-string "") :directory nil)))

(defun get-path (node)
  "Return the full pathname of NODE by walking up the tree."
  (let (path)
    (loop
       :for n = node :then (du-node-parent n)
       :if n :do (push (node-object n) path) :end
       :while n)
    (apply #'nos:path-append path)))

(defun get-path-device (path)
  #-unix nil
  ;; @@@ This is bad. We shouldn't have to do 2 ‘stat’ calls.
  ;; Figure out a way to do it cleanly.
  ;; I feel like we need a metadata object that can have
  ;; whatever most specific things the system provides, but
  ;; also have any attribute missing, rather than the least
  ;; common denominator of nos:file-info.
  #+unix (uos:file-status-device (uos:stat path)))

(defmethod initialize-instance
    :after ((o du-dir-node) &rest initargs &key &allow-other-keys)
  "Initialize a du-dir-node."
  (declare (ignore initargs))
  (with-slots (name parent size) o
    (with-slots (show-all exclude one-file-system starting-device show-progress)
	*du*
      (let* ((full-name (get-path o))
	     dirs path info device)
	;;(format t "full-name = ~s~%" full-name)
	;;(read-line)
	(labels ((get-info (path)
		   (or info
		       (setf info
			     (handler-case
				 (get-file-info path :follow-links nil)
			       (opsys-error (c)
				 (if show-progress
				     (progn
				       (tt-move-to-col 0)
				       (tt-erase-to-eol)
				       (tt-format "~a~%" c))
				     (progn
				       (princ c) (terpri)))
				 nil)))))
		 (get-device (path)
		   (or device (setf device (get-path-device path))))
		 (get-dirs (path)
		   (setf dirs (handler-case
				  (read-directory :dir path :full t)
				(opsys-error (c)
				  (if show-progress
				      (progn
					(tt-move-to-col 0)
					(tt-erase-to-eol)
					(tt-format "~a: ~a~%" path c))
				      (format t "~a: ~a~%" path c))
				  nil)))))
	  (when (and one-file-system (not starting-device))
	    (setf starting-device (get-device full-name)))
	  (when (or (not one-file-system)
		    (equal starting-device (get-device full-name)))
	    (get-dirs full-name))
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
			(push (make-instance
			       'du-file-node
			       :name (to-small-string (dir-entry-name f))
			       :parent o
			       :size my-size)
			      (node-branches o))))))
		 ((and (eq type :directory)
		       (not (equal (dir-entry-name f) ".."))
		       (or (null exclude)
			   (not (position (path-to-absolute path)
					  exclude :test #'equal)))
		       (or (not one-file-system)
			   (equal starting-device (get-device path))))
		  ;; (format t "--> ~s ~s~%" (path-to-absolute path) exclude)
		  ;; (force-output)
		  (setf new (make-instance
			     'du-dir-node
			     :name (to-small-string (dir-entry-name f))
			     :parent o))
		  (push new (node-branches o))
		  (incf size (du-node-size new)))))
	    (setf (node-branches o)
		  (sort-muffled (node-branches o) #'> :key #'du-node-size)))
	  (when show-progress
	    (tt-move-to-col 0)
	    (tt-write-string (shrink-pathname
			      full-name
			      :to (1- (terminal-window-columns *terminal*))))
	    (tt-erase-to-eol)
	    (tt-finish-output)
	    ;; (tt-get-char)
	    ))))))

(defmethod display-node ((node du-node) level)
  (declare (ignorable level))
  (with-slots (name size) node
    (display-node-line node (s+ (display-prefix node level)
				(format nil "~10a ~a~%"
					(print-size size :stream nil
						    :traditional t)
					(from-small-string name))))))

(defclass du-tree-viewer (tree-viewer du-viewer)
  ()
  (:documentation "Tree viewer for du."))

(defmethod initialize-instance
    :after ((o du-tree-viewer) &rest initargs &key &allow-other-keys)
  "Initialize a du-tree-viewer."
  (declare (ignore initargs))
  ;; Open the root node so we see something.
  (setf (node-open (tree-viewer::root o)) t))

(defmethod gather-data (directories (data tree-container))
  (setf (data-container-data data)
	(cond
	  ((not (listp directories))
	   (make-instance 'du-top-node
			  :name (to-small-string directories)
			  :parent nil
			  :directory (current-directory)))
	  ((= (length directories) 1)
	   (make-instance 'du-top-node
			  :name (to-small-string (elt directories 0))
			  :parent nil
			  :directory (current-directory)))
	  (t
	   ;; (format t "directories = ~s~%" directories)
	   ;; (read-line)
	   (make-instance 'du-glom-node
			  :name (to-small-string "Directories")
			  :parent nil
			  :directories directories)))))

(defmethod dispose-data ((data tree-container))
  ;; This is somewhat unnecessary unless your data is large, and can of course
  ;; be ineffective if there're
  (setf (data-container-data data) nil))

(defmethod point-item ((viewer du-tree-viewer))
  "Return the directory the point is in for a tree-container."
  (get-path (tree-viewer::current viewer)))

(defmethod reload-current-item ((view du-tree-viewer))
  ;; @@@
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file list

(defclass table-container (data-container)
  ((package
    :initarg :package :accessor table-container-package #|:initform nil|#
    :documentation "Package for path names."))
  (:documentation "Disk usage file list data."))

(defmethod initialize-instance
    :after ((o table-container) &rest initargs &key &allow-other-keys)
  "Initialize a table-container."
  (declare (ignore initargs))
  (when (or (not (slot-boundp o 'package))
	    (not (slot-value o 'package)))
    (setf (slot-value o 'package) (make-package (gensym "DU-TEMP")))))

(defun gather-item (file data)
  (let ((info (ignore-errors (nos:file-info file))))
    (when info
      (cons (nos:file-info-size info)
	    (list
	     (mapcar
	      (_ (intern _ (table-container-package data)))
	      (let ((pp (nos:parse-path file)))
		(if (nos:os-pathname-absolute-p pp)
		    (cons "/" (nos:os-pathname-path pp))
		    (nos:os-pathname-path pp)))))))))

(defun sort-data (data)
  (setf (data-container-data data)
	(sort ;; @@@ Would it be faster to build a sorted tree?
	 (data-container-data data)
	 #'> :key #'car)))

;; To save space, we split the path and store it as symbols in a package, but
;; this means we have to manually delete the package to reclaim the space.
(defmethod gather-data (directories (data table-container))
  "Gather a list of paths and file sizes."
  (let ((full-list
	  (loop :for dir :in directories
	    :nconc
	    (let (list end new)
	      (nos:map-directory
	       #|
	       (_ (let ((i (ignore-errors (nos:file-info _))))
		    (when i
		      (setf new
			    (list
			     (cons (nos:file-info-size i)
				   (list
				    (mapcar
				    (_ (intern _ (table-container-package data)))
				    (let ((pp (nos:parse-path _)))
				      (if (nos:os-pathname-absolute-p pp)
					  (cons "/" (nos:os-pathname-path pp))
					  (nos:os-pathname-path pp)))
				    )))))
		      (when end
			(rplacd end new))
		      (setf end new)
		      (when (null list)
			(setf list new)))))
	       |#
	       (_ (let ((i (gather-item _ data)))
		    (when i
		      (setf new (list i))
		      (when end
			(rplacd end new))
		      (setf end new)
		      (when (null list)
			(setf list new)))))
	       :dir dir :recursive t :errorp nil)
	      list))))
    ;; (setf (data-container-data data)
    ;; 	  (sort ;; @@@ Would it be faster to build a sorted tree?
    ;; 	   full-list
    ;; 	   #'> :key #'car))
    ;; (setf (data-container-data data)
    ;; 	   full-list)
    (setf (data-container-data data) full-list)
    (sort-data data)
    ))

(defmethod dispose-data ((data table-container))
  (delete-package (table-container-package data))
  (setf (data-container-data data) nil))

(defun format-path (cell width)
  "Format a path cell."
  ;; @@@ this is stupid & non-portable
  (let* ((s nos:*directory-separator-string*)
	 (absolute (string= (first cell) nos:*directory-separator-string*))
	 (str (format nil (s+ "~:[~;" s "~]~{~a~^" s "~}")
		      absolute
		      (if absolute (rest cell) cell))))
    (if width
	(format nil "~va" width str)
	str)))

;; @@@ We should probably move the formatters (from e.g. ls, ps, du, df) to
;; separate package, so they can be consistent and not duplicated - a.k.a DRY.
(defun format-the-size (size &optional (format :human))
  (flet ((human ()
	   (remove #\space (print-size size
				       :traditional t :stream nil :unit ""))))
    (case format
      (:human (human))
      (:bytes
       (format nil "~d" size))
      (otherwise
       (human)))))

(defun size-formatter (n width)
  (format nil "~v@a" width
	  (format-the-size n (or (and *du* (du-app-size-format *du*)) :human))))

(defclass du-table-viewer (table-viewer du-viewer)
  ()
  (:documentation "Table viewer for du."))

(defmethod current-cell ((o du-table-viewer))
  (format-path (call-next-method o) nil))

(defmethod point-item ((viewer du-table-viewer))
  "Return the directory the point is in for a tree-container."
  (current-cell viewer))

(defmethod reload-current-item ((viewer du-table-viewer))
  (setf (current-cell viewer)
	(gather-item (point-item viewer) (data *du*)))
  ;; (sort-data ???)
  )

#|
(defmethod initialize-instance
    :after ((o du-table-viewer) &rest initargs &key &allow-other-keys)
  "Initialize a du-table-viewer."
  (declare (ignore initargs))
  (when (slot-boundp o 'data)
    (setf (table-viewer::table-viewer-table o)
	  (table:make-table-from
	   (slot-value o 'data)
	   :columns '((:name "Size" :type number)
		      (:name "Path" :format #'format-path))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-file (o)
  "Save the du tree in a file."
  (with-slots (data) o
    (save nil data)
    (tree-viewer::message o "Saved in ~a" *default-ouput-file*)))

(defun load-file (o)
  "Load the du tree from a file."
  (with-slots (data reload) o
    (load-data nil data)
    (setf reload t)
    (message o "Loaded from ~a" *default-ouput-file*)
    (quit o)))

(defun view-file (o)
  "View the file with “view”."
  (tt-finish-output)
  (terminal-end *terminal*)
  (handler-case
      (view (get-path (tree-viewer::current o)))
    (simple-error (c)
      (message o "Error: ~a" c)))
  ;; (terminal-start *terminal*)
  (terminal-reinitialize *terminal*)
  (tt-clear)
  (tt-finish-output))

(defkeymap *du-ctrl-x-keymap* ()
  `((,(ctrl #\s)	. save-file)
    (,(ctrl #\f)	. load-file)))

(defkeymap *du-keymap* ()
  `((,(ctrl #\x)	. *du-ctrl-x-keymap*)
    (#\v		. view-file)
    (#\g		. reload-item)
    (#\G		. reload-all)))

(defun reload-all (o)
  "Reload all the data."
  (declare (ignore o)) ;; unfortunately separate
  (with-slots (reload data directories) *du*
    (when (fui:popup-y-or-n-p
	   "Are you sure you want to recompute the all sizes?")
      (dispose-data data)
      (gather-data directories data)
      (setf reload t))))

(defun reload-item (o)
  "Reload the current item."
  (declare (ignore o)) ;; unfortunately separate
  (with-slots (reload viewer data) *du*
    (when (fui:popup-y-or-n-p
	   (format nil "Are you sure you want to recompute the sizes for ~s?"
		   (point-item data)))
      (reload-current-item viewer)
      (setf reload t))))

(defun du (&key (directories '(".")) #|follow-links tree |#
	     (show-progress nil) (all t) resume file exclude as-list
	     non-human-size one-file-system)
  "Show disk usage.
 DIRECTORIES    - A list of directories to start from, which defaults to the
                  current directory.
 TREE           - A previously constructed tree to use.
 SHOW-PROGRESS  - True to show the progress while gathering data.
 ALL            - True to show sizes for all files. Otherwise it just shows
                  directory totals. This can take much more memory.
 RESUME         - True to resume from the last time it was run.
 FILE           - Is a file to load data from.
 EXCLUDE        - Sub-tree(s) to exclude.
 AS-LIST        - Show as a flat list, instead of a tree.
 NON-HUMAN-SIZE - Show sizes in bytes.
"  
  (let* ((*du*
	  (make-instance 'du-app
	   :show-progress show-progress
	   :show-all all
	   :exclude (mapcar #'path-to-absolute
			    (if (or (null exclude) (listp exclude))
				exclude
				(list exclude)))
	   :one-file-system one-file-system
	   :data (make-instance (if as-list 'table-container 'tree-container))
	   :directories directories
	   :size-format (if non-human-size :bytes :human))))
    ;; (format t "exclude = ~s~%" (exclude *du*))
    ;; (format t "show-progress = ~s~%" (show-progress *du*))
    ;; (force-output)
    (with-slots (data viewer show-progress reload) *du*
      (cond
	(resume
	 (if *last-tree*
	     (progn
	       (dispose-data data)
	       (setf data *last-tree*))
	     (format t "Nothing to resume.~%")))
	(file
	 (load-data file data))
	(t
	 (when *last-tree*
	   (dispose-data *last-tree*))
	 (if show-progress
	     (with-terminal ()
	       (terminal-get-size *terminal*)
	       (when (not (data-container-data data))
		 (gather-data directories data)))
	     (when (not (data-container-data data))
	       (gather-data directories data)))))
      (setf *last-tree* data)
      (loop :with result
	:do
	(setf reload nil)
	(cond
	  (as-list
	   (let ((table (table:make-table-from
			 (data-container-data data)
			 :columns
			 `((:name "Size" :type number :format ,#'size-formatter)
			   (:name "Path" :format ,#'format-path)))))
	     (view-table table :type 'du-table-viewer)
	     (setf result table)))
	  (t
	   (with-terminal ()
	     (setf viewer
		   (make-instance 'du-tree-viewer
				  :root (data-container-data data)
				  :bottom (- (tt-height) 2)))
	     (push *du-keymap* (inator-keymap viewer))
	     (event-loop viewer)
	     (setf result (data-container-data data)))))
	:while reload
	:finally (return result)))))

#+lish
(lish:defcommand du
  ((show-progress boolean :short-arg #\p :default nil
    :help "True to show progress while gathering data.")
   (all boolean :short-arg #\a :default t
    :help "True to include files as well as directories in the output.")
   (exclude case-preserving-object :short-arg #\e #| :repeating t |#
    :help "Exclude the sub-tree starting at these directories. Can be a
directory or a list of directories.")
   (one-file-system boolean :short-arg #\x
    :help "Don't count directories on different file systems.")
   (resume boolean :short-arg #\r
    :help "True to resume viewing the last tree.")
   (file pathname :short-arg #\f
    :help "Load a tree from a file.")
   (as-list boolean :short-arg #\l
    :help "Show as a flat list, instead of a tree.")
   (non-human-size boolean :short-arg #\h :help "Show sizes in bytes.")
   (directories directory :repeating t
    :help "Directory to show usage for."))
  :keys-as keys
  "Show disk usage."
  (when (not directories)
    (setf (getf keys :directories) '(".")))
  (typecase exclude
    (null)
    (symbol
     (setf (getf keys :exclude) (string exclude)))
    (list
     (setf (getf keys :exclude) (mapcar #'string exclude))))
  (setf lish:*output* (apply #'du keys)))

;; EOF
