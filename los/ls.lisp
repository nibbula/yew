;;;
;;; ls.lisp - List your stuff.
;;;

(defpackage :ls
  (:documentation "This is a command I type too much.")
  (:use :cl :dlib :dlib-misc :opsys :dtime :terminal :terminal-ansi :grout
	:table :table-print :terminal-table :ochar :fatchar :fatchar-io :theme
        :style :magic :collections :result)
  (:export
   #:!ls
   #:ls
   #:list-files
   #:file-item-name
   #:file-item-directory
   #:file-item-info
   #:file-item-creation-date
   #:file-item-access-date
   #:file-item-modification-date
   #:file-item-size
   #:file-item-type
   #:item-full-path
   ))
(in-package :ls)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro when-not-missing (before &body after)
    "Only do ‘after’ if we don't get an error that indicates a missing file
in ‘before’."
    `(tagbody
	(handler-case
	    (progn ,before)
	  (opsys-error (c)
	    (if #+unix
		(/= (opsys-error-code c) uos:+ENOENT+)
		#+(and windows (not unix))
		(not (find (opsys-error-code c)
			   `(,wos:+ERROR-FILE-NOT-FOUND+
			     ,wos:+ERROR-SHARING-VIOLATION+)))
		(signal c)
		(go MISSING))))
	(progn
	  ,@after)
      MISSING))

  (defmacro with-error-handling ((&optional thing) &body body)
    "Evaluate ‘body’ continuably handling file related errors appropriately
regarding ‘ls-state-signal-errors’."
    (declare (ignore thing))
    (with-unique-names (thunk)
      `(flet ((,thunk ()
		,@body))
	 (restart-case
	     (if (ls-state-signal-errors *ls-state*)
		 (,thunk)
		 (handler-case
		     (,thunk)
		   ((or stream-error file-error nos:opsys-error) (c)
		     (finish-output)
		     (let ((*print-pretty* nil))
		       (format *error-output*
			       ;; "~a: ~a ~a~%" ,thing (type-of c) c))
			       "~a ~a~%" (type-of c) c))
		    (invoke-restart 'continue))))
	   (continue ()
	     :report "Skip this error.")
	   ;; (skip-all ()
	   ;;   :report "Skip remaining errors.")
	   )))))

;; Dynamic state
(defstruct ls-state
  today			; dtime for now
  about-a-year		; dtime for about a year ago
  table-renderer	; so we don't have to keep remaking it
  outer-terminal	; keep track of *terminal* when we were called
  mixed-bag		; True if we were asked to list some dirs and non-dirs
  recurring		; True if we're doing multiple listings
  (first-dir t)		; True if this is the first directory printed.
  signal-errors		; True to signal errors
  nice-table)		; Keep the result table around
(defparameter *ls-state* nil)

(defclass file-item (file-result)
  ((name
    :initarg :name #| :accessor file-item-name |#
    :documentation "Name of the file, likely without a directory.")
   (directory
    :initarg :directory :accessor file-item-directory
    :documentation "The directory the file is in."))
  (:documentation "The minimal file item. Just the name."))

(defmethod print-object ((object file-item) stream)
  "Print a file-item to STREAM."
  (with-slots (name directory) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~a" directory name))))

(defmethod file-result-os-pathname ((result file-item))
  (os-pathname (path-append (file-item-directory result)
			    (file-item-name result))))

(defclass file-item-with-info (file-item)
  ((info
   :initarg :info :accessor file-item-info
   :documentation "Whatever."))
  (:documentation "A generic file item with more info."))

(defclass file-item-dir (file-item-with-info)
  ()
  (:documentation
   "The file item with directory info. INFO slot is DIR-ENTRY structure retuned
by nos:read-directory."))

(defclass file-item-full (file-item-with-info)
  ()
  (:documentation "A full OS specific file info."))

#+unix
(defclass file-item-unix (file-item-full)
  ()
  (:documentation "uos:stat."))

#+windows
(defclass file-item-windows (file-item-full)
  ()
  (:documentation "wos:whatever."))

(defgeneric file-item-name (item)
  (:documentation "Return the file-item name.")
  (:method ((item string))
    item)
  (:method ((item file-item))
    (slot-value item 'name)))

(defgeneric file-item-creation-date (item)
  (:documentation "Return creation time as a dtime.")
  #+unix
  (:method ((item file-item-unix))
    ;; This is wrong-ish. See unix:convert-file-info
    (make-dtime :seconds (uos:unix-to-universal-time
			  (uos:timespec-seconds
			   (uos:file-status-modify-time
			    (file-item-info item))))))
  (:method ((item file-item-full))
    (make-dtime :seconds (nos:file-info-creation-time
			  (file-item-info item)))))

(defgeneric file-item-access-date (item)
  (:documentation "Return access time as a dtime.")
  #+unix
  (:method ((item file-item-unix))
    (make-dtime :seconds (uos:unix-to-universal-time
			  (uos:timespec-seconds
			   (uos:file-status-access-time
			    (file-item-info item))))))
  (:method ((item file-item-full))
    (make-dtime :seconds (nos:file-info-access-time
			  (file-item-info item)))))

(defgeneric file-item-modification-date (item)
  (:documentation "Return modification time as a dtime.")
  #+unix
  (:method ((item file-item-unix))
    ;; This is wrong-ish. See unix:convert-file-info
    (make-dtime :seconds (uos:unix-to-universal-time
			  (uos:timespec-seconds
			   (uos:file-status-change-time
			    (file-item-info item))))))
  (:method ((item file-item-full))
    (make-dtime :seconds (nos:file-info-modification-time
			  (file-item-info item)))))

(defgeneric file-item-size (item)
  (:documentation "Return the size in bytes.")
  #+unix
  (:method ((item file-item-unix))
    (uos:file-status-size (file-item-info item)))
  (:method ((item file-item-full))
    (nos:file-info-size (file-item-info item))))

(defgeneric file-item-type (item)
  (:documentation "Return the type.")
  #+unix
  (:method ((item file-item-unix))
    (uos:file-type-symbol (uos:file-status-mode (file-item-info item))))
  (:method ((item file-item-full))
    (nos:file-info-type (file-item-info item))))

;; @@@ figure out the best way to get rid of this
(defgeneric file-item-as-dir-entry (item)
  (:documentation "Return the size in bytes.")
  (:method ((item file-item-dir))
    (file-item-info item))
  #+unix
  (:method ((item file-item-unix))
    (make-dir-entry
     :name (file-item-name item)
     :type (uos:file-type-symbol
	    (uos:file-status-mode (file-item-info item)))))
  (:method ((item file-item-full))
    (make-dir-entry
     :name (file-item-name item)
     :type (nos:file-info-type (file-item-info item))))
  (:method ((item file-item))
    (file-item-as-dir-entry (make-full-item item)))
  (:method ((item string))
    (file-item-as-dir-entry (make-full-item item))))

(defun full-path (dir file)
  (if dir
      (path-append dir (file-item-name file))
      (file-item-name file)))

(defun item-full-path (item)
  (if (file-item-directory item)
      (path-append (file-item-directory item) (file-item-name item))
      (file-item-name item)))

(defun crap-file-info (file)
  "Like file-info but return blank info for errors."
  (handler-case
      (nos:file-info file)
    #+(and windows (not unix))
    (wos:windows-error (c)
      (opsys::make-file-info))))

(defun make-full-item (item &optional dir)
  (etypecase item
    (string
     (make-instance #+unix 'file-item-unix
		    #-unix 'file-item-full
		    :name item
		    :directory dir
		    :info
		    #-unix (crap-file-info (full-path dir item))
		    #+unix (uos:lstat (full-path dir item))))
    ((or file-item-dir file-item)
     (let ((this-dir (or dir (file-item-directory item))))
       (make-instance #+unix 'file-item-unix
		      #-unix 'file-item-full
		      :name (file-item-name item)
		      :directory this-dir
		      :info
		      #-unix (crap-file-info
			      (full-path this-dir (file-item-name item)))
		      #+unix (uos:lstat
			      (full-path this-dir (file-item-name item))))))
    ;; (file-item-full
    ;;  item)
    ))

(defun make-at-least-dir (item)
  (etypecase item
    (string (make-full-item item))
    (file-item-dir item)))

(defun ensure-full-info (file-list dir)
  (mapcar (_ (make-full-item _ dir)) file-list))

(defun format-the-date (time format &optional today a-year)
  "Given a universal-time, return a date string according to FORMAT."
  (case format
    (:nibby
     (date-string :time time))
    (:normal
     (let ((d (make-dtime :seconds time)))
       (if (dtime< d (dtime- (or today (ls-state-today *ls-state*))
			     (or a-year (ls-state-about-a-year *ls-state*))))
	   ;; (format-date "~a ~2d ~4,'0d" (:month-abbrev :date :year)
	   (format-date "~a ~2d ~5d" (:month-abbrev :date :year)
			:time time)
	   (format-date "~a ~2d ~2,'0d:~2,'0d" (:month-abbrev :date :hour :min)
			:time time))))))

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

(defun get-extended-type (file-item)
  #+unix
  (let ((mode (uos:file-status-mode (file-item-info file-item))))
    (cond
      ((uos:is-set-uid mode) :setuid)
      ((uos:is-set-gid mode) :setgid)
      ((and (uos:is-directory mode) (uos:is-other-writable mode)
	    (uos:is-sticky mode))
       :sticky-other-writable)
      ((and (uos:is-directory mode) (uos:is-other-writable mode))
       :other-writable)
      ((uos:is-sticky mode) :sticky)
      ((and (uos:is-regular-file mode)
	    (uos:is-executable (file-item-info file-item))) :executable)
      ;; @@@ see how to detect: orphan, missing, capability
      (t (uos:file-type-symbol mode))))
  #-unix
  ;;(file-item-type (file-item-info file-item)))
  (file-item-type file-item))

(defun get-styled-file-name (file-item)
  "File is either a string or a dir-entry. Return a possibly fat-string."
  (typecase file-item
    (dir-entry
     (styled-file-name file-item))
    (string
     (styled-file-name (file-item-as-dir-entry file-item)))
    (file-item
     (if (ls-state-mixed-bag *ls-state*)
	 ;; Make a full path name.
	 (progn
	   (styled-file-name
	    (path-append (file-item-directory file-item)
			 (file-item-name file-item))
	    :type (get-extended-type file-item)))
	 (styled-file-name (file-item-as-dir-entry file-item)
			   :type (get-extended-type file-item))))
    (t file-item)))

(defun plain-file-name (file)
  (typecase file
    (dir-entry (dir-entry-name file))
    (t file)))

(defun mime-type-string (file table)
  (or (gethash file table)
      (setf (gethash file table)
	    (with-error-handling ()
	      (let ((type (magic:guess-file-type (item-full-path file))))
		(or (and type
			 (s+ (magic:content-type-category type)
			     "/"
			     (magic:content-type-name type)))
		    ""))))))

(defun sort-files-by (file-list key &optional reverse dir)
  (case key
    (:name
     (sort-muffled file-list (if reverse #'string> #'string<)
		   :key #'file-item-name))
    (:iname
     (sort-muffled file-list (if reverse #'string-greaterp #'string-lessp)
		   :key #'file-item-name))
    (:size
     (sort-muffled (ensure-full-info file-list dir)
		   (if reverse #'> #'<)
		   :key #'file-item-size))
    (:type
     (sort-muffled (ensure-full-info file-list dir)
		   (if reverse #'string> #'string<)
		   :key (_ (string (file-item-type _)))))
    (:access-time
     (sort-muffled (ensure-full-info file-list dir)
		   (if reverse #'dtime< #'dtime>)
		   :key (_ (file-item-access-date _))))
    (:creation-time
     (sort-muffled (ensure-full-info file-list dir)
		   (if reverse #'dtime< #'dtime>)
		   :key (_ (file-item-creation-date _))))
    (:modification-time
     (sort-muffled (ensure-full-info file-list dir)
		   (if reverse #'dtime< #'dtime>)
		   :key (_ (file-item-modification-date _))))
    (:extension
     (let ((str-func (if reverse #'string> #'string<)))
       (sort-muffled (sort-muffled file-list (if reverse #'string> #'string<)
				   :key #'file-item-name)
		     (lambda (a b)
		       (cond
			 ((and (not (car a)) (not (car b)))
			  (funcall str-func (cdr a) (cdr b)))
			 (t
			  (funcall str-func (car a) (car b)))))
		   :key (_ (cons
			    (path-extension (file-item-name _))
			    (file-item-name _))))))
    (:mime-type
     ;; This is quite slow.
     (let ((str-func (if reverse #'string> #'string<))
	   (table (make-hash-table :test #'equal)))
       (sort-muffled file-list str-func
		     :key (_ (mime-type-string _ table)))))
    #-sbcl (:none ; scbl starting around 2.4.4 complains about unreachable code
     file-list)
    (otherwise
     file-list)
    ))

(defun make-default-state ()
  (make-ls-state
   :outer-terminal *terminal*
   :today (get-dtime)
   :about-a-year (make-dtime :seconds (weeks-to-time 50))
   :table-renderer (make-instance
		    'terminal-table:terminal-table-renderer)))

(defun get-cols ()
  "A desperate attempt to get the width of the window."
  (or (terminal-window-columns
       (or (ls-state-outer-terminal *ls-state*)
	   *terminal*))
      (let ((c (nos:env "COLUMNS")))
	(and c (parse-integer c)))
      80))

#+unix
(defun unix-stat (path)
  (handler-case
      (uos:stat path)
    (uos:posix-error (c)
      (when (= (opsys-error-code c) uos:+ENOENT+)
	(uos:lstat path)))))

#+unix
(defun colorize-symbolic-mode (mode)
  (if (uos:is-symbolic-link mode)
      (uos:symbolic-mode mode)
      (let ((s (fatchar-io:fs+ (uos:symbolic-mode mode))))
	(flet ((check-w (n tag)
		 ;; (when (eql (fatchar-c (oelt s n)) #\w)
		 (when (ochar= (oelt s n) #\w)
		   (setf (oelt s n)
			 (copy-fatchar
			  (oelt (themed-string `(:file :type ,tag :style) #\w)
				0))))))
	  (check-w 5 :group-writable)
	  (check-w 8 :other-writable)
	  s))))

(defun list-long (file-list date-format size-format)
  "Return a table filled with the long listing for files in FILE-LIST."
  (let ((today (ls-state-today *ls-state*))
	(a-year (ls-state-about-a-year *ls-state*)))
    (setf date-format (keywordify date-format)
	  size-format (keywordify size-format))
    (labels ((get-info (file)
	       (with-error-handling ()
		 #+unix (uos:lstat (item-full-path file))
		 #-unix (crap-file-info (item-full-path file))))
	     (broken (f)
	       (themed-string '(:file :type :broken-link :style) f))
	     #+unix
	     (link-name (file)
	       (handler-case
		   (let ((name (uos:readlink (item-full-path file))))
		     (if (file-exists
			  (if (path-absolute-p name)
			      name
			      (path-append (path-directory-name
					    (item-full-path file))
					   name)))
			 name
			 (broken name)))
		 (opsys-error (c)
		   (broken (error-message (opsys-error-code c)))))))
    #+unix
    (make-table-from
     (loop :with s
	:for file :in file-list
	:do (setf s (get-info file))
	:collect
	(list
	 (colorize-symbolic-mode (uos:file-status-mode s))
	 (uos:file-status-links s)
	 (or (user-name (uos:file-status-uid s)) (uos:file-status-uid s))
	 (or (group-name (uos:file-status-gid s)) (uos:file-status-gid s))
	 (uos:file-status-size s)
	 (uos:unix-to-universal-time
	  (uos:timespec-seconds
	   (uos:file-status-modify-time s)))
	 (if (uos:is-symbolic-link (uos:file-status-mode s))
	     (fs+ (get-styled-file-name file) " -> "
		  (link-name file))
	     (get-styled-file-name file))))
     :columns
     `((:name "Mode")
       (:name "Links" :type number)
       (:name "User")
       (:name "Group")
       (:name "Size" :align :right :type number
	      :format ,(lambda (n width)
			 (format nil "~v@a" width
				 (format-the-size n size-format))))
       (:name "Date"
	      :format ,(lambda (n width)
			 (format nil "~va" width
				 (format-the-date n date-format today a-year))))
       (:name "Name")))
    #-unix
    (make-table-from
     (loop
	:for file :in file-list
	;;:for s = (crap-file-info (dir-entry-name file))
	:for s = (get-info file)
	:collect (list
		  (file-info-type s)
		  (file-info-flags s)
		  (file-info-size s)
		  (os-time-seconds (file-info-modification-time s))
		  (get-styled-file-name file)))
     :columns
     `((:name "Type")
       (:name "Flags")
       (:name "Size" :align :right :type number
	      :format ,(lambda (n width)
			 (format nil "~v@a" width
				 (format-the-size n size-format))))
       (:name "Date"
	      :format ,(lambda (n width)
			 (format nil "~va" width
				 (format-the-date n date-format today a-year))))
       (:name "Name"))))))

(defun format-short (file dir show-size)
  (if show-size
      (fs+ (format nil "~6@a "
		   (format-the-size (file-item-size (make-full-item file dir))))
	   (get-styled-file-name file))
      (get-styled-file-name file)))

(defun format-short-item (item show-size size-format max-width)
  (if show-size
      (fs+ (format nil "~v@a "
		   max-width
		   (format-the-size (file-item-size (make-full-item item))
				    size-format))
	   (get-styled-file-name item))
      (get-styled-file-name item)))

(defun check-file-item (item)
  (when (not (typep item '(or nos:path-designator file-item)))
    (error "Sorry. I don't know how to list a ~s: ~s." (type-of item) item)))

(defun is-dir (x)
  "Take a file-item or a path, and return true if it's a directory."
  (etypecase x
    (file-item (eq (file-item-type x) :directory))
    (t (probe-directory x))))

(defun gather-file-info (args)
  "Gather file information for displaying as specified in the plist ‘args’,
which is mostly the args from ‘ls’ command. Returns two values, the list of
‘file-item's gathered, and a list of directories to be recursively listed."
  (let* (main-list dir-list file-list
	 (sort-by (getf args :sort-by))
	 (recursive (getf args :recursive))
	 results item more)
    (labels ((file-path (x)
	       (etypecase x
		 (file-item (item-full-path x))
		 (t x)))
	     (file-item-for (d f)
	       ;; (format t "file-item-for ~s ~s~%" d f)
	       (etypecase d
		 (file-item d)
		 (nos:dir-entry
		  (make-full-item (dir-entry-name d) (file-path f)))
		 (t
		  (make-full-item (path-file-name f)
				  (path-directory-name f))))))
      ;; Separate the files into directories and regular files if we weren't
      ;; given the -d flag.
      (if (not (getf args :directory))
	  (with-error-handling ()
	    (omapn (lambda (file)
		     (with-simple-restart (continue "Skip the item.")
		       (check-file-item file)
		       (if (is-dir file)
			   (push file dir-list)
			   (push file file-list))))
		   (getf args :files))
	    (setf file-list (nreverse file-list)
		  dir-list (nreverse dir-list)))
	  (setf file-list (getf args :files)))
      ;; (dbugf :ls "file-list ~s~%dir-list ~s~%" file-list dir-list)

      ;; Collect the file-list into main-list.
      (omapn (lambda (file)
	       (when-not-missing
		(with-error-handling ()
		  (setf item (file-item-for file file)))
		(push item main-list))
	       (setf (ls-state-mixed-bag *ls-state*) t))
	     file-list)

      ;; Deal with the dir-list.
      (if (cdr dir-list)
	  ;; If it's more than one, list them after.
	  (setf more dir-list)
	  ;; otherwise list it now
	  (when dir-list
	    (let ((file (first dir-list)))
	      (with-error-handling ((file-path file))
		(loop :for x :in
		   (read-directory
		    :full t :dir (file-path file)
		    :omit-hidden (not (getf args :hidden)))
		   :do
		   (when-not-missing
		    (setf item (file-item-for x file))
		    (when (and recursive (eq (file-item-type item) :directory))
		      (push item more))
		    (push item results)))))))

      ;; Group all the individual files into a list.
      (setf results (cond
		      (main-list
		       (append (list (nreverse main-list)) (list results)))
		      (results
		       (list (nreverse results)))
		      (t nil))
	    sort-by (if sort-by (keywordify sort-by) :none))
      (when (not (eq sort-by :none))
	(setf results
	      (loop :for list :in results
		 :collect
		 (progn
		   ;; Get rid of possible NILs from errors
		   (setf list (remove-if #'null list))
		   (when (getf args :ignore-backups)
		     (setf list (delete-if (_ (ends-with "~" _)) list
					   :key #'file-item-name)))
		   (sort-files-by list sort-by (getf args :reverse))))
	      more
	      (sort-files-by more sort-by (getf args :reverse))))
      (values results more))))

(defun print-dir-label (x)
  (if (not (ls-state-first-dir *ls-state*))
      (grout-format "~%")
      (setf (ls-state-first-dir *ls-state*) nil))
  ;; (grout-format "~a:~%" (file-item-directory x)))
  (grout-format "~a:~%" x))

(defun gather-results (files args)
  "Get the ressults tables, for when quiet is set."
  (flet ((get-it (x)
	  "Print the item list."
	   (when (null x)
	     (return-from get-it nil))
	   (when (getf args :long)
	     (setf (ls-state-nice-table *ls-state*)
		   (list-long x (getf args :date-format)
			      (getf args :size-format))))))
    (omapn (_ (get-it _)) files)))

(defun present-files (files args label-dir-p)
  "Show the FILES, displayed according the the plist ‘args’. See the ‘ls’
command for details. If ‘label-dir-p’ is true, print directory labels."
  ;; (format t "present files -> ~s~%" files)
  ;; (format t "grout ~s~%grout-stream ~s~%term ~s~%stdout ~s~%"
  ;; 	     *grout* (grout-stream *grout*) *terminal* *standard-output*)
  (labels ((size-max-width (items)
	     "Calcuate the maximum width size of the size field for the items."
	     (loop :for f :in items
		:maximize
		(char-util:display-length (format-the-size
					   (file-item-size
					    (make-full-item f))
					   (getf args :size-format)))))
	   (print-it (x)
	     "Print the item list."
	     (when (null x)
	       (return-from print-it nil))
	     (when label-dir-p
	       (print-dir-label (file-item-directory (car x))))
	     (if (getf args :long)
		 ;; Long format
		 (apply #'grout::%grout-print-table grout:*grout*
			(setf (ls-state-nice-table *ls-state*)
			      (list-long x (getf args :date-format)
					 (getf args :size-format)))
			`(:long-titles nil :trailing-spaces nil
			  :print-titles ,(not (getf args :omit-headers))
			  ,@(when (getf args :wide) '(:max-width nil))))
		 ;; 1 column format
		 (if (getf args :1-column)
		     (if (getf args :show-size)
			 (grout:grout-print-table
			  (setf (ls-state-nice-table *ls-state*)
				(make-table-from
				 (mapcar (_ (list
					     (format-the-size
					      (file-item-size
					       (make-full-item _))
					      (getf args :size-format))
					     (get-styled-file-name _))) x)
				 :columns
				 '((:name "Size" :align :right :type number)
				   (:name "Name"))))
			  :print-titles nil)
			 (mapcar
			  (_ (grout-format
			      "~a~%" (get-styled-file-name _))) x))
		     ;; Columns format
		     (let ((max-width (if (getf args :show-size)
					  (size-max-width x) 6)))
		       (print-columns
			(mapcar (_ (format-short-item
				    _ (getf args :show-size)
				    (getf args :size-format) max-width)) x)
			:smush t
			:format-char "/fatchar-io:print-string/"
			;; :stream (or *terminal* *standard-output*)
			;; :stream (grout-stream *grout*)
			;; :columns (terminal-window-columns
			;; 	  (or (ls-state-outer-terminal *ls-state*)
			;; 	      *terminal*))))))))
			:columns (get-cols)))))))
    (let ((*print-pretty* nil))
      (when files
	(print-it (car files))
	(loop :for list :in (cdr files) :do
	     (when list
	       (grout-princ #\newline)
	       (print-it list)))))))

;; Something like a customized assert.
(defmacro check-thing (test datum (format &rest args) &body body)
  "Check that ‘test’ is true, or signal an type-error, printing with ‘format’
and ‘args’, and allow setting ‘datum’, and evaluating ‘body’."
  (with-names (%datum %value %args)
    `(progn
       (loop :with ,%datum = ,datum :and ,%args = ,@args
         :while (not ,test) :do
	 (setf ,datum
	       (restart-case
		   (error 'simple-type-error
			  :datum ,%datum
			  :expected-type t
			  :format-control ,format
			  :format-arguments (list ,%args))
		 (use-value (,%value)
		   :report (lambda (stream)
			     (format stream "Use a new value for ~s." ',datum))
		   :interactive
		   (lambda ()
		     (multiple-value-list
		      (eval
		       (read-from-string
			(rl:rl
			 :prompt
			 (format nil "Enter a new value for ~s (evaluated): "
				 ',datum))))))
		   ,%value))))
       ,@body)))

(defun list-files (&rest args &key files long 1-column wide hidden directory
				case-insensitive sort-by reverse date-format
				show-size size-format collect nice-table quiet
				ignore-backups omit-headers recursive
				signal-errors
				&allow-other-keys)
  "List or collect files. See !ls for a description of arguments."
  (declare (ignorable files long 1-column wide hidden directory sort-by reverse
		      date-format show-size size-format collect nice-table quiet
		      ignore-backups omit-headers recursive signal-errors))
  ;; It seems like we have to do our own defaulting.
  (when (not files)
    (setf files (list (current-directory))
	  (getf args :files) files))

  (check-thing (collection-p files) files
   ("Files to list should be a nos:path-designator or a collection ~
    of nos:path-designator, not a ~a." (type-of files))
   (setf (getf args :files) files))

  (when (not sort-by)
    (setf (getf args :sort-by) :name))
  (when (and case-insensitive (eql (keywordify (getf args :sort-by)) :name))
    (setf (getf args :sort-by) :iname))
  (when (not date-format)
    (setf (getf args :date-format) :normal))
  (when (getf args :nice-table)
    (setf (getf args :collect) t	; nice-table implies collect
	  collect t))

  (let* ((*ls-state* (or *ls-state* (make-default-state)))
	 file-info more results)
    (labels
      ((recur ()
         (let ((new-args (copy-list args)))
	   (setf (ls-state-recurring *ls-state*) t)
	   (loop :for x :in more :do
             (setf (getf new-args :files) (list x))
	     ;; (format t "~&--> ") (finish-output) (read-line)
	     (apply #'list-files new-args))))
       (pick-results ()
         (if nice-table
	     (ls-state-nice-table *ls-state*)
	     (if (= (length file-info) 1)
		 (car file-info)
	         file-info)))
       (body ()
         (cond
	  (file-info
	   (if quiet
	       (gather-results file-info args)
	       (present-files file-info args
			      (or recursive
				  (ls-state-recurring *ls-state*)))))
	  ;; ((and (/= (length files) 1) (is-dir (car files)))
	  ;;  ;; (print-dir-label (car files))))
	  ;;  (print-dir-label (item-full-path (make-full-item (car files)))))
	  )
	 (if collect
	     (if more
		 (progn
		   (setf results (recur))
		   (push (pick-results) results))
	       (pick-results))
	   (progn
	     (when more
	       (recur))
	     (values)))))

      ;;(with-error-handling ()
      (setf (values file-info more) (gather-file-info args))
      (if quiet
	  (body)
	  (with-grout ()
	    (body))))))

;; End
