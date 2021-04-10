;;;
;;; ls.lisp - list your stuff
;;;

;; Actually I really hate this command. I'm so sick of hierarchical file
;; systems. I want to mutate this into a tag browser to surf my metadata.
;;
;; I mostly just wanted to implement this old square wheel because:
;;   - Unix ls loses at making columns out of multibyte characters.
;;   - I wish it had slightly better column smushing behavior.
;;     (not that this does much better yet)
;;   - Object collecting.
;;   - It's the command I type the most. So I want it to be in Lisp.
;;   - I dream of someday finally "rm /usr/bin /bin".

(defpackage :ls
  (:documentation
   "This is a shitty fake implementation of the command I type the most.")
  (:use :cl :dlib :dlib-misc :opsys :dtime :terminal :terminal-ansi :grout
	:table :table-print :terminal-table :fatchar :fatchar-io :theme :style
	:magic :collections)
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
   ))
(in-package :ls)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro when-not-missing (before &body after)
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

  (defmacro with-error-handling ((thing) &body body)
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

(defclass file-item ()
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

(defun make-full-item (item &optional dir)
  (etypecase item
    (string
     (make-instance #+unix 'file-item-unix
		    #-unix 'file-item-full
		    :name item
		    :directory dir
		    :info
		    #-unix (nos:get-file-info (full-path dir item))
		    #+unix (uos:lstat (full-path dir item))))
    ((or file-item-dir file-item)
     (let ((this-dir (or dir (file-item-directory item))))
       (make-instance #+unix 'file-item-unix
		      #-unix 'file-item-full
		      :name (file-item-name item)
		      :directory this-dir
		      :info
		      #-unix (nos:get-file-info
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
	    (get-extended-type file-item)))
	 (styled-file-name (file-item-as-dir-entry file-item)
			   (get-extended-type file-item))))
    (t file-item)))

(defun plain-file-name (file)
  (typecase file
    (dir-entry (dir-entry-name file))
    (t file)))

(defun mime-type-string (file table)
  (or (gethash file table)
      (setf (gethash file table)
	    (with-error-handling (t)
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
    (:none
     file-list)
    (otherwise
     file-list)))

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

;; (defun pad-to (string width) ;; @@@ also in puca (maybe move to dlib-misc?)
;;   (if (< (olength string) width)
;;       (oconcatenate string (make-string (- width (olength string))
;; 					:initial-element #\space))
;;       string))

#+unix
(defun colorize-symbolic-mode (mode)
  (if (uos:is-symbolic-link mode)
      (uos:symbolic-mode mode)
      (let ((s (fatchar-io:fs+ (uos:symbolic-mode mode))))
	(flet ((check-w (n tag)
		 (when (eql (fatchar-c (oelt s n)) #\w)
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
    #+unix
    (make-table-from
     (loop :with s
	:for file :in file-list
	;;:for s = (unix-stat (item-full-path file))
	:do (setf s (uos:lstat (item-full-path file)))
	:collect
	(list
	 (colorize-symbolic-mode (uos:file-status-mode s))
	 (uos:file-status-links s)
	 (or (user-name (uos:file-status-uid s)) (uos:file-status-uid s))
	 (or (group-name (uos:file-status-gid s)) (uos:file-status-gid s))
	 ;;(format-the-size (uos:file-status-size s) (keywordify size-format))
	 (uos:file-status-size s)
	 ;; (format-the-date
	 ;; 	(uos:unix-to-universal-time
	 ;; 	 (uos:timespec-seconds
	 ;; 	  (uos:file-status-modify-time s)))
	 ;; 	(keywordify date-format))
	 (uos:unix-to-universal-time
	  (uos:timespec-seconds
	   (uos:file-status-modify-time s)))
	 (if (uos:is-symbolic-link (uos:file-status-mode s))
	     (fs+ (get-styled-file-name file) " -> "
		  (uos:readlink (item-full-path file)))
	     (get-styled-file-name file))))
     ;; :column-names
     ;; '("Mode" "Links" "User" "Group" ("Size" :right) "Date" "Name")
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
	;;:for s = (nos:get-file-info (dir-entry-name file))
	:for s = (nos:get-file-info (item-full-path file))
	:collect (list
		  (file-info-type s)
		  (file-info-flags s)
		  ;; (format-the-size (file-info-size s))
		  (file-info-size s)
		  ;; (format-the-date
		  ;;  (os-time-seconds
		  ;;   (file-info-modification-time s))
		  ;;  (keywordify date-format))
		  (os-time-seconds (file-info-modification-time s))
		  (get-styled-file-name file)
		  ))
     ;; :column-names
     ;; '("Type" "Flags" ("Size" :right) "Date" "Name")
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
       (:name "Name")))))

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

(defun is-dir (x)
  "Take a file-item or a path, and return true if it's a directory."
  (etypecase x
    (file-item (eq (file-item-type x) :directory))
    (t (probe-directory x))))

(defun gather-file-info (args)
  "Gather file information for displaying as specified in the plist ARGS, which
is mostly the args from ‘ls’ command. Returns two values, the list of FILE-ITEMs
gathered, and a list of directories to be recursively listed."
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
      #|
      (loop :for file :in (getf args :files)
	 :do
	 (if (and (is-dir file) (not (getf args :directory)))
	     (with-error-handling ((file-path file))
	       (dbugf :ls "Jang dir ~s~%" (file-path file))
	       (loop :for x :in
		  (read-directory
		   :full t :dir (file-path file)
		   :omit-hidden (not (getf args :hidden)))
		  :do
		  (when-not-missing
		   (setf item (file-item-for x file))
		   ;; (format t "Floop~%")
		   (when (and recursive (eq (file-item-type item) :directory))
		     (push item more))
		   (push item results))))
	     (progn
	       (dbugf :ls "Jing file~%")
	       (when-not-missing
		(setf item (file-item-for file file))
		;; (when (and recursive (eq (file-item-type item) :directory))
		;;   (push item more))
		(push item main-list))
	       (setf (ls-state-mixed-bag *ls-state*) t))))
      |#
      ;; Separate the files into directories and regular files if we weren't
      ;; given the -d flag.
      (if (not (getf args :directory))
	  (progn
	    (loop :for file :in (getf args :files)
	       :do
	       (if (is-dir file)
		   (push file dir-list)
		   (push file file-list)))
	    (setf file-list (nreverse file-list)
		  dir-list (nreverse dir-list)))
	  (setf file-list (getf args :files)))
      ;; (dbugf :ls "file-list ~s~%dir-list ~s~%" file-list dir-list)

      ;; Collect the file-list into main-list.
      (loop :for file :in file-list
	 :do
	    (when-not-missing
	     (with-error-handling (t)
	       (setf item (file-item-for file file)))
	     ;; (when (and recursive (eq (file-item-type item) :directory))
	     ;;   (push item more))
	     (push item main-list))
	    (setf (ls-state-mixed-bag *ls-state*) t))

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

      ;; group all the individual files into a list
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

(defun present-files (files args label-dir-p)
  "Show the FILES, displayed according the the plist ARGS. See the lish ‘ls’
command for details. If LABEL-DIR is true, print directory labels."
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
				 :column-names '(("Size" :right) "Name")))
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
	(loop :for list :in (cdr files) ::do
	     (when list
	       (grout-princ #\newline)
	       (print-it list)))))))

(defun list-files (&rest args &key files long 1-column wide hidden directory
				sort-by reverse date-format show-size
				size-format collect nice-table ignore-backups
				omit-headers recursive signal-errors)
  "List or collect files. See !ls for a description of arguments."
  (declare (ignorable files long 1-column wide hidden directory sort-by reverse
		      date-format show-size size-format collect nice-table
		      ignore-backups omit-headers recursive signal-errors))
  ;; It seems like we have to do our own defaulting.
  (when (not files)
    (setf files (list (current-directory))
	  (getf args :files) files))
  (when (not sort-by)
    (setf (getf args :sort-by) :name))
  (when (not date-format)
    (setf (getf args :date-format) :normal))
  (when (getf args :nice-table)
    (setf (getf args :collect) t	; nice-table implies collect
	  collect t))

  (let* ((*ls-state* (or *ls-state* (make-default-state)))
	 file-info more results)
    (with-grout ()
      (flet ((recur ()
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
		       file-info))))
	;;(with-error-handling ()
	(setf (values file-info more) (gather-file-info args))
	(cond
	  (file-info
	   (present-files file-info args (or recursive
					     (ls-state-recurring *ls-state*))))
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
	      (values)))))))

(defparameter *sort-fields*
  `("none" "name" "iname" "size" "access-time" "creation-time"
    "modification-time" "extension" "type" "mime-type"))

#+lish
(lish:defcommand ls
  ((files pathname :repeating t :help "The file(s) to list.")
   (long boolean :short-arg #\l :help "True to list in long format.")
   (1-column boolean :short-arg #\1 :help "True to list one file per line.")
   (wide boolean :short-arg #\w
    :help "True to not truncate the output to the terminal width.")
   (hidden boolean :short-arg #\a :help "True to list hidden files.")
   (directory boolean :short-arg #\d
    :help "True to list the directory itself, not its contents.")
   (sort-by choice :long-arg "sort" :help "Field to sort by."
    :default "name"
    ;; :choices ''("none" "name" "size" "access-time" "creation-time"
    ;; 	      "modification-time" "extension" "type" "mime-type"))
    :choices *sort-fields*)
   (date-format lenient-choice :short-arg #\f :help "Date format to use."
    :default "normal"
    :choices '("normal" "nibby"))
   (reverse boolean :short-arg #\r :help "Reverse sort order.")
   (show-size boolean :short-arg #\s :help "True to show the file size.")
   (non-human-size boolean :short-arg #\h :help "True to show sizes in bytes.")
   (omit-headers boolean :short-arg #\H :help "True to omit table headers.")
   (size-format lenient-choice :long-arg "size-format" :default "human"
		:choices '("human" "bytes")
		:help "Format to show sizes with.")
   (collect boolean :short-arg #\c :help "Collect results as a sequence.")
   (nice-table boolean :short-arg #\N :help "Collect results as a nice table.")
   (recursive boolean :short-arg #\R :help "Recursively list sub-directories.")
   (signal-errors boolean :short-arg #\E
    :help "True to signal errors. Otherwise print them to *error-output*.")
   ;; Short cut sort args:
   (by-extension boolean :short-arg #\X :help "Sort by file name extension.")
   (by-size boolean :short-arg #\S :help "Sort by size, largest first.")
   (by-time boolean :short-arg #\t :help "Sort by time, newest first.")
   (ignore-backups boolean :short-arg #\B :help "Ignore files ending in ~")
   (help boolean :long-arg "help" :help "Show the help."))
  :keys-as args
  :accepts (sequence list)
  "List files in a peculiar way that's not really compatible with the
traditional ‘ls’ command."
  ;; Translate some args
  (when by-extension
    (remf args :sort-by)
    (setf args (append args '(:sort-by :extension)))
    (remf args :by-extension))
  (when by-size
    (remf args :sort-by)
    (setf args (append args '(:sort-by :size)))
    (remf args :reverse)
    (setf args (append args '(:reverse t)))
    (remf args :by-size))
  (when by-time
    (remf args :sort-by)
    ;; (setf args (append args '(:sort-by :modification-time)))
    (setf args (append args '(:sort-by :creation-time)))
    (remf args :by-time))
  (when non-human-size
    (remf args :size-format)
    (setf args (append args '(:size-format :bytes))))
  (remf args :non-human-size)
  (when nice-table
    (setf args (append args '(:collect t))))
  (when help
    (lish::print-command-help (lish:get-command "ls"))
    (return-from !ls (values)))
  (flet ((thunk ()
	   (if (and lish:*input* (listp lish:*input*))
	       (apply #'list-files :files lish:*input* args)
	       ;; (progn
	       ;; 	 (setf (getf args :files)
	       ;; 	       (append lish:*input* (getf args :files)))
	       ;; 	 (apply #'list-files args))
	       (apply #'list-files args))))
    (if (or collect nice-table)
	(setf lish:*output*
	      (let ((result (thunk)))
		(etypecase result
		  (list (nreverse result))
		  (table result))))
	(thunk))))

;; End
