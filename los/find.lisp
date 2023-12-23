;;;
;;; find.lisp - Something vaguely like unix find
;;;

(defpackage :find
  (:documentation "Find files in an old fashioned and slow manner.")
  (:use :cl :dlib :opsys :collections :cl-ppcre #| :lparallel |# :char-util)
  (:export
   ;; Main entry point
   #:find-files
   #:!find
   ))
(in-package :find)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; (declaim (optimize (speed 3)) (optimize (safety 0))
;; 	 (optimize (debug 2)) (optimize (space 0))
;; 	 (optimize (compilation-speed 0)))

;; (declaim (optimize (debug 3)))

(defvar *ignored-files*
  `(".git")
  "File names to ignore.")

(defun ignored-file-name-p (file-name)
  "Return true if ‘file-name’ should be ignored."
  (or (superfluous-file-name-p file-name)
      (and *ignored-files* (find file-name *ignored-files* :test #'equal))))

;; Of course this isn't really "safe".
(defun safe-shell-line (command &rest args)
  ;; Handle encoding errors
  #+sbcl (let (line)
	   (handler-case
	       (with-process-output (s command args)
		 (setf line (read-line s)))
	   (sb-int:stream-decoding-error (e)
	     (let (r)
	       (cond
		 ((setf r (find-restart 'attempt-resync))
;		  (format t "Decoding error, resyncing ~s.~%" r)
		  (invoke-restart r))
		 ((setf r (find-restart 'force-end-of-file))
;		  (format t "Decoding error, force EOF ~s.~%" r)
		  (invoke-restart r))
		 ((setf r (find-restart 'abort e))
;		  (format t "Decoding error, Who fuking cares? ~s.~%" r)
;		  (format t "Cmd: ~s ~s~%" command args)
;		  (format t "Line: ~s~%" line)
;		  (invoke-restart r)
		  )
		 (t
;		  (format t "Decoding error, can't find a restart! So what!~%")
;		  (format t "Cmd: ~s ~s~%" command args)
;		  (format t "Line: ~s~%" line)
;		  (signal e)
		  )))))
	   line)
  ;; Don't worry about it
  #-sbcl (with-process-output (s command args)
	       (read-line s))
)

(defun file-type (f)
  ;;(or (safe-shell-line "file" "-b" f) "")
  (or (ignore-errors			; @@@ bogus!
	(magic:content-type-description (magic:guess-file-type f)))
      ""))

(defun normalize-filter (string)
  "Return STRING in Unicode normalized form NFD."
  (char-util:normalize-string string))

(defun remove-combining-filter (string)
  "Return STRING in Unicode normalized form NFD, with combining characters
removeed."
  (remove-if #'char-util:combining-char-p
	     (char-util:normalize-string string)))

(defun prepare (pattern verbose regexp case-insensitive filter)
  (declare (ignore verbose))
  ;;(when verbose
  ;;    (format t "prepare ~a~%" pattern))
  (if regexp
      (progn
	;; (format t "pattern = ~s ~s~%" (type-of pattern) pattern)
	(if (functionp pattern)
	    pattern
	    (create-scanner
	     (if filter (funcall filter pattern) pattern)
	     :case-insensitive-mode case-insensitive)))
      (if filter (funcall filter pattern) pattern)))

(defgeneric compare (pattern data verbose case-insensitive filter)
  (:documentation "Compare pattern and data. Return nil if no match."))

;; I don't think I can do anything about the SBCL performance note about the
;; unsed function in the CL-PPCRE search macro. CL-PPCRE has a lot of
;; performance notes, but seems very fast anyway. :)
(defmethod compare ((pattern string) (data string) verbose case-insensitive
		    filter)
  "Compare strings."
  (declare (type simple-string pattern data)
	   (ignore verbose) #| (type boolean verbose) |#)
  ;;(when verbose
  ;;  (format t "compare ~s ~s~%" pattern data))
  (if case-insensitive
      (search pattern (if filter (funcall filter data) data) :test #'char-equal)
      (search pattern (if filter (funcall filter data) data))))

(defmethod compare ((pattern function) (data string) verbose case-insensitive
		    filter)
  "Compare string and regexp."
  (declare (ignore verbose case-insensitive))
  ;;(if verbose
  ;;    (format t "compare ~s ~s~%" pattern data))
  ;; (format t "compare pattern = ~s ~s~%" (type-of pattern) pattern)
  (scan pattern (if filter (funcall filter data) data)))

(defun compare-perm (stat perm)
  "Return true if the permissions match."
  #+unix
  (cond
    ((integerp perm)
     (= (os-unix:file-status-mode stat) perm))
    ((stringp perm)
     ;; @@@ stupid compare for now (need work in opsys)
     (equal (format nil "~o" (os-unix:file-status-mode stat)) perm))
    (t
     (error "Unknown permission type ~s ~a" perm (type-of perm))))
  #-unix
  (declare (ignore stat perm))
  nil)


(defun compare-user (stat user)
  "Return true if the user matches."
  #+unix
  (or (and (integerp user) (= (os-unix:file-status-uid stat) user))
      (and (stringp user) (equal (os-unix:file-status-uid stat)
				 (os-unix:user-id :name user))))
  #-unix
  (declare (ignore stat user))
  nil)

(defun compare-group (stat group)
  "Return true if the group matches."
  #+unix
  (or (and (integerp group) (= (os-unix:file-status-gid stat) group))
      (and (stringp group) (equal (os-unix:file-status-gid stat) (group-id group))))
  #-unix
  (declare (ignore stat group))
  nil)

(defun file-size (info)
  #+unix (os-unix:file-status-size info)
  #-unix (file-info-size info))

;; @@@ This is an indication we should be doing something different.
;; A compiled expression?
(defun compare-size (info size)
  "Return true if the size matches."
  (let (result expr)
    (etypecase size
      (number
       (setf result (= (file-size info) size)))
      (string
       (setf expr `(let ((size ,(file-size info)))
		     ,(read-from-string size))
	     result (eval expr))
       ;;(format t "string ~a = ~a~%" expr result)
       )
      (list
       ;;(format t "list ~a~%" size)
       (setf result
	     (eval `(let ((size ,(file-size info)))
		      ,size)))))
    ;;(format t "result = ~a~%" result)
    result))

(defun type-char (info)
  #+unix (os-unix:file-type-char (os-unix:file-status-mode info))
  #-unix
  ;; @@@ maybe we should make a non-system specific file-type-char?
  (case (file-info-type info)
    (:regular	    #\r)
    (:directory	    #\d)
    (:symbolic-link #\l)
    (:device	    #\c)
    (:other	    #\o)))

;; This is really just to allow compatibility with unix find which uses #\f
;; instead of #\r for regular files.
(defun type-char-equal (c1 c2)
  "Return true if file type characters ‘c1’ and ‘c1’ are equal."
  (cond
    ((eql #\f c1)
     (type-char-equal #\r c2))
    ((eql #\f c2)
     (type-char-equal c1 #\r))
    (t
     (char-equal c1 c2))))

(defun compare-type (info type)
  "Return true if the type matches."
  ;; (format t "compare-type ~s ~s ~s~%" (type-char info) type)
  (let ((c (type-char info)))
    (cond
      ((stringp type)
       (when (> (length type) 0)
	 (type-char-equal c (char type 0))))
      ((characterp type)
       (type-char-equal c type))
      ((symbolp type)
       (or (and c (type-char-equal c (char (string type) 0)))))
	   ;; (and (eq type @@@ match keyword!
      (t
       (error "Unknown file type ~s ~a" type (type-of type))))))

(defun modification-time (info)
  #+unix (os-unix:file-status-modify-time info)
  #-unix (file-info-modification-time info))

(defun compare-time (info time)
  "Return true if the time matches."
  ;; @@@ need a lot of work here
  (equal (modification-time info) time))

(defun compare-device (stat device)
  "Return true if the device is the same."
  #+unix
  (equal (os-unix:file-status-device stat) device)
  #-unix #-unix
  (declare (ignore stat device))
  t) ;; @@@ fixme for non-unix

;; @@@ fix to not duplicate stat?
(defun dir-device (dir)
  #+unix (uos:file-status-device (uos:stat dir))
  #-unix nil) ;; @@@

;; @@@ fix to not duplicate stat?
(defun same-device-p (dir device)
  #+unix (if (typep device 'boolean)
	     t
	     (compare-device (uos:stat dir) device))
  #-unix t) ;; @@@

(defun resilient-stat (path follow-links)
  "Stat that returns the link information when the symbolic link is missing."
  #+unix (or (and follow-links (ignore-errors (os-unix:stat path)))
	     (os-unix:lstat path))
  #-unix (or (and follow-links (ignore-errors (get-file-info path)))
	     (get-file-info path :follow-links nil)))

(defun gather-start-names (dir)
  "Return two values:
    a list of file names in ‘dir’
    a list sub-directory names of ‘dir’,
omitting ignored names. ‘dir’ can be a collection, in which case it combines the
lists for all the directories. The names are returned prefixed with the
directory. If there are any errors reading directories, just print them."
  (let ((sub-dirs)
	(result))
    (flet ((collect-dir (d #| append |#)
	     (loop
	       :with path
	       :for f :in (read-directory :dir d :full t)
	       :when (not (ignored-file-name-p (dir-entry-name f)))
	       :do
		  (setf path
			;; (if append
			;;     (nos:path-append d (dir-entry-name f))
			;;     (dir-entry-name f)))
			(nos:path-append d (dir-entry-name f)))
		  (when (eql (dir-entry-type f) :directory)
		    (push path sub-dirs))
	       :and
	       :collect path)))
      (handler-case
	  (etypecase dir
	    (nos:path-designator
	     (setf result (collect-dir dir #|nil|#)))
	    ((or list vector collection)
	     (omap-as 'list
		      (_ (when (not (ignored-file-name-p _))
			   (setf result (nconc result (collect-dir _ #|t|#)))))
		      dir)))
	;; Just print the error and go on.
	(error (e)
	  (format t "~a: ~a~%" dir e))))
    (values result sub-dirs)))

;; (defun matches-expr (filename expr)
;;   (typecase expr
;;     (cons
;;      (case (car expr)
;;        (#:
;;      )
;;     ((or string 

;; @@@ Add a parallel/threaded option?
;; @@@ Add an option to return a file-info instead of a string/pathname?

#|
This is totally not going to work the way I want.

(depfun pfind-files (&key (dir ".")
		     ;; options
		     (follow-links t)
		     verbose
		     (regexp t)
		     (collect t)
		     max-depth
		     min-depth
		     (parallel t)
		     ;; tests
		     name file-type path perm group user size type time
		     expr
		     ;; actions
		     do action
		     (print nil))
  "Find files. Start in directory DIR.
Tests are:
  :name		  Matches file name.
  :file-type	  Matches file type as returned by “file”.
  :path		  Matches full path.
  :perm           Matches permissions.
  :group          Matches group, a numeric GID or a name.
  :user           Matches user, a numeric UID or a name.
  :size           Matches size, a number with possible suffix.
  :type           Matches file type, one of: b, c, d, f, l, p, s.
  :time           Matches time, in some format or another.
  :expr		  Matches the expression, which consists of operators:
                   (and expr ...)
                   (or expr ...)
                   (not expr)
                   (name string)
                   (file-type string)
                   (perm string) or (permissions string)
                   (path string)
Actions:
  :do		  Calles the function with the full path for every match.
  :collect	  True to return the file name(s) as a list. (default t)
  :print	  True to print the file name. (default nil)
Options:
  :follow-links   True to follow symbolic links. (default t)
  :verbose	  True to print directories as they are traversed.
  :max-depth	  Maximum depth to consider.
  :min-depth	  Minimum depth to consider.
  :regexp	  True if matching is done with regular expressions. (default t)
"
  (flet ((doit ()
	   (find-files :dir dir
			:follow-links follow-links
			:verbose verbose
			:regexp regexp
			:collect collect
			:max-depth max-depth
			:min-depth min-depth
			:parallel parallel
			:name name
			:file-type file-type
			:path path
			:perm perm
			:group group
			:user user
			:size size
			:type type
			:time time
			:expr expr
			:do do
			:action action
			:print print)))
    (cond
      (parallel
       (if lparallel:*kernel*
	   (doit)
	   (let ((lparallel:*kernel*
		  (lparallel:make-kernel (processor-count))))
	     (doit))))
      (t (doit)))))
|#

(defun find-files (&key (dir ".")
		   ;; options
		   (follow-links t)
		   verbose
		   (regexp t)
		   (case-mode :smart)
		   (unicode-normalize t)
		   unicode-remove-combining
		   (collect t)
		   max-depth
		   min-depth
		   same-device
		   (parallel nil)
		   ;; tests
		   name file-type path perm group user size type time
		   expr test
		   ;; actions
		   do action
		   (print nil))
  "Find files. Start in directory ‘dir’.
Tests are:
  ‘name’          Matches file name.
  ‘file-type’     Matches file type as returned by “file”.
  ‘path’          Matches full path.
  ‘perm’          Matches permissions.
  ‘group’         Matches group, a numeric GID or a name.
  ‘user’          Matches user, a numeric UID or a name.
  ‘size’          Matches size, a number with possible suffix.
  ‘type’          Matches file type, one of: b, c, d, f, l, p, s.
  ‘time’          Matches time, in some format or another.
  ‘expr’          Matches the expression, which consists of operators:
                  (and expr ...)
                  (or expr ...)
                  (not expr)
                  (name string)
                  (file-type string)
                  (perm string) or (permissions string)
                  (path string)
  ‘test’          Satisfies the boolean predicate given the file name.
Actions:
  ‘do’            Calles the function with the full path for every match.
  ‘collect’       True to return the file name(s) as a list. (default t)
  ‘print’         True to print the file name. (default nil)
Options:
  ‘follow-links’  True to follow symbolic links. (default t)
  ‘verbose’       True to print directories as they are traversed.
  ‘max-depth’     Maximum depth to consider.
  ‘min-depth’     Minimum depth to consider.
  ‘same-device’   Consider only files only the same device we started on.
  ‘regexp’        True if matching is done with regular expressions. (default t)
  ‘case-mode’     The mode for matching letter characters. One of :smart,
                  :sensitive, or :insensitive. (default :smart)
                  :smart is insensitive if there are no upper case letters,
                  sesitive otherwise. :sesitive matches the exact case.
                  :insensitive matches any case.
"
  (declare (type (or integer null) min-depth max-depth))
  (when (not (or (stringp dir) (pathnamep dir) (ordered-collection-p dir)))
    (error "DIR should be a string or a pathname or an ordered collection."))
  (when do
    (when action
      (error "Provided both ACTION and DO."))
    (setf action do))

  (block nil
  (let* ((no-test (not (or name file-type path perm group user size type time
                           test)))
	 (files '())
	 (need-to-stat (or perm user group size type time same-device))
	 (depth 0)
	 (dir-list)
	 (sub-dirs)
	 #|
	 (dir-list
	  (handler-case
	      (if (collection-p dir)
		  (loop :for d :in dir
			:when (not (ignored-file-name-p d))
			:nconc (mapcar (_ (nos:path-append d _))
				       (remove-if (_ (ignored-file-name-p _))
                                (nos:read-directory :dir d #|:full t|#))))
		  (read-directory :dir dir #|:full t |#))
	    ;; Just print the error and go on.
	    (error (e)
	      (format t "~a: ~a~%" dir e))))
	 (sub-dirs (loop :for d :in dir-list
			 :if (eql (dir-entry-type d) :directory)
		      :collect (dir-entry-name d)))
	 |#
	 (case-insensitivity (or (and (eq case-mode :smart)
				      (typep name 'sequence)
				      (notany #'upper-case-p name))
				 (eq case-mode :insensitive)))
	 (filter (cond
		   (unicode-remove-combining #'remove-combining-filter)
		   (unicode-normalize #'normalize-filter)
		   (t nil)))
	 actions)
    (declare (type list files) (type integer depth))
    ;; Convert args into regex scanners.
    (when name
      (setf name (prepare name verbose regexp case-insensitivity filter)))
    (when path
      (setf path (prepare path verbose regexp case-insensitivity filter)))
    (when file-type
      (setf file-type (prepare file-type verbose regexp case-insensitivity
			       filter)))
    (when print
      (push (_ (format t "~a~%" _)) actions))
    (when collect
      (push (_ (setf files (nconc files (list _)))) actions))
    (when action
      (cond
	((or (functionp action)
	     (and (symbolp action) (fboundp action)))
	 (push action actions))
	((and (stringp action)
	      (fboundp (symbolify action)))
	 (push (symbolify action) actions))
	(t
	 (error "action ~s doesn't seem be callable." action))))

    ;; @@@ fix this
    (when (eq same-device t)
      (unless (typep dir 'nos:path-designator)
	(error "Sorry, but ‘same-device’ doesn't work yet with multiple ~
                starting ‘dirs’. "))
      (setf same-device (dir-device dir)))

    (flet ((perform-action (full)
	     (loop :for a :in actions :do (funcall a full))
	     nil))
      (multiple-value-setq (dir-list sub-dirs) (gather-start-names dir))
      (when (or (not min-depth) (>= depth min-depth))
	(loop :for f :in dir-list
	   :if (not (ignored-file-name-p #|(dir-entry-name f)|# f)) :do
	   (let* ((n #|(dir-entry-name f)|# f)
		  (full #|(path-append dir n) |# f)
		  (name-matched
		   (or (not name) (compare name n verbose case-insensitivity
					   filter)))
		  (path-matched
		   (or (not path)
		       (compare path full verbose case-insensitivity filter)))
		  (stat (when need-to-stat (resilient-stat full follow-links)))
		  (perm-matched  (or (not perm)  (compare-perm  stat perm)))
		  (user-matched  (or (not user)  (compare-user  stat user)))
		  (group-matched (or (not group) (compare-group stat group)))
		  (size-matched  (or (not size)  (compare-size  stat size)))
		  (type-matched  (or (not type)  (compare-type  stat type)))
		  (time-matched  (or (not time)  (compare-time  stat time)))
		  (device-matched
		    (or (typep same-device 'boolean)
			(compare-device stat same-device)))
		  (file-type-matched (or (not file-type)
					 (compare file-type
						  (file-type full) verbose
						  case-insensitivity
						  filter)))
		  (test-matched  (or (not test) (funcall test full))))
	     (cond
	       ((and name-matched type-matched path-matched perm-matched
		     user-matched group-matched size-matched time-matched
		     file-type-matched test-matched device-matched)
		(perform-action full))
	       (no-test
		(perform-action full)))))))
    (when (and max-depth (>= depth max-depth))
      ;; (when collect (setf lish:*output* files))
      ;;(return-from find-files files)
      (return files))
    (loop :for f :in sub-dirs
      :if (and (not (ignored-file-name-p f))
	       (same-device-p f same-device))
      :do
;;;	    (let ((sub (concatenate 'string dir *directory-separator* f))
       (let ((sub #|(nos:path-append dir f) |# f)
	     result)
	 ;; (when (eq same-device t)
	 ;;   (setf same-device (dir-device sub)))

	 (if verbose (format t "subdir: ~a~%" sub))
	 (setf result
	       ;; (if parallel
	       ;; 	   (pfind-files :dir sub
	       ;; 			:follow-links follow-links
	       ;; 			:verbose verbose
	       ;; 			:regexp regexp
	       ;; 			:collect collect
	       ;; 			:parallel parallel
	       ;; 			:name name
	       ;; 			:file-type file-type
	       ;; 			:path path
	       ;; 			:perm perm
	       ;; 			:group group
	       ;; 			:user user
	       ;; 			:size size
	       ;; 			:type type
	       ;; 			:time time
	       ;; 			:expr expr
	       ;; 			:do action
	       ;; 			:print print)
	       (find-files :dir sub
			   :follow-links follow-links
			   :verbose verbose
			   :regexp regexp
			   :case-mode case-mode
			   :unicode-normalize unicode-normalize
			   :unicode-remove-combining unicode-remove-combining
			   :collect collect
			   :max-depth max-depth
			   :min-depth min-depth
			   :same-device same-device
			   :parallel parallel
			   :name name
			   :file-type file-type
			   :path path
			   :perm perm
			   :group group
			   :user user
			   :size size
			   :type type
			   :time time
			   :expr expr
			   :do action
			   :test test
			   :print print))
	       ;; )
	 (if collect (setf files (nconc files result)))))
    ;; (format t "files = ~a~%" files)
    ;; (when collect (setf lish:*output* files))
    files)))

#| Unused code: (might use again someday)
       ;; remove the :dir keyword and argument from the args
       (let* ((p (position :dir args))
 	     (new-args
 	      (if (not p)
 		  args
 		  (loop with l = args and len = (length args)
 			for i from 0 below len
 			if (not (or (= i p) (= i (1+ p))))
 			collect (car l)
 			do (setf l (cdr l))))))
|#

(defmacro define-flurp (flurp-name func-name doc (&rest args))
  "Define a FLURP which is a function that calls a function with the same
arguments, but different defaults. No attempt is made to check that the
arguments are the same."
  (let ((all-vars (lambda-list-vars args :keywords-p t)))
    `(defun ,flurp-name (,@args)
       ,doc
       (,func-name ,@all-vars))))

(define-flurp find-files-interactive find-files
  "FIND-FILES-INTERACTIVE is like FIND-FILES but changes the defaults of
 COLLECT to NIL and PRINT to T."
  (&key
   (dir ".")
   ;; options
   (follow-links t)
   verbose
   (regexp t)
   (case-mode :smart)
   (unicode-normalize t)
   unicode-remove-combining
   (collect nil)
   max-depth
   min-depth
   same-device
   ;; tests
   name file-type path perm group user size type time
   test expr
   ;; actions
   do action (print t)))

;; @@@ How about try a pseudo-sql syntax?

(defparameter *attrs*
  #(name path size type flags access-time modification-time creation-time
    #+unix birth-time
    #+unix user
    #+unix group
    #+unix device
    #+unix permissions
    ))

(defparameter *ops*
  '((($a = $b) 	        (equal $a $b))
    (($a equal $b) 	(equal $a $b))
    (($a <> $b)		(not (equal $a $b)))
    (($a != $b)		(not (equal $a $b)))
    ((> gt)		(> $a $b))
    ((< lt)		(< $a $b))
    ((>= ge)		(>= $a $b))
    ((<= le)		(<= $a $b))
    (($a between $b)	(and (> $a $b)))
    (($a like $b) 	(like $a $b))
    ((in ($x …))	(in $x))))

(defun is-attr (a)
  "Return true if A is a file attribute."
  (position (symbolify a) *attrs*))

#|
select [attr]* [from dir] [where expr] [order [by]] [group [by]]

(defun nfind (&rest args)
  (let (attrs)
    (loop :with s
       :for a :in args :do
       (setf s (symbolify a))
       (cond
	 ((is-attr s) (push s attrs))
	 (())

|#

;; End
