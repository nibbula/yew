;;
;; find.lisp - Replacement for unix find (eventually)
;;

(defpackage :find
  (:documentation "Find files in an old fashioned and slow manner.")
  (:use :cl :dlib :opsys :cl-ppcre #| :lparallel |#)
  (:export
   ;; Main entry point
   #:find-files
   ))
(in-package :find)

(declaim (optimize (speed 0)) (optimize (safety 3))
 	 (optimize (debug 3)) (optimize (space 0))
 	 (optimize (compilation-speed 0)))

;; (declaim (optimize (speed 3)) (optimize (safety 0))
;; 	 (optimize (debug 2)) (optimize (space 0))
;; 	 (optimize (compilation-speed 0)))

;; (declaim (optimize (debug 3)))

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

(defun prepare (pattern verbose regexp case-insensitive)
  (declare (ignore verbose))
  ;;(when verbose
  ;;    (format t "prepare ~a~%" pattern))
  (if regexp
      (progn
	;; (format t "pattern = ~s ~s~%" (type-of pattern) pattern)
	(if (functionp pattern)
	    pattern
	    (create-scanner pattern :case-insensitive-mode case-insensitive)))
      pattern))

(defgeneric compare (pattern data verbose case-insensitive)
  (:documentation "Compare pattern and data. Return nil if no match."))

;; I don't think I can do anything about the SBCL performance note about the
;; unsed function in the CL-PPCRE search macro. CL-PPCRE has a lot of
;; performance notes, but seems very fast anyway. :)
(defmethod compare ((pattern string) (data string) verbose case-insensitive)
  "Compare strings."
  (declare (type simple-string pattern data)
	   (ignore verbose) #| (type boolean verbose) |#)
  ;;(when verbose
  ;;  (format t "compare ~s ~s~%" pattern data))
  (if case-insensitive
      (search pattern data :test #'char-equal)
      (search pattern data)))

(defmethod compare ((pattern function) (data string) verbose case-insensitive)
  "Compare string and regexp."
  (declare (ignore verbose case-insensitive))
  ;;(if verbose
  ;;    (format t "compare ~s ~s~%" pattern data))
  ;; (format t "compare pattern = ~s ~s~%" (type-of pattern) pattern)
  (scan pattern data))

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

(defun compare-type (info type)
  "Return true if the type matches."
  (let ((c (type-char info)))
    (cond
      ((stringp type)
       (when (> (length type) 0)
	 (char-equal c (char type 0))))
      ((characterp type)
       (char-equal c type))
      ((symbolp type)
       (or (and c (char-equal c (char (string type) 0)))))
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

(defun resilient-stat (path)
  "Stat that returns the link information when the symbolic link is missing."
  #+unix (or (ignore-errors (os-unix:stat path)) (os-unix:lstat path))
  #-unix (or (ignore-errors (get-file-info path))
	     (get-file-info path :follow-links nil)))

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
		   (collect t)
		   max-depth
		   min-depth
		   (parallel nil)
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
  :case-mode      The mode for matching letter characters. One of :smart,
                  :sensitive, or :insensitive. (default :smart)
                  :smart is insensitive if there are no upper case letters,
                  sesitive otherwise. :sesitive matches the exact case.
                  :insensitive matches any case.
"
  (declare (type (or integer null) min-depth max-depth))
  (when (not (or (stringp dir) (pathnamep dir)))
    (error "DIR should be a string or a pathname."))
  (when do
    (when action
      (error "Provided both ACTION and DO."))
    (setf action do))

  (block nil
  (let* ((no-test (not (or name file-type path perm group user size type time)))
	 (files '())
	 (need-to-stat (or perm user group size type time))
	 (depth 0)
	 (dir-list
	  (handler-case (read-directory :dir dir :full t)
	    ; just print the error and go on
	    (error (e)
		   (format t "~a: ~a~%" dir e)
		   ;;(apply #'format t
		   ;;	  (simple-condition-format-control e)
		   ;;	  (simple-condition-format-arguments e))
		   ;;(terpri)
		   )))
	 (sub-dirs (loop :for d :in dir-list
			 :if (eql (dir-entry-type d) :directory)
		      :collect (dir-entry-name d)))
	 (case-insensitivity (or (and (eq case-mode :smart)
				      (typep name 'sequence)
				      (notany #'upper-case-p name))
				 (eq case-mode :insensitive)))
	 actions)
    (declare (type list files) (type integer depth))
    ;; Convert args into regex scanners.
    (when name
      (setf name (prepare name verbose regexp case-insensitivity)))
    (when path
      (setf path (prepare path verbose regexp case-insensitivity)))
    (when file-type
      (setf file-type (prepare file-type verbose regexp case-insensitivity)))
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
    (flet ((perform-action (full)
	     (loop :for a :in actions :do (funcall a full))
	     nil))
      (when (or (not min-depth) (>= depth min-depth))
	(loop :for f :in dir-list
	   :if (not (superfluous-file-name-p (dir-entry-name f))) :do
	   (let* ((n (dir-entry-name f))
		  (full (format nil "~a~a~a" dir *directory-separator* n))
		  (name-matched
		   (or (not name) (compare name n verbose case-insensitivity)))
		  (path-matched
		   (or (not path)
		       (compare path full verbose case-insensitivity)))
		  (stat (when need-to-stat (resilient-stat full)))
		  (perm-matched  (or (not perm)  (compare-perm  stat perm)))
		  (user-matched  (or (not user)  (compare-user  stat user)))
		  (group-matched (or (not group) (compare-group stat group)))
		  (size-matched  (or (not size)  (compare-size  stat size)))
		  (type-matched  (or (not type)  (compare-type  stat type)))
		  (time-matched  (or (not time)  (compare-time  stat time)))
		  (file-type-matched (or (not file-type)
					 (compare file-type
						  (file-type full) verbose
						  case-insensitivity))))
	     (cond
	       ((and name-matched type-matched path-matched perm-matched
		     user-matched group-matched size-matched time-matched
		     file-type-matched)
		(perform-action full))
	       (no-test
		(perform-action full)))))))
    (when (and max-depth (>= depth max-depth))
      ;; (when collect (setf lish:*output* files))
      ;;(return-from find-files files)
      (return files))
    (loop :for f :in sub-dirs
       ;;:if (not (or (equal ".." f) (equal "." f)))
       :if (not (superfluous-file-name-p f))
       :do
;;;	    (let ((sub (concatenate 'string dir *directory-separator* f))
       (let ((sub (s+ dir *directory-separator* f))
	     result)
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
			       :collect collect
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
   (collect nil)
   max-depth
   min-depth  
   ;; tests
   name file-type path perm group user size type time
   expr
   ;; actions
   do action (print t)))

;; This isn't POSIX (or any Unix) compatible. So what. The find commands on
;; various Unix systems seem to be incompatible, even though POSIX has
;; standardized it, and despite GNU and BSD adding cross-compatibiliy flags.
;; I seem to have to look up the arguments almost every time. If you want to
;; make your own compatible "find", be my guest.  Arguments to Unix find are
;; one of most complicated of any Unix command. For example it can take dates
;; in cvs(1) date format, such as "1 hour ago".  The whole thing is rather
;; insane, because file system designers keep adding new types of metadata to
;; their filesystem, which a standardized find has little chance of being able
;; to use.
;;
;; I really would prefer to use some SQL-like (or rather CLSQL) syntax for
;; finding files.
;;
;; @@@ I _would_ anyway like to add some more flags, like -time and -newer,
;; and maybe -size and -type.

#+lish
(lish:defcommand find
;  "find [-lvrcp] [-d dir] [-n name] [-t type] [-p path] [-e expr] [-a action]"
;  (&key (dir ".") follow-links verbose regexp collect
;	name file-type path expr action print)
  ((dir		 pathname :short-arg #\d :default "."
    :help        "Directory to start from.")
   (follow-links boolean  :short-arg #\l :default t
    :help        "True to follow symbolic links.")
   (verbose	 boolean  :short-arg #\v
    :help        "True to print directories as they are traversed.")
   (max-depth	 integer  :long-arg "max-depth"
    :help        "Maximum depth of files to consider.")
   (min-depth	 integer  :long-arg "min-depth"
    :help        "Minimum depth of files to consider.")
   (regexp	 boolean  :short-arg #\r :default t
    :help        "True if matching is done with regular expressions.")
   (case-mode	 choice  :short-arg #\C :default "smart"
		 :choices (:smart :sensitive :insensitive)
    :help        "The mode for matching letter characters. One of 'smart',
                  'sensitive', or 'insensitive'. (default 'smart')
                  'smart' is insensitive if there are no upper case letters,
                  sesitive otherwise. 'sesitive' matches the exact case.
                  'insensitive' matches any case.")
   (collect	 boolean  :short-arg #\c :default (lish:accepts :sequence)
    :help        "Return the file name as a list.")
   (name	 string   :short-arg #\n
    :help        "Matches file name.")
   (file-type	 string   :short-arg #\t
    :help        "Matches file type as returned by “file”.")
   (path	 pathname :short-arg #\P
    :help        "Matches full path.")
   (size	 string   :long-arg "size"
    :help        "Matches size.")
   (type	 string   :long-arg "type"
    :help        "Matches type.")
   (time	 string   :long-arg "time"
    :help        "Matches time.")
   (perm	 string   :long-arg "perm"
    :help        "Matches permissions.")
   (group	 string   :long-arg "group"
    :help        "Matches group.")
   (user	 string   :long-arg "user"
    :help        "Matches user.")
   (expr	 string   :short-arg #\e
    :help        "Matches expression (NOT IMPLEMENTED YET)")
   (action	 object   :short-arg #\a :long-arg "do"
    :help        "Calls the function with the full path for every match.")
   (print	 boolean  :short-arg #\p :default t
    :help        "True to print the file name."))
  :keys-as all-keys
  "Find files recursively in subdirectories. Start in the directory given
by --dir, which defaults to the current directory."
  (dbugf :accepts "find *accepts* = ~s~%" lish:*accepts*)
  (when (lish:accepts :sequence 'sequence :list)
    ;;(dbugf :accepts "find sending output to *output*~%")
    ;; (format *debug-io* "something accepts ~s~%"
    ;; 	    (lish:accepts :sequence 'sequence :list))
    (setf collect t))
  (let ((ff (apply #'find-files-interactive all-keys)))
    (when collect (setf lish:*output* ff))
    ;; (format *debug-io* "end of find, *output* = ~s~%" lish:*output*)
    ;; (finish-output *debug-io*)
    ff))

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

;; EOF
