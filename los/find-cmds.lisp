;;;
;;; find-cmds.lisp - Commands for find.
;;;

(in-package :find)

;; This isn't POSIX (or any Unix) compatible. So what. The find commands on
;; various Unix systems seem to be incompatible, even though POSIX has
;; standardized it, and despite GNU and BSD adding cross-compatibiliy flags.
;; I seem to have to look up the arguments almost every time. If you want to
;; make your own compatible "find", be my guest.  Arguments to Unix find are
;; one of most complicated of any Unix command. For example it can take dates
;; in cvs(1) date format, such as "1 hour ago".  The whole thing is rather
;; insane, because file system designers keep adding new types of metadata to
;; their filesystems, which a standardized find has little chance of being able
;; to use.
;;
;; I really would prefer to use some SQL-like (or rather CLSQL) syntax for
;; finding files.
;;
;; @@@ I _would_ anyway like to add some more flags, like -time and -newer,
;; and maybe -size and -type.

(lish:defcommand find
;  "find [-lvrcp] [-d dir] [-n name] [-t type] [-p path] [-e expr] [-a action]"
;  (&key (dir ".") follow-links verbose regexp collect
;	name file-type path expr action print)
  ((thing	 case-preserving-object :required nil :help "A thing to find.")
   (dir		 case-preserving-object #|pathname|# :short-arg #\d :default "."
    :help        "Directory to start from.")
   (follow-links boolean  :short-arg #\l :default t
    :help        "True to follow symbolic links.")
   (verbose	 boolean  :short-arg #\v
    :help        "True to print directories as they are traversed.")
   (max-depth	 integer  :long-arg "max-depth"
    :help        "Maximum depth of files to consider.")
   (min-depth	 integer  :long-arg "min-depth"
    :help        "Minimum depth of files to consider.")
   (same-device  boolean  :short-arg #\x
    :help        "Don't consider directories on devices different than the
                  starting directory.")
   (regexp	 boolean  :short-arg #\r :default t
    :help        "True if matching is done with regular expressions.")
   (case-mode	 choice  :short-arg #\C :default :smart
		 :choices '(:smart :sensitive :insensitive)
    :help        "The mode for matching letter characters. One of 'smart',
                  'sensitive', or 'insensitive'. (default 'smart')
                  'smart' is insensitive if there are no upper case letters,
                  sesitive otherwise. 'sesitive' matches the exact case.
                  'insensitive' matches any case.")
   (unicode-normalize boolean :short-arg #\u :default t
    :help "Normalize unicode before comparison.")
   (unicode-remove-combining boolean :short-arg #\U
    :help "Normalize and remove unicode combining characters.")
   (collect	 boolean  :short-arg #\c :default '(lish:accepts :sequence)
    :help        "Return the file name as a list.")
   (name	 string   :short-arg #\n
    :help        "Matches file name.")
   (file-type	 string   :short-arg #\T
    :help        "Matches file type as returned by “file”.")
   (path	 pathname :short-arg #\P
    :help        "Matches full path.")
   (size	 string   :long-arg "size"
    :help        "Matches size.")
   (type	 string   :short-arg #\t :long-arg "type"
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
   (test	 object   :long-arg "test"
    :help        "A function that given the path name, returns true if we
                 should include the file.")
   (action	 object   :short-arg #\a :long-arg "do"
    :help        "Calls the function with the full path for every match.")
   (print	 boolean  :short-arg #\p :default t
    :help        "True to print the file name.")
   )
  :args-as args
  "Find files recursively in subdirectories. Start in the directory given
by --dir, which defaults to the current directory."
  (dbugf :accepts "find *accepts* = ~s~%" lish:*accepts*)
  (when (lish:accepts :sequence 'sequence :list)
    ;;(dbugf :accepts "find sending output to *output*~%")
    ;; (format *debug-io* "something accepts ~s~%"
    ;; 	    (lish:accepts :sequence 'sequence :list))
    (setf collect t))
  (typecase thing
    (null #| ignore it |#)
    (symbol (setf (getf args :name) (string thing)))
    ((or string pathname) (setf (getf args :name) thing))
    (t (error "I don't know what to do with the ~s ~s." (type-of thing) thing)))
  (remf args :thing)
  (when (not (or (consp dir) (vectorp dir) #|(collection-p dir)|#))
    ;; (format t "dir = ~s ~s~%" (type-of dir) dir)
    (setf dir (princ-to-string dir)
	  (getf args :dir) dir))
  (let ((ff (apply #'find-files-interactive args)))
    (when collect (setf lish:*output* ff))
    ;; (format *debug-io* "end of find, *output* = ~s~%" lish:*output*)
    ;; (finish-output *debug-io*)
    ff))

;; End
