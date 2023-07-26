;;;
;;; locate-cmds.lisp - Commands for locate.
;;;

(in-package :locate)

(lish:defargtype locate-database (lish:arg-pathname)
  "A designator for a locate database. Can be either a locate:db or a pathname."
  ())

(lish:defcommand locate
  ((regexp string :help "Regular expression to match paths.")
   (database locate-database :short-arg #\d :help "Database to use.")
   (reload boolean :short-arg #\r :help "Reload the database.")
   (basename boolean :short-arg #\b :help "Match only the basename.")
   (collect boolean :short-arg #\c :help "Collect values to return.")
   (print boolean :short-arg #\p :default t :use-supplied-flag t
    :help
    "True to print matching paths. Print defaults to false when collect is given")
   (statistics boolean :short-arg #\S :help "Show database statistics."))
  :keys-as keys
  "Locate files."
  (when (and (not statistics) (not regexp))
    (error "Locate what?"))
  (when (and collect (not print-supplied-p))
    (setf print nil))
  (remf keys :regexp)
  (remf keys :reload)
  (when (not statistics)
    (remf keys :statistics))
  (when reload
    (setf *locate-db* nil))
  (when database
    (typecase database
      (db #| dont' mess with anythting |#)
      ((or string pathname)
       ;; Convert to :file keyword
       (remf keys :database)
       (setf (getf keys :file) database))
      (t
       (error "Database argument should be a database or a file name."))))

  (cond
    (statistics (show-statistics database (getf keys :file)))
    (t (setf lish:*output* (apply #'locate `(,regexp ,@keys))))))

;; End
