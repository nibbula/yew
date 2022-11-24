;;;
;;; ls-cmds.lisp - Commands for ls.
;;;

(in-package :ls)

(defparameter *sort-fields*
  `("none" "name" "iname" "size" "access-time" "creation-time"
    "modification-time" "extension" "type" "mime-type"))

(lish:defcommand ls
  ((files pathname :repeating t :help "The file(s) to list.")
   (long boolean :short-arg #\l :help "True to list in long format.")
   (1-column boolean :short-arg #\1 :help "True to list one file per line.")
   (wide boolean :short-arg #\w
    :help "True to not truncate the output to the terminal width.")
   (hidden boolean :short-arg #\a :help "True to list hidden files.")
   (directory boolean :short-arg #\d
    :help "True to list the directory itself, not its contents.")
   (case-insensitive boolean :short-arg #\C
    :help "Sorting by name is case insensitive.")
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
   (collect boolean :short-arg #\c :help "Collect results as a sequence."
    :default t :use-supplied-flag t)
   (nice-table boolean :short-arg #\N :help "Collect results as a nice table."
    :default nil :use-supplied-flag t)
   (quiet boolean :short-arg #\q :help "Suppress output.")
   (recursive boolean :short-arg #\R :help "Recursively list sub-directories.")
   (signal-errors boolean :short-arg #\E
    :help "True to signal errors. Otherwise print them to *error-output*.")
   ;; Short cut sort args:
   (by-extension boolean :short-arg #\X :help "Sort by file name extension.")
   (by-size boolean :short-arg #\S :help "Sort by size, largest first.")
   (by-time boolean :short-arg #\t :help "Sort by time, newest first.")
   (ignore-backups boolean :short-arg #\B :help "Ignore files ending in ~"))
  :args-as args
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
  (when (and (not collect-supplied-p)
	     (not nice-table-supplied-p))
    (when long
      (setf (getf args :nice-table) t)))
  (when nice-table
    (setf args (append args '(:collect t))))
  ;; Default to collecting if the receiver accepts
  ;; (when (and (not collect-supplied-p)
  ;; 	     (not nice-table-supplied-p))
  ;;   (cond
  ;;     ((lish:accepts 'sequence)
  ;;      (setf (getf args :collect) t))
  ;;     ((lish:accepts 'table:table)
  ;;      (setf (getf args :nice-table) t))))
  (flet ((thunk ()
	   (typecase lish:*input*
	     (null
	      (apply #'list-files args))
	     ;; @@@ It might be nice if we could say collection of
	     ;; nos:path-designator, but I think the only way to do it
	     ;; efficiently, would be as typed containers.
	     (nos:path-designator
	      (apply #'list-files :files (list lish:*input*) args))
	     (cons
	      (apply #'list-files :files lish:*input* args))
	     (t ;; just pretend we can handle it?
	      (apply #'list-files :files lish:*input* args)))))
    (if (or collect nice-table)
	(setf lish:*output*
	      (let ((result (thunk)))
		(etypecase result
		  (list (nreverse result))
		  (table result))))
	(thunk))))

;; End
