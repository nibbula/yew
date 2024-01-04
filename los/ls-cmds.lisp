;;;
;;; ls-cmds.lisp - Commands for ls.
;;;

(in-package :ls)

(defparameter *sort-fields*
  `("none" "name" "iname" "size" "access-time" "creation-time"
    "modification-time" "extension" "type" "mime-type"))

(lish:defcommand ls
  ((files pathname :repeating t :help "The file(s) to list.")
   (long boolean :short-arg #\l :help "List in long format.")
   (1-column boolean :short-arg #\1 :use-supplied-flag t
    :help "List one file per line.")
   (wide boolean :short-arg #\w
    :help "Don't truncate the output to the terminal width.")
   (hidden boolean :short-arg #\a :help "List hidden files.")
   (directory boolean :short-arg #\d
    :help "List the directory itself, not its contents.")
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
   (show-size boolean :short-arg #\s :help "Show the file size.")
   (non-human-size boolean :short-arg #\h :help "Show sizes in bytes.")
   (omit-headers boolean :short-arg #\H :help "Omit the table headers.")
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
    :help "Signal errors. Otherwise print them to *error-output*.")
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
  ;; @@@ Testing for 'string-stream is bogus. Maybe we should add pipe-p or
  ;; something.
  (when (and (not 1-column-supplied-p) (typep *standard-output* 'string-stream))
    ;; Default to 1 column output when piping.
    (setf (getf args :1-column) t))
  (flet ((thunk ()
	   (lish:with-files-or-input (files)
	     (apply #'list-files :files files args))))
    (if (or collect nice-table)
	;; @@@ maybe we should flatten the results
	(setf lish:*output* (thunk))
	(thunk))))

;; End
