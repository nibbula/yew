;;;
;;; cal-cmds.lisp - Command definition for cal.
;;;

(in-package :cal)

(lish:defcommand cal
  ((year integer :optional t :help "The year to show a calendar for.")
   (month integer :optional t :help "The month to show a calendar for."))
  :args-as args
  "Print a calendar."
  (let ((arg-count (/ (length args) 2)))
    (cond
      ((zerop arg-count)
       (setf year (current-year)))
      ((= 1 arg-count)
       (setf year (getf args :year)))
      ((= 2 arg-count)
       (psetf month (getf args :year)
	      year (getf args :month)))
      (t
       ;; @@@ Maybe there should be a way to do totally manual argument
       ;; processing?
       ))
    (setf lish:*output* (print-calendar :year year :month month))))

;; If you really want to specify a complex format like posix date, just use
;; format-date. If you want to set the date use, set-date.
(lish:defcommand date
  ((format choice :short-arg #\f :default *default-date-format*
    :choices '("unix" "net" "iso" "filename" "nibby" "lisp")
    :help "Format for the date.")
  ;; (timezone choice :short-arg #\z
  ;;  :help "Use this timezone instead of the default.")
  )
  "Print the date."
  (write-line
   (case (keywordify format)
     (:unix
      #+unix
      (if (find "+DATE-FMT+" uos:*nl-items* :key #'symbol-name :test #'equal)
	  (mini-strftime (uos:nl-langinfo uos::+DATE-FMT+))
	  (mini-strftime (uos:nl-langinfo uos::+D-T-FMT+)))
      #-unix
      (mini-strftime "%a %d %b %Y %r %Z"))
     (:lisp
      (princ-to-string (get-universal-time)))
     (otherwise
      (date-string :format (keywordify format))))))

#| @@@ finish simple-parse-date
(defcommand set-date
  ((date string :optional t :help "String date representation to set.")
   (seconds integer :short-arg #\s :help "Lisp Universal time in seconds.")
   (nanoseconds integer :short-arg #\n :default 0
    :help "Nanosecond part of universal time.")
   (from string :short-arg #\f :help "Where to get the date from."))
  "Set the date."
  (when (and date from)
    (error "Only specify one of either date or from."))
  (if (or date from (and *input* (typep *input* (or string integer))))
      ;; Until we have our own network date thing
      (cond
	(date
	 (etypecase date
	   (integer
	    (when (not (typep nanosecond integer))
	      (error "Nanoseconds must be an integer."))
	    (nos:set-time seconds nanoseconds))
	   (string
	    (let ((time (simple-parse-date date)))
	      (nos:set-time time)))
	   (t
	    (error "Date should be an integer or string."))))
	(from
	 (error "We don't know how to do remote date setting yet.")))
      (format t "~a~%" (date-string :format :net)))
  (values))
|#

#|
* Examples:

** Customized style:

cal 1 2021 | print-table -r (make-instance
                             'terminal-table:terminal-box-table-renderer
                              :box-style table-print::*fancy-box*
                              :box-color :magenta :title-color :cyan :x 0)

** Bigger:

(defun boo ()
  (let ((cal (cal::calendar-table)))
    (omap
      (_
       (loop for i from 0 below (olength _)
             do (setf (oelt _ i)
                        (s+  "   " (oelt _ i) "   "))))
      cal)
    (omap (_ (setf (table:column-align _) :wrap
                   (table:column-width _) 8))
          (table:table-columns cal))
    ;; (print (table:table-columns cal))
    (tt-newline)
    (table-print:output-table cal
     (make-instance 'terminal-table:terminal-box-table-renderer :box-style
                    table-print:*fancy-box* :box-color :magenta :title-color
                    :cyan)
     *terminal*)
    (tt-finish-output)))
|#

;; End
