;;;
;;; uniq.lisp - Print unique lines.
;;;

(defpackage :uniq
  (:documentation "Print unique lines.")
  (:use :cl :dlib :collections)
  (:export
   #:uniq
   #:!uniq
   ))
(in-package :uniq)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ we could curry up a specific skipper lambda to make this faster?
(defun snip-line (line skip-chars skip-fields width-limit)
  (let ((pos 0))
    (when (and skip-fields (numberp skip-fields) (> skip-fields 0))
      (multiple-value-bind (s e ss ee)
	  (ppcre:scan (s+ "(\\W*(\\w+)){" (1+ skip-fields) "}") line)
	(declare (ignore s e ee))
	;; (setf pos (svref ss 1))
	;; Have to include the blanks
	(setf pos (svref ss 0))
	))
    (when (and skip-chars (numberp skip-chars) (> skip-chars 0))
      (incf pos skip-chars))
    (if width-limit
	(subseq line (min pos (length line) width-limit)
		(min (+ pos width-limit) (length line)))
	(subseq line (min pos (length line))))))

(defun uniq (&key (file *standard-input*) adjacent ignore-case only-unique
	       only-repeated print-count skip-chars skip-fields collect
	       (quiet nil quiet-provided-p) width-limit)
  "Print unique lines.

- FILE          A file or stream to read line from. Defaults to *standard-input*.
- ADJACENT      True to consider only adjacent lines. This is like POSIX uniq.
- IGNORE-CASE   Ignore character case when comparing lines.
- ONLY-UNIQUE   Print only lines that are not repeated.
- ONLY-REPEATED Print only lines that are repeated.
- PRINT-COUNT   Print a count of duplicate when printing lines.
- SKIP-CHARS    Number of characters to skip when comparing lines.
- SKIP-FIELDS   Number of fields to skip when comparing lines. Fields are
                separated by white space.
- COLLECT       Return a list of lines. With PRINT-COUNT true, return a list of 
                conses of (count . line).
- QUIET         Don't print lines. Probably only useful with collect. Defaults
                to true when collect is true.
- WIDTH-LIMIT   Number of characters to compare when comparing lines. NIL means
                compare the whole line."
  (when (and quiet (not collect))
    (warn "Quiet was specified without collect."))
  (when (and collect (not quiet-provided-p))
    (setf quiet t))

  (when (and only-unique only-repeated)
    (warn #.(s+ "Both only-unique and only-repeated were specified, which "
		"produces nothing, slowly.")))

  (let ((table (when (not adjacent)
		 (make-hash-table :test (if ignore-case #'equalp #'equal))))
	line results line-thing line-saver line-getter)
    (declare (type (or string null) line)
	     (type list results))
    ;; When we are snipping the line for comparison, we have to save the full
    ;; line for output in the hash table, so the hash value is
    ;; (count . full-line) Otherwise we can use the key to retrieve the line,
    ;; so the hash value is just the count.
    (labels ((line-snipper (line)
	       (snip-line line skip-chars skip-fields width-limit))
	     (save-line-with-skip (line)
	       (let* ((line-key (funcall line-thing line))
		      (h (gethash line-key table)))
		 (if h
		     (progn
		       (incf (car h))
		       ;; only save the first one
		       ;; (setf (cdr h) line)
		       )
		     (setf (gethash line-key table)
			   (cons 0 line)))))
	     (save-line (line)
	       (incf (gethash line table 0)))
	     (get-line-with-skip (rec)
	       "Return the line and count from a snipped line table mapping."
	       (values (cdr (svref rec 1)) (car (svref rec 1))))
	     (get-line (rec)
	       "Return the line and count."
	       (values (svref rec 0) (svref rec 1)))
	     (take-action-p (count)
	       (or (not (or only-unique only-repeated))
		   (and only-unique (= count 1) (not only-repeated))
		   (and only-repeated (> count 1) (not only-unique)))))
      ;; We set the functions to call.
      (if (or width-limit skip-chars skip-fields)
	  (setf line-thing #'line-snipper
		line-saver #'save-line-with-skip
		line-getter #'get-line-with-skip)
	  (setf line-thing #'identity
		line-saver #'save-line
		line-getter #'get-line))
      (macrolet ((adjacent-body ()
		   "The body taking action in the adjacent loops."
		   `(progn
		      (setf compare-line (when line (funcall line-thing line)))
		      (if last-line
			  (if (funcall compare compare-line compare-last-line)
			      (incf count)
			      (progn
				(when (take-action-p count)
				  (when (not quiet)
				    (if print-count
					(format t "~d ~a~%" count last-line)
					(format t "~a~%" last-line)))
				  (when collect
				    (if print-count
					(push (cons count last-line) results)
					(push last-line results))))
				(setf count 1
				      last-line line
				      compare-last-line compare-line)))
			  (progn
			    (incf count)
			    (setf last-line line
				  compare-last-line compare-line))))))
	(etypecase file
	  ((or stream string pathname)
	   (with-open-file-or-stream (str file)
	     (if adjacent
		 (loop
		    :with count = 0
		    :and compare = (if ignore-case #'equalp #'equal)
		    :and last-line
		    :and compare-line
		    :and compare-last-line
		    :while (setf line (read-line str nil))
		    :do
		    (adjacent-body)
		    :finally (adjacent-body))
		 (loop
		    :while (setf line (read-line str nil))
		    :do
		    (funcall line-saver line)))))
	  (sequence
	   (if adjacent
	       (let (last-line compare-last-line compare-line
		     (count 0)
		     (compare (if ignore-case #'equalp #'equal)))
		 (omapn (_ (adjacent-body)) file)
		 (adjacent-body))
	       (omapn (_ (funcall line-saver line)) file))))
	;; The action loop for hashing.
	(omapk (_
		(multiple-value-bind (line count) (funcall line-getter _)
		  (when (take-action-p count)
		    (when (not quiet)
		      (if print-count
			  (format t "~d ~a~%" count line)
			  (format t "~a~%" line)))
		    (when collect
		      (push (if print-count (cons count line) line) results)))))
	       table)
	;; (table-print:print-as-table table)
	(and collect (nreverse results))))))

#+lish
(lish:defcommand uniq
  ((file pathname :help "Files to show unique lines for.")
   (adjacent boolean :short-arg #\a
    :help "Only consider adjacent lines. This may be much more efficient.")
   (ignore-case boolean :short-arg #\i :help "Ignore case when matching.")
   (only-unique boolean :short-arg #\u
    :help "Print only lines that are not repeated.")
   (only-repeated boolean :short-arg #\d
    :help "Print only lines that are repeated.")
   (print-count boolean :short-arg #\c
    :help "Print a count of the number of times the line occurs.")
   (skip-chars integer :short-arg #\s
    :help "Number of characters to skip when comparing lines.")
   (skip-fields integer :short-arg #\f
    :help "Number of fields to skip when comparing lines.")
   (collect boolean :short-arg #\C :help "Collect output in a list.")
   (quiet boolean :short-arg #\q :help "Don't produce output.")
   (width-limit integer :short-arg #\w
    :help "Compare only this many characters of a line."))
  :args-as args
  :accepts sequence
  "Print unique lines. Note that unlike POSIX uniq, we consider non-adjacent 
duplicate lines, unless adjacent is specified."
  (when (not file)
    (setf (getf args :file)
	  (if (and lish:*input* (typep lish:*input* 'sequence))
	      lish:*input*
	      *standard-input*)))
  (if collect
      (setf lish:*output* (apply #'uniq args))
      (apply #'uniq args)))

;; End
