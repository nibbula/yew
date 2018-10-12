;;
;; grep.lisp - “Global” Regular Expression Print
;;

;; TODO:
;;   - return objects instead of printing
;;     file references / line references

(defpackage :grep
  (:documentation "Regular expression search in streams.")
  (:use :cl :cl-ppcre :opsys :dlib :grout :fatchar :stretchy)
  (:export
   #:grep
   #:grep-files
   ))
(in-package :grep)

(declaim (optimize (speed 0) (safety 3) (debug 3)
		   (space 0) (compilation-speed 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 0)
;; 		   (space 2) (compilation-speed 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 3)
;; 		   (space 0) (compilation-speed 0)))


;;;(define-constant +color-loop+
(defparameter +color-loop+
    '#1=(:red :yellow :blue :green :magenta :cyan :white . #1#))

(defun print-fat-line (fat-line)
  (let ((part (make-array 10 :element-type 'character
			  :fill-pointer 0 :adjustable t)))
    (with-output-to-string (str part)
      (loop :with last-attr :and last-fg
	 :for c :across fat-line :do
	 (when (or (not (eq (first (fatchar-attrs c)) last-attr))
		   (not (eq (fatchar-fg c) last-fg)))
	   (when (> (length part) 0)
	     (grout-princ part)
	     (setf (fill-pointer part) 0))
	   (setf last-attr (first (fatchar-attrs c))
		 last-fg (fatchar-fg c)))
	 (princ (fatchar-c c) str)
	 (if (position :underline (fatchar-attrs c))
	     (grout-set-underline t)
	     (grout-set-underline nil))
	 (if (or (not (fatchar-fg c)) (eq (fatchar-fg c) :default))
	     (grout-set-color :default :default)
	     (grout-set-color (fatchar-fg c) :default)))
      (when (> (length part) 0)
	(grout-princ part))
      (grout-set-underline nil)
      (grout-set-color :default :default))))

;; If you want fast, don't use color.

(defvar *fat-line* nil)

(defun set-region (fat start end color attr)
  (declare (type (vector fatchar *) fat)
	   (type fixnum start end)
	   (type keyword color attr))
  (loop :for i fixnum :from start :below end :do
     (setf (fatchar-fg (aref fat i)) color)
     (pushnew attr (fatchar-attrs (aref fat i)))))

(defun print-match (use-color pattern scanner line)
  (if use-color
      (progn
	(when (not *fat-line*)
	  (setf *fat-line* (make-array (length line)
				       :adjustable t :fill-pointer 0
				       :element-type 'fatchar
				       :initial-element (make-fatchar))))
	(setf (fill-pointer *fat-line*) 0)
	(loop
	   :for c :across line :and i = 0 :then (1+ i)
	   :do (stretchy-append *fat-line* (make-fatchar :c c)))
	(if scanner
	    ;; regexp
	    (do-scans (s e rs re scanner line)
	      (set-region *fat-line* s e (first +color-loop+) :underline)
	      (loop
		 :for start :across rs
		 :for end :across re
		 :for color = (cdr +color-loop+) :then (cdr color) ;)
		 :do
		 (when (and start end)
		   (set-region *fat-line* start end (car color) :underline))))
	    ;; fixed string
	    (loop :with pos
	       :and start = 0
	       :and pattern-len = (length pattern)
	       :and color = +color-loop+
	       :while (setf pos (search pattern line :start2 start))
	       :do
	       (set-region *fat-line* pos (+ pos pattern-len)
			   (car color) :underline)
	       (setf start (+ pos pattern-len)
		     #| color (cdr color) |#)))
	(print-fat-line *fat-line*)
	(grout-princ #\newline))
      ;; No color
      (progn
	(grout-format "~a~%" line))))

(defun print-prefix (use-color prefix)
  (declare (ignore use-color))
  (grout-color :magenta :default (princ-to-string prefix))
  (grout-color :cyan :default ":"))

(defun grep (pattern file-or-stream
	     &key
	       (output-stream *standard-output*)
	       count extended fixed file ignore-case quiet invert
	       line-number files-with-match files-without-match
	       filename use-color collect
	       scanner
	       &allow-other-keys)
  "Print occurances of the regular expression PATTERN in STREAM.
Aruguments are:
  OUTPUT-STREAM       - Where to print the output, Defaults to *STANDARD-OUTPUT*.
  COUNT               - True to show a count of matches.
  EXTENDED            - True to use extended regular expressions.
  FIXED               - True to search for fixed strings only, not regexps.
  IGNORE-CASE         - True to ignore character case when matching.
  QUIET               - True to not print matches.
  INVERT              - True to only print lines that don't match.
  LINE-NUMER          - True to preced matching lines by a line number.
  FILES-WITH-MATCH    - True to print only the file name for matches.
  FILES-WITHOUT-MATCH - True to print only the file name for matches.
  FILENAME            - Name of the file to print before the matching line.
  USE-COLOR           - True to highlight substrings in color.
  COLLECT             - True to return the results.
  SCANNER	      - A PPCRE scanner as returned by CREATE-SCANNER.
Second value is the scanner that was used.
"  
  (declare (ignore file) ;; @@@
	   #| (type stream output-stream) |#)
  (when (or files-with-match files-without-match)
    ;; @@@ For efficiency we should probably arrange for an early exit if
    ;; either of these is true, providied we aren't otherwise collecting the
    ;; results.
    (setf quiet t))
  
  (let* ((*fat-line* nil)
	 line (match-count 0) (line-count 0) result match matches
	 (check-it (if fixed
		       (lambda () (search pattern line))
		       (lambda () (scan scanner line)))))
    (declare (type fixnum line-count match-count))
    (setf scanner (and (not fixed)
		       (or scanner
			   (create-scanner
			    pattern
			    :extended-mode extended
			    :case-insensitive-mode ignore-case))))
    (with-grout (*grout* output-stream)
      (with-open-file-or-stream (stream file-or-stream)
	(setf matches
	      (loop :while (setf line (resilient-read-line stream nil nil))
		 :do
		 (setf result (funcall check-it)
		       match nil)
		 (cond
		   ((or (and result (not invert))
			(and (not result) invert))
		    (progn
		      (incf match-count)
		      (when filename
			(when (not quiet)
			  (print-prefix use-color filename))
			(when collect
			  (push filename match)))
		      (when line-number
			(when (not quiet)
			  (print-prefix use-color (1+ line-count)))
			(when collect
			  (push line-count match)))
		      (when (not quiet)
			(print-match use-color pattern scanner line))
		      (when collect
			(push line match))))
		   ((or (and (not result) (not invert))
			(and result invert))
		    #| don't print match |#))
		 (incf line-count)
		 :when (and collect match)
		 :collect (if (or filename line-number)
			      (nreverse match)
			      (car match))))))
    (values
     (if collect
	 matches
	 (if count match-count (/= 0 match-count)))
     scanner)))

(defun native-pathname (str)
  #-sbcl str
  #+sbcl (sb-ext:native-pathname str))

(defun grep-files (pattern &rest keywords
		   &key files recursive
		     (output-stream *standard-output*)
		     count extended fixed ignore-case quiet invert
		     line-number files-with-match files-without-match
		     use-color collect no-filename)
  "Call GREP with PATTERN on FILES. Arguments are:
  FILES     - A list of files to search.
  RECURSIVE - If FILES contain directory names, recursively search them.
 See the documentation for GREP for an explanation the other arguments."
  (declare (ignorable count extended ignore-case invert
		      recursive line-number use-color fixed)) ;; @@@
  (let (results scanner)
    (flet ((call-grep (pattern stream &optional args)
	     "Call grep with the same arguments we got."
	     (if args
		 (apply #'grep pattern stream :scanner scanner args)
		 (grep pattern stream :scanner scanner))))
      ;;(with-term-if (use-color output-stream)
      (with-grout (*grout* output-stream)
	(cond
	  ((null files)
	   (setf results (call-grep pattern *standard-input* keywords)))
	  (t
	   (when (not (consp files))
	     (setf files (list files)))
	   (loop :with result :and info
	      :for f :in files
	      :if (not (file-exists f)) :do
	      ;; XXX this isn't unix, make this a real error.
	      ;; But unfortunately errors are quite disruptive here, so we
	      ;; also need an option to suppress or gather errors, which should
	      ;; probably be the default.
	      (format *error-output*
		      "~a: No such file or directory~%" f)
	      :else :do
	      (setf info (get-file-info f))
	      (if (eq :directory (file-info-type info))
		  (format *error-output* "~a: Is a directory~%" f) ;; XXX
		  (with-open-file (stream (native-pathname f))
		    (multiple-value-setq (result scanner)
		      (call-grep pattern stream
				 (if (not no-filename)
				     (append keywords
					     `(:filename ,f))
				     keywords)))
		    (cond
		      ((and result files-with-match (not quiet))
		       (grout-format "~a~%" f))
		      ((and (not result) files-without-match (not quiet))
		       (grout-format "~a~%" f)))))
	      ;;:when collect :nconc result))))
	      (when collect
		(mapc (_ (push _ results)) result)))
	   (setf results (nreverse results))))
	;;:when collect :collect result))
	(when (and collect files-with-match)
	  (setf results
		(remove-duplicates (mapcar #'first results) :test #'equal)))
	results))))

#+lish
(lish:defcommand grep
  ((pattern string
    :optional nil
    :help "Regular expression to search for.")
   (files pathname
    :repeating t
    :help "Files to search in.")
   (files-with-match boolean
    :short-arg #\l
    :help "True to print only the file name (list) once for matches.")
   (files-without-match boolean
    :short-arg #\L
    :help "True to print only the file name (list) of files with no matches.")
   (ignore-case boolean
    :short-arg #\i
    :help "True to ignore character case when matching.")
   (invert boolean
    :short-arg #\v
    :help "True to only print lines that don't match.")
   (no-filename boolean
    :short-arg #\h
    :help "True to never print filenames (headers) with output.")
   (line-number boolean
    :short-arg #\n
    :help "True to print line numbers.")
   (quiet boolean
    :short-arg #\q
    :help "True to not produce any output.")
   (fixed boolean
    :short-arg #\F
    :help "True to search for a fixed strings, not regular expressions.")
   ;; (line-up boolean :short-arg #\l
   ;;  :help "Line up matches.")
   (use-color boolean
    :short-arg #\c :default t
    :help "True to highlight substrings in color.")
   (collect boolean
    :short-arg #\s :default '(lish:accepts :sequence)
    :help "True to collect matches in a sequence.")
   (positions boolean :short-arg #\p
    :help "True to send positions to Lish output. Equivalent to -nqs, except
it's only quiet if the receiving command accepts sequences."))
  :accepts (:stream :sequence)
  "Search for patterns in input."
  (let (result)
    (cond
      ((lish:accepts :sequence)
       (dbugf :accepts "HOWDUUUU output accepts a sequence~%")
       (setf collect t)
       (when positions
	 (setf quiet t)))
      ((lish:accepts :grotty-stream)
       (dbugf :accepts "grep going to grotty, so in color~%")
       (setf use-color t))
      (t
       (dbugf :accepts "grep output accepts ~s~%" lish::*accepts*)))
    (dbugf :accepts "no files given~%")
    (dbugf :accepts "type-of *input* = ~s~%" (type-of lish:*input*))
    (dbugf :accepts "*input* = ~s~%" lish:*input*)
    (when positions
      (setf line-number t collect t))
    (setf result
	  (grep-files pattern
		      :files (or files (and lish:*input*
					    (typep lish:*input* 'sequence)
					    lish:*input*))
		      :files-with-match files-with-match
		      :files-without-match files-without-match
		      :no-filename no-filename
		      :fixed fixed
		      :ignore-case ignore-case
		      :invert invert
		      :line-number line-number
		      :quiet quiet
		      :use-color use-color
		      :collect collect))
    (if collect
	(progn
	  (dbugf :accepts "YOOOOOOO! output to *output*~%")
	  (setf lish:*output* result))
	result)))

;; EOF
