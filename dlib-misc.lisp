;;
;; dlib-misc.lisp - Dan's library of miscellaneous useful functions.
;;

;; $Revision: 1.15 $

;; See dlib.lisp for the most essential stuff.
;; This is for things that are nice, but not essential.
;;
;; This is mostly separate from dlib so I can depend on other stuff like OPSYS.
;;
;; I don't like the name “MISC”. Let’s think of something better.
;; Maybe something like: dlib dlib-1 dlib-2 etc.

(defpackage :dlib-misc
  (:use :common-lisp :dlib #+mop :mop :opsys)
  (:documentation
   "More of Dan's generally useful miscellaneous functions.")
  (:export
   ;; load-package , dequire
   #:load-package
   #:load-use-package
   #:*dequire-path*
   #:dequire
   #:randomize-array
   #:justify-text
   #+mop #:describe-class
   #:show-expansion
   #:printenv
   #:char-apropos
   #:date-string
   #:simple-parse-time
   #:do-at
   #:print-properties
   #:print-columns
   #:print-size
   #:dir
   #:abspath
   #:dirname
   #:basename
   #:unintern-conflicts
   #:show-features
   #:describe-environment
   #:describe-packages
   #:describe-package
   #:autoload
  )
)
(in-package :dlib-misc)

#+clisp
(defun load-again (module &rest other-args &key &allow-other-keys)
  "Load without complaining about constant redefinitions."
  (let ((custom:*suppress-check-redefinition* t))
    (apply 'load module other-args)))

;;;
;;; dequire
;;;
;;; Perhaps better than require, and easier than ASDF, but not as good.
;;; @@@ Should REALLY use logical pathnames
;;;
;;; I made this before I knew how to use ASDF. I don't recommend anyone use it.
;;; Even myself.

(defvar *dequire-path* `(,(concatenate 'string
				       (namestring (user-homedir-pathname))
				       "src/lisp")
			 ".")
  "List of directories to look in for packages.")
(defvar *dequire-table* (make-hash-table)
  "Hash table of filenames and modifcation times for an autoload
   package symbol.")
(defvar *dequire-currently-loading* nil
  "")


(defstruct dequire-entry
  pathname	; full path name of the file loaded (string)
  date		; time loaded (fixnum)
  deps		; entries we are dependent on (list of keywords)
)

(defun dequire-add-dep (e dep)
  "Add a dep as a dependency of dequire-entry e."
  (pushnew dep (dequire-entry-deps e)))

(defun dequire-needs-reload (e)
  "Return true if the entry needs to be reloaded because dependency files
   have a newer write date."
  (let ((entry-date (dequire-entry-date e)))
    (if (< entry-date (file-write-date (dequire-entry-pathname e)))
	t ; source file is newer
	(loop for d in (dequire-entry-deps e)
	      do (let ((dep (gethash d *dequire-table*)))
		   (when (and dep (< (dequire-entry-date dep)
				     (file-write-date
				      (dequire-entry-pathname dep)))
		     (return t))))
	      finally (return nil)))))

(defun dequire-do-load (n base &key (verbose t))
  "Load the file with keyword name N and base file name string BASE."
  (let* ((f (concatenate 'string base ".lisp")) ; @@@ what about .lsp ?
	 (e (or (gethash n *dequire-table*)
		(make-dequire-entry
		 :pathname	(truename f)
		 :date		(file-write-date f))))
	 (saved-current *dequire-currently-loading*))
;;    (format t "loading ~a~%" base)
    ;; If we load something while we are already loading,
    ;; add the new file to the dependency list of the already loading file.
    (when *dequire-currently-loading*
      (dequire-add-dep
       (gethash *dequire-currently-loading* *dequire-table*) n))
    (setf (dequire-entry-date e) (file-write-date f))
    (setf (gethash n *dequire-table*) e)
    (setf *dequire-currently-loading* n)
    (load base :verbose verbose)	; load source or compiled
    (setf *dequire-currently-loading* saved-current)))

(defun dequire-find-file (name)
  (let ((dir
	 (find-if #'(lambda (f)
		    (probe-file
		     (concatenate 'string f "/" name ".lisp")))
		  *dequire-path*)))
    (if dir 
	(concatenate 'string dir "/" name)
	name)))

(defun dequire (n &key force (verbose t))
  "Load a file living somewhere in the *dequire-path*.
   If the file has not changed since it was last loaded, don't bother."
  (let ((basename (string-downcase n))
	(h (gethash n *dequire-table*)))
    (if h
	;; already loaded
	(if (or force (dequire-needs-reload h))
	    (dequire-do-load n (dequire-find-file basename) :verbose verbose)
	    (when verbose
	      (format t "~&;; Load package: ~:@(~A~) unchanged~%"
		      basename)))
	;; not already loaded
	(dequire-do-load n (dequire-find-file basename) :verbose verbose)))
  (values))

(defmacro load-package (n &rest r &key &allow-other-keys)
  `(dequire ,n ,@r))

(defmacro load-use-package (p)
  "Load and then use a package."
  `(progn (load-package ,p) (use-package ,p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Maybe this should be put somewhere else, since it's seldom used.
(defun randomize-array (s &key (factor 3))
  "Randomize the order of elements in an array. Factor is how many random
swaps to do times the length of the array. "
  (let* ((len (length s))
	 (swaps (* factor len)))
    (loop for i from 0 to swaps
	  do (let* ((a (random len))
		    (b (random len))
		    (aux (aref s a)))
	       (setf (aref s a) (aref s b))
	       (setf (aref s b) aux))))
  s)

;; This is mostly just here so I can remember how to do it.
;; It's very consing, but remarkably succinct.
;; It might be nice if it got the COLS from the implementation's idea of it.
(defun justify-text (s &key (cols 80) (stream *standard-output*)
			 (prefix "") (separator #\space))
  "Print the string S right justified by words, into COLS characters wide, on the stream STREAM. SEPARATOR is a character or string which separate the input words. PREFIX is printed in front of each line."
  (format stream (format nil "~a~a~a~d~a"
			 "~a~{~<~%" prefix "~1," cols ":;~a~> ~}")
	  prefix (split-sequence separator s :omit-empty t)))

;; This only works with a MOP
#+mop
(defun describe-class (class &optional (stream *standard-output*))
  (let ((symb nil))
    (ctypecase class
      ((or string keyword)
       (setf symb (make-symbol (string-upcase class)))
       (setf class (find-class symb)))
      (class (setf symb (class-name class)))
      (symbol (setf symb class
		    class (find-class class))))
    (format stream "~a : ~a~%" symb
	    (documentation symb 'class))
    (let ((max-width (loop for s in (class-slots class)
			   maximize
			   (length (string (slot-definition-name s))))))
      (loop for s in (class-slots class)
	    do (format stream "  ~(~va ~a~) ~a ~a~%"
		       max-width (slot-definition-name s)
		       (slot-definition-type s)
		       (aref (string (slot-definition-allocation s)) 0)
		       (documentation s t)))))
  (values))

(defun show-expansion (form &optional full)
  "Show a pretty printed macro expansion of the form. If full is true, expand all macros recursively."
  ;; @@@ This is wrong since it lowercases everything (like strings) too.
  ;; @@@ Fix to use pretty printer customization to lowercase.
  (format t "~%~(~?~%~)~%" "~:w"
	  (list (if full (macroexpand form) (macroexpand-1 form)))))

; calculating independant digits of pi
; (defun pipi (d)
;   (let ((x 0) (n 1) p)
;     (loop while (< n d)
;       do
;       (setf p (/ (* (- (* 120 n) 89) (+ n 16))
; 		 (* (* (* (- (* 512 n) 1024) (+ n 712)) (- n 206)) (+ n 21)))
; 	    x (+ (* 16 x) p))
;       (incf n)
;       (* 16 x)))

(defun printenv (&optional original-order)
  "Like the unix command."
  (let ((mv (reduce #'max (nos:environ)
		    :key #'(lambda (x) (length (symbol-name (car x))))))
	(sorted-list (if original-order
			 (nos:environ)
			 (sort (nos:environ) #'string-lessp
			       :key #'(lambda (x) (symbol-name (car x)))))))
    (loop :for v :in sorted-list
       :do (format t "~va ~30a~%" mv (car v) (cdr v)))))

;; Perhaps it would be more efficient if we could use the implementation's own
;; list, instead of having to go thru non-existent code points here, but it's
;; not like the speed is a big problem now, and this seems portable.
(defun char-apropos (name)
  "List characters with names matching NAME."
  (let ((match-name
	 (if (symbolp name)
	     (symbol-name name)
	     name)))
    (loop :for c :from 0 :below char-code-limit
       :do (let* ((code  (code-char c))
		  (name  (when code (char-name code)))
		  (match (when name (search match-name name :test #'equalp))))
	     (when match
	       (format t "#x~8,'0x ~c ~a~%" c code name))))))

;; alias for
(setf (symbol-function 'character-apropos) #'char-apropos)

(defparameter +day-abbrevs+ #("Mon" "Tue" "Wed"
			      "Thu" "Fri" "Sat" "Sun")) ; @@@ i18n

(defparameter +month-abbrevs+ #("Jan" "Feb" "Mar" "Apr"
				"May" "Jun" "Jul" "Aug"
				"Sep" "Oct" "Nov" "Dec")) ; @@@ i18n

(defun tz-minutes (tz)
  (* 60 (nth-value 1 (truncate tz))))

(defun tz-hours (tz)
  (truncate tz))

(defun date-string (&key (time (get-universal-time)) format
			 (gmt-p nil gmt-p-set))
  "Return a formated date string. A universal time can be provided with the ~
TIME keyword. FORMAT can be one of:
  :net          - an RFC822 formatted date. ~
  :filename     - a format that works well for a user readable file name.
  anything else - some format that Nibby likes.~
If GMT-P is true, the date is in Grenwich Mean Time, otherwise it's in the ~
current time zone."
; This makes a format default to GMT:
;  (when (and (not gmt-p-set) (find format '(:rfc822 :rfc :net)))
;    (setf gmt-p t))
  (declare (ignore gmt-p-set))
  (multiple-value-bind (seconds minutes hours date month year day
				daylight-p zone)
      (if gmt-p
	  (decode-universal-time time 0)
	  (decode-universal-time time))
    (declare (ignore daylight-p))
    (case format
      ((:rfc822 :rfc :net)
       (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~c~2,'0d~2,'0d"
	       (aref +day-abbrevs+ day)
	       date (aref +month-abbrevs+ (1- month)) year
	       hours minutes seconds
	       (if (< zone 0) #\+ #\-) (tz-hours zone) (tz-minutes zone)))
      (:filename
       (format nil "~d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"
	       year month date hours minutes seconds))
      (otherwise
       (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	       year month date hours minutes seconds)))))

;; (ulet (s1 s2 s3) body) ->
;; (let ((s1 (gensym)) (s2 (gensym)) (s3 (gensym))) body)

(defmacro format-date (format (&rest values)
		       &key (time (get-universal-time))
			 (stream nil)
			 (gmt-p nil))
  "Call #'format with FORMAT and the given date fields in VALUES. 
VALUES is a sequence of any of the following keywords:
  :seconds :minutes :hours :date :month :year :day :daylight-p :zone
  :day-abbrev :month-abbrev"
  (with-unique-names (seconds minutes hours date month year day daylight-p zone)
    (let ((args (loop :for v :in values
		   :collect
		   (etypecase v
		     (keyword
		      (case v
			(:day-abbrev `(aref +day-abbrevs+ ,day))
			(:month-abbrev `(aref +month-abbrevs+ (1- ,month)))
			(:std-zone
			 `(format nil "~c~2,'0d~2,'0d"
				  (if (< ,zone 0) #\+ #\-)
				  (tz-hours ,zone) (tz-minutes ,zone)))
			(otherwise
			 (symbol-name v))))))))
    `(multiple-value-bind (,seconds ,minutes ,hours ,date ,month ,year ,day
				    ,daylight-p ,zone)
	 (if ,gmt-p
	     (decode-universal-time ,time 0)
	     (decode-universal-time ,time))
       (format stream format ,args)))))

(defun simple-parse-time (str)
  "Parse a string into a universal-time. Format is:
HH [ ':' MM [ ':' SS ] ] [ PM | AM ]
The date part is considered to be the current date."
  (let (hour (min 0) (sec 0) (i 0) am-pm (len (length str)))
    (flet ((done ()
	     (multiple-value-bind (seconds minutes hours date month year day
					   daylight-p zone) (get-decoded-time)
	       (declare (ignore seconds minutes hours day daylight-p zone))
	       (encode-universal-time
		sec min hour date month year))))
      (multiple-value-setq (hour i) (parse-integer str :junk-allowed t))
      (when (not hour)
	(error "Time must start with a number."))
      (when (or (> hour 23) (< hour 0))	; fuck 24 as midnight
	(error "Hour must be in the range 0 to 23."))
      (if (>= i len)
	  (return-from simple-parse-time (done)) ;; only gave the hour
	  (progn
	    (cond
	      ((eql (aref str i) #\:)	; gonna give mintues
	       (incf i)
	       (multiple-value-setq (min i)
		 (parse-integer str :junk-allowed t :start i))
	       (when (not min)
		 (error "Minutes must be a number."))
	       (when (or (> min 59) (< min 0))
		 (error "Minutes must be in the range 0 to 59.")))
	      ;; @@@ This doesn't handle space after the hour, eg "1 am"
	      ((and (< (1+ i) len)	; give just am/pm
		    (position (aref str i) '(#\a #\p) :test #'equalp)
		    (equalp (aref str (1+ i)) #\m))
	       (setf am-pm (char-downcase (aref str i))))
	      (t
	       (format t "i=~a len=~a~%" i len)
	       (error "Hour must be followed by either :MM or AM/PM.")))))
      (cond
	((and (< i len) (eql (aref str i) #\:))	; seconds
	 (incf i)
	 (multiple-value-setq (sec i)
	   (parse-integer str :junk-allowed t :start i))
	 (when (not sec)
	   (error "Second colon must be followed by number of seconds.")))
	((and (< (1+ i) len)
	      (position (aref str i) '(#\a #\p) :test #'equalp)
	      (equalp (aref str (1+ i)) #\m))
	 (setf am-pm (char-downcase (aref str i)))))
      (when (or (< sec 0) (> sec 59))
	(error "Seconds must be in the range 0 to 59."))
      (when (eql am-pm #\p)
	(when (> hour 12)
	  (error "Hour must be less than 13 with AM/PM."))
	(incf hour 12))
      (done))))

(defmacro do-at (time form)
  "Call func with args at time. Time is a universal time or a string."
  (let ((tm (gensym)))
  `(progn
    (let (,tm)
      (etypecase ,time
	(string
	 (setf ,tm (simple-parse-time ,time)))
	(integer
	 (setf ,tm ,time)))
      (loop :until (>= (get-universal-time) ,tm)
	:do (sleep .2))
      ,form))))

(defun print-properties (prop-list)
  "Print a set of names and values nicely in two vertical columns."
  (let ((label-length (loop :for (name nil)
			 :in prop-list
			 :maximize (length name))))
    (loop :for (name value) :in prop-list
       :do (format t "~va: ~a~%"
		   label-length
		   (string-capitalize
		    (substitute #\space #\_ (substitute #\space #\- name)))
		   value))))

(defun print-columns (comp-list &key (columns 80) (stream *standard-output*)
				  (format-char #\a))
  (let ((len (length comp-list)))
    (let* ((max-len
	 ;; This, although terse, may be inefficient w/ big lists (@@@ test!):
; 	 (apply #'max (mapcar #'length comp-list))))
	    (loop :with m = 0
		  :for c :in comp-list :do
;		  (setf m (max m (length c)))
		  (setf m (max m (length
				  (format nil
					  (format nil "~~~c" format-char)
					  c))))
		  :finally (return (1+ m))))
	   (width (1- columns))
	   (ccc   (floor width max-len))
	   (cols  (if (zerop ccc) 1 ccc))
	   (rows  (if (zerop cols) len (ceiling len cols)))
	   (col   0)
	   (row   0)
	   (a     (make-array `(,cols ,rows) :initial-element nil)))
      ;; for each line,  for each col , elt mod
;    (tt-format e "~a~%~{~a~%~}" rows comp-list)
      (when (> max-len width) (setf max-len width))
;    (tt-format e "max-len = ~d width = ~a cols = ~a rows = ~a len = ~a~%"
;	       max-len width cols rows len)
      (loop :for c :in comp-list
	    :do
	    (setf (aref a col row) c)
	    (incf row)
	    (when (>= row rows)
	      (incf col)
	      (setf row 0)))
      (loop :for r :from 0 :below rows
	 :do
	 (loop :for c :from 0 :below cols
	    :do
	    (when (aref a c r)
	      ;; (format stream
	      ;; 	    (format nil (format nil "~~v~c"
	      ;; 				format-char)
	      ;; 		    max-len (aref a c r)))))
	      (write-string
	       (format nil (format nil "~~v~c" format-char)
		       max-len (aref a c r))
	       stream)))
	 (terpri)))))

(defparameter *iec-size-prefixes*
  #(nil "kibi" "mebi" "gibi" "tebi" "pebi" "exbi" "zebi" "yobi" "buttload"))

(defparameter *traditional-size-prefixes*
  #(nil "kilo" "mega" "giga" "tera" "peta" "exa" "zetta" "yotta" "buttload"))

(defparameter *iec-size-abbreviations*
  #(nil "Ki" "Mi" "Gi" "Ti" "Pi" "Ei" "Zi" "Yi" "**"))

(defparameter *traditional-size-abbreviations*
  #(nil "k" "M" "G" "T" "P" "E" "Z" "Y" "*"))

(defparameter *iec-sizes*
  (make-array '(11) :initial-contents 
	      (loop :for i :from 0 :to 10 :collect (expt 1024 i))))

(defun print-size (size &key (stream t) long unit traditional abbrevs)
  "Print a size with standard binary units. If LONG is true, print the long unit name. If UNIT is supplied, it should be a string of the unit to be prefixed. UNIT defaults to ‘B’ or “byte” depending on LONG. ABBREVS is a custom list of size abbreviations. If TRADITIONAL is non-nil, use traditional units."
  (setf unit (or unit (or (and long "byte") "B")))
  (let ((prefixes (if traditional *traditional-size-prefixes*
		      *iec-size-prefixes*)))
    (setf abbrevs (or abbrevs
		      (if traditional *traditional-size-abbreviations*
			  *iec-size-abbreviations*)))
    (flet ((pr (i)
	     (let ((n (/ size (svref *iec-sizes* i))))
	       (format stream "~:[~3,1f~;~d~]~@[ ~a~]~@[~a~]"
		       (eql 1 (denominator n)) n
		       (svref (if long
				  prefixes
				  abbrevs) i)
		       (when (svref abbrevs i) unit)))))
      (loop :for i :from 0 :to 9
	 :do
	 (when (< size (svref *iec-sizes* (1+ i)))
	   (return-from print-size (pr i))))
      (pr 9))))

#+clisp (ext:without-package-lock ("EXT")
	  (unintern (find-symbol "DIR" :ext) :ext)
	  (unintern (find-symbol "DIR" :cl-user) :cl-user)
	  ) ; mine is better :-P

(defun show-features ()
  "Print the features list nicely."
  (print-columns (sort (copy-seq *features*) #'string<) :format-char #\s))

(defparameter *env-funcs*
  '(
    lisp-implementation-type
    lisp-implementation-version
    short-site-name
    long-site-name
    machine-instance
    machine-type
    machine-version
    software-type
    software-version
    user-homedir-pathname
    internal-time-units-per-second
    ))

(defun describe-environment (&optional (stream *standard-output*))
  "Print out the environmental thingys."
  (let ((max-len (loop :for f :in *env-funcs*
		       :maximize (length (string f)))))
    (loop :for f :in *env-funcs*
	  :do
	  (format stream "~va  : ~a~%"
		  max-len (string-capitalize f)
		  (if (fboundp f)
		      (apply f nil)
		      (symbol-value f)))))
  (write-string (with-output-to-string (*standard-output*) (room)) stream)
  (values))

(defun package-symbol-count (pack &key (external nil))
  (declare (type package pack))
  (let ((sc 0))
    (if external
	(do-external-symbols (s pack)
	  #-(or ccl) (declare (ignore s)) (incf sc))
	(do-symbols          (s pack)
	  #-(or ccl) (declare (ignore s)) (incf sc)))
    sc))

(defun describe-packages (&key include-systems)
  "List packages in a hopefully consise format. If INCLUDE-SYSTEMS is true, it will also list packages it thinks are ASDF system packages."
  (format t "~30a ~5a ~a~%" "Package Name" "Count" "Package Deps")
  (format t "~30,,,'-a ~5,,,'-a ~43,,,'-a~%" "-" "-" "-")
  (let* ((paks (copy-seq (list-all-packages)))
	 (spaks (sort paks #'(lambda (p1 p2)
			       (string< (package-name p1)
					(package-name p2)))))
	 name nicks nice-used-by)
    (loop :for p :in spaks
       :do
       (setf nicks (package-nicknames p)
	     nice-used-by (mapcar
			   #'(lambda (p)      ; shortest package id
			       (reduce #'(lambda (a b) ; shortest sequence
					   (if (< (length a) (length b)) a b))
				       `(,(package-name p)
					  ,@(package-nicknames p))))
			   (package-use-list p)))
       (when (or include-systems
		 (not (and (equal nice-used-by '("CL" "ASDF"))
			   (ends-with "-SYSTEM" (package-name p)))))
	 (setf name (format nil "~a ~:[~;~:*~a~]" (package-name p) nicks))
	 (format t "~(~30a ~5d ~s~)~%" name
		 (package-symbol-count p :external t)
		 nice-used-by)))))

(defun describe-package (p &key symbols)
  "Describe a package. SYMBOLS is :ext (or just non-nil) to show external symbols, :all to show internal symbols too."
  (setf p (find-package p))
  (format t "Package ~a~%" (package-name p))
  (let* ((doc     (documentation p t))
	 (nicks   (package-nicknames p))
	 (uses    (mapcar #'package-name (package-use-list p)))
	 (used-by (mapcar #'package-name (package-used-by-list p)))
	 (shadow  (package-shadowing-symbols p))
	 (ext-count (package-symbol-count p :external t)))
    (if nicks
	(format t "~17a ~{~a ~}~%"
		(format nil "Nickname~p:" (length nicks)) nicks))
    (format t "~17a ~d~%" "External Symbols:" ext-count)
    (format t "~17a ~d~%" "Internal Symbols:"
	    (package-symbol-count p))
    (if shadow (format t "~17a ~a~%" "Shadowing Symbols:" shadow))
    (if uses (format t "~17a ~{~a ~}~%" "Uses:" uses))
    (if used-by (format t "~17a ~{~a ~}~%" "Used by:" used-by))
    (if doc (format t "Doc:~%~a~%" doc))
    (when (and symbols (> ext-count 0))
      (progn
	(format t "External Symbols:~%")
	(let* ((syms (sort (loop :for v :being :the external-symbols :in p
			      :collect v)
			   #'string-lessp))
	       (max (if symbols
			(apply #'max
			       (mapcar
				#'(lambda (s) (length (string s)))
				syms)))))
	  (when syms
	    (loop :for s :in syms
		  :do
		  (format t "~(~va ~a~)~%" max s
			  (cond
			    ((fboundp s) (fdefinition s))
			    ((boundp s) (type-of (symbol-value s)))
			    (t "<unbound>"))))))))
    (when (eql symbols :all)
      (progn
	(format t "Internal Symbols:~%")
	(let* ((syms (sort (loop :for v :being :the :present-symbols :in p
			      :collect v) #'string-lessp))
	       (max (if syms
			(apply #'max
			       (mapcar
				#'(lambda (s) (length (string s)))
				syms)))))
	  (when syms
	    (loop :for s :in syms
		  :do
		  (format t "~(~va ~a~)~%" max s
			  (cond
			    ((fboundp s) (fdefinition s))
			    ((boundp s) (type-of (symbol-value s)))
			    (t "<unbound>"))))))))))

(defun dir (&optional (pattern "*.*"))
  "Simple portable CL only directory listing."
  (declare (optimize (debug 3) (safety 0) (speed 0)))
  (setf pattern (pathname pattern))
  (when (not (pathname-name pattern))
    (setf pattern (merge-pathnames pattern (pathname "*.*"))))
  (let ((table
	 (loop
	    :for p :in #-clisp (directory pattern)
	    	       #+clisp (append (directory "*/") (directory "*"))
	    :collect
	    (if (or (pathname-name p) (pathname-type p))
		;; maybe a normal file
		(list
		 (format nil "~@[~a~]~@[.~a~]" (pathname-name p)
			 (pathname-type p))
		 (file-author p)
		 (print-size
		  (with-open-file (s p) (file-length s)) :stream nil)
		 (date-string :time (file-write-date p)))
		;; possibly a directory
		(list
		 (format nil "~a/"
			 #-ccl (car (last (pathname-directory p)))
			 #+ccl (car (last (pathname-directory p)))
			 )
		 (file-author p)
		 "-"
		 (date-string :time (file-write-date p)))))))
    (setf table (sort table #'string< :key #'first))
    (table:nice-print-table table '("Name" "Author" ("Size" :right) "Date"))))

(defun unintern-conflicts (package conflicting-package)
  "Unintern all symbols in PACKAGE that conflict with CONFLICTING-PACKAGE."
  (when (not (packagep package))
    (setf package (find-package package)))
  (when (not (packagep conflicting-package))
    (setf conflicting-package (find-package conflicting-package)))
  (do-external-symbols (sym conflicting-package)
    (let ((old-sym (find-symbol (string sym) package)))
      (when old-sym
	(unintern old-sym package)
;	(format t "Uninterning ~a from ~a~%" old-sym package)
	))))

;; Simple mindless ASDF autoloader.
(defmacro autoload (symbol system doc-string &optional macro)
  "Define a function to load an ASDF system which contains a redefinition of that function, and call it. In other words, define a stub function which automatically loads the real function from a package and then calls it. The actual package should be the same name as the ASDF package and define function with the same name. If it's a macro, pass MACRO as true, mmkay?"
  (let ((doit
	 (if macro
	     `(eval (list* (intern (string ',symbol) ,system) args))
	     `(apply (intern (string ',symbol) ,system) args))))
    `(,(if macro 'defmacro 'defun) ,symbol (&rest args)
       ,doc-string
       (asdf:load-system ,system)
;       (unintern ',symbol ,*package*)
       (unintern-conflicts ,*package* ,system)
       (use-package ,system ,*package*)
       (when (not (fboundp ',symbol))
	 (error "Autoload of ~a didn't define ~a." ,system ',symbol))
       ,doit)))

#|
(defun read-text ()
  "A very simplistic text reader. Mostly useful if you just want to paste some text into lisp and have it be stored as lines of words."
  (loop :with s
     :while (string/= "" (setf s (read-line)))
     :collect (split-sequence #(#\space #\tab) s :omit-empty t :by-group t)))
|#

;; Soundex:
;;
;; 1. Retain the first letter of the name and drop all other occurrences
;;    of a, e, i, o, u, y, h, w.
;;
;; 2. Replace consonants with digits as follows (after the first letter):
;;      b, f, p, v → 1
;;      c, g, j, k, q, s, x, z → 2
;;      d, t → 3
;;      l → 4
;;      m, n → 5
;;      r → 6
;;
;; 3. If two or more letters with the same number are adjacent in the original
;;    name (before step 1), only retain the first letter; also two letters with
;;    the same number separated by 'h' or 'w' are coded as a single number,
;;    whereas such letters separated by a vowel are coded twice. This rule also
;;    applies to the first letter.
;;
;; 4. Iterate the previous step until you have one letter and three
;;    numbers. If you have too few letters in your word that you can't assign
;;    three numbers, append with zeros until there are three numbers. If you
;;    have more than 3 letters, just retain the first 3 numbers.

(defparameter *soundex* #("bfpv"	; = 1
			  "cgjkqsxz"	; = 2
			  "dt"		; = 3
			  "l"		; = 4
			  "mn"		; = 5
			  "r")		; = 6
  "Soundex table to translate from consonants to numbers.")

(defun soundex-to-number (in)
  (let ((result (copy-seq (subseq in 1))))
    (loop :for tt :across *soundex* :and i = 1 :then (1+ i) :do
       (setf result
	     (nsubstitute-if (char (format nil "~d" i) 0)
			     #'(lambda (c) (position c tt))
			     result)))
    (s+ (subseq in 0 1) result)))

(defun soundex-remove-doubles (in)
  (let ((result
	 (loop :with last = nil
	    :for c :across in :and i = 0 :then (1+ i)
	    :if (not (and last (char= c last)))
	      :collect c
	    :if (not (or (char-equal c #\h) (char-equal c #\w)))
	      :do (setf last c))))
    (setf result (make-array (length result)
			     :element-type 'character
			     :initial-contents result))))

(defun soundex-remove-vowels (in)
  (s+ (subseq in 0 1)
      (remove-if
       #'(lambda (c)
	   (position c "aeiouyhw" :test #'char-equal))
       (subseq in 1))))

(defun soundex-pad-or-truncate (in)
  (cond
    ((< (length in) 4)
     (with-output-to-string (str)
       (princ in str)
       (dotimes (n (- 4 (length in)))
	 (princ #\0 str))))
    ((> (length in) 4)
     (subseq in 0 4))
    (t
     in)))

(defun soundex (in)
  (if (or (not in) (= 0 (length in)))
      in
      (string-upcase
       (soundex-pad-or-truncate
	(soundex-to-number
	 (soundex-remove-vowels
	  (soundex-remove-doubles in)))))))

(defun soundex-verify (w)
  (let* ((cmd (s+ "perl -e \"use Text::Soundex; print soundex('" w "');\""))
	 (w1 (funcall (intern "COMMAND-OUTPUT-WORDS" :lish) cmd))
	 (w2 (soundex w)))
;    (format t "cmd = ~a~%" cmd)
    (when (not (equal w1 w2))
      (format t "soundex fail ~w perl:'~w' lisp:'~w'~%" w w1 w2))))

(defun test-soundex (&optional (file "/usr/share/dict/words"))
  (with-open-file (stm file)
    (loop :with w
       :while (setf w (read-line stm nil nil))
       :do (soundex-verify w))))

;; I'm not totally convinced that this should be here.
(defun abspath (path)
  "Turn the PATH into an absolute path."
  ;; Make sure path is a string.
  (setf path (etypecase path
	       (null (return-from abspath nil))
	       (string path)
	       (pathname (namestring path))))
    (let* ((p (if (char= #\/ (char path 0))
		 path			; already absolute
		 (concatenate 'string (current-directory) "/" path)))
	   (pp (split-sequence #\/ p :omit-empty t)))
      (macrolet
	  ((get-rid-of (str snip)
	     "Get rid of occurances of STR by snipping back to SNIP, which
              is a numerical expression in terms of the current position POS."
	     `(loop :with start = 0 :and pos
		 :while (setq pos (position ,str pp
					    :start start :test #'equal))
		 :do (setq pp (concatenate 'list
					   (subseq pp 0 (max 0 ,snip))
					   (subseq pp (1+ pos)))))))
	;; Get rid of ".."
;	(dbug "starting with ~s~%" pp)
	(get-rid-of "." pos)
;	(dbug "after . ~s~%" pp)
	(get-rid-of ".." (1- pos))
;	(dbug "after .. ~s~%" pp)
	)
      (if (zerop (length pp))
	  "/"
	  (apply #'concatenate 'string
		 (loop :for e :in pp :collect "/" :collect e)))))

(defun clip-path (path side)
  "Return the directory portion of a path."
  ;; Go backwards from the end until we hit a separator.
  (let ((i (1- (length path))))
    (loop :while (and (>= i 0) (char/= #\/ (char path i)))
       :do (decf i))
    (if (eq side :dir)
	(if (< i 0)
	    (subseq path 0 0)
	    (subseq path 0 i))
	(if (< i 0)
	    path
	    (subseq path (1+ i))))))

(defun dirname (path)
  "Return the directory portion of a path."
  (clip-path path :dir))

(defun basename (path)
  "Return the last portion of a path."
 (clip-path path :file))

;; End
