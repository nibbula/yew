;;;
;;; dlib-misc.lisp - Library of miscellaneous useful functions.
;;;

;; See dlib.lisp for the most essential stuff.
;; This is for things that are nice, but not essential.
;; Things for mostly interactive use are in dlib-interactive.
;;
;; This is mostly separate from dlib so I can depend on other stuff like OPSYS,
;; and to keep dlib minimal.
;;
;; I don't like the name “misc”. Let’s think of something better.

(defpackage :dlib-misc
  (:use :cl :dlib :opsys :char-util :glob :collections :stretchy :dtime
	#+use-re :re #-use-re :ppcre)
  ;; Also has an inplicit dependency on ASDF.
  (:documentation "More generally dubious miscellaneous functions.")
  (:export
   ;; general
   #:randomize-vector
   #:parse-integer-with-radix
   #:parse-bit-rate
   #:tree-ify
   #:un-tree-ify
   #:oquote-format
   #:group-by-alist
   #:group-by-hash
   #:group-by
   #:frequencies
   #:partition
   #:*default-ellipsis*
   #:shrink-pathname
   #:scan-over-string

   ;; hooks
   #:add-hook
   #:remove-hook
   #:run-hooks

   ;; printing
   #:untabify
   #:justify-text
   #:calculate-line-endings
   #:print-values
   #:print-values*
   #:print-values-of
   #:print-columns-sizer
   #:print-columns-array
   #:print-columns
   #:print-size
   #:center

   ;; spin
   #:*default-spin-string*
   #:*spin-strings*
   #:*plain-spin-string*
   #:*unicode-disk-spin-string*
   #:*unicode-scan-spin-string*
   #:*unicode-digit-spin-string*
   #:*unicode-sparkle-spin-string*
   #:*unicode-braille-spin-string*
   #:*unicode-square-spin-string*
   #:*emoji-spin-string*
   #:*emoji-moon-spin-string*
   #:spin
   #:with-spin

   ;; systems and packages 
   #:*loadable-systems*
   #:add-asdf-directory
   #:loadable-systems
   #:loadable-system-p
   #:*quickloadable-systems*
   #:quickloadable-systems
   #:clear-loadable-system-cache
   #:ensure-package
   #:unintern-conflicts
   #:d-autoload
   #:system-depends-list
   #:all-system-dependencies
   #:system-pathnames

   ;; I/O
   #:safe-file-length
   #:slurp
   #:slurp-binary
   #:byte-vector-8
   #:spit
   #:spit-append
   #:spit-binary
   ;; #:file-contents
   #:write-lines
   #:confirm
   #:get-file-local-variables
   #:read-limited-line
   ))
(in-package :dlib-misc)

#+clisp
(defun load-again (module &rest other-args &key &allow-other-keys)
  "Load without complaining about constant redefinitions."
  (let ((custom:*suppress-check-redefinition* t))
    (apply 'load module other-args)))

;; Maybe this should be put somewhere else, since it's seldom used.
(defun randomize-vector (vector)
  "Randomize the order of elements in an vector. Returns the mutated vector."
  (when (not (vectorp vector))
    (error "Argument must be a vector, not a ~a." (type-of vector)))
  (loop :with len = (length vector)
	:for i :from (1- len) :downto 1
	:do (rotatef (aref vector i) (aref vector (random len))))
  vector)

;; @@@ This should probably support the same args as PARSE-INTEGER.
(defun parse-integer-with-radix (str)
  "Parse an integer from a string, allowing for a Lisp radix prefix."
  (cond
    ((and str (> (length str) 2)
	  (char= (char str 0) #\#))
     (cond
       ((position (char str 1) #(#\b #\o #\x) :test #'char-equal)
	(parse-integer str :junk-allowed nil :start 2
		       :radix (cond
				((char-equal (char str 1) #\b) 2)
				((char-equal (char str 1) #\o) 8)
				((char-equal (char str 1) #\x) 16)
				(t 10))))
       ((digit-char-p (char str 1))
	(let (radix start)
	  (cond
	    ((char-equal #\r (char str 2))
	     (setf radix (parse-integer str :start 1 :end 2 :junk-allowed nil)
		   start 3))
	    ((and (digit-char-p (char str 2))
		  (char-equal #\r (char str 3)))
	     (setf radix (parse-integer str :start 1 :end 3 :junk-allowed nil)
		   start 4))
	    (t
	     (error "Malformed radix in integer ~a." str)))
	  (parse-integer str :junk-allowed nil :start start :radix radix)))
       (t
	(error "Malformed integer ~a." str))))
    (t
     (parse-integer str :junk-allowed nil))))

(defparameter *bit-suffixes*
  #("b" "bps" "ps" "b/s" "/s"))

(defparameter *byte-suffixes*
  #("b" "bps" "ps" "b/s" "/s" "B" "Bps" "BPS" "PS" "B/s" "B/S" "/S"))

(defparameter *bit-units*
  `((#\k :bits  1024)
    (#\K :bytes #.(* 1024 8))
    (#\m :bits  #.(* 1024 1024))
    (#\M :bytes #.(* 1024 1024 8))
    (#\g :bits  #.(* 1024 1024 1024))
    (#\G :bytes #.(* 1024 1024 1024 8))))

(defun parse-bit-rate (string)
  "Return a bit rate based on STRING. Many variant spellings are allowed:

  bits:  k, kb, kbps, kps, kb/s, k/s;
  bytes: K, Kb, Kbps, Kps, Kb/s, K/s,
            KB, KBps, KBPS, KPS, KB/s, KB/S, K/S."
  (with-input-from-string (stream string)
    (let (number c rest unit)
      (labels ((number-char-p (c)
		 (or (digit-char-p c)
		     (find c #(#\. #\/ #\- #\+ #\d #\f #\e) :test #'equal)))
	       (get-potential-number ()
		 (let ((num-str
			(loop :do (setf c (read-char stream nil))
			   :while (and c (number-char-p c))
			   :collect c)))
		   (unread-char c stream)
		   (safe-read-from-string (coerce num-str 'string))))
	       (eat-whitespace ()
		 (loop :do (setf c (read-char stream nil))
		    :while (and c (whitespace-p c)))
		 (when c (unread-char c stream)))
	       (get-rest ()
		 (when (not rest)
		   (setf rest (read-line stream nil))))
	       (suffix-p (type)
		 (let (pos)
		   (eat-whitespace)
		   (get-rest)
		   ;; (format t "rest = ~s~%" rest)
		   (and rest
			(some (_ (and (setf pos (search _ rest))
				      (zerop pos)))
			      (ecase type
				(:bits *bit-suffixes*)
				(:bytes *byte-suffixes*)))))))
	(setf number (get-potential-number))
	(when (not (numberp number))
	  (error "Bit rate doesn't start with a number."))
	(eat-whitespace)
	(setf c (read-char stream nil))
	(cond
	  ((and (setf unit (find c *bit-units* :key #'first))
		(suffix-p (second unit)))
	   (* number (third unit)))
	  ((suffix-p :bits) number)
	  ((suffix-p :bytes) (/ number 8))
	  (t ;; no units or suffix, assume k bits
	   (* number 1024)))))))

(defun group-by-alist (function sequence &key test)
  "Return an alist of lists the items of SEQUENCE, grouped by the results of
calling FUNCTION on them. Equality is test by TEST, as usual."
  (when (not test)
    (setf test #'equal))
  (let (result item key)
    (omapn (lambda (x)
	     (if (setf item (assoc (setf key (funcall function x))
				   result :test test))
		 (push x (cdr item))
		 (setf result (acons key (list x) result))))
	   sequence)
    result))

(defparameter *empty-hash* (gensym "gabba"))

(defun group-by-hash (function sequence &key test)
  "Return a hash table of lists the items of SEQUENCE, grouped by the results of
calling FUNCTION on them. Equality is test by TEST, as usual."
  (when (not test)
    (setf test #'equal))
  (let ((result (make-hash-table :test test)))
    (omapn (lambda (x)
	     (push x (gethash (funcall function x) result)))
	   sequence)
    result))

(defun group-by (result-type function sequence &key test)
  "Return a RESULT-TYPE of the items of SEQUENCE, grouped by the results of
calling FUNCTION on them. RESULT-TYPE can be :HASH or :ALIST. Equality is test
by TEST, as usual. This is just wrapper around the group-by-hash and
group-by-alist functions."
  (case (keywordify result-type)	; sorry, more junk keywords
    (:hash (group-by-hash function sequence :test test))
    ((or :list :alist) (group-by-alist function sequence :test test))))

(defun frequencies (sequence &key (test #'eql) table func)
  "Return a hash table with the counts of occurrences the elements of ‘sequence’.
‘test’ is the hash table test function to use, which defaults to #'eql.
If ‘table’ is given, use it as the starting table and update it's counts.
‘func’ is a one argument function which takes the existing count and returns
an updated count. If ‘func’ is NIL it defaults to 1+."
  (when (and table (not (eq (coerce (hash-table-test table) 'function)
			    (coerce test 'function))))
    (error "The test function argument ~s doesn't match the test in the table ~
            argument ~s." test (hash-table-test table)))
  (let ((result-table (or table (make-hash-table :test test))))
    ;; @@@ One would hope the compiler could do this optimization.
    (if func
	;; I also hope most compilers don't have to do the hash twice?
	(omapn (_ (setf (gethash _ result-table)
			(funcall func (gethash _ result-table 0))))
	       sequence)
	(omapn (_ (incf (gethash _ result-table 0))) sequence))
    result-table))

;; @@@ We could make a generic one that returns a sequence of the same type,
;; but it's probably not as quick and do we need it?
(defun partition (by sequence)
  "Return two lists, the first containing items of ‘sequence’ for which the
function ‘by’ returns true, the second for which it returns false. The order of
elements in the result sequence is unspecified, but this currently non-parallel
version probably retains the order. See also: ‘group-by’."
  (let (a b)
    (with-collecting-into* (a b)
      (omapn (_ (if (funcall by _) (collect-a _) (collect-b _))) sequence))))

(defvar *default-ellipsis*
  (string (code-char #x2026)) ; #\HORIZONTAL_ELLIPSIS
  "A string to indicate something was elided.")

(defun shrink-pathname (path &key (to 70) (ellipsis *default-ellipsis*)
			       abbreviate)
  "Make a path name fit in the given width, shrinking in a way to preserve
useful information. 
  ‘to’           The limit on the length.
  ‘ellipsis’     A string to use to indicate omission, which defaults to
                 ‘*default-ellipsis*.’
  ‘abbreviate’   True to allow abbreviating leading directories to one letter."
  (declare (type string ellipsis) (type fixnum to))
  (let* ((str (safe-namestring (quote-filename path)))
	 (len (length str)))
    (declare (type string str))
    (if (> len to)
	(if abbreviate
	    (let (result (result-len len))
	      (loop :for p :on (nos:split-path path) :do
		 ;; Stop a the first abbreviation that puts us under the limit.
		 (if (< (- result-len (1- (length (car p)))) to)
		     (progn
		       ;; Add the rest of the non-abbreviated elements.
		       (setf result (append (reverse result)
					    (list (subseq (car p) 0 1))
					    (cdr p)))
		       (return t))
		     (progn
		       ;; Add the abbreviated element.
		       (push (subseq (car p) 0 1) result)
		       ;; Subtract the piece we removed from to total length.
		       (decf result-len (1- (length (car p)))))))
	      (if (equal (car result) nos:*directory-separator-string*)
		  (progn
		    ;; Don't double add the first /
		    (pop result)
		    (s+ nos:*directory-separator*
			(join-by-string result nos:*directory-separator*)))
		  (join-by-string result nos:*directory-separator*)))
	    ;; Replace excess by an ellipsis in the center
	    (let* ((ellipsis-length (length ellipsis))
		   (half (- (truncate to 2) ellipsis-length)))
	      (declare (type fixnum ellipsis-length half))
	      (s+ (subseq str 0 half) ellipsis
		  (subseq str (- len (+ half ellipsis-length 1))))))
	str)))

;; @@@ Perhaps this could be merged with the one in RL?
(defun scan-over-string (string pos direction &key function not-in)
  "If FUNCTION is provied move over characters for which FUNCTION is true.
If NOT-IN is provied move over characters for which are not in it.
DIRECTION is :forward or :backward. Moves over characters in STRING starting
at POS. Returns the new position after moving."
  (when (and (not function) not-in)
    (setf function #'(lambda (c) (not (oposition c not-in)))))
  (if (eql direction :backward)
      ;; backward
      (loop :while (and (> pos 0)
			(funcall function (oelt string (1- pos))))
	 :do (decf pos))
      ;; forward
      (let ((len (olength string)))
	(loop :while (and (< pos len)
			  (funcall function (oelt string pos)))
	   :do (incf pos))))
  pos)

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Hooks - a simple, old-fashioned convention.
 ╰|#

(defmacro add-hook (var func)
  "Add a hook function FUNC to the hook variable VAR."
  `(pushnew ,func ,var))

(defmacro remove-hook (var func)
  "Remove hook function FUNC from the hook variable VAR."
  `(setf ,var (delete ,func ,var)))

(defun run-hooks (var &rest args)
  "Apply ‘args’ to the hooks in ‘var’. ‘var’ can be a single function designator
or a list of function designators."
  (if (listp var)
      (loop :for f :in var
	    :do (apply f args))
      (apply var args)))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Printing
 ╰|#

(defun untabify (line &optional (col 0))
  "Return a new string with all tabs in the ‘line’ to the appropriate number
of spaces, preserving columns. Assumes that ‘line’ is starting in column ‘col’,
which defaults to zero."
  (with-output-to-string (str)
    (loop
       :for c :across line
       :do
       (if (eql c #\tab)
           (loop :for i :from 0 :below (- 8 (rem col 8))
              :do
              (write-char #\space str)
              (incf col))
           (progn
             (write-char c str)
             (incf col))))))

;; This can handle some wide and fatchars, but it only gives reasonable output
;; for the the subset of unicode scripts that have a non-context-dependant
;; word separator character (a.k.a. western style). This is certainly not an
;; implementation of the unicode line breaking algorithm. Perhaps someday we
;; can make it work in a more universal text rendering system, but it would be
;; nice to at least have it support anything that we can reasonably render in
;; a character grid system (i.e. terminals).
(defun %justify-text (text &key (cols 80) (stream *standard-output*)
			     (prefix "") (separator #\space) omit-first-prefix
			     (start-column 0))
  (declare (type fixnum cols))
  (let ((len             (olength text))
	(i               0)
	(last-word-start 0)
	(first-word      t)
	(column          start-column)
	(sep-len	 (display-length separator))
	(prefix-len      (display-length prefix)))
    (declare (type fixnum len i last-word-start column))
    (flet ((write-word ()
	     (if (< (+ column (if first-word 0 sep-len)) cols)
		 (progn
		   (when (not first-word)
		     (princ separator stream)
		     (incf column sep-len))
		   (setf first-word nil)
		   (princ (osubseq text last-word-start (min i len)) stream))
		 (progn
		   (princ #\newline stream)
		   (if prefix
		       (progn
			 (princ prefix stream)
			 (setf column prefix-len))
		       (setf column 0))
		   (princ (osubseq text last-word-start i) stream)
		   (incf column (+ (display-length
				    (osubseq text last-word-start i))))))))
      (when (and prefix (not omit-first-prefix))
	(princ prefix stream)
	(setf column (+ prefix-len start-column)))
      (loop
	 :while (< i len)
	 :do
	 (cond
	   ((char= (simplify-char (oelt text i)) #\Newline)
	    (write-word)
	    (write-char #\newline stream)
	    (if prefix
		(progn
		  (princ prefix stream)
		  (setf column prefix-len))
		(setf column 0))
	    (setf first-word t)
	    (incf i)
	    (setf last-word-start i))
	   ((char= (simplify-char (oelt text i)) separator)
	    (write-word)
	    (incf i)
	    ;; eat multiple spaces
	    (loop :while (and (< i len)
			      (char= (simplify-char (oelt text i)) separator))
	       :do (incf i))
	    (setf last-word-start i))
	   (t
	    (incf column (display-length (oelt text i)))
	    (incf i))))
      (when (< last-word-start i)
	(write-word)))))

;; This is only broken into two parts for the convenience of output to a
;; string when ‘stream’ is nil.
(defun justify-text (text &key (cols 80) (stream *standard-output*)
			    (prefix "") (separator #\space) omit-first-prefix
			    (start-column 0))
  "Try to output substrings of TEXT to STREAM, separated by SEPARATOR, so that
they fit in COLS columns of fixed sized characters. Output PREFIX before each
line. If OMIT-FIRST-PREFIX is true, don't output the prefix on the first line.
If STREAM is nil, return a string of the output."
  (if stream
      (%justify-text text :cols cols :stream stream
		     :prefix prefix :separator separator
		     :omit-first-prefix omit-first-prefix
		     :start-column start-column)
      (with-output-to-string (output)
	(%justify-text text :cols cols :stream output
		       :prefix prefix :separator separator
		       :omit-first-prefix omit-first-prefix
		       :start-column start-column))))

;; @@@ Perhaps we could consider caching or memoizing this? Espeically when
;; the buffer hasn't changed.
;; @@@ I'm not sure this is the right place for this.

(defun calculate-line-endings (buffer start-column end-column spots
				column-spots autowrap-delay)
  "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer.
  buffer          The string to compute endings for.
  start-column    The column number of the first character in ‘buffer’.
  end-column      The number columns in the view, after which it wraps.
  spots           An alist of character indexes to set the line and column of.
  column-spots    An alist of line and column pairs to set the character
                  indexes of.
  autowrap-delay  True if the intended output device has autowrap delay."
  (let (endings
	(col start-column)		; Start after the prompt
	(char-width 0)
	(last-col start-column)
	(line 0)
	(i 0)
	c cc spot)
    ;; (dbugf :rl "endings start-column ~s~%" start-column)
    ;; (if-dbugf (:rl)
    ;; 	      (symbol-call :deblarg :debugger-wacktrace 20))
    (flet ((past-edge ()
	     "Return true when we're past the right edge. If end-column is NIL,
              we never get past it."
	     (and end-column (> (+ col char-width) end-column)))
	   (set-spot (x)
	     (declare (ignore x))
	     (when (and spots (setf spot (assoc i spots)))
	       ;;(dbugf :rl "set-spot ~a~%" x)
	       (rplacd spot (cons line col)))
	     (when (and column-spots
			(setf spot (assoc `(,line ,col) column-spots
					  :test #'equal)))
	       (rplacd spot i))))
      (loop :while (< i (olength buffer))
	 :do
	   (setf c (oelt buffer i)
		 ;; cc (if (fatchar-p c) (fatchar-c c) c))
		 cc (simplify-char c))
	   (if (and (characterp cc) (char= cc #\newline))
	       (progn
		 (when (and (not autowrap-delay) (past-edge))
		   (push (cons (1- i) last-col) endings)
		   (setf last-col col)
		   (setf col 0) ;; @@@ left-margin
		   (incf line)
		   (set-spot "newline wrap"))
		 (push (cons (1- i) last-col) endings)
		 (setf last-col col)
		 ;; (when (< col (1- end-column))
		 ;;   (incf col))		; just for the cursor?
		 (set-spot "NL")
		 (incf line)
		 (setf col 0) ;; @@@ left-margin
		 ;;(set-spot "NL")
		 )
	       (progn
		 (setf char-width (display-length (oelt buffer i)))
		 (if (past-edge)
		     (progn
		       (push (cons (1- i) last-col) endings)
		       (when autowrap-delay
			 (set-spot "wrap"))
		       (setf last-col col)
		       (setf col 0)
		       (incf line)
		       (when (not autowrap-delay)
			 (set-spot "wrap"))
		       (setf col char-width))
		     (progn
		       (set-spot "normal")
		       (setf last-col col)
		       (incf col char-width)))))
	   (incf i))

      ;; Make sure we get the last one
      (when (past-edge)
	(push (cons (1- i) last-col) endings))

      ;; Spot in empty buffer
      (when (and spots (zerop (olength buffer)) (setf spot (assoc 0 spots)))
	(rplacd spot (cons line col)))

      ;; Spot after end
      (when (past-edge)
	(incf line)
	(setf col 0)) ;; @@@ left-margin
      (set-spot "End")
      endings)))

(defun print-values (value-list &optional (stream t))
  "Print a vertical list of values. VALUE-LIST is a list of symbols whose
values are printed. Symbols in the VALUE-LIST must either be dynamic variables
or fbound to a function, which called with no arguments to get the value.
Use PRINT-VALUES* if you want to print lexical variables."
  (let ((max-len (loop :for f :in value-list
		       :maximize (length (string f)))))
    (loop :for f :in value-list :do
       (format stream "~va  : ~s~%"
	       max-len (string-capitalize f)
	       (if (fboundp f)
		   (apply f nil)
		   (symbol-value f))))))

;; This can do everything the unstarred version can do, but it causes
;; potential over-abundant code generation, which is why I'm keeping both
;; versions.
(defmacro print-values* (value-list &optional (stream t))
  "Print a vertical list of values. VALUE-LIST is an unquoted list of symbols
whose values are printed. If the symbol is FBOUND to a function it is called
with no arguments to get the value. Unlike PRINT-VALUES, this can print
lexical variables."
  (let* ((max-len (loop :for f :in value-list
		     :maximize (length (string f))))
	 (spudgers
	  (loop :with snork
	     :for f :in value-list
	     :do (setf snork (if (fboundp f) `(apply ',f nil) f))
	     :collect
	     `(format ,stream "~va  : ~s~%" ,max-len (string-capitalize ',f)
		      ,snork))))
    `(progn ,@spudgers)))

(defun print-values-of (value-list object &key (stream t) prefix (error-p t)
					    (value-format "~S"))
  "Print a vertical list of results of applying functions to OBJECT.
VALUE-LIST is a list of symbols who are functions of one argument, which can
be OBJECT. PREFIX is optionally a prefix to remove from symbols in the value
list before printing. VALUE-FORMAT is a format string with which the value
is printed. This is useful for printing, e.g. slots of a structure or class."
  (let* ((fixed-list (if prefix
			 (mapcar (_ (remove-prefix (string _) (string prefix)))
				 value-list)
			 value-list))
	 (max-len (loop :for f :in fixed-list
		     :maximize (length (string f))))
	 (format-string (if value-format
			    (s+ "~va  : " value-format "~%")
			    "~va  : ~s~%")))
    (loop :for f :in value-list :do
       (format stream format-string
	       max-len (string-capitalize
			(if prefix
			    (remove-prefix (string f) (string prefix))
			    (string f)))
	       (if (fboundp f)
		   (if error-p
		       (apply f (list object))
		       (ignore-errors (apply f (list object))))
		   (symbol-value f))))))

(defconstant +inter-space+ 2
  "Minimum spaces between columns for ‘print-columns’.")

(defun format-length (object format-char)
  "Return the length in characters to print OBJECT with FORMAT-CHAR."
  (cond
    ;; ((and (stringp object) (eql format-char #\a))
    ((and (typep object 'standard-class)
	  (find-method #'display-length nil
		       (list (find-class (type-of object)))))
     (display-length object))
    (t
     (display-length (format nil (s+ "~" format-char) object)))))

(defun smush-output (list cols height format-char stream row-limit screen-width)
  "Output LIST to STREAM in column major COLS and HEIGHT lines, limited to
ROW-LIMIT. Items are printed with FORMAT-CHAR."
  (loop
     :with len = (length list)
     :and format-str      = (format nil "~~v~a" format-char)
     :and format-str-last = (format nil "~~~a" format-char)
     :and a = (make-array (length list) :initial-contents list)
     :and limit = (if row-limit (min height row-limit) height)
     :and last-col = (1- (length cols))
     :and x
     :for i :from 0 :below limit :do
     (setf x 0)
     (loop
	:with n
	:for c :in cols
	:for j = 0 :then (1+ j)
	:do
	(setf n (+ (* j height) i))
	(when (< n len)
	  (if (= j last-col)
	      (progn
		(format stream format-str-last (aref a n))
		(incf x (format-length (aref a n) format-char)))
	      (progn
		(format stream format-str c (aref a n))
		(incf x c)))))
     (when (/= x screen-width)
       (terpri stream))))

(defun smush-columns (list cols rows lengths)
  "Return a list of the smallest column sizes for the putting the LIST in COLS
and ROWS. Return nil if the list can't fit. Second value is the extra space in
the last column, or the reason it didn't fit, either :TOO-NARROW or :TOO-WIDE."
  (let (col-list extra max-len (l list) (col 0) row)
    (loop :with i = 0
       :do
       (setf max-len 0 row 0)
       ;; Go down the column.
       (loop
	  :do
	  ;; Add space between cols except for the last row.
	  (setf max-len (max max-len
			     (+ ;(format-length (car l) format-char)
			      (aref lengths i)
			      (if (= col (1- cols)) 0 +inter-space+))))
	  (setf l (cdr l))
	  (incf i)
	  (incf row)
	  :while (and l (< row rows)))
       ;; Save the amount of blank space left in the last column.
       (when (and (not l) (< row rows))
	 (setf extra (- rows row)))
       (push max-len col-list)
       (incf col)
       :while (and l (< col cols)))
    (cond
      ((not (null l))			; not all items fit
       (values nil :too-narrow))
      ((and (< col (1- cols))		; didn't fill up all columns
	    (> rows 1))			; but multiple rows
       (values nil :too-wide))
      (t
       (values (nreverse col-list) extra)))))

;; @@@ This is linear, so we should be able to calculate the minimal
;; @@@ intersection of the rows and cols directly without iterating.
(defun print-columns-smush (list &key (columns 80) (stream *standard-output*)
				   (format-char #\a) prefix suffix
				   smush row-limit)
  (declare (ignore prefix suffix smush)) ;; @@@
  (when (not list)
    (return-from print-columns-smush 0))
  (let ((screen-width columns)
	;; (terminal:with-terminal (tty 'terminal-ansi:terminal-ansi)
	;;    (terminal:terminal-window-columns tty)))
	(len 0)
	(max-len 0)
	(min-len most-positive-fixnum)
	(area 0)
	;; max-area
	cols rows
	new-rows new-cols
	col-list
	last-col-list last-new-rows
	extra not-fit-count
	(cached-lengths (make-array (length list)
				    :element-type 'fixnum
				    :initial-element 0))
	)
    ;; Compute the length, maximum item length, and minimum area.
    (loop :with l
       :for i :in list
       :for n = 0 :then (1+ n)
       :do
       (setf l (+ (setf (aref cached-lengths n)
			(format-length i format-char)) +inter-space+)
	     max-len (max max-len l)
	     min-len (min min-len l))
       (incf area l)
       (incf len))
    ;; (setf max-area (* max-len len))
    ;; (dbug "List length:  ~d~%" len)
    ;; (dbug "Screen width:  ~d~%" screen-width)
    ;; (dbug "Minimum area: ~d ~d~%" area (ceiling area screen-width))
    ;; (dbug "Max item len: ~d~%" max-len)
    ;; (dbug "Min item len: ~d~%" min-len)
    ;; (dbug "Maxium area : ~d ~d~%" max-area
    ;; 	    (floor max-area screen-width))

    (setf cols (max 1 (floor screen-width max-len))
	  rows (ceiling len cols)
	  new-rows rows
	  new-cols cols
	  col-list (make-list cols :initial-element max-len)
	  last-col-list col-list
	  last-new-rows new-rows
	  not-fit-count 0)

    (cond
      ((< (- area +inter-space+) columns)
       ;; It can fit on one line.
       (format stream (s+ "~{~" format-char "~^"
                          (format nil "~v,1@t" +inter-space+)
                          "~}~%") list)
       (setf last-new-rows 1))
      (t
	(loop
	   :while (and (< not-fit-count 4)
		       (<= (apply #'+ col-list)
			   #| (- screen-width min-len) |#
			   screen-width)
		       #| (and extra (> extra 2)) |#
		       (> new-rows 1))
	   :do
	   (if col-list
	       ;; save the previous results
	       (setf last-col-list col-list
		     last-new-rows new-rows
		     not-fit-count 0)
	       (incf not-fit-count))

	   #|
	   (if (and extra #| (>= extra (1- new-cols)) |# (> extra 1))
	       ;;(decf new-rows #| (max (truncate extra 2) 1) |# )
	       (if (= not-fit-count 0)
		   (incf new-cols)
		   (decf new-rows)))
	   |#

	   (cond
	     ((eq extra :too-narrow)
	      (incf new-cols))
	     ((eq extra :too-wide)
	      (decf new-rows))
	     (t
	      (decf new-rows)))
	   
	   (multiple-value-setq (col-list extra)
	     (smush-columns list new-cols new-rows cached-lengths))
	   ;; (dbug "Squish: ~d x ~d (~d)~25t~a~%"
	   ;; 	 new-cols new-rows extra col-list)
	   )
        (smush-output list last-col-list last-new-rows format-char stream
		  row-limit screen-width)))

    ;; Output the last good setup
    ;; (dbug "Squish Final: ~d x ~d (~d ~d)~25t~a~%"
    ;; 	  (length last-col-list) last-new-rows
    ;; 	  extra not-fit-count last-col-list)
    last-new-rows))

(defun print-columns-sizer (list &key (columns 80) (stream *standard-output*)
			     (format-char #\a) prefix suffix smush row-limit)
  "Return how many rows it might take to print list. Also returns the number of
columns and the maximum width of a column."
  (declare (ignore stream smush row-limit))
  (let* ((len (length list))
	 (format-str (format nil "~~~a" format-char))
	 (max-len
	  ;; This, although terse, may be inefficient w/big lists (@@@ test!):
	  (loop :with m = 0
	     :for c :in list :do
	     (setf m (max m (length (format nil format-str c))))
	     :finally (return (1+ m))))
	 (width (- #| (1- columns) |# columns
		   (if prefix (length prefix) 0)
		   (if suffix (length suffix) 0)))
	 (ccc   (floor width max-len))
	 (cols  (if (zerop ccc) 1 ccc))
	 (rows  (ceiling len cols))) ;; cols can't be zero here
    (when (> max-len width) (setf max-len width))
    (values rows cols max-len)))

(defun print-columns-array (list rows cols)
  "Return an array with the size given by ROWS and COLS, filled in with items
from LIST in column-major order."
  (let ((a (make-array `(,cols ,rows) :initial-element nil)))
    (loop :with col = 0 :and row = 0
       :for c :in list
       :do
       (setf (aref a col row) c)
       (incf row)
       (when (>= row rows)
	 (incf col)
	 (setf row 0)))
    a))

;; @@@ consider:
;; (defun print-columns-output (array rows cols max-len
;; 			     &key prefix suffix)
;;   )

(defun print-columns (list &rest keys
		      &key (columns 80) (stream *standard-output*)
			(format-char #\a) prefix suffix smush row-limit)
  "Print the LIST on STREAM with as many columns as will fit in COLUMNS fixed
width character cells. Items are sorted down the columns, then across. Return
the number of rows.
FORMAT-CHAR is used to print the items, as with FORMAT.
PREFIX is a string to prepend to each row.
SUFFIX is a string to append to each row."
  (declare (ignorable columns))
  (when smush
    (return-from print-columns (apply #'print-columns-smush list keys)))
  (multiple-value-bind (rows cols max-len)
      (apply #'print-columns-sizer list keys)
    (let* ((format-str (format nil "~~v~a" format-char))
	   (a (print-columns-array list rows cols))
	   (limit (if row-limit (min rows row-limit) rows)))
      ;; output the array
      (loop :for r :from 0 :below limit
	 :do
	 (when prefix
	   (write-string prefix stream))
	 (loop :for c :from 0 :below cols
	    :do
	    (if (aref a c r)
		(progn
		  ;; (write-string ;; @@@ Why the superfluous write-string?
		  ;;  (format nil format-str max-len (aref a c r))
		  ;;  stream))
		  (format stream format-str max-len (aref a c r)))
		;; If we have a suffix, fill out blank space
		(if suffix
		    ;; (write-string ;; @@@ Why the superfluous write-string?
		    ;;  (format nil "~va" max-len #\space)
		    ;;  stream))))
		    (format stream "~va" max-len #\space))))
	 (when suffix
	   (write-string suffix stream))
	 (terpri stream)))
    rows))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +size-names+ 11
    "Number of sizes in the size name arrays."))

(define-constant +binary-size-prefixes+
  #(nil "kibi" "mebi" "gibi" "tebi" "pebi" "exbi" "zebi" "yobi" "ronni"
    "quetti" "buttload")
  "Standard prefixes for sizes in bits."
  'vector-equal)

(define-constant +traditional-size-prefixes+
  #(nil "kilo" "mega" "giga" "tera" "peta" "exa" "zetta" "yotta" "ronna"
    "quetta" "buttload")
  "Traditional prefixes for sizes in bits."
  'vector-equal)

(define-constant +binary-size-abbreviations+
  #(nil "Ki" "Mi" "Gi" "Ti" "Pi" "Ei" "Zi" "Yi" "Ri" "Qi" "**")
  "Abbreviations for standard sizes in bits."
  'vector-equal)

(define-constant +traditional-size-abbreviations+
  #(nil "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q" "*")
  "Traditional abbreviations for standard sizes in bits."
  'vector-equal)

(define-constant +binary-sizes+
  #.(make-array (1+ +size-names+) :initial-contents
	      (loop :for i :from 0 :to +size-names+ :collect (expt 1024 i)))
  "Binary bit size multipliers."
  'vector-equal)

(define-constant +decimal-sizes+
  #.(make-array (1+ +size-names+) :initial-contents
	      (loop :for i :from 0 :to +size-names+ :collect (expt 1000 i)))
  "Decimal bit size multipliers."
  'vector-equal)

(defun print-size (size &key (stream t) long unit abbrevs
			  (traditional t)
			  (binary t)
			  (format "~:[~3,1f~;~d~]~@[ ~a~]~@[~a~]"))
  "Print a size with standard binary units.
If LONG is true, print the long unit name.
If UNIT is supplied, it should be a string of the unit to be prefixed.
UNIT defaults to ‘B’ or “byte” depending on LONG.
ABBREVS is a custom list of size abbreviations.
If TRADITIONAL is true, use traditional units for binary units, e.g.
kilobyte instead of the kibibyte.
If BINARY is true (the default), use powers of two sizes, otherwise use powers
of ten sizes. If BINARY is false, TRADITIONAL defaults to true.
FORMAT is the format to print the number with, which gets passed 4 values:
  1 - a boolen indicating if the number is an integer or not
  2 - the number
  3 - the prefix
  3 - the unit
FORMAT defaults to \"~:[~3,1f~;~d~]~@[ ~a~]~@[~a~]\""
  (setf unit (or unit (or (and long "byte") "B")))
  (let ((prefixes (if traditional
		      +traditional-size-prefixes+
		      +binary-size-prefixes+))
	(sizes (if binary +binary-sizes+ +decimal-sizes+)))
    (setf abbrevs (or abbrevs
		      (if traditional +traditional-size-abbreviations+
			  +binary-size-abbreviations+)))
    (flet ((pr (i)
	     (let* ((divisor (svref sizes i))
		    (n (/ size divisor))
		    (rem (rem size divisor)))
	       (format stream format
		       (zerop rem) n
		       (svref (if long
				  prefixes
				  abbrevs) i)
		       (when (svref abbrevs i) unit)))))
      (loop :for i :from 0 :below +size-names+
	 :do
	 (when (< size (svref sizes (1+ i)))
	   (return-from print-size (pr i))))
      (pr +size-names+))))

(defun center (object width &key (pad-char #\space))
  "Return a string with ‘object’ surrounded by approximately equal amounts of
‘pad-char’ on either side to fill up ‘width’ characters. ‘pad-char’ defaults
to #\space. The object is converted to a string with princ-to-string. If the
printed object is larger than the ‘width’, the whole is string is returned.
This works for unicode characters that can occupy variable amounts of fixed
width character cells, with the exception that if the width of ‘pad-char’
isn't 1, the result will potentially not be exactly ‘width’ wide."
  (let ((pad-len (display-length pad-char)))
    (when (zerop pad-len)
      (error "pad-char can't be zero width."))
    (let* ((s (princ-to-string object))
	   (len (display-length s)))
      (cond
	((>= len width)
	 s)
	(t
	 (let* ((half (round width 2))
		(half-str (round len 2))
		(front-pad (- half half-str))
		(back-pad (- width front-pad len)))
	   (with-output-to-string (str)
	     (loop :for i :from 0 :below front-pad :by pad-len
		   :do (write-char pad-char str))
	     (write-string s str)
	     (loop :for i :from 0 :below back-pad :by pad-len
		   :do (write-char pad-char str)))))))))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Spin
 ╰|#

(defvar *spin* nil
  "Index into the spinner string.")

(defvar *spin-string* nil
  "The string of characters to animate.")

(defvar *spin-length* nil
  "The pre-calculated length of the spin string.")

(defvar *spin-spun* nil
  "True if the spinner was spun at least once.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *spin-strings* nil
    "List of spin strings.")

  (defmacro defspin (name string doc)
    `(progn
       (defparameter ,name ,string ,doc)
       (push (list ',name ,string) dlib-misc:*spin-strings*)))
  
  (defspin *plain-spin-string* "|/-\\"
    "Simple ASCII baton.")

  (defspin *unicode-disk-spin-string* "◒◐◓◑"
    "Spin string using common unicode characters.")

  (defspin *unicode-scan-spin-string* "▏▎▍▌▋▊▉█"
    "Spin string using common unicode characters.")

  (defspin *unicode-digit-spin-string* "➊➋➌➍➎➏➐➑➒➓"
    "Spin string using common unicode characters.")

  (defspin *unicode-sparkle-spin-string* "❋❊❈❇❇··"
    "Spin string using common unicode characters.")

  (defspin *unicode-braille-spin-string* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    "Spin string using common unicode characters.")

  (defspin *unicode-square-spin-string* "◰◳◲◱"
    "Spin string using common unicode characters.")

  (defspin *emoji-spin-string* "🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚🕛"
    "Spin string with fancy emoji clock face characters.")

  (defspin *emoji-moon-spin-string* "🌕🌖🌗🌘🌑🌒🌓🌔"
    "Spin string with the emoji moon.")

  (defvar *default-spin-string* *plain-spin-string*
    "The default spin string.")
  )

(defun spin (&optional (stream *standard-output*))
  "Do one iteration of a spin animation."
  (when *spin*
    (let ((s (char *spin-string* *spin*)))
      (write-char s stream)
    (dotimes (i (display-length s))
      (write-char #\backspace stream))
    (finish-output stream)
    (incf *spin*)
    (setf *spin-spun* t)
    (when (>= *spin* *spin-length*)
      (setf *spin* 0)))))

(defun unspin (&optional (stream *standard-output*))
  "Hopefully remove the spinning character."
  (write-char #\space stream)
  (write-char #\backspace stream)
  (finish-output stream))

(defmacro with-spin ((&key spin-string)
		     &body body)
  "Evaluate the BODY with things set up so that you can call (SPIN)
repeatedly to get the next frame of the spinner animation displayed.
SPIN-STRING can be given and defaults to the value of *DEFAULT-SPIN-STRING*."
  `(let* ((*spin-string* ,(or spin-string
			      '(symbol-value '*default-spin-string*)))
	  (*spin-length* (length *spin-string*))
	  (*spin* 0)
	  (*spin-spun* nil))
     (prog1 (progn ,@body)
       (when *spin-spun*
	 (unspin)))))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Packages and systems
 ╰|#

(defvar *loadable-systems* nil
  "Cached list of ASDF loadable packages. Set to NIL to recompute.")

(defvar *loadable-system-table* nil
  "Cached table of ASDF loadable packages. Set to NIL to recompute.")

(defun asdf-systems-in-directory (dir &key as-strings)
  "Return a list of systems in ‘dir’. If ‘as-strings’ is true, return them as
strings, otherwise as keywords."
  (loop :with base :and result
     :for f :in (glob (path-append dir "*.[Aa][Ss][Dd]"))
     :do (spin)
     :collect
     (progn
       (setf base (path-file-name f)
	     result (subseq base 0 (- (length base) 4)))
       (if as-strings
	   result
	   (keywordify result)))))

(defun add-asdf-directory (directory)
  "Add directory to the list of loadable systems and the cache if it exists."
  (when directory
    (let ((new-systems (asdf-systems-in-directory directory)))
      (when new-systems
	(setf *loadable-systems*
	      (append new-systems *loadable-systems*))
	(when *loadable-system-table*
	  (loop :for sys :in new-systems :do
	    (setf (gethash sys *loadable-system-table*) t)))))))

;; This is an horrible hack. I wish we could ask ASDF and Quicklisp.
(defun loadable-systems (&key as-strings)
  "List of potentially ASDF loadable systems."
  (labels ((place-dir (p)
	     "Resolve place into a directory."
	     (with-output-to-string (s)
	       (if (listp p)
		   (loop :for e :in p
		      :if (eq e :home)
		      :do (write-string
			   (namestring (user-homedir-pathname)) s)
		      :else
		      :do (write-string e s))
		   (write-string p s)))))
    (or (and *loadable-systems*
	     (or (and (and as-strings (every #'stringp *loadable-systems*))
		      *loadable-systems*)
		 (and (not as-strings) (every #'keywordp *loadable-systems*)
		      *loadable-systems*)))
	(setf *loadable-systems*
	      (with-spin ()
		(let ((s-dirs (loop :for e in asdf:*source-registry-parameter*
				 :if (and (listp e) (eq (car e) :directory))
				 :collect (place-dir (cadr e))))
		      (c-dirs (mapcar #'namestring asdf:*central-registry*)))
		  (append
		   (loop :for d :in (concatenate 'list s-dirs c-dirs)
		      :append
		      (asdf-systems-in-directory d :as-strings as-strings))
		   #+quicklisp
		   ;; Quicklisp
		   (loop :for d :in (ql-dist:all-dists)
		      :append
		      (loop :for s :in (ql-dist:installed-systems d)
			 :do (spin)
			 :collect
			 (if as-strings
			     (ql-dist:name s)
			     (keywordify (ql-dist:name s))))))))))))

(defvar *quickloadable-systems* nil
  "Cached list of Quickload loadable systems. Set to NIL to recompute.")

(defun quickloadable-systems (&key as-strings)
  "List of packages the quickload can maybe load, if it can download them."
  #-quicklisp (declare (ignore as-strings))
  (or *quickloadable-systems*
      #+quicklisp
      (setf *quickloadable-systems*
	    (with-spin ()
	      (loop :for d :in (ql-dist:all-dists)
		 :append
		 (loop :for s :in (ql-dist:provided-systems d)
		    :do (spin)
		    :collect
		    (if as-strings
			(string-downcase (ql-dist:name s))
			(keywordify (ql-dist:name s)))))))))

(defun ensure-loadable-systems-table ()
  "If it doesn't already exist, populate the *loadable-system-table* from the
loadable-systems function."
  (when (not *loadable-system-table*)
    (setf *loadable-system-table* (make-hash-table :test 'equal))
    (loop :for p :in (loadable-systems :as-strings t)
	  :do (setf (gethash p *loadable-system-table*) t))
    *loadable-system-table*))

(defun loadable-system-p (system-designator)
  "Return true if SYSTEM-DESIGNATOR denotes a loadable system."
  (ensure-loadable-systems-table)
  (gethash (string system-designator) *loadable-system-table*))

(defun clear-loadable-system-cache ()
  "This should be done whenever packages are added or removed or the search
configuration is changed."
  (setf *loadable-systems* nil
	*loadable-system-table* nil
	*quickloadable-systems* nil))

(defun ensure-package (package)
  "Try to load PACKAGE as an ASDF system if we can't find it as a package."
  (when (not (find-package package))
    (asdf:load-system package)))

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
	;; (format t "Uninterning ~a from ~a~%" old-sym package)
	))))

;; Simple mindless ASDF autoloader.
(defmacro d-autoload (symbol system doc-string &optional macro)
  "Define a function to load an ASDF system which contains a redefinition of
that function, and call it. In other words, define a stub function which
automatically loads the real function from a package and then calls it. The
actual package should be the same name as the ASDF package and define function
with the same name. If it's a macro, pass MACRO as true, mmkay?"
  (let ((doit
	 (if macro
	     `(eval (list* (intern (string ',symbol) ,system) args))
	     `(apply (intern (string ',symbol) ,system) args)))
	;;(symbol-string (string symbol))
	)
    (if (and (asdf:find-system system)
	     (find-package system)
	     (let ((sym (find-symbol (symbolify symbol) system)))
	       (and sym (fboundp sym))))
	`(progn
	   ;; Don't do anything if it's already loaded, except use it.
	   ;; (unintern-conflicts *package* ,system)
	   ;; (use-package ,system)
	   )
	`(,(if macro 'defmacro 'defun) ,symbol (&rest args)
	   ,doc-string
	   (asdf:load-system ,system)
	   (unintern-conflicts *package* ,system)
	   ;;(unintern (find-symbol ,symbol-string) ,*package*)
	   ;;(unintern ,symbol ,*package*)
	   ;;(unintern (make-symbol ,symbol-string) *package*)
	   (use-package ,system)
	   ;;(import (find-symbol ,symbol-string ,system))
	   (when (not (fboundp ',symbol))
	     (error "Autoload of ~a didn't define ~a." ,system ',symbol))
	   ,doit))))

(defun system-depends-list (system-name)
  "Return the depends-on list for SYSTEM-NAME."
  (typecase system-name
    (null nil)
    (list
     ;; Could be:?
     ;; | ( :feature FEATURE-EXPRESSION dependency-def )
     ;; | ( :version simple-component-name version-specifier )
     ;; | ( :require module-name )
     (let ((s (asdf/find-component::resolve-dependency-spec nil system-name)))
       (when s
	 (list (asdf:component-name s)))))
    ((or string symbol)
     (let ((sys (asdf:find-system system-name nil)))
       (when sys
	 (asdf:system-depends-on sys))))))

(defun all-system-dependencies (system &key sort-p names-p)
  "Return a list of systems of all recursive dependencies for the designanted
ASDF ‘system’. ‘system’ can be a list, in which case the dependencies of the
systems are merged without duplicates. If ‘sort-p’ is true sort the list by
name. If ‘names-p’ is true return system names instead of system objects."
  (let ((tab (make-hash-table :test #'equal))
        (results))
    (labels
	((do-deps (system dep-list)
           (loop
	     :with spec
	     :for dep in dep-list
	     :when (setf spec (asdf/find-component::resolve-dependency-spec
			       system dep))
	       :do (add-system spec)))
	 (add-dependencies (system)
	   (do-deps system (asdf:system-defsystem-depends-on system))
	   (do-deps system (asdf:system-depends-on system)))
         (add-system (system)
	   (let ((name (asdf:component-name system)))
           (when (not (gethash name tab))
             (setf (gethash name tab) t)
             (push system results)
	     (add-dependencies system)))))
      (typecase system
	(list
	 (dolist (s system)
	   (add-dependencies (asdf:find-system s))))
	((or symbol string asdf:system)
	 (add-dependencies (asdf:find-system system)))
	(t
	 (error "System is a not a known type: ~s" system)))
      (when names-p
	(setf results (mapcar #'asdf:component-name results)))
      (when sort-p
	(setf results (sort results #'string<
			    :key (if names-p
				     #'identity
				     #'asdf:component-name))))
      results)))

(defun system-pathnames (system)
  "Return a list of pathnames that are components of SYSTEM, including the
system definition file."
  (let ((sys (asdf/system:find-system system)))
    (append (mapcar (_ (asdf/component:component-pathname _))
		    (asdf/component:component-children sys))
	    (list (asdf:system-source-file sys)))))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ I/O
 ╰|#

#|
(defun read-text ()
  "A very simplistic text reader. Mostly useful if you just want to paste some
text into lisp and have it be stored as lines of words."
  (loop :with s
     :while (string/= "" (setf s (read-line)))
     :collect (split-sequence #(#\space #\tab) s :omit-empty t :by-group t)))
|#

(defun dprobe-file (literal-filename)
  "Like probe-filename, but treat strings as literal."
  (probe-file (if (stringp literal-filename)
		  (quote-filename literal-filename)
		  literal-filename)))

(defun safe-file-length (stream)
  "Like FILE-LENGTH, but don't error, just return NIL if we can't figure it out."
  (handler-case
      (file-length stream)
    (type-error (c)
      (declare (ignore c)))))

(defun unsigned-byte-8-p (x)
  "Type predicate for (unsigned-byte 8)."
  (typep x '(unsigned-byte 8)))

(deftype byte-vector-8 ()
  "A vector which every element is an (unsigned-byte 8)."
  '(or (vector (unsigned-byte 8))
       (vector (satisfies unsigned-byte-8-p))))

(defparameter *slurp-buffer-size* 4096)

;; This is kind of some duplication from stuff in lish/piping.lisp.
;;
;; I don't think this should ever a slurp a URL, because security?.
;; Anyway Drakma does it better.

(defun slurp (file &key (external-format :default)
			 element-type
			 count)
  "Return an array of ‘element-type’, with the contents read from ‘file’.
‘element-type’ defaults to character or the stream-element-type for streams.
‘external-format’ is as in ‘open’. If ‘count’ is non-nil, only read that many
elements."
  (let (stream (close-me nil) buffer pos len result)
    (macrolet ((copy-loop (outputer)
		 ;; For hopefully higher speed, two versions of the loop.
		 `(if count
		      (loop :do
			 (setf pos (read-sequence buffer stream))
			 (when (> pos 0)
			   ,outputer)
			 (decf count pos)
			 :while (and (= pos len) (> count 0)))
		      (loop :do
			 (setf pos (read-sequence buffer stream))
			 (when (> pos 0)
			   ,outputer)
			 :while (= pos len)))))
      (unwind-protect
	 (progn
	   (setf stream
		 (etypecase file
		   (null *standard-input*)
		   (stream file)	 ; already a stream
		   ((or string pathname) ; a file name
		    (setf close-me t)
		    (open file
			  :external-format external-format
			  :element-type (or element-type 'character))))
		 element-type (or element-type (stream-element-type stream)))
	   (if (and (typep stream 'file-stream)
		    ;; The length can be quite wrong, since it's probably in
		    ;; octets and we read it in characters. But hopefully it
		    ;; shouldn't be *less* than we need.
		    (progn
		      (setf len (safe-file-length stream))
		      (when (and len count)
			(setf len (min count len)))
		      len)
		    (not (zerop len)))
	       (progn
		 ;; Read the whole thing at once.
		 (setf buffer (make-array len :element-type element-type
					      :adjustable t))
		 (setf pos (read-sequence buffer stream))
		 (adjust-array buffer pos)
		 (setf result buffer))
	       (progn
		 ;; Read in chunks and write to a string or stretchy array.
		 (setf len *slurp-buffer-size*
		       buffer (make-array len :element-type element-type)
		       result
		       (if (and (subtypep (stream-element-type stream)
					  'character)
				(eq element-type 'character))
			   (progn
			     (with-output-to-string
				 (str nil :element-type element-type)
			       (copy-loop (write-sequence buffer str :end pos))))
			   (progn
			     (let ((str (make-array len
						    :adjustable t
						    :fill-pointer 0
						    :element-type element-type)))
			       (copy-loop (stretchy-append str buffer))
			       str))
			   )))))
      (when (and close-me stream)
	(close stream))))
    result))

(defun slurp-binary (file &key count)
  "Return an array of octets, with the contents read from FILE. If COUNT is
non-nil, only read that many elements. This just a shorthand for calling SLURP
with the appropriate ELEMENT-TYPE."
  (slurp file :element-type '(unsigned-byte 8) :count count))

(defun spit (file object)
  "Write OBJECT to FILE. If FILE is a file name, create it."
  (with-open-file (stream file
			  :direction :output
			  :if-does-not-exist :create)
    (typecase object
      (string
       (write-string object stream))
      (t
       (princ object stream)))))

(defun spit-append (file object)
  "Write OBJECT to FILE. If FILE is a file name, open it and append to it. If
it doesn't exist, create it."
  (with-open-file (stream file
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :append)
    (typecase object
      (string
       (write-string object stream))
      (t
       (princ object stream)))))

(defun spit-binary (file object &key count append)
  "Output a vector of octets to ‘file’. ‘object’ should be a ‘byte-vector-8’.
If ‘count’ is non-nil, only write that many elements. If ‘append’ is true, the
file is created if it doesn't exist, or appended if it does."
  (with-open-file (stream file :direction :output
			       :if-does-not-exist :create
			       :if-exists (if append :append :error)
			       :element-type '(unsigned-byte 8))
    (assert (typep object '(or (vector (unsigned-byte 8))
			       (vector (satisfies unsigned-byte-8-p)))))
    (if count
	(write-sequence object stream :start 0 :end count)
	(write-sequence object stream))))

;; We can't put this in dlib1 where other things like get-lines are, because
;; then it won't be able to handle generic sequences.
(defun write-lines (file-or-stream lines)
  "Write the collection of ‘lines’ to ‘file-or-stream’. If the file doesn't
exist, it is created."
  (flet ((stream-loop (stream)
	   ;; We could do this, but it could lose the fantastical qualities of
	   ;; the the lines.
	   ;; (omapn (_ (write-line (princ-to-string _) stream)) lines)
	   (omapn (_ (princ _ stream) (write-char #\newline stream)) lines)))
    (if (streamp file-or-stream)
	(stream-loop file-or-stream)
	(with-open-file (stream file-or-stream
				:direction :output :if-does-not-exist :create)
	  (stream-loop stream)))))

;; This seems less sploodgey than the whole slurp/spit/barf metaphor.
;; But unfortunately it's in cl-user in Allegro.
;; (defalias 'file-contents 'slurp)
;; (defsetf file-contents (file) (string) `(spit ,file ,string))

(defun confirm (action &key (output *standard-output*)
			 (input *standard-input*)
			 (confirming-input #\y)
			 (eof-confirms t))
  "A general confirmer using streams. ACTION is a description of the action
you want to confirm. CONFIRMING-INPUT can be either a character or a string,
which defaults to #\\y. If EOF-CONFIRMS is true (the default), then and end of
file is accepted as confirmation."
  (assert (or (stringp confirming-input) (characterp confirming-input)))
  (format output "~%Do you really want to ~a? " action)
  (finish-output output)
  (let ((l (read-line input nil nil)))
    (or (and eof-confirms (not l))	; EOF = confirm (i.e. hit ^D)
	(and (stringp l)
	     (etypecase confirming-input
	       (string (equalp l confirming-input))
	       (character (and (> (length l) 0)
			       (equalp (aref l 0) confirming-input))))))))

(defun get-file-local-variables (file)
  "Return an alist of file local variables, like Emacs, e.g. variables that
are in the first couple of lines that are like:
  -*- MODE-NAME -*-
or
  '-*-' [ <variable> ':' <value> ';' ]* '-*-'
with the last ';' being optional.
Return NIL if we can't find local variables or if the format is messed up.
"
  (with-open-file (in file)
    (let ((line (read-line in nil))
	  start end var-list-string result)
      ;; If it has an interpreter spec, check the next line.
      (when (begins-with "#!" line)
	(setf line (read-line in nil)))
      (setf start (search "-*-" line))
      (when start
	(incf start 3)
	(setf end (search "-*-" line :start2 start)))
      (when end
	(setf var-list-string (subseq line start end)))
      (and var-list-string
	   (multiple-value-bind (a b)
	       (scan-to-strings "^[ \\t]*([^ \\t:;]+)[ \\t]*$"
				      var-list-string)
	     (if a
		 `(("mode" ,b))
		 (progn
		   (loop :for v :in (split ";[ \\t]*" var-list-string)
		      :do
		      (register-groups-bind
		       (name value)
		       ("[ \\t]*([^ :]+)[ \\t]*:[ \\t]*([^ ;]+)" v)
		       (push (list name value) result)))
		   result)))))))

;; Objectible version of one in dlib.
(defun oquote-format (s)
  "Quote a thing to send to format, so that any possible format directives
are printed rather than interpreted as directives, which really just means:
repleace a single tilde with double tidles."
  (oreplace-subseq "~" "~~" s))

;; Old Mac EOL style is hopefully dead.
;; I think this is mostly useful when you don't want potentially endless lines
;; to cause a denial of service problem by exceeding memory. Of course this will
;; still hang on endless lines, but so could any read that is waiting for a
;; character. Hang prevention is probably better solved with a different
;; approach.
(defun read-limited-line (&optional (stream *standard-input*) (limit 88)
			    (eof-error-p t) eof-value)
  "Read a line from STREAM, but no longer than LIMIT. If the line is longer than
LIMIT, the line up until that is returned, and the rest of the line is consumed
and discarded. STREAM defaults to *standard-input*. Note that this only supports
unix EOL style. But it will at least function with windows EOL style, it will
just include the #\return. EOF-ERROR-P and EOF-VALUE should behave as in
READ-LINE. Returns the line and MISSING-NEWLINE-P similar to READ-LINE, except
MISSING-NEWLINE-P is also true if we hit the limit before the newline."
  (let* ((count 0) c (missing-newline-p t)
	 (line
	  (with-output-to-string (out)
	    (loop :while (and (not
			       (equal eof-value
				      (setf c (read-char
					       stream eof-error-p eof-value))))
			      (< count limit)
			      (not (eql c #\newline)))
	       :do
	       (write-char c out)
	       (incf count)))))
    (when (eql c #\newline)
      (setf missing-newline-p nil))
    (if (and (zerop (length line)) (eql c eof-value))
	(values eof-value missing-newline-p)
	(progn
	  ;; Eat the rest of the line.
	  (when (and (= count limit) (not (eql c #\newline)))
	    (loop :while (and (not
			       (equal eof-value
				      (setf c (read-char
					       stream eof-error-p eof-value))))
			      (not (eql c #\newline)))))
	  ;; It doesn't matter if we got a newline later.
	  ;; (setf missing-newline-p (not (char= c #\newline)))
	  (values line missing-newline-p)))))

;; End
