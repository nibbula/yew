;;;
;;; glob.lisp - Shell style file name pattern matching
;;;

(defpackage :glob
  (:documentation "Shell style file name pattern matching.
Main functions are:
 fnmatch      - Checks whether a string matches a pattern.
 glob         - Returns a list of pathnames that match a pattern.
 pattern-p    - Returns true if the string might have pattern characters.
 expand-tilde - Returns the pathname with ~ home directories expanded.

The documentation for ‘fnmatch’ describes the pattern syntax a little.
")
  (:use :cl :dlib :opsys :collections :char-util)
  (:export
   #:pattern-p
   #:fnmatch
   #:expand-tilde
   #:glob
   #:glob+
   ))
(in-package :glob)

;; ToDo:
;;  - ALL or OMIT-HIDDEN option to not omit files starting with '.'
;;  - Fix exponential behavior, e.g:
;;    touch !(make-string 100 :initial-element #\a)
;;    (glob "a*a*a*a*a*b")

;; (declaim (optimize (speed 0) (safety 3) (debug 3)
;; 		   (space 0) (compilation-speed 0)))

;; These are don't really do everything that the UNIX/POSIX versions do.
;; I only put in the features I needed.

(declaim (type (vector string) +special-chars+))
(define-constant +special-chars+ #("[*?" "[*?{}" "[*?~" "[*?{}~")
  "Array of strings that have special meaning for glob patterns. Indexed bitwise
for braces and tildes." 'vector-equal)

(defstruct char-class
  "A class of characters, defined by a function. "
  (name "" :type string)
  (function #'identity :type function))

(defparameter *character-classes* '()
  "Named character classes.")

(defparameter *character-class-table* (make-hash-table :test #'equal)
  "Named character classes.")

(define-constant +space-chars+ (vector #\space #\tab #\newline (ctrl #\L) #\vt)
  "Characters in the space character class." 'vector-equal)

(define-constant +punct-chars+ "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  "Characters in the space character class." 'equal)

(defstruct char-set
  "A set of characters. Can have ranges, classes and individual characters.
RANGES is a list of (low . high) pairs. CLASSES are a vector of CHAR-CLASS,
which is tested for inclusion by a function. STRING is individual characters."
  (ranges nil :type (or null (simple-array (cons fixnum fixnum))))
  classes
  (string nil :type (or null string))
  (inverted nil :type boolean))

(defmacro defcc (name &body body)
  "Define a character class an. C is the character parameter in the body."
  (with-names (cc)
    (let ((funky (if (and (consp body) (consp (car body))
			  (eql (caar body) 'function))
		     body
		     `((function (lambda (c) ,@body))))))
      `(progn
	 (let ((,cc (make-char-class
		     :name ,name
		     :function ,@funky)))
	   (push ,cc *character-classes*)
	   (setf (gethash ,name *character-class-table*) ,cc))))))

;; @@@ Yes, I know these are stupid and incompatible. This is just an
;; emulation of Unicode. It's better than no Unicode, but it falls back to
;; ASCII for things not in Common Lisp. I should use cl-unicode or something
;; to get these right, or just do whatever CL-PPCRE does.

;; These should be defined in order from least to most specific.
(defcc "xdigit" (digit-char-p c 16))
(defcc "ascii" (< (char-code c) 128))
(defcc "word"  (or (alphanumericp c) (char= c #\_)))
(defcc "print" #'graphic-char-p)
(defcc "graph" (and (graphic-char-p c) (not (char= #\space c))))
(defcc "alnum" #'alphanumericp)
(defcc "alpha" #'alpha-char-p)
(defcc "lower" #'lower-case-p)
(defcc "upper" #'upper-case-p)
(defcc "digit" #'digit-char-p)
(defcc "cntrl" (or (< (char-code c) 31) (= (char-code c) 127)))
(defcc "punct" (not (not (find c +punct-chars+))))
(defcc "space" (not (not (find c +space-chars+))))
(defcc "blank" (or (char= c #\space) (char= c #\tab)))

(defun pattern-p (pattern &optional braces tilde)
  "Return true if PATTERN has any special characters in it.
If BRACES is true, consider curly brace '{' '}' characters as special characters.
If TILDE is true, count tilde '~~' characters as special characters."
  (declare (type string pattern))
  (when (and pattern (length pattern))
    (let* ((ind (logior (ash (if tilde 1 0) 1) (if braces 1 0)))
	   (specials (svref +special-chars+ ind)))
      (declare (type string specials))
;      (format t "~a ~x~%" specials ind)
      (not (null (find-if #'(lambda (c)
			      (declare (type character c))
			      (find c specials)) pattern))))))

(defun find-char-class (c)
  "Find the most restrictive character class for a character C."
  ;; look thru *char-classes*
  (loop :for k :in *character-classes* :do
     (when (funcall (char-class-function k) c)
       (return-from find-char-class k)))
  nil)

(defun find-named-char-class (name)
  "Find the most restrictive character class for a character C."
  (gethash name *character-class-table*))

;; This is probably stupid for a one shot comparison. Also compile isn't machine
;; compile just more like gather.
(defun compile-char-set (s)
  "Convert char set description into a char-set."
  (let* ((set (make-char-set))
	 (ranges nil)
	 (cc-str
	  (with-output-to-string (str)
	    (loop :for e :in s :do
	       (cond
		 ((eql e :not)
		  (setf (char-set-inverted set) t))
		 ((consp e)
		  (cond
		    ((eql (car e) :equiv)
		     (let ((cc (find-char-class (cadr e))))
		       (if cc
			   (push cc (char-set-classes set))
			   (princ (cadr e) str)))) ; XXX in a class by itself
		    ((eql (car e) :class)
		     (push (cadr e) (char-set-classes set)))
		    ((characterp (car e))
		     ;; (push (vector (char-code (car e))
		     ;; 		   (char-code (cadr e)))
		     (push (cons (char-code (car e))
				 (char-code (cadr e)))
			   ranges))
		    (t
		     (error "Unkown type in char set"))))
		 ((characterp (princ e str))))))))
    (when ranges
      (setf (char-set-ranges set)
	    (make-array (length ranges)
			:element-type 'cons
			:initial-contents ranges)))
    (setf (char-set-string set) cc-str)
    set))

#|
Stuff between square brackets:
  ranges:			a-z
  equivalence clases:		[=a=]      (e.g.: a ā á ä ã ...)
  named classes:		[:digit:]  (e.g.: 1 2 3 １ ２ ３ 一 二 三)
  collating symbols:		[.splot.]   (presumably some locale thing???)
    maybe unicode, like: LATIN_SMALL_LETTER_A_WITH_ACUTE
NOTE:
  Can't be empty, so [] is invalid, []] matches a single ']'
  '-' as first or last character is not a range.
  '!' as first character is negation (¡¡¡ what about ^ !!!)
  POSIX doesn't support backslash '\' as an escape char, but other things do

|#

(defun get-char-set (pattern start-pos &optional (compile t))
  "Return a symbolic character set parsed from the string PATTERN, starting at
position START-POS. A symbolic character set is a list consisting of:
 - Characters to match.
 - Character ranges, as a sublist with two character values.
 - Named classes, as a keyword, e.g :digit
Second value is where the character set ended."
  (declare (type string pattern)
	   (type fixnum start-pos))
  (let ((result '()) (i start-pos) (got-close-bracket nil))
    (declare (type fixnum i))
    (loop :with c
       :while (< i (length pattern))
       :do
       (setf c (char pattern i))
       (case c
	 ;; begin
	 ((#\! #\^)
	  (if (= i start-pos)
	      (pushnew :not result)
	      (pushnew c result)))
	 (#\-
	  ;; If the dash is at the start or end, it's just a dash.
	  (if (or (= i start-pos) (char= #\] (char pattern (1+ i))))
	      (pushnew #\- result)
	      (progn ;; Otherwise make a real range
		(pushnew (list (pop result) (char pattern (1+ i))) result)
		(incf i))))
	 (#\[ ;; class or symbol
	  (incf i)
	  (setf c (char pattern i))
	  (case c
	    (#\=
	     (incf i)
	     (pushnew `(:equiv ,(char pattern i)) result)
	     (incf i)
	     (if (not (string= (subseq pattern i (+ i 2)) "=]"))
		 (error "missing =]"))
	     (incf i))
	    (#\:
	     (incf i)
	     (let* ((end (or (position #\: (subseq pattern i))
			     (error "Missing ending : in character ~
                                       class name.")))
		    (name (subseq pattern i (+ i end)))
		    (class (find-named-char-class name)))
	       (when (not class)
		 (error "Unknown character class ~s" name))
	       (pushnew `(:class ,class) result)
	       (incf i end))
	     (incf i))			; the ending : ?
	    (#\. #| @@@ TODO |# )
	    (#\]
	     (error "Invalid empty character class"))
	    (t
	     (error "Invalid character class type indicator '~a' at ~d"
		    c i))))
	 (#\]
	  ;; If we're the first, or second (where the first is ^ or !),
	  ;; treat it as a normal character.
	  (if (or (= i start-pos)
		  (and (= i (1+ start-pos))
		       (or (find (char pattern (1- i))
				 '(#\! #\^) :test #'char=))))
	      (pushnew c result)
	      (progn
		(setf got-close-bracket t)
		(loop-finish))))
	 (t ;; regular char
	  (pushnew c result)))
       (incf i))
    (if got-close-bracket
	(values (if compile (compile-char-set result) result) i)
	(values nil nil))))

(defun match-char-set (set c)
  "Return true if C is in char-set SET."
  (declare (type character c)
	   (type char-set set))
  (labels
      ((do-string ()
	 (when (char-set-string set)
	   (find c (char-set-string set))))
       (do-ranges ()
	 (when (char-set-ranges set)
	   (loop :for r :across (char-set-ranges set) :do
	      ;; (when (and (>= (char-code c) (elt r 0))
	      ;; 		 (<= (char-code c) (elt r 1)))
	      (let ((first (car r))
		    (second (cdr r)))
		(declare (type fixnum first second))
		(when (and (>= (char-code c) first)
			   (<= (char-code c) second))
		  (return-from do-ranges t))))))
       (do-classes ()
	 (when (char-set-classes set)
	   (loop :for r :in (char-set-classes set) :do
	      (when (funcall (char-class-function r) c)
		(return-from do-classes t))))))
    (if (char-set-inverted set)
	(not (or (do-string) (do-ranges) (do-classes)))
	(or (do-string) (do-ranges) (do-classes)))))

;; This doesn't do braces. Brace expansion seems to vary between shells. Mine
;; is no exception to this. I suppose maybe some day we could move the brace
;; expansion code from the shell to here, but whereas glob is quite useful
;; elsewhere, braces seems fairly hackish and like you'd mostly use it in a
;; shell command line where you don't want to do a full-blown loop iteration.

;; We could make a wrapper to hide the keywords, but who cares? There might be
;; some circumstance where they help.

(declaim (ftype (function (string string &key
				  (:pattern-start fixnum)
				  (:string-start fixnum)
				  (:escape boolean)
				  (:ignore-case boolean)) boolean)
		fnmatch))
(defun fnmatch (pattern string &key (pattern-start 0) (string-start 0)
				 (escape t) ignore-case)
  "Return true if the STRING matches the PATTERN, with shell matching. '*' is
any number of characters, '?' is one character, [] is some complictated range
stuff that you can look up in unix or something, but breifly: 
  '^' or '!' - as the first character in the set, negates the set
  '-'        - anywhere not as the first or last character, is the range of
               characters 
  '['        - starts a class name symbol, which we don't fully support yet
               and is not enitrely POSIX conformant, but is something like:
     xdigit - hex digits
     ascii  - ASCII codes, i.e. less than 128
     word   - alpha or underscore '_'
     print  - GRAPHIC-CHAR-P
     graph  - GRAPHIC-CHAR-P but not #\space
     alnum  - ALPHANUMERICP
     alpha  - ALPHA-CHAR-P
     lower  - LOWER-CASE-P
     upper  - UPPER-CASE-P
     digit  - DIGIT-CHAR-P
     cntrl  - Control characters
     punct  - Things considered punctuation.
     space  - #\space #\tab #\newline #\formfeed
     blank  - #\space or #\tab

. PATTERN-START and STRING-START
are mostly used internally for recursion, but you can go ahead and use them if
you want. They are indexes that default to 0."
  (declare (type string pattern string)
	   (type fixnum pattern-start string-start)
	   (type boolean escape ignore-case))
  (let ((p pattern-start) (s string-start)
	(plen (length pattern))
        (slen (length string))
	(quoting nil))
    (declare (type fixnum s p plen slen)
	     (type boolean quoting))
    (flet ((match-literal ()
	     ;;(when (char/= (char pattern p) (char string s))
	     ;; The 'if' version is slightly faster than 'funcall' in sbcl.
	     (when (if ignore-case
		       (char-not-equal (char pattern p) (char string s))
		       (char/= (char pattern p) (char string s)))
	       ;;(dbug "literal mismatch ~a /= ~a~%"
	       ;;     (char pattern p) (char string s))
	       (return-from fnmatch nil))
	     ;;(dbug "literal ~a~%" (char string s))
	     (incf s) (incf p)
	     (values))
	   (string-eq (a b)
	     (if ignore-case (string-equal a b) (string= a b))))
      (declare (dynamic-extent (function match-literal)))

      (if (and pattern string
	       (not (zerop (length pattern)))
	       (not (zerop (length pattern)))
	       (pattern-p pattern))
	  (loop
	     :while (and (< p plen) (< s slen))
	     ;;:do
	     ;;(dbug "~a~%~v,,,va|~%~a~%~v,,,va|~%"
	     ;;   pattern p #\space "" string s #\space "")
	     :if quoting :do
	       (match-literal)
	       (setf quoting nil)
	     :else :do
	     (case (char pattern p)
	       (#\*
		(let* ((next (1+ p))
		       (next-char (if (< next plen) (char pattern next) nil)))

		  ;; Eat ajacent *'s
		  (when (eq #\* next-char)
		    (loop :while (and (< p plen) (char= #\* (char pattern p)))
		       :do
		       ;;(dbug "eating ~a~%" (char pattern p))
		       ;; use one string char for each ?
		       ;; (when (char= #\? (char pattern p))
		       ;;   (incf s))
		       (incf p))
		    (setf next p))

		  ;; * at the end means we matched the rest
		  (when (or (= next plen) (= p plen))
		    (return-from fnmatch t))

		  ;; Try to find the rest of the pattern by recursing.  If we
		  ;; don't find the rest of the pattern here, just advance the
		  ;; string and we will (probably) search again next loop
		  ;; iteration, otherwise advance the pattern to get past the
		  ;; *.
		  ;;
		  ;; The limit of how much we can recurse is how many
		  ;; non-adjacent *'s there are in the pattern. I suppose we
		  ;; could arrange for this to be in the tail position if
		  ;; there's a problem.
		  (loop :while (< s slen)
		     :do
		     (if (fnmatch pattern string
				  :pattern-start next
				  :string-start s
				  :escape escape
				  :ignore-case ignore-case)
			 ;; (incf p)
			 (return-from fnmatch t) ; all done
			 (incf s)))))
	       (#\[
		(multiple-value-bind (set new-pos)
		    (get-char-set pattern (1+ p))
		  (if set
		      (progn
			(when (not (match-char-set set (char string s)))
			  (return-from fnmatch nil))
			(setf p new-pos)
			(incf s)
			(incf p))
		      ;; If there was no set, just match a literal [
		      (if (char/= (char string s) #\[)
			  (return-from fnmatch nil)
			  (progn (incf s) (incf p))))))
	       (#\]
		;; get-char-set should always eat the close ]
		(if (/= p 0) (error "extra ]")))
	       (#\?
		;; skip it
		;;(dbug "skipping ? ~a~%" (char string s))
		(incf s) (incf p))
	       (#\\
		(if escape
		    (progn
		      (setf quoting t)
		      (incf p))
		    (match-literal)))
	       (otherwise
		;; compare literal characters
		(match-literal))))
	  ;; Not a pattern, just do regular compare
	  (progn
	    ;;(dbug "non pattern compare~%")
	    (return-from fnmatch (string-eq pattern string))))

      ;; Eat trailing *'s
      (loop :while (and (< p plen) (char= (char pattern p) #\*)) :do
	 ;;(dbug "eating trailing *~%")
	 (incf p))

      ;; If we got thru both, we matched.
      (and (= s slen) (= p plen) t))))

#|
If we wanted instead to translate into regexps:
  * -> .*
  ? -> .
  [] -> should work almost the same?
  {a,b,c} -> (a|b|c)

Glob patterns:
  '*' can't match '/'
  '*' doesn't match anything starting with '.', unless it's given explicitly
|#

(defun expand-tilde (w)
  "Return a the expansion of the word W starting with tilde '~'."
  (declare (type string w))
  ;; If we don't start with a tilde, just return now.
  (when (not (and w (stringp w) (> (length w) 0) (char= (char w 0) #\~)))
    (return-from expand-tilde w))
  (let* ((end-of-user (or (position-if #'(lambda (c)
					   (declare (type character c))
					   (not (user-name-char-p c)))
				       (subseq w 1))
			  (1- (length w))))
	 (username (if (and end-of-user (> end-of-user 0))
		       (subseq w 1 (1+ end-of-user))
		       ""))
	 home)
    (cond
      ((and (> (length username) 0) (setf home (user-home username)))
       (s+ home (subseq w (1+ end-of-user))))
      ((and (>= (length w) 2) (eql *directory-separator* (char w 1)))
       (s+ (namestring (user-homedir-pathname)) (subseq w (+ end-of-user 2))))
      ((string= w "~")
       (namestring (user-homedir-pathname)))
      (t w))))

#|

Let's say I have:

		   corge
	    bar -> gralt.c
     boo -> baz
     foo    foo
     one
     two           lemon
     zoo -> bar    foo
	    baz -> bark.c
	    foo
	    quux -> pidge.c
		    snoo

Pattern          Matches
---------------- -------------------
*oo/b*/*.c       boo/bar/gralt.c
                 zoo/baz/bark.c

*/*/*.c		 boo/bar/gralt.c
		 zoo/baz/bark.c
		 zoo/quux/pidge.c

match & dir  - f(prefix: boo) readir boo 

|#

(defun is-really-a-directory (dir entry)
  "Return true if the ENTRY in is a directory. ENTRY is a DIR-ENTRY structure."
  (or (eq :directory (dir-entry-type entry))
      ;; If it's a link, we have to stat it to check.
      (and (eq :link (dir-entry-type entry))
	   (let ((s (ignore-errors
		      (get-file-info (s+ dir *directory-separator*
					 (dir-entry-name entry))))))
	     (and s (eq :directory (file-info-type s)))))))

(defun trailing-directory-p (path)
  "Return true if PATH has a trailing directory indicator."
  (declare (type string path))
  (char= (char path (1- (length path))) *directory-separator*))

(defun dir-append (dir file)
  "This is like a slightly simpler ‘path-append’ from OPSYS."
  ;; (declare (type (or string null) dir file))
  (if dir
      ;; If it's the root dir "/", so we don't end up with doubled slashes.
      (if (and (char= (char dir 0) *directory-separator*)
	       (= (length dir) 1))
	  (s+ *directory-separator* file)
	  (s+ dir *directory-separator* file))
      file))

;; This is called recursively for each directory, depth first, for exapnding
;; GLOB patterns. Of course most of the work is done by ‘fnmatch’.
(defun dir-matches (path &key dir escape recursive mark-directories test)
  "Takes a ‘path’, which is a list of strings which are path elements, usually
with patterns, and a directory ‘dir’, which is a string directory path,
without patterns, and returns a list of string paths that match the ‘path’ in
the directory ‘dir’ and it's subdirectories. Returns NIL if nothing matches."
  (declare (type boolean escape recursive mark-directories))
  (when (not path)
    (return-from dir-matches nil))
  ;; (dbugf :glob "path = ~s dir = ~s~%" path dir)
  (let ((path-element (car path))
	(actual-dir (or dir "."))
	result name recursive-match is-dir more-path #| either |#)
    (flet ((starts-with-dot (string)
	     (char= (char string 0) #\.))
	   (append-result (thing)
	     (setf result (append result thing)))
	   (decorated-name (name)
	     (if (and mark-directories is-dir)
		 (s+ name *directory-separator*)
		 name))
	   (path-match (entry path)
	     "True if directory ‘entry’ should match path element ‘path’."
	     (or
	      ;; Only match explicit current "." and parent ".." elements.
	      ;; Only match anything like ".*" when the pattern starts with ".".
	      (and (equal "." path) (equal "." name))
	      (and (equal ".." path) (equal ".." name))

	      ;; It's a directory which matches without the trailing slash
	      (and (trailing-directory-p path)
		   (is-really-a-directory dir entry)
		   (fnmatch
		    (subseq path 0 (position *directory-separator* path
					     :from-end t))
		    name :escape escape))

	      ;; Or just a normal match.
	      (fnmatch path (dir-entry-name entry) :escape escape))))
      (declare (ftype (function (dir-entry string) boolean) path-match)
	       (ftype (function (string) (values string &optional))
		      decorated-name))
      (when recursive
	(setf recursive-match (search "**" path-element)))
      (loop
	 :for entry :in (ignore-conditions (opsys-error)
			  (read-directory
			   :dir actual-dir :full t
			   :omit-hidden (not (starts-with-dot path-element))))
	 :do (setf name      (dir-entry-name entry)
		   is-dir    (is-really-a-directory actual-dir entry)
		   more-path (> (length path) 1)
		   #|either    nil |#) ; for debugging
	 :when (or recursive-match (path-match entry path-element))
	 :do
	 ;; (dbugf :glob "path-element = ~s name = ~s " path-element name)
	 (when (or (not more-path) (and recursive-match
					(path-match entry (cadr path))))
	   ;; There's no further path elements, so just append the match.
	   ;; (dbugf :glob "spoot ~s~%" name)
	   (let ((full-path (dir-append dir (decorated-name name))))
	     (when (or (not test)
		       (and test (funcall test full-path)))
	       (append-result (list full-path))))

	   #|(setf either t) |#)
	 (when (and is-dir (or more-path recursive-match))
	   ;; If it's a directory, get all the sub directory matches.
	   ;; (dbugf :glob "~s is dir~%" name)
	   (append-result (dir-matches
			   (or (and recursive-match path)
			       (cdr path))
			   :dir (dir-append dir (decorated-name name))
			   :escape escape
			   :test test
			   :recursive recursive))
	   #|(setf either t) |#)
	 ;; (when (not either)
	 ;;   (dbugf :glob "not cool~%"))
	 )
      result)))

(defparameter *dir-sep-string* (string *directory-separator*))

(defun glob (pattern &key mark-directories (escape t) (sort t) braces (tilde t)
		       twiddle limit (recursive t) test)
  "Generate a list of files matching ‘pattern’, which is a matched by the
fnmatch function. Other arguments are:
 ‘mark-directories’  Put a slash at the end of directories.
 ‘escape’            Allow \\ to escape the meaning of special characters.
 ‘sort’              Sort file names in a directory alphabetically.
 ‘braces’            Allow braces processing.
 ‘tilde’             Allow tilde processing, which gets users home directories.
 ‘twiddle’           A synonym for TILDE.
 ‘limit’             An integer limit to the number of pathnames.
 ‘recursive’         Use double stars ** to match down through directories.
 ‘test’              A function which given a file name, returns true if it
                     should be included in the results."
  (declare (ignore braces limit)
	   (type boolean mark-directories escape sort tilde twiddle recursive))
  (setf tilde (or tilde twiddle))
  (let* ((expanded-pattern (if tilde (expand-tilde pattern) pattern))
	 (os-path (os-pathname expanded-pattern))
	 (path (os-pathname-path os-path))
	 (dir (when (path-absolute-p os-path)
		(path-root os-path))))
    (when (trailing-directory-p expanded-pattern)
      (rplaca (last path) (s+ (car (last path)) *dir-sep-string*)))
    (if sort
	(sort-muffled (dir-matches path
				   :dir dir :escape escape :recursive recursive
				   :mark-directories mark-directories :test test)
		      #'string<)
	(dir-matches path :dir dir :escape escape :recursive recursive
			  :mark-directories mark-directories :test test))))

(defun glob+ (pattern &optional type-chars)
  "Glob for files of a specific type indicated by ‘type-chars’.
Information about the type characters can be found in uos::*file-type-data* on
posix."
  #+unix
  (flet ((char-to-key (char)
	   "Convert a file type character into a keyword."
	   (let ((i (find char opsys-unix::*file-type-data*
			  :key #'opsys-unix::file-type-info-char)))
	     ;; I guess it's better to fail on unknown letters
	     (if i
		 (opsys-unix::file-type-info-symbol i)
		 (error "Unknown file type character ~s" char))))
	 (of-type (file type)
	   (eq (ignore-errors ;; @@@ too broad
		(uos:file-type-symbol
		 (uos:file-status-mode
		  (uos:lstat file))))
	       type)))
    (typecase type-chars
      (character
       (let ((type-key (char-to-key type-chars)))
	 (glob pattern :test (_ (of-type _ type-key)))))
      ((or cons vector)
       (let ((type-keys
	       (omapcan-as 'list (_ (let ((r (char-to-key _)))
				      (when r (list r))))
			   type-chars)))
	 (glob pattern :test
	       (_ (find-if (lambda (type) (of-type _ type)) type-keys)))))
      (null
       ;; Just do a normal glob if type-chars aren't provided.
       (glob pattern))
      (t
       (error "glob+ unknown type ~a for type-chars ~s" (type-of type-chars)
	      type-chars))))
  ;; @@@ we should be able on windows too
  #-unix
  (flet ((char-to-key (char)
	   "Convert a file type character into a keyword."
	   (let ((i (car (rassoc char opsys::*file-type-char*))))
	     ;; I guess it's better to fail on unknown letters
	     (when (not i)
	       (error "Unknown file type character ~s" char))
	     i))
	 (of-type (file type)
	   (eq (ignore-errors ;; @@@ too broad
		(nos:file-info-type
		 (nos:file-info file)))
	       type)))
    (typecase type-chars
      (character
       (let ((type-key (char-to-key type-chars)))
	 (glob pattern :test (_ (of-type _ type-key)))))
      ((or cons vector)
       (let ((type-keys
	       (omapcan-as 'list (_ (let ((r (char-to-key _)))
				      (when r (list r))))
			   type-chars)))
	 (glob pattern :test
	       (_ (find-if (lambda (type) (of-type _ type)) type-keys)))))
      (null
       ;; Just do a normal glob if type-chars aren't provided.
       (glob pattern))
      (t
       (error "glob+ unknown type ~a for type-chars ~s" (type-of type-chars)
	      type-chars)))))

;; End
