;;
;; glob.lisp - Shell style file name pattern matching
;;

;; $Revision: 1.3 $

(defpackage :glob
  (:documentation "Shell style file name pattern matching")
  (:use :cl :dlib :opsys)
  (:export
   #:pattern-p
   #:fnmatch
   #:expand-tilde
   #:glob
   #:wordexp
   ))
(in-package :glob)

(declaim (optimize (speed 0) (safety 3) (debug 3)
		   (space 0) (compilation-speed 0)))

;; Maybe I should have just wrote this in terms of regexps instead. :(
;; I should test the speed of my code vs. re-translation to CL-PPCRE.
;;
;; These are don't really do everything that the UNIX/POSIX versions do. I
;; only put in the features I needed to implement LISH. If you need other
;; features, feel free to add them, but I recommend keeping it fairly
;; compatible with POSIX.

;; @@@ actually constant
(defparameter *special-chars* #("[*?" "[*?{}" "[*?~" "[*?{}~")
  "Array of strings that have special meaning for glob patterns. Indexed bitwise for braces and tildes.")

(defstruct char-class
  "A class of characters, defined by a function. "
  name
  function)

(defparameter *character-classes* '()
  "Named character classes.")

;; @@@ actually constant
(defparameter *space-chars* #(#\space #\tab #\newline #\^L #\vt)
  "Characters in the space character class.")

;; @@@ actually constant
(defparameter *punct-chars* "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  "Characters in the space character class.")

(defstruct char-set
  "A set of characters. Can have ranges, classes and individual characters. RANGES is a list of (low . high) pairs. CLASSES are a vector of CHAR-CLASS, which is tested for inclusion by a function. STRING is individual characters."
  ranges
  classes
  string
  inverted)

(defmacro defcc (name &body body)
  "Define a character class an. C is the character parameter in the body."
  (let ((funky (if (and (consp body) (consp (car body))
			(eql (caar body) 'function))
		   body
		   `((function (lambda (c) ,@body))))))
    `(push (make-char-class
	    :name ,name
	    :function ,@funky) *character-classes*)))

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
(defcc "punct" (not (not (find c *punct-chars*))))
(defcc "space" (not (not (find c *space-chars*))))
(defcc "blank" (or (char= c #\space) (char= c #\tab)))

(defun pattern-p (pattern braces tilde)
  "Return true if PATTERN has any special characters in it."
  (when (and pattern (length pattern))
    (let* ((ind (logior (ash (if tilde 1 0) 1) (if braces 1 0)))
	   (specials (svref *special-chars* ind)))
;      (format t "~a ~x~%" specials ind)
      (not (null (find-if #'(lambda (c) (find c specials)) pattern))))))

(defun find-char-class (c)
  "Find the most restrictive character class for a character C."
  ;; look thru *char-classes*
  (loop :for k :in *character-classes* :do
     (when (funcall (char-class-function k) c)
       (return-from find-char-class k)))
  nil)

;; This is stupid for a one shot comparison.
(defun compile-char-set (s)
  "Convert char set description into a char-set."
  (let* ((set (make-char-set))
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
		     (push (vector (char-code (car e))
				   (char-code (cadr e)))
			   (char-set-ranges set)))
		    (t
		     (error "Unkown type in char set"))))
		 ((characterp (princ e str))))))))
    (setf (char-set-string set) cc-str)
    set))

#|
Stuff between braces:
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
  "Return a symbolic character set parsed from the string PATTERN, starting at position START-POS. A symbolic character set is a list consisting of:
 - Characters to match.
 - Character ranges, as a sublist with two character values.
 - Named classes, as a keyword, e.g :digit
Second value is where the character set ended."
  (let ((result '()) (i start-pos) (got-close-bracket nil))
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
	  (let ((range-start i))
	    (setf c (char pattern i))
	    (case c
	      (#\=
	       (incf i)
	       (pushnew `(:equiv ,(char pattern i)) result)
	       (incf i)
	       (if (not (string= (subseq pattern i (+ i 2)) "=]"))
		   (error "missing =]"))
	       (incf i))
	      (#\: #| @@@ TODO |# )
	      (#\. #| @@@ TODO |# )
	      (#\]
	       (if (= i range-start)
		   (error "Invalid empty character class")
		   (pushnew #\] result)))
	      (t
	       (error "Invalid character class type indicator '~a' at ~d"
		      c i)))))
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
  (labels
      ((do-string ()
	 (when (char-set-string set)
	   (find c (char-set-string set))))
       (do-ranges ()
	 (when (char-set-ranges set)
	   (loop :for r :in (char-set-ranges set) :do
	      (when (and (>= (char-code c) (elt r 0))
			 (<= (char-code c) (elt r 1)))
		(return-from do-ranges t)))))
       (do-classes ()
	 (when (char-set-classes set)
	   (loop :for r :in (char-set-classes set) :do
	      (when (funcall (char-class-function r) c)
		(return-from do-classes t))))))
    (if (char-set-inverted set)
	(not (or (do-string) (do-ranges) (do-classes)))
	(or (do-string) (do-ranges) (do-classes)))))

;; We could make a wrapper to hide the keywords, but who cares? There might be
;; some circumstance where they help.

(defun fnmatch (pattern string &key (pattern-start 0) (string-start 0))
  "Return true if the STRING matches the PATTERN, with shell matching. '*' is any number of characters, '?' is one character, [] is some complictated range stuff that you can look up in unix or something. PATTERN-START and STRING-START are mostly used internally for recursion, but you can go ahead and use them if you want. They are indexes that default to 0."
  (let ((p pattern-start) (s string-start)
	(plen (length pattern))
        (slen (length string)))
    (if (and pattern string
	     (not (zerop (length pattern)))
	     (not (zerop (length pattern)))
	     (pattern-p pattern nil nil))
	(loop
	   :while (and (< p plen) (< s slen))
	   :do
	   (dbug "~a~%~v,,,va|~%~a~%~v,,,va|~%"
		 pattern p #\space "" string s #\space "")
	   (case (char pattern p)
	     (#\*
	      (let* ((next (1+ p))
		     (next-char (if (< next plen) (char pattern next) nil)))

		;; Eat ajacent *'s
		(when (eq #\* next-char)
		  (loop :while (and (< p plen) (char= #\* (char pattern p)))
		     :do
		     (dbug "eating ~a~%" (char pattern p))
		     ;; use one string char for each ?
		     ;; (when (char= #\? (char pattern p))
		     ;;   (incf s))
		     (incf p))
		  (setf next p))

		;; * at the end means we matched the rest
		(when (or (= next plen) (= p plen))
		  (return-from fnmatch t))

		;; Try to find the rest of the pattern by recursing.
		;; If we don't find the rest of the pattern here, just advance
		;; the string and we will (probably) search again next loop
		;; iteration, otherwise advance the pattern to get past the *.
		;;
		;; The limit of how much we can recurse is how many
		;; non-adjacent *'s there are in the pattern. I suppose we
		;; could arrange for this to be in the tail position if
		;; there's a problem.
		(loop :while (< s slen)
		   :do 
		   (if (fnmatch pattern string
				:pattern-start next
				:string-start s)
					; (incf p)
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
	      (dbug "skipping ? ~a~%" (char string s))
	      (incf s) (incf p))
	     (otherwise
	      ;; compare literal characters
	      (when (char/= (char pattern p) (char string s))
		(dbug "literal mismatch ~a /= ~a~%"
		      (char pattern p) (char string s))
		(return-from fnmatch nil))
	      (dbug "literal ~a~%" (char string s))
	      (incf s) (incf p))))
	;; Not a pattern, just do regular compare
	(progn
	  (dbug "non pattern compare~%")
	  (return-from fnmatch (string= pattern string))))

    ;; Eat trailing *'s
    (loop :while (and (< p plen) (char= (char pattern p) #\*)) :do
       (dbug "eating trailing *~%")
       (incf p))

    ;; If we got thru both, we matched.
    (and (= s slen) (= p plen))))

#|
If we wanted instead to translate into regexps:
  * -> .*
  ? -> .
  [] -> should work the same?

Glob patterns:
  '*' can't match '/'
  '*' doesn't match anything starting with '.', unless it's given explicitly
|#

(defun expand-tilde (w)
  "Return a the expansion of the word W starting with tilde '~'."
  ;; If we don't start with a tilde, just return now.
  (when (not (and w (stringp w) (> (length w) 0) (char= (char w 0) #\~)))
    (return-from expand-tilde w))
  (let* ((end-of-user (or (position-if #'(lambda (c)
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
      ((and (>= (length w) 2) (eql *directory-separator* (aref w 1)))
       (s+ (namestring (user-homedir-pathname)) (subseq w (+ end-of-user 2))))
      ((equal w "~")
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

;; This is called recursively for each directory, depth first, for exapnding
;; GLOB patterns. Of course most of the work is done by FNMATCH.
(defun dir-matches (path &optional dir)
  "Takes a PATH, which is a list of strings which are path elements, usually with GLOB patterns, and a directory DIR, which is a string directory path, without patterns, and returns a list of string paths that match the PATH in the directory DIR and it's subdirectories. Returns NIL if nothing matches."
  (flet ((squip-dir (d f)
	   "Basiclly append the file in name in F to the directory D."
	   (if d
	       (if (and (char= (char d 0) #\/) (= (length d) 1))
		   (s+ "/" (dir-entry-name f))
		   (s+ dir "/" (dir-entry-name f)))
	       (dir-entry-name f)))
	 (starts-with-dot (p) (char= (char p 0) #\.)))
    (when path
      (loop :with p = (car path)
	 :for f :in (read-directory :dir (or dir ".") :full t
				    :omit-hidden (not (starts-with-dot p)))
	 ;; Only match explicit current "." and parent ".." elements.
	 ;; Only match anything like ".*" when the pattern starts with ".".
	 :if (or (and (equal "." p) (equal "." (dir-entry-name f)))
		 (and (equal ".." p) (equal ".." (dir-entry-name f)))
		 (fnmatch p (dir-entry-name f)))
           :when (= (length path) 1)
	     :append (list (squip-dir dir f))
           :else :if (eq :dir (dir-entry-type f))
	     :append (dir-matches (cdr path) (squip-dir dir f))))))

(defun glob (pattern &key mark-directories escape sort braces tilde limit)
  "PATTERN is a shell pattern as matched by FNMATCH.
  MARK-DIRECTORIES true, means put a slash at the end of directories.
  ESCAPE true, means allow \\ to escape the meaning of special characters.
  SORT true, means sort file names in a directory alphabetically.
  BRACES true, means allow braces processing.
  TILDE true, means allow tilde processing, which gets users home directories.
  LIMIT as an integer, means limit the number of pathnames to LIMIT."
  (declare (ignore mark-directories escape sort braces limit))
  (let* ((expanded-pattern (if tilde (expand-tilde pattern) pattern))
	 (path (split-sequence #\/ expanded-pattern :omit-empty t))
	 (dir (when (char= (char expanded-pattern 0) #\/) "/")))
    (dir-matches path dir)))

;; Despite Tim Waugh's <twaugh@redhat.com> wordexp manifesto
;; <http://cyberelk.net/tim/articles/cmdline/>, "When is a command line not a
;; line?", I think all that shell expansion stuff is kind of stupid, even
;; though I agree if you're going to do something complex, hackish, and
;; important to get right, you should do it in a reusable library. I only do
;; it, even in shell programs, when I'm forced to. It's probably much easier,
;; clearer, more secure, more maintainable, to do the equivalent in Lisp,
;; POSIX be damned. In the shell you can _only_ get arithmetic evaluation, and
;; string functions, and so on, in those weird expansions, but it doesn't mean
;; it's good. We can do it some other way.
(defun wordexp ()
  (error "Don't."))

;; EOF
