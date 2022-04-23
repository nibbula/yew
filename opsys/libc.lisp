;;;
;;; libc.lisp - Interface to C library functions
;;;

;;
;; Limited stdio & standard C library support
;;
;; I think this should only be used for compatibility / interoperability.
;; Use Lisp streams (of some sort) for normal code. For example, other
;; libraries sometimes operate on stdio FILE pointers, such as, curses,
;; bzip2, openssl, etc.
;;
;; Also it should probably be in a separate optional package.
;;
;; @@@ Do we need a better way to deal with O/S specific stuff in here?
;; Apparantly the C standard library isn't as standard as one might think.

; (define-foreign-library libc
;     ((:and cygwin unix)	(:default "cygwin1")
;      (unix		(:default "libc"))))

;; #+(and unix cygwin)
;; (define-foreign-library libc (:default "cygwin1"))
;; #+(and unix (not cygwin))
;; (define-foreign-library libc (:default "libc"))

;; (use-foreign-library libc)

(defpackage libc
  (:documentation "Interface to the C library.")
  (:use :cl :cffi :dlib :opsys-base)
  (:export
   ;; stdio
   #:*stdin* #:*stdout* #:*stderr*
   #-(and windows (not unix)) #:fileno
   #:fopen #:fclose #:fflush
   #:fgetc #:getc #:getchar #:fgets #:gets
   #:printf #:fprintf #:sprintf #:snprintf
   #:fputc #:putc #:putchar #:fputs #:puts
   #:fread #:fwrite
   #:fscanf #:scanf #:sscanf
   #:fsetpos #:fgetpos #:fseek #:ftell
   #:perror #:setbuf #:ungetc
   #:ctermid

   ;; ctype
   #-(and windows (not unix)) #:iswblank
   #:iswalnum #:iswalpha #:iswascii #:iswcntrl #:iswdigit
   #:iswgraph #:iswhexnumber #:iswideogram #:iswlower #:iswnumber
   #:iswphonogram #:iswprint #:iswpunct #:iswrune #:iswspace #:iswspecial
   #:iswupper #:iswxdigit

   #-(and windows (not unix)) #:isascii
   #-(and windows (not unix)) #:isblank
   #:isalnum #:isalpha #:iscntrl #:isdigit #:isgraph
   #:ishexnumber #:isideogram #:islower #:isnumber #:isphonogram #:isprint
   #:ispunct #:isrune #:isspace #:isspecial #:isupper #:isxdigit

   ;; i18n
   #:locale-categories
   #:lc-category
   #:setup-locale-from-environment
   #:+LC-ALL+
   #:+LC-COLLATE+
   #:+LC-CTYPE+
   #:+LC-MONETARY+
   #:+LC-NUMERIC+
   #:+LC-TIME+
   #:+LC-MESSAGES+
   #:+LC-COLLATE+
   #:+LC-MESSAGES+
   #:+LC-PAPER+
   #:+LC-NAME+
   #:+LC-ADDRESS+
   #:+LC-TELEPHONE+
   #:+LC-MEASUREMENT+
   #:+LC-IDENTIFICATION+
   #:+LC-LAST+
   #:setlocale

   ;; stdlib
   #:system

   ;; compatibility
   #:c-escape
   ))
(in-package :libc)

(defctype file-ptr :pointer)		; (FILE *)
(defctype fpos-t
    #+(and darwin 64-bit-target) :int64
    #-(and darwin 64-bit-target) :int32)

(defcvar (#+darwin "__stdinp"  #-darwin "stdin"  *stdin*)  file-ptr)
(defcvar (#+darwin "__stdoutp" #-darwin "stdout" *stdout*) file-ptr)
(defcvar (#+darwin "__stderrp" #-darwin "stderr" *stderr*) file-ptr)

(defcfun fopen file-ptr (path :string) (mode :string))
(defcfun fclose :int (file file-ptr))
#-windows (defcfun fileno :int (file file-ptr))
(defcfun fflush :int (file file-ptr))
(defcfun fgetc :int (file file-ptr))
(defcfun getc :int (file file-ptr))
(defcfun getchar :int)
(defcfun fgets :string (str :string) (size :int) (file file-ptr))
(defcfun gets :string (str :string))
(defcfun printf :int (format :string) &rest)
(defcfun fprintf :int (file file-ptr) (format :string) &rest)
(defcfun sprintf :int (str :string) (format :string) &rest)
(defcfun snprintf :int (str :string) (size size-t) (format :string) &rest)
(defcfun fputc :int (c :int) (file file-ptr))
(defcfun putc :int (c :int) (file file-ptr))
(defcfun putchar :int (c :int))
(defcfun fputs :int (s :string) (file file-ptr))
(defcfun puts :int (s :string))
(defcfun fread size-t (ptr :pointer) (size size-t) (nitems size-t)
	 (file file-ptr))
(defcfun fwrite size-t (ptr :pointer) (size size-t) (nitems size-t)
	 (file file-ptr))
(defcfun fscanf :int (file file-ptr) (format :string) &rest)
(defcfun scanf :int  (format :string) &rest)
(defcfun sscanf :int (s :string) (format :string) &rest)

(defcfun fsetpos :int (file file-ptr) (pos fpos-t))
(defcfun fgetpos :int (file file-ptr) (pos fpos-t))
(defcfun fseek :int (file file-ptr) (offset :long) (whence :int))
(defcfun ftell :int (file file-ptr))

(defcfun perror :void (s :string))

(defcfun setbuf :int (file file-ptr) (buf :string))
(defcfun ungetc :int (file file-ptr))

(defcfun ctermid :string (s :string)) ;; useless?

(defcfun system :int (command :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctype & wctype - character classification from the standard C library

(defctype wint-t :int32)

(defcfun iswalnum :int (wc wint-t))
(defcfun iswalpha :int (wc wint-t))
#-windows (defcfun iswblank :int (wc wint-t))
(defcfun iswcntrl :int (wc wint-t))
(defcfun iswdigit :int (wc wint-t))
(defcfun iswgraph :int (wc wint-t))
(defcfun iswlower :int (wc wint-t))
(defcfun iswprint :int (wc wint-t))
(defcfun iswpunct :int (wc wint-t))
(defcfun iswspace :int (wc wint-t))
(defcfun iswupper :int (wc wint-t))
(defcfun iswxdigit :int (wc wint-t))

;; I think these are not standard. BSD?
;;(defcfun iswrune :int (wc wint-t))
;;(defcfun iswascii :int (wc wint-t))
;;(defcfun iswhexnumber :int (wc wint-t))
;;(defcfun iswideogram :int (wc wint-t))
;;(defcfun iswnumber :int (wc wint-t))
;;(defcfun iswphonogram :int (wc wint-t))
;;(defcfun iswspecial :int (wc wint-t))

(defcfun isalnum :int (c :int))
(defcfun isalpha :int (c :int))
#-windows (defcfun isascii :int (c :int))
#-windows (defcfun isblank :int (c :int))
(defcfun iscntrl :int (c :int))
(defcfun isdigit :int (c :int))
(defcfun isgraph :int (c :int))
(defcfun islower :int (c :int))
(defcfun isprint :int (c :int))
(defcfun ispunct :int (c :int))
(defcfun isspace :int (c :int))
(defcfun isupper :int (c :int))
(defcfun isxdigit :int (c :int))

;; Non-standard.
;;(defcfun ishexnumber :int (c :int))
;;(defcfun isideogram :int (c :int))
;;(defcfun isnumber :int (c :int))
;;(defcfun isphonogram :int (c :int))
;;(defcfun isspecial :int (c :int))
;;(defcfun isrune :int (c :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *lc-categories* nil
    "List of defined category names.")

  #-linux
  (progn
    (define-to-list *lc-categories*
      #(#(+LC-ALL+      0 "Entire locale generally.")
	#(+LC-COLLATE+  1 "String collation routines.")
	#(+LC-CTYPE+    2 "Character types. Upper and lower case,
                         alphabetic or non-alphabetic characters, etc.")
	#(+LC-MONETARY+ 3 "For formatting monetary values.")
	#(+LC-NUMERIC+  4 "For formatting numbers.  This controls the
                         formatting of decimal points in input and
                         output of floating point numbers.")
	#(+LC-TIME+     5 "For formatting dates and times.")

	#-windows
	#(+LC-MESSAGES+ 6 "For message catalogs, see catopen(3) function.")
	#-windows
	#(+LC-LAST+     7 "Highest locale category + 1.")
	#+windows
	#(+LC-LAST+     6 "Highest locale category + 1.")
	)))

  #+linux ;; and also probably GNU libc
  (progn
    (define-to-list *lc-categories*
      #(#(+LC-CTYPE+    0 "Character types. Upper and lower case,
                         alphabetic or non-alphabetic characters, etc.")
	#(+LC-NUMERIC+  1 "For formatting numbers.  This controls the
                         formatting of decimal points in input and
                         output of floating point numbers.")
	#(+LC-TIME+     2 "For formatting dates and times.")
	#(+LC-COLLATE+  3 "String collation routines.")
	#(+LC-MONETARY+ 4 "For formatting monetary values.")
	#(+LC-MESSAGES+ 5 "For message catalogs, see catopen(3) function.")
	#(+LC-ALL+      6 "Entire locale generally.")
	;; GNU extensions:
	#(+LC-PAPER+          7 "Settings related to standard paper sizes.")
	#(+LC-NAME+           8 "Formatting of salutations for persons.")
	#(+LC-ADDRESS+        9 "Formatting of addresses andd geography.")
	#(+LC-TELEPHONE+      10 "Settings related to paper sizes.")
	#(+LC-MEASUREMENT+    11 "Settings about units and measurement.")
	#(+LC-IDENTIFICATION+ 12 "Locale metadata?")
	#(+LC-LAST+           13 "Highest locale category + 1."))))

  ;; Get rid of the fake LAST entry.
  (setf *lc-categories* (delete '+LC-LAST+ *lc-categories*)))

(defcfun ("setlocale" real-setlocale) :string (category :int) (locale :string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +lc-category-alist+
      (loop :for c :in *lc-categories*
	 :collect (cons (keywordify
			 (remove-suffix
			  (remove-prefix (string c) "+LC-") "+"))
			(symbol-value c)))))

(defun locale-categories ()
  "Return a list of locale category keywords, These can be used with setlocale."
  (mapcar #'car +lc-category-alist+))

(defun lc-category (c)
  "Return an valid integer locale category given a keyword. If the argument is
already a valid integer locale category, it is returned, otherwise an error is
signaled."
  (ctypecase c
   (number
    (if (and (>= c 0) (< c +LC-LAST+))
	c
	(error "Locale category ~s out of range" c)))
   (keyword
    (or (cdr (assoc c +lc-category-alist+))
	(error "Invalid locale category ~s" c)))))

(defun setlocale (category &optional locale)
  "See manpage for setlocale(3). CATEGORY can be a keyword or integer."
  (let ((result (real-setlocale (lc-category category)
				(or locale (cffi:null-pointer)))))
    (or result
	(error "setlocale of locale ~s for category ~a failed."
	       locale category))))

(define-constant +lc-env-type+
    (loop :for (k . nil) :in +lc-category-alist+
       :collect (cons k (s+ "LC_" (string-upcase k)))))

(defun c-escape (string)
  "Convert C backslash escapes in ‘string’. Returns either string, possibly
modified in place, or a new string."
  (let ((in 0) (out 0) (len (length string)) new u-start)
    (declare (type fixnum in out))
    (labels ((add (c)
	     (if new
		 (if (< out (length new))
		     (setf (char new out) c)
		     (vector-push-extend c new))
		 (setf (char string out) c))
	     (incf out))
	   (get-c ()
	     (prog1 (char string in)
	       (incf in)))
	   (uget-c ()
	     (let ((cc (get-c)))
	       (when (or (not (digit-char-p cc 16))
			 (> in len))
		 (error "Incomplete C universal character name ~s"
			(subseq string u-start
				(min (length string) in))))
	       cc))
	   (realloc ()
	     (setf new (make-array (+ (length string) 5)
				   :element-type 'character
				   :initial-contents string
				   :fill-pointer out
				   :adjustable t))))
      (loop
	:with backslash = nil :and c
	:while (< in len)
	:do
	(setf c (get-c))
	(if backslash
	    (progn
	      (case c
		(#\a (add (code-char 7))) ; #\bel
		(#\b (add (code-char 8))) ; #\backspace
		(#\e (add (code-char 27))) ; #\escape [non-standard]
		(#\f (add (code-char 12))) ; #\page
		(#\n (add (code-char 10))) ; #\newline
		(#\r (add (code-char 13))) ; #\return
		(#\t (add (code-char 9)))  ; #\tab
		(#\v (add (code-char 11))) ; #\vt
		((#\\ #\") (add c))	   ; pre-done ?
		((#\' #\?) (add c))	   ; really?
		(#\x
		 ;; any number of hex digits, until non-hex digit, making a byte
		 (multiple-value-bind (result pos)
		     (parse-integer string :start in
					   :radix 16
					   :junk-allowed nil)
		   (when (not new)
		     (realloc))
		   (setf in pos)
	           (add (code-char result))))
		(#\u ;; 4 hex digits makeing a unicode code point <= #x9999
		 (setf u-start in)
		 (add (code-char
		       (logior
			(ash (digit-char-p (uget-c) 16) 12)
			(ash (digit-char-p (uget-c) 16) 8)
			(ash (digit-char-p (uget-c) 16) 4)
			     (digit-char-p (uget-c) 16)))))
		(#\U ;; 8 hex digits makeing a unicode code point <= #x99999999
		 (setf u-start in)
		 (let ((code (logior
			      (ash (digit-char-p (uget-c) 16) 28)
			      (ash (digit-char-p (uget-c) 16) 24)
			      (ash (digit-char-p (uget-c) 16) 20)
			      (ash (digit-char-p (uget-c) 16) 16)
			      (ash (digit-char-p (uget-c) 16) 12)
			      (ash (digit-char-p (uget-c) 16) 8)
			      (ash (digit-char-p (uget-c) 16) 4)
			      (digit-char-p (uget-c) 16))))
		   (when (> code char-code-limit)
		     (error "Character escape outside of code range #x~x" code))
		   (add (code-char code))))
		(t
		 ;; Octal escape
		 (let ((cc (char-code c)))
		   (cond
		     ((<= (char-code (digit-char 0)) cc (char-code (digit-char 7)))
		      (let ((start (1- in))
			    (end (min len (+ in 2))))
			;; (format t "chow: (subseq ~d ~d) ~s => ~s~%"
			;; 	start end cc
			;; 	(subseq string start end))
			(multiple-value-bind (n pos)
			    (parse-integer string :start start :end end
						  :radix 8 :junk-allowed t)
			  (setf in pos)
			  ;; (format t "n = ~s in = ~s~%" n in)
			  (add (code-char n)))))
		     (t
		      (error "Unknown C escape char \\~c #\\~:*~:c [#x~x]"
			     c cc))))))
	      (setf backslash nil))
	      ;; Not a backslash
	      (if (char= c #\\)
		  (setf backslash t)
		  (add c)))))
    (if new
	new
	(if (< out len)
	    ;; This sort of foils our whole strategy.
	    (subseq string 0 out)
	    string))))

;; EOF
