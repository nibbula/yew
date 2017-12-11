;;
;; libc.lisp - Interface to C library functions
;;

;;
;; Limited stdio & standard C library support
;;
;; I think this should only be used for compatibility / interoperability.
;; Use Lisp streams (of some sort) for normal code. For example, other
;; libraries sometimes operate on stdio FILE pointers, such as, curses,
;; bzip2, openssl, etc.
;;
;; Also it should probably be in a separate optional package.

; (define-foreign-library libc
;     ((:and cygwin unix)	(:default "cygwin1")
;      (unix		(:default "libc"))))

;; #+(and unix cygwin)
;; (define-foreign-library libc (:default "cygwin1"))
;; #+(and unix (not cygwin))
;; (define-foreign-library libc (:default "libc"))

;; (use-foreign-library libc)

(in-package :opsys)

(defctype file-ptr :pointer)		; (FILE *)
(defctype fpos-t
    #+(and darwin 64-bit-target) :int64
    #-(and darwin 64-bit-target) :int32)

(defcvar (#+darwin "__stdinp"  #-darwin "stdin"  *stdin*)  file-ptr)
(defcvar (#+darwin "__stdoutp" #-darwin "stdout" *stdout*) file-ptr)
(defcvar (#+darwin "__stderrp" #-darwin "stderr" *stderr*) file-ptr)

(defcfun fopen file-ptr (path :string) (mode :string))
(defcfun fclose :int (file file-ptr))
(defcfun fileno :int (file file-ptr))
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

(defcfun system :int (command :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctype & wctype - character classification from the standard C library

(defctype wint-t :int32)

(defcfun iswalnum :int (wc wint-t))
(defcfun iswalpha :int (wc wint-t))
(defcfun iswblank :int (wc wint-t))
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
(defcfun isascii :int (c :int))
(defcfun isblank :int (c :int))
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

(defconstant +LC-ALL+      0 "Entire locale generally.")
(defconstant +LC-COLLATE+  1 "String collation routines.")
(defconstant +LC-CTYPE+    2 "Character types. Upper and lower case, ~
			      alphabetic or non-alphabetic characters, etc.")
(defconstant +LC-MONETARY+ 3 "For formatting monetary values.")
(defconstant +LC-NUMERIC+  4 "For formatting numbers.  This controls the ~
			      formatting of decimal points in input and ~
			      output of floating point numbers.")
(defconstant +LC-TIME+     5 "For formatting dates and times.")
(defconstant +LC-MESSAGES+ 6 "For message catalogs, see catopen(3) function.")
(defconstant +LC-LAST+     7 "Highest locale category + 1.")

(defcfun ("setlocale" real-setlocale) :string (category :int) (locale :string))

(define-constant +lc-category-alist+ `((:all      . ,+LC-ALL+)
				       (:collate  . ,+LC-COLLATE+)
				       (:ctype    . ,+LC-CTYPE+)
				       (:monetary . ,+LC-MONETARY+)
				       (:numeric  . ,+LC-NUMERIC+)
				       (:time     . ,+LC-TIME+)
				       (:messages . ,+LC-MESSAGES+)))

(defun lc-category (c)
  "Return an valid integer locale category given a keyword. If the argument ~
   is already a valid integer locale category, it is returned, otherwise an ~
   error is signaled."
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

(define-constant +lc-env-type+ `((:all      . "LANG")
				 (:collate  . "LC_COLLATE")
				 (:ctype    . "LC_CTYPE")
				 (:monetary . "LC_MONETARY")
				 (:numeric  . "LC_NUMERIC")
				 (:time     . "LC_TIME")
				 (:messages . "LC_MESSAGES")))

(defun setup-locale-from-environment ()
  "Do appropriate setlocale calls based on the current settings of LC_*
environment variables."
  (loop :with e = nil
	:for f :in +lc-env-type+
	:do
	(when (setf e (environment-variable (cdr f)))
	  (setlocale (car f) e))))

;; EOF
