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
   ))
(in-package :libc)

(defctype file-ptr :pointer)		; (FILE *)
(defctype fpos-t
    #+(and darwin 64-bit-target) :int64
    #-(and darwin 64-bit-target) :int32)

(defcvar (#+darwin "__stdinp"  #-darwin "stdin"  *stdin*) file-ptr
  "Standard input stream FILE pointer.")
(defcvar (#+darwin "__stdoutp" #-darwin "stdout" *stdout*) file-ptr
  "Standard output stream FILE pointer.")
(defcvar (#+darwin "__stderrp" #-darwin "stderr" *stderr*) file-ptr
  "Standard error stream FILE pointer.")

(defcfun fopen file-ptr
  "Open the file named by the string ‘path’. Return the stream or a null pointer
if it fails."
(path :string) (mode :string))
(defcfun fclose :int
  "Close the given FILE pointer. Return 0 if successful, other"
  (file file-ptr))
#-windows (defcfun fileno :int
  "Return the file descriptor of a FILE." (file file-ptr))
(defcfun fflush :int
  "Force buffered output to be written, or discard buffered input."
  (file file-ptr))
(defcfun fgetc :int
  "Get the next character from the stream."
  (file file-ptr))
(defcfun getc :int
  "Get the next character from the stream."
  (file file-ptr))
(defcfun getchar :int
  "Get the next character from the standard input *stdin*.")
(defcfun fgets :string
  "Read ‘size’ characters from ‘file’ into ‘string’, until a newline."
  (str :string) (size :int) (file file-ptr))
(defcfun gets :string
  "Read from *stdin* into ‘str’ until a newline or EOF."
  (str :string))
(defcfun printf :int
  "Formatted output to *stdout* as decribed by the string ‘format’ and the rest
of the arguments."
  (format :string) &rest)
(defcfun fprintf :int
  "Formatted output to ‘file’ as decribed by the string ‘format’ and the rest
of the arguments."
  (file file-ptr) (format :string) &rest)
(defcfun sprintf :int
  "Formatted output to the string ‘str’ as decribed by the string ‘format’ and
the rest of the arguments."
  (str :string) (format :string) &rest)
(defcfun snprintf :int
  "Formatted output of at most ‘size’ bytes to the string ‘str’ as decribed by
the string ‘format’ and the rest of the arguments."
  (str :string) (size size-t) (format :string) &rest)
(defcfun fputc :int
  "Write a character ‘c’ to ‘file’."
  (c :int) (file file-ptr))
(defcfun putc :int
  "Write a character ‘c’ to ‘file’."
  (c :int) (file file-ptr))
(defcfun putchar :int
  "Write a character ‘c’ to *stdout*."
  (c :int))
(defcfun fputs :int
  "Write a string ‘s’ to stream ‘file’."
  (s :string) (file file-ptr))
(defcfun puts :int
  "Write a string ‘s’ and a newline to *stdout*."
  (s :string))
(defcfun fread size-t
  "Read ‘nitems’ of size ‘size’ into ‘ptr’ from ‘file’."
  (ptr :pointer) (size size-t) (nitems size-t) (file file-ptr))
(defcfun fwrite size-t
  "Write ‘nitems’ of size ‘size’ into ‘ptr’ to ‘file’."
  (ptr :pointer) (size size-t) (nitems size-t) (file file-ptr))
(defcfun fscanf :int
  "Read formatted input from ‘file’, according to the string ‘format’ into the
rest of the arguments."
  (file file-ptr) (format :string) &rest)
(defcfun scanf :int
  "Read formatted input from *stdin*, according to the string ‘format’ into the
rest of the arguments."
  (format :string) &rest)
(defcfun sscanf :int
  "Read formatted input from string ‘s’, according to the string ‘format’ into
the rest of the arguments."
  (s :string) (format :string) &rest)

(defcfun fsetpos :int
  "Set the position in ‘file’ to ‘pos’."
  (file file-ptr) (pos fpos-t))
(defcfun fgetpos :int
  "Get the position in ‘file’ to ‘pos’."
  (file file-ptr) (pos fpos-t))
(defcfun fseek :int
  "Move the position in ‘file’ to ‘offset’ according to ‘whence’."
  (file file-ptr) (offset :long) (whence :int))
(defcfun ftell :int
  "Return the position in ‘file’."
  (file file-ptr))

(defcfun perror :void
  "Print a description of the last error to *stderr* prefixed by the string ‘s’."
  (s :string))

(defcfun setbuf :int
  "Set the buffer for stream ‘file’ to ‘buf’ of BUFSIZ bytes. If ‘buf’ is a
null-pointer, the stream is unbuffered."
  (file file-ptr) (buf :string))
(defcfun ungetc :int
  "Make the character ‘c’ available for reading from ‘file’."
  (c :int)
  (file file-ptr))

(defcfun ctermid :string
  "Return a string that names the controlling terminal for the process. If ‘s’
is given, put it at most L_cetermid characters of the name in it. Otherwise
it returns a “static” buffer."
  (s :string)) ;; useless?

(defcfun system :int
  "Run the string ‘command’ as a command to system shell."
  (command :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctype & wctype - character classification from the standard C library

(defctype wint-t :int32)

(defcfun iswalnum :int
  "Return non-zero if the wide character ‘wc’ is alpha-numeric."
  (wc wint-t))
(defcfun iswalpha :int
  "Return non-zero if the wide character ‘wc’ is alphabetic."
  (wc wint-t))
#-windows (defcfun iswblank :int
  "Return non-zero if the wide character ‘wc’ is blank."
  (wc wint-t))
(defcfun iswcntrl :int
  "Return non-zero if the wide character ‘wc’ is a control character."
  (wc wint-t))
(defcfun iswdigit :int
  "Return non-zero if the wide character ‘wc’ is a digit."
  (wc wint-t))
(defcfun iswgraph :int
  "Return non-zero if the wide character ‘wc’ is a graphic character."
  (wc wint-t))
(defcfun iswlower :int
  "Return non-zero if the wide character ‘wc’ is a lower case character."
  (wc wint-t))
(defcfun iswprint :int
  "Return non-zero if the wide character ‘wc’ is a printing character."
  (wc wint-t))
(defcfun iswpunct :int
  "Return non-zero if the wide character ‘wc’ is a punctuation character."
  (wc wint-t))
(defcfun iswspace :int
  "Return non-zero if the wide character ‘wc’ is a space character."
  (wc wint-t))
(defcfun iswupper :int
  "Return non-zero if the wide character ‘wc’ is an upper case character."
  (wc wint-t))
(defcfun iswxdigit :int
  "Return non-zero if the wide character ‘wc’ is a hexadecimal digit."
  (wc wint-t))

;; I think these are not standard. BSD?
;;(defcfun iswrune :int (wc wint-t))
;;(defcfun iswascii :int (wc wint-t))
;;(defcfun iswhexnumber :int (wc wint-t))
;;(defcfun iswideogram :int (wc wint-t))
;;(defcfun iswnumber :int (wc wint-t))
;;(defcfun iswphonogram :int (wc wint-t))
;;(defcfun iswspecial :int (wc wint-t))

(defcfun isalnum :int
  "Return non-zero if the character ‘c’ is alpha-numeric."
  (c :int))
(defcfun isalpha :int
  "Return non-zero if the character ‘c’ is alphabetic."
  (c :int))
#-windows (defcfun isascii :int
  "Return non-zero if the character ‘c’ is 7-bit ASCII."
  (c :int))
#-windows (defcfun isblank :int
  "Return non-zero if the character ‘c’ is blank."
  (c :int))
(defcfun iscntrl :int
  "Return non-zero if the character ‘c’ is a control character."
  (c :int))
(defcfun isdigit :int
  "Return non-zero if the character ‘c’ is a digit."
  (c :int))
(defcfun isgraph :int
  "Return non-zero if the character ‘c’ is a graphic character."
  (c :int))
(defcfun islower :int
  "Return non-zero if the character ‘c’ is a lower case character."
  (c :int))
(defcfun isprint :int
  "Return non-zero if the character ‘c’ is a printing character."
  (c :int))
(defcfun ispunct :int
  "Return non-zero if the character ‘c’ is a punctuation character."
  (c :int))
(defcfun isspace :int
  "Return non-zero if the character ‘c’ is a space character."
  (c :int))
(defcfun isupper :int
  "Return non-zero if the character ‘c’ is an upper case character."
  (c :int))
(defcfun isxdigit :int
  "Return non-zero if the character ‘c’ is a hexadecimal digit."
  (c :int))

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

;; EOF
