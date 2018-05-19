;;
;; libmagic.lisp - Interface to libmagic.
;;

(defpackage :libmagic
  (:documentation "Simple interface to libmagic.")
  (:use :cl :cffi :opsys)
  (:export
   #:+MAGIC-NONE+
   #:+MAGIC-DEBUG+
   #:+MAGIC-SYMLINK+
   #:+MAGIC-COMPRESS+
   #:+MAGIC-DEVICES+
   #:+MAGIC-MIME-TYPE+
   #:+MAGIC-CONTINUE+
   #:+MAGIC-CHECK+
   #:+MAGIC-PRESERVE-ATIME+
   #:+MAGIC-RAW+
   #:+MAGIC-ERROR+
   #:+MAGIC-MIME-ENCODING+
   #:+MAGIC-MIME+
   #:+MAGIC-APPLE+
   #:+MAGIC-NO-CHECK-COMPRESS+
   #:+MAGIC-NO-CHECK-TAR+
   #:+MAGIC-NO-CHECK-SOFT+
   #:+MAGIC-NO-CHECK-APPTYPE+
   #:+MAGIC-NO-CHECK-ELF+
   #:+MAGIC-NO-CHECK-TEXT+
   #:+MAGIC-NO-CHECK-CDF+
   #:+MAGIC-NO-CHECK-TOKENS+
   #:+MAGIC-NO-CHECK-ENCODING+
   #:+MAGIC-NO-CHECK-BUILTIN+
   #:+MAGIC-NO-CHECK-ASCII+
   #:+MAGIC-NO-CHECK-FORTRAN+
   #:+MAGIC-NO-CHECK-TROFF+
   #:+MAGIC-VERSION+
   #:+MAGIC-PARAM-MAX-RECURSION+
   #:magic_t
   #:magic-open
   #:magic-close
   #:magic-getpath
   #:magic-file
   #:magic-descriptor
   #:magic-buffer
   #:magic-error
   #:magic-setflags
   #:magic-version
   #:magic-load
   #:magic-compile
   #:magic-check
   #:magic-list
   #:magic-errno
   #:magic-setparam
   #:magic-getparam
   ;; Interface for magic
   #:guess-content
   ))
(in-package :libmagic)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(define-foreign-library libmagic
  (:linux (:or "libmagic.so" "libmagic.so.1"))
  (t (:default "libmagic")))

(use-foreign-library libmagic)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +MAGIC-NONE+	    #x000000 "No flags")
(defconstant +MAGIC-DEBUG+	    #x000001 "Turn on debugging")
(defconstant +MAGIC-SYMLINK+	    #x000002 "Follow symlinks")
(defconstant +MAGIC-COMPRESS+	    #x000004 "Check inside compressed files")
(defconstant +MAGIC-DEVICES+	    #x000008 "Look at the contents of devices")
(defconstant +MAGIC-MIME-TYPE+	    #x000010 "Return the MIME type")
(defconstant +MAGIC-CONTINUE+	    #x000020 "Return all matches")
(defconstant +MAGIC-CHECK+	    #x000040 "Print warnings to stderr")
(defconstant +MAGIC-PRESERVE-ATIME+ #x000080 "Restore access time on exit")
(defconstant +MAGIC-RAW+	    #x000100 "Don't translate unprintable chars")
(defconstant +MAGIC-ERROR+	    #x000200 "Handle ENOENT etc as real errors")
(defconstant +MAGIC-MIME-ENCODING+  #x000400 "Return the MIME encoding")
(defparameter +MAGIC-MIME+
  (logior +MAGIC-MIME-TYPE+ +MAGIC-MIME-ENCODING+))
(defconstant +MAGIC-APPLE+	#x000800 "Return the Apple creator and type")

(defconstant +MAGIC-NO-CHECK-COMPRESS+ #x001000
  "Don't check for compressed files")
(defconstant +MAGIC-NO-CHECK-TAR+      #x002000 "Don't check for tar files")
(defconstant +MAGIC-NO-CHECK-SOFT+     #x004000 "Don't check magic entries")
(defconstant +MAGIC-NO-CHECK-APPTYPE+  #x008000 "Don't check application type")
(defconstant +MAGIC-NO-CHECK-ELF+      #x010000 "Don't check for elf details")
(defconstant +MAGIC-NO-CHECK-TEXT+     #x020000 "Don't check for text files")
(defconstant +MAGIC-NO-CHECK-CDF+      #x040000 "Don't check for cdf files")
(defconstant +MAGIC-NO-CHECK-TOKENS+   #x100000 "Don't check tokens")
(defconstant +MAGIC-NO-CHECK-ENCODING+ #x200000 "Don't check text encodings")

(defconstant +MAGIC-NO-CHECK-BUILTIN+ 
  (logior +MAGIC-NO-CHECK-COMPRESS+
	    +MAGIC-NO-CHECK-TAR+
	    +MAGIC-NO-CHECK-APPTYPE+
	    +MAGIC-NO-CHECK-ELF+
	    +MAGIC-NO-CHECK-TEXT+
	    +MAGIC-NO-CHECK-CDF+
	    +MAGIC-NO-CHECK-TOKENS+
	    +MAGIC-NO-CHECK-ENCODING+)
  "No built-in tests; only consult the magic file")

;; Defined for backwards compatibility (renamed)
(defconstant +MAGIC-NO-CHECK-ASCII+	+MAGIC-NO-CHECK-TEXT+)

;; Defined for backwards compatibility; do nothing
(defconstant +MAGIC-NO-CHECK-FORTRAN+	#x000000 "Don't check ascii/fortran")
(defconstant +MAGIC-NO-CHECK-TROFF+	#x000000 "Don't check ascii/troff")

(defconstant +MAGIC-VERSION+ 514 "This implementation")
)

;;typedef struct magic_set *magic_t;
(defctype magic_t :pointer)

(defcfun magic-open magic_t (flags :int))
(defcfun magic-close :void (cookie magic_t))

(defcfun magic-getpath	  :string (filename :string) (x :int))
(defcfun magic-file	  :string (cookie magic_t) (filename :string))
(defcfun magic-descriptor :string (cookie magic_t) (fd :int))
(defcfun magic-buffer	  :string (cookie magic_t)
	                  (buffer :pointer) (length size-t))

(defcfun magic-error :string (cookie magic_t))
(defcfun magic-setflags :int (cookie magic_t) (flags :int))

(defcfun magic-version :int)
(defcfun magic-load    :int (cookie magic_t) (filename :string))
(defcfun magic-compile :int (cookie magic_t) (filename :string))
(defcfun magic-check   :int (cookie magic_t) (filename :string))
(defcfun magic-list    :int (cookie magic_t) (filename :string))
(defcfun magic-errno   :int (cookie magic_t))

(defconstant +MAGIC-PARAM-MAX-RECURSION+ 0)
(defcfun magic-setparam :int (cookie magic_t) (param :int) (value :pointer))
(defcfun magic-getparam :int (cookie magic_t) (param :int) (value :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *magic-db* nil
  "A magic_t representing the current magic database.")

(defvar *magic-default-flags* +MAGIC-SYMLINK+
  "Default flags to use.")

(defun magic-db ()
  (or *magic-db*
      (prog1 (setf *magic-db* (magic-open *magic-default-flags*))
	(magic-load *magic-db* (cffi:null-pointer)))))

(defun guess-content (thing thing-type &optional (guess-type :content))
  "Return a string describing the content of THING.

THING-TYPE can be either :FILE or :BUFFER. For :BUFFER, THING should be a byte
vector, a.k.a (array (unsigned-byte 8) *).

GUESS-TYPE can be one of:
  :CONTENT            (the default), for a description
  :MIME               for a MIME type
  :ENCODING           for the character encoding
  :MIME-AND-ENCODING  for the MIME type and the encoding separated by ';'"
  (ccase guess-type
    (:content		(magic-setflags (magic-db)
			  (logior *magic-default-flags* +MAGIC-NONE+)))
    (:mime              (magic-setflags (magic-db)
			  (logior *magic-default-flags* +MAGIC-MIME-TYPE+)))
    (:encoding          (magic-setflags (magic-db)
			  (logior *magic-default-flags* +MAGIC-MIME-ENCODING+)))
    (:mime-and-encoding (magic-setflags (magic-db)
			  (logior *magic-default-flags* +MAGIC-MIME+))))
  (ccase thing-type
    (:buffer (magic-buffer (magic-db) thing (length thing)))
    (:file   (magic-file   (magic-db) thing))))

;; EOF
