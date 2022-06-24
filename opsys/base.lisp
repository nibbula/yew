;;;
;;; opsys-base.lisp - Helper functions, setup, and types which are not system
;;;                   specific, and need to come before the system specific
;;;                   package.

(defpackage :opsys-base
  (:documentation "Helper functions and setup which are not system specific.")
  (:use :cl :cffi :trivial-gray-streams :fake-dlib)
  (:export
   ;; Stuff in this file:
   #:*directory-separator*
   #:*directory-separator-string*
   #:*path-separator*
   #:*path-variable*

   #:config-feature
   #:function-defined
   #:define-enum-list
   #:define-to-list
   #:quote-filename
   #:safe-namestring
   #:split-path
   #:c-escape

   ;; Things in types.lisp:
   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode

   #:size-t
   #:string-designator

   #:user-info
   #:user-info-p
   #:make-user-info
   #:user-info-name
   #:user-info-id
   #:user-info-full-name
   #:user-info-home-directory
   #:user-info-shell
   #:user-info-primary-group-id
   #:user-info-guid
   #:user-info-picture

   #:terminal-mode
   #:terminal-mode-p
   #:make-terminal-mode
   #:terminal-mode-echo
   #:terminal-mode-line
   #:terminal-mode-raw
   #:terminal-mode-timeout

   #:window-size
   #:window-size-p
   #:make-window-size
   #:window-size-rows
   #:window-size-columns
   #:window-size-width
   #:window-size-height

   #:os-time
   #:os-time-p
   #:make-os-time
   #:os-time-seconds
   #:os-time-nanoseconds
   #:os-time< #:os-time<= #:os-time> #:os-time>= #:os-time= #:os-time/=

   #:file-type
   #:*file-type-char*
   #:file-info
   #:file-info-p
   #:make-file-info
   #:file-info-creation-time
   #:file-info-access-time
   #:file-info-modification-time
   #:file-info-size
   #:file-info-type
   #:file-info-flags

   #:filesystem-info
   #:filesystem-info-p
   #:make-filesystem-info
   #:filesystem-info-device-name
   #:filesystem-info-mount-point
   #:filesystem-info-type
   #:filesystem-info-total-bytes
   #:filesystem-info-bytes-free
   #:filesystem-info-bytes-available

   #:os-pathname
   #:os-pathname-path
   #:os-pathname-absolute-p
   #:os-pathname-device
   #:os-pathname-host
   #:path-designator
   #:make-os-pathname
   #:copy-os-pathname

   #:os-process
   #:os-process-p
   #:make-os-process
   #:os-process-id
   #:os-process-parent-id
   #:os-process-user
   #:os-process-size
   #:os-process-name

   #:process-handle
   #:process-handle-value

   #:event-set #:make-event-set #:event-set-list #:event-set-os-data
   #:*event-set*
   #:os-event #:os-event-triggered
   #:signal-event #:signal-event-number
   #:io-event #:io-event-handle
   #:input-available-event
   #:output-possible-event
   #:output-finished-event
   #:io-error-event
   #:network-event
   #:network-connection-available-event
   #:os-process-event #:os-process-event-handle
   #:child-died-event #:child-died-event-reason
   #:child-stopped-event
   #:terminal-event
   #:terminal-size-change-event
   #:terminal-size-change-event-width
   #:terminal-size-change-event-height
   #:timer-event #:timer-event-timer
   #:timer-expired-event
   #:timer-triggered-event
   #:system-message-event

   #:opsys-error
   #:opsys-error-code
   #:opsys-resumed
   #:opsys-resized

   ;; os-stram
   #:os-stream
   #:os-stream-handle
   #:os-stream-open
   #:os-stream-system-type
   #:os-input-stream
   #:os-stream-input-buffer
   #:os-stream-position
   #:os-stream-input-fill
   #:os-stream-unread-char
   #:os-stream-got-eof
   #:os-output-stream
   #:os-stream-output-buffer
   #:os-stream-output-position
   #:os-stream-output-fill
   #:os-io-stream
   #:os-binary-stream
   #:os-binary-input-stream
   #:os-binary-output-stream
   #:os-binary-io-stream
   #:os-character-stream
   #:os-stream-encoding
   #:os-character-input-stream
   #:os-character-output-stream
   #:os-stream-column
   #:os-character-io-stream
   #:os-file-error
   #:*buffer-size*
   #:*input-buffer-size*
   #:*output-buffer-size*
   #:fill-buffer
   #:flush-buffer
   ))
(in-package :opsys-base)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(declaim (type character *directory-separator*))
(defparameter *directory-separator*
  #-windows #\/
  #+(and windows (not cygwin)) #\\
  "Character that separates directories in a path.")

(defparameter *directory-separator-string* (string *directory-separator*)
  "The directory separator character as a string, for convenience or
efficiency.")

;; Like on windows this is #\; right? But not cygwin?
(declaim (type character *path-separator*))
(defparameter *path-separator*		; @@@ defconstant?
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

(defparameter *path-variable*
  #-windows "PATH"
  ;;#+windows "%PATH%"
  #+windows "PATH"
  "The environment variable which stores the command search paths.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff to assist in feature frobbing and portability.

(defmacro config-feature (f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ,f *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-defined (sym pack)
    "True if SYM is an external function defined in package PACK."
    (multiple-value-bind (found-symbol status)
	(find-symbol (symbol-name sym) (find-package pack))
      (and found-symbol (eql status :external) (fboundp found-symbol)))))

;; The comments about define-constant apply to this as well.
;; This has to be a macro so it can be used in read time expressions
;; in this file.
;; (defmacro featurep (symbol)
;;   "True if the SYMBOL is in *FEATURES*."
;;   `(not (null (find ,symbol *features*))))

;; This is so we can use the #_ reader macro on openmcl without it interfering
;; with other lisps. On other lisps we define it to do nothing.
;;
;; @@@ This is a bad idea, since it tries to modify the current and perhaps
;; only system readtable, for the sake of implementation specific syntax, which
;; we could invoke some other way, and which fails on other implementations,
;; such as Allegro.
;; #-openmcl (eval-when (:execute)
;; 	    #. (set-dispatch-macro-character
;; 		#\# #\_
;; 		(flet ((pr (stream subchar arg)
;; 			 (declare (ignore subchar arg))
;; 			 (read stream t nil t)))
;; 		  (setf (fdefinition '|#_-reader|) (function pr)))))

;; Constant defining macros.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-enum-list (list-var constant-array &key (start 0))
    "Define enumerated constants and put the names in LIST-VAR. The elements
of CONSTANT-ARRAY should look like: #(name value docstring). START is a integer
to start with, that defaults to 0."
    (with-names (offset)
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,offset ,start))
	     ,@(loop :with name :and doc :and i = 0
		  :for c :across constant-array
		  :do
		  (setf name  (aref c 0)
			doc   (aref c 1))
		  :collect
		  `(defconstant ,name (+ ,offset ,i) ,doc)
		  :do (incf i))))
	 ,@(loop :for c :across constant-array
	      :collect
	      `(push ',(aref c 0) ,list-var)))))

  (defmacro define-to-list (list-var constant-array)
    "Define constants and put the names in LIST-VAR. The elements of
CONSTANT-ARRAY should look like: #(name value docstring)."
    `(progn
       ,@(loop :with name :and value :and doc
	    :for c :across constant-array :do
	    (setf name  (aref c 0)
		  value (aref c 1)
		  doc   (if (>= (length c) 3) (aref c 2) nil))
	    :collect
	      (if doc
		  `(defconstant ,name ,value ,doc)
		  `(defconstant ,name ,value))
	    :collect
	    `(push ',name ,list-var)))))

;; Generic things

;; Define :32-bit-target or :64-bit-target
#+(and (or darwin linux freebsd openbsd netbsd windows)
       (or x86_64 x86-64 arm64 aarch64))
  (config-feature :64-bit-target)
#+ecl (eval-when (:compile-toplevel :load-toplevel :execute)
	(when (= (cffi:foreign-type-size :long) 8)
	  (config-feature :64-bit-target)))
#+(and (not 64-bit-target) (or x86 ppc sparc arm))
  (config-feature :32-bit-target)

#+(and 32-bit-target 64-bit-target) (error "Can't be both 32 & 64 bits!")

(defparameter *need-quoting*
  #-windows "[*?;:"
  #+windows "[*?;"
  "Characters that may need escaping in a pathname.")

;; I am probably unable to express how unfortunate this is.
(defun quote-filename (namestring)
  "Try to quote a file name so none of it's characters are noticed specially
by the Lisp pathname monster. This is useful just before passing strings to
standard functions that take a pathname designator, such as OPEN."
  (typecase namestring
    (pathname namestring)
    (string
     (with-output-to-string (str)
       (loop :for c :across namestring :do
	  (when (position c *need-quoting*)
	    (princ #\\ str))
	  (princ c str))))))

#|  (let ((result namestring))
      (flet ((possibly-quote (c)
	     (when (position c result)
	       ;; It's just not possible to write code this inefficient in C.
	       (setf result (join-by-string (split-sequence c result)
					    (s+ #\\ c))))))
      (loop :for c :across "[*;:" :do
	 (possibly-quote c))
      result)))
|#

(defun unquote-filename (namestring)
  "Assuming a filename was quoted. This is useful to get back to the real file
name after it was a pathname. If given a pathname, it just returns it.
If your normal string filename with characters that need quoting in namestrings,
got turned into a namestring, you should do this before passing it to things
that expect an O/S filename string, like many things in OPSYS.
Unfortunately if you do this to an O/S filename string, it can make it invalid."
  (typecase namestring
    (pathname namestring)
    (string
     (with-output-to-string (str)
       (loop :for i :from 0 :below (max 0 (1- (length namestring))) :do
	 (cond
	   ((and (char= (char namestring i) #\\)
		 (position (char namestring (1+ i)) *need-quoting*))
	    (princ (char namestring (1+ i)) str)
	    (incf i))
	   (t
	    (princ (char namestring i) str))))
       (princ (char namestring (1- (length namestring))) str)))))

(declaim (ftype (function (t) string) safe-namestring))
(defun safe-namestring (pathname)
  "Like CL:NAMESTRING, but if pathname is a string, just return it. This is
useful for accepting pathnames or strings in case namestring would interpret
any characters in strings specially."
  (etypecase pathname
    (pathname
     (let ((ns (namestring pathname)))
       (check-type ns string)
       ns))
    (string pathname)))

;; This is a workaround for not depending on split-sequence.
;; so instead of (split-sequence *directory-separator* p :omit-empty t)
(declaim (ftype (function (t) list)))
(defun split-path (path)
  "Return a list of components of PATH."
  (let* ((our-path (safe-namestring path))
	 (len (length our-path))
	 result)
    (declare (type string our-path) (type fixnum len))
    (when (and (plusp len)
	       (char= (char our-path 0) *directory-separator*))
      (setf result (list (string *directory-separator*))))
    (if (zerop len)
	(list our-path)
	(append result
		(loop :with i fixnum = 0 :and piece
		   :while (< i len) :do
		   (setf piece
			 (with-output-to-string (str)
			   (loop :while (and (< i len)
					     (char/= (char our-path i)
						     *directory-separator*))
			      :do
			      (princ (char our-path i) str)
			      (incf i))))
		   :if (and piece (/= (length piece) 0))
		   :collect piece
		   :do (incf i))))))

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
		     ((<= (char-code (digit-char 0)) cc
			  (char-code (digit-char 7)))
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
