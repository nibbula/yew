;;;
;;; opsys.lisp - Interface to operating systems
;;;

;; This file is for system independent functions.
;;
;; For a given function we should choose one of:
;;
;;  - Be fully implemented in the system specific package, and be re-exported
;;    by this package. Use the defos* macros.
;;  - Be partially implemented in this package and use appropriate functions
;;    in the system specific package, likely conditionalized by features.
;;  - Be fully implemented in this package, if there's little or no variance
;;    between systems.
;;  - Be in implemented in a language specific module (e.g. libc.lisp)
;;    if it's something that would be found in a standard library for that
;;    language. We would like these to be optional.
;;  - Be implemented in opsys-base, if they are needed to be used by the
;;    system specific packages, and are generic enough.
;;  - Be a generic function, which is defined in generic.lisp and imported and
;;    re-exported from opsys and into the system specific package, with method
;;    definitions here and/or in the system specific package. It should be
;;    specified if the system specific package must implement some methods.
;;
;; Conventions:
;;
;;  - Call anything defined by defcstruct like: foreign-<C struct Name> This
;;    hopefully makes it more obvious that you are dealing with a foreign
;;    struct instead of a Lisp struct.
;;
;;  - In foreign-* structs, use the C names, e.g. with underscores, for slot
;;    names, (e.g. "tv_usec"). This makes it easier to translate from C code,
;;    and follow along with the C documentation. If the C equivalent would be
;;    CamelCased, like on Windows, see the conventions in ms/package.lisp.
;;
;;  - If there's a C struct that callers need to access, provide a lisp struct
;;    instead. This avoids having to access it carefully with CFFI macros,
;;    memory freeing issues, and type conversion issues.
;;
;;  - Put +plus-earmuffs+ on constants. Put *star-earmuffs* on dynamic variables.

(in-package :opsys)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;; (declaim (optimize (speed 3)) (optimize (safety 0))
;;    	 (optimize (debug 3)) (optimize (space 0))
;;     	 (optimize (compilation-speed 0)))
;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
;; 		   (compilation-speed 0)))

;; General outline for adding something with defos*
;;   - Add a definition with defos* in this file.
;;   - Add the symbol to the export list in package.lisp.
;;   - Add a definition in <os>/<appropriate-file>.lisp.
;;   - Add the symbol to the export list in <os>/package.lisp.

(defmacro defosthing (name type test &optional doc)
  "Import a thing from the proper OS specific package and set it's
documenatation."
  (let ((sym (intern (symbol-name name) #+unix :os-unix #+windows :os-ms)))
    `(progn
       (when (,test ',sym)
	 (import '(,sym))
	 (when ,doc
	   (setf (documentation ',name ,type) ,doc))))))

(defmacro defosfun (name lambda-list &optional doc)
  (declare (ignore lambda-list))
  `(defosthing ,name 'function fboundp ,doc))

(defmacro defosvar (name &optional doc)
  `(defosthing ,name 'variable boundp ,doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

(defosfun error-message (error-code)
  "Return a string or something describing the ERROR-CODE. We really make very
little claims about this function, but it should do what it's reasonable to
expect. Like for example on a Unix system it should be like strerror.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental information

(defosfun environment ()
  "Return an a-list of the system environment. The elements are conses
(VARIABLE-NAME . VALUE), where VARIABLE-NAME is a keyword and VALUE is a string.")

(defosfun environment-variable (var)
  "Return a string with the value of the system environment variable name VAR.")

(defosfun env (var)
  "Return a string with the value of the system environment variable name VAR.")

;; Are these all the arguements? Maybe not. Maybe it's just the args which
;; wheren't processed. If you give it ALL-P true then hopefully it is.
;; @@@ Actually I think it might be good to have LISP-ARGS be able to return
;; all the arguments.
(defun lisp-args (#| &key all-p |#)
  "Arguments given when starting the Lisp system."
  #+sbcl     sb-ext:*posix-argv*
  #+clisp    (ext:argv)
  #+cmu	     ext:*command-line-strings*
  #+openmcl  (ccl::command-line-arguments)
  #+excl     (sys:command-line-arguments) 
  #+ecl	     (ext:command-args)
  #-(or sbcl clisp cmu openmcl excl ecl)
  (missing-implementation 'lisp-args))

;; This is really an obsolescent thing.
(defosfun memory-page-size ()
  "Get the system's memory page size, in bytes.")

(defosfun processor-count ()
  "Return the number of processors in the system.")

(defosfun system-info-names ()
  "Return a sequence of availabile system information symbols.")

(defosfun system-info-description (name)
  "Return a description of the system information value NAME. NAME should be
one of symbols retuned system-info-names.")

(defosfun get-system-info (names)
  "Return system information. NAMES can be a single keyword or a list of
keywords, which should be in the value returned by SYSTEM-INFO-NAMES. When
given one keyword, just the value is retuned. When given multiple keywords, an
alist of (:keyword . value) is returned. On certain systems getting multiple
names at once may be more efficent.")

(defosfun os-machine-instance ()
  "Like MACHINE-INSTANCE, but without implementation variation.")

(defosfun os-machine-type ()
  "Like MACHINE-TYPE, but without implementation variation.")

(defosfun os-machine-version ()
  "Like MACHINE-VERSION, but without implementation variation.")

(defosfun os-software-type ()
  "Like SOFTWARE-TYPE, but without implementation variation.")

(defosfun os-software-version ()
  "Like uname -v, but for every operating system. Note that this is reversed
from most CL:SOFTWARE-VERSION, which is what OS-SOFTWARE-RELEASE returns.")

(defosfun os-software-release ()
  "Like CL:SOFTWARE-VERSION, but without implementation variation. Note that
this reversed from historical CL and is more like the POSIX notion of release
and version.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User database
;; 

(defosfun get-user-info (&key name id)
  "Return a user structure from the user database. You can look up by either
NAME or ID. If you specifiy both, it just uses the ID. If you specify neither,
it signals an error.")

(defosfun user-name (&optional id)
  "Return the name of the user with ID, which defaults to the current user.")

(defosfun user-home (&optional (user (user-name)))
  "Return the namestring of the given USER's home directory or nil if the ~
user is not found.")

(defosfun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user.")

(defosfun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user.")

(defosfun user-name-char-p (c)
  "Return true if C is a valid character in a user name.")

(defosfun valid-user-name (username)
  "Return true if USERNAME could be a valid user name, but not that the user
actually exists.")

(defosfun get-next-user ()
  "Return the next user structure from the user database.")

(defosfun user-list ()
  "Return the list of all users.")

(defosfun refresh-user-list ()
  "Make GET-NEXT-GROUP or GROUP-LIST return potentially updated data.")

(defosfun is-administrator ()
  "Return true if you are root, or effectively root.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group database
;; 

(defosfun group-name (&optional id)
  "Return the name of the group with ID. Defaults to the current group.")

(defosfun group-id (&optional name)
  "Return the ID of the group NAME. Defaults to the current group.")

(defosfun get-next-group ()
  "Return the next group structure from the group database.")

(defosfun group-list ()
  "Return the list of all groups.")

(defosfun refresh-group-list ()
  "Make GET-NEXT-GROUP or GROUP-LIST return potentially updated data.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

(defosfun users-logged-in ()
  "Return a list of names of logged in users.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

;; Unless otherwise specified, functions that take a file name should be
;; able to take a ‘path-designator’. But note that that doesn't include
;; a ‘cl:pathname’. In general, the system specific code just implements the
;; string method, and we provide the os-pathname method here.

(defmethod os-namestring ((path string))
  path)

(defmethod os-namestring ((path pathname))
  ;; (safe-namestring path)
  (namestring path)
  )

(defmethod file-info (path &key (follow-links t))
  (file-info (os-namestring path) :follow-links follow-links))

;; Deprecated.
;; (define-alias 'get-file-info 'file-info 'function)
(defun get-file-info (path &key (follow-links t))
  (file-info path :follow-links follow-links))

(defmethod file-accessible-p (path &optional access)
  (file-accessible-p (os-namestring path) access))

(defmethod file-exists (path)
  (file-exists (os-namestring path)))

(defmethod os-delete-file (path)
  (os-delete-file (os-namestring path)))

(defmethod os-rename-file (from to)
  (os-rename-file (os-namestring from) (os-namestring to)))

(defmethod set-file-time ((file pathname) &key access-time modification-time)
  (set-file-time (os-namestring file) :access-time access-time
		 :modification-time modification-time))

(defmethod set-file-time ((file os-pathname) &key access-time modification-time)
  (set-file-time (os-namestring file) :access-time access-time
		 :modification-time modification-time))

(defmethod set-file-time ((file stream) &key access-time modification-time)
  (set-file-time (stream-system-handle file)
		 :access-time access-time
		 :modification-time modification-time))

(defmethod make-symbolic-link (from to)
  (make-symbolic-link (os-namestring from) (os-namestring to)))

(defmethod symbolic-link-target (link-name)
  (symbolic-link-target (os-namestring link-name)))

;; (defmacro with-temp-file ((var &optional template) &body body)
;;   "Evaluate the body with the variable VAR bound to a POSIX file descriptor with a temporary name. The file is supposedly removed after this form is done."
;;   (unwind-protect (progn @@@

;; I might like to have:
;;
;; (defun get-stream-file-name (stream)
;;   (with-foreign-string (path MAXPATHLEN)
;;     (syscall (fcntl F_GETPATH path)))
;;   )

#+clisp
(defvar *stream-handles-func*
  (let (s)
    (when (find-if (_ (and (setf s (find-symbol "STREAM-HANDLES" _))
			   (fboundp s)))
		   '(:ext :socket))
      s))
  "CLisp Function to return a stream handle.")

(defun stream-system-handle (stream &optional (direction :output))
  "Return the operating system handle for a stream. If there is more than one
system handle, return an arbitrary one, or the one specified by `DIRECTION`,
which can be `:INPUT` or `:OUTPUT`. If there isn't one, return NIL."
  #+sbcl (declare (ignore direction))
  #+sbcl
  (cond
    ((and (typep stream 'synonym-stream)
	  (synonym-stream-symbol stream))
     (stream-system-handle (symbol-value (synonym-stream-symbol stream))))
    ((typep stream 'sb-sys:fd-stream)
     (slot-value stream 'sb-impl::fd)))
  #+ccl
  (cond
    ((and (typep stream 'synonym-stream)
	  (synonym-stream-symbol stream))
     (stream-system-handle (symbol-value (synonym-stream-symbol stream))
			   (or direction
			       (if (eq stream *standard-output*)
				   :output
				   :input))))
    ((typep stream 'ccl::echoing-two-way-stream)
     (stream-system-handle (slot-value stream (if (eql direction :output)
						  'ccl:output-stream
						  'ccl:input-stream))))
    ((typep stream 'ccl::basic-stream)
     (ccl::ioblock-device
      (ccl::basic-stream-ioblock stream)))
    ;; ((typep stream 'ccl::fundamental-stream)
    ;;  (ccl::ioblock-device
    ;;   (ccl::stream-ioblock stream t)))
    ((typep stream 'ccl::fd-stream)
     (ccl::ioblock-device
      (ccl::stream-ioblock stream t)))
    )
  #+cmu
  (cond
    ((typep stream 'two-way-stream)
     (stream-system-handle
      (cond
	((or (eq stream *standard-input*) (eq direction :input))
	 (two-way-stream-input-stream stream))
	((or (eq stream *standard-output*) (eq direction :output))
	 (two-way-stream-output-stream stream)))))
    ((and (typep stream 'synonym-stream)
	  (synonym-stream-symbol stream))
     (stream-system-handle (symbol-value (synonym-stream-symbol stream))))
    ((typep stream 'system:fd-stream)
     (slot-value stream 'lisp::fd)))
  #+clisp
  (cond
    ((and (typep stream 'synonym-stream)
	  (synonym-stream-symbol stream))
     (stream-system-handle (symbol-value (synonym-stream-symbol stream))
			   (or direction
			       ;; This trick doesn't work because they're
			       ;; the same.
			       (if (eq stream *standard-output*)
				   :output
				   :input))))
    ((typep stream 'string-stream) nil)
    ((typep stream 'stream)
     (when *stream-handles-func*
       (multiple-value-bind (in out) (funcall *stream-handles-func* stream)
	 (if (eql direction :output)
	     out in)))))
  #+excl (declare (ignore direction))
  #+excl
  (cond
    ((typep stream 'excl::terminal-simple-stream)
     (excl::stream-output-handle stream)
     ;; (slot-value stream 'excl::output-handle)
     ;; (slot-value stream 'excl::input-handle)
     )
    (t
     (excl::stream-output-handle stream)))
  #+lispworks nil
  #+abcl nil
  #+ecl (declare (ignore direction))
  #+ecl (and (typep stream 'file-stream) (ext:file-stream-fd stream))
  #-(or ccl sbcl cmu clisp lispworks abcl ecl excl)
  (missing-implementation 'stream-system-handle))

;; This is really just for the os-stream code.
(defosfun stream-handle-direction (handle)
  "Return a direction for an stream handle, or NIL if there isn't one.")

(defosfun system-handle-close (handle)
  "Close a system handle. Closing a handle still associated with a stream, such
as obtained from ‘stream-system-handle’, could be trouble on some
implementations.")

(defosfun with-os-file ((var filename &key
			     (direction :input)
			     (if-exists :error)
			     (if-does-not-exist :error)) &body body)
  "Evaluate the body with the variable VAR bound to an O/S file descriptor
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

;; @@@ maybe we should rename this to directory? or directory-contents?
(defosfun read-directory (&key dir append-type full omit-hidden)
  "Return a list of the file names in DIR as strings. DIR defaults to the ~
current directory. If APPEND-TYPE is true, append a character to the end of ~
the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, return a list of dir-entry structures instead of file name ~
strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :dir :block-device :regular :link :socket
  :whiteout :undefined
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
")

(defosfun map-directory (function
			 &key dir append-type full omit-hidden collect recursive)
  "Call FUNCTION with the file name of each file in directory DIR. DIR defaults ~
to the current directory. If APPEND-TYPE is true, append a character to the end ~
of the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, call FUNCTION with a list of dir-entry structures instead of ~
file name strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :directory :block-device :regular :link
  :socket :whiteout :undefined
Be aware that DIR-ENTRY-TYPE type can't really be relied on, since many
systems return :UNKNOWN or something, when the actual type can be determined
by FILE-INFO-TYPE.
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
If COLLECT is true, return the results of calling FUNCTION as a list.
If RECURSIVE is true, descend breadth-first into sub-directories.
")

(defosfun change-directory (&optional path)
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given.")

(defosfun current-directory ()
  "Return the full path of the current working directory as a string.")

(defsetf current-directory change-directory
  "Set the current directory.")

(defparameter *default-directory-mode* #o755
  ;; @@@ should we mention umask?
  "The default POSIX style permissions for creating a directory.")

(defmethod make-directory (path &key (mode *default-directory-mode*))
  (make-directory (os-namestring path) :mode mode))

(defmethod delete-directory (path)
  (delete-directory (os-namestring path)))

(defmethod probe-directory (dir)
  (probe-directory (os-namestring dir)))

;; (defmethod directory-p ((path string))
;;   (directory-p (os-namestring path)))

(defmethod directory-p ((path pathname))
  (directory-p (os-namestring path)))

(defmethod directory-p ((path stream))
  (declare (ignore path))
  ;; @@@ maybe there's some possibility to have directory streams
  nil)

(defmethod directory-p (path)
  (declare (ignore path))
  nil)

(defmethod ensure-directory ((directory string)
			     &key (make-parents t)
			       (mode *default-directory-mode*))
  "If DIRECTORY doesn't exist, create it.
 - MAKE-PARENTS   If true, create the parent diretories. (default t)
 - MODE           POSIX style permissions mode for created directories.
                  Uses *default-directory-mode* if not specified."
  (when (not (file-exists directory))
    (loop :with prefix = ""
       :for component :in (nos:split-path directory)
       :do
	 (setf prefix (path-append prefix component))
	 (when (not (file-exists prefix))
	   (when (not make-parents)
	     (cerror "Create ~s?" "Parent directory ~s doen't exist."
		     prefix))
	   (make-directory prefix :mode mode)))))

(defmethod ensure-directory (directory &key (make-parents t)
					 (mode *default-directory-mode*))
  (ensure-directory (os-namestring directory)
		    :make-parents make-parents :mode mode))

(defosfun without-access-errors (&body body)
  "Evaluate the body while ignoring typical file access error from system
calls. Returns NIL when there is an error.")

(defmacro in-directory ((dir) &body body)
  "Evaluate the body with the current directory set to DIR."
  (let ((old-dir (gensym "old-dir")))
    `(let ((,old-dir (current-directory)))
       (unwind-protect
	  (progn
	    (change-directory ,dir)
	    ,@body)
	 (change-directory ,old-dir)))))

(defalias 'with-working-directory 'in-directory)

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
	  (if (or ;; They keep changing this!!
	       (and (function-defined '#:make-directory :posix)
		    (function-defined '#:delete-directory :posix))
	       (and (function-defined '#:make-directory :ext)
		    (function-defined '#:delete-directory :ext)))
	      (config-feature :os-t-has-new-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defosfun path-to-absolute (path)
  "Return the PATH converted into an absolute path.")

;; (setf (symbol-function 'abspath) #'path-to-absolute)
(defalias 'abspath 'path-to-absolute)

(defosfun %path-absolute-p (path)
  "Return true if PATH is an absolute path.")

(defun path-absolute-p (path)
  "Return true if the PATH is an absolute path."
  (etypecase path
    (pathname
     (let ((dir (pathname-directory path)))
       (and dir (eq :absolute (car dir)))))
    (os-pathname
     (%path-absolute-p (os-pathname-namestring path)))
    (string
     (%path-absolute-p path))
    (null nil)))

;;(setf (symbol-function 'absolute-path-p) #'path-absolute-p)
(defalias 'absolute-path-p 'path-absolute-p)

(defun clip-path (path side)
  "Return a portion of ‘path’. If ‘side’ is :dir return the directory, otherwise
return the file portion."
  (let* ((our-path (safe-namestring path))
	 (i (1- (length our-path)))
	 (file-start i)
	 (dir-end i))
    ;; Go backwards from the end until we hit a separator.
    (loop :while (and (>= i 0) (char/= *directory-separator* (char our-path i)))
       :do (decf i))
    (setf file-start (1+ i))
    (if (and (zerop i) (char= *directory-separator* (char our-path 0)))
	(setf dir-end 1)
	(progn
	  ;; Go backwards over any more separators.
	  (loop :while (and (>= i 0)
			    (char= *directory-separator* (char our-path i)))
	     :do (decf i))
	  (setf dir-end (1+ i))))
    ;;(dlib:dbugf :path "i = ~s dir-end ~s file-start ~s~%" i dir-end file-start)
    (if (eq side :dir)
	(if (< i 0)
	    (make-string 0) ;;(subseq our-path 0 0)
	    (subseq our-path 0 dir-end))
	(if (< i 0)
	    path			 ; there was no separator
	    (subseq our-path file-start)))))

(defmethod path-directory-name (path)
  (clip-path (os-namestring path) :dir))

(setf (symbol-function 'dirname) #'path-directory-name)

#|
(defmethod path-file-name ((path string))
  "Return the last portion of a PATH. This is similar to FILE-NAMESTRING."
  (clip-path (or (and (pathnamep path) (safe-namestring path)) path) :file))

(defmethod path-file-name ((path pathname))
  "Return the last portion of a PATH. This is similar to FILE-NAMESTRING."
  (clip-path (safe-namestring path) :file))
|#

(defmethod path-file-name (path)
  "Return the last portion of a PATH. This is similar to FILE-NAMESTRING."
  (clip-path (os-namestring path) :file))

(setf (symbol-function 'basename) #'path-file-name)

(defun path-append (first-path &rest paths)
  "Append the elements ‘paths’ to ‘first-path’. Put a directory separator
between them if there isn't one already."
  (when (not (or (stringp first-path) (pathnamep first-path)))
    (error "FIRST-PATH should be pathname designator."))
  (flet ((trailing-separator-p (s)
	   (char= (char s (1- (length s))) *directory-separator*)))
    (let ((any nil)
	  (last-was-separator nil)
	  (ns (safe-namestring first-path)))
      (with-output-to-string (str)
	(when (not (zerop (length ns)))
	  (setf any t)
	  (setf last-was-separator (trailing-separator-p ns))
	  (princ ns str))
	(loop :for p :in paths :do
	   (when (not (or (stringp p) (pathnamep p)))
	     (error "Elements in ‘paths’ should be pathname designators."))
	   (setf ns (safe-namestring p))
	   (when (not (zerop (length ns)))
	     (when (and any (not last-was-separator)
			(char/= (char ns 0) *directory-separator*))
	       (princ *directory-separator* str))
	     (setf last-was-separator (trailing-separator-p ns))
	     (princ ns str)
	     (setf any t)))))))

(defalias 'p+ 'path-append)

(defun path-snip-ext (path)
  "Remove the extension from a file name, which for this simple function means
just removing everything after the last period '.'"
  (let* ((our-path (safe-namestring path))
	 (pos (position #\. our-path :from-end t)))
    (if (and pos (/= pos 0)) (subseq our-path 0 pos) our-path)))

(defun path-extension (path)
  "Return the extension from a file name, which for this simple function means
just everything after the last period '.'"
  (let* ((our-path (safe-namestring path))
	 (pos (position #\. our-path :from-end t)))
    (when (and pos (/= pos 0)) (subseq our-path (1+ pos)))))

;; @@@ or maybe os-pathname-parse
(defosfun parse-path (path)
  "Return an os-pathname for path.")

(defun os-pathname (path)
  "Return an os-pathname for path."
  (parse-path path))

(defosfun os-pathname-namestring (os-path)
  "Return a namestring for an os-pathname.")

(defosfun os-pathname-pathname (os-path)
  "Return a pathname for an os-pathname.")

(defmethod print-object ((object os-pathname) stream)
  "Print a os-pathname to STREAM."
  (with-slots (path) object
    (print-unreadable-object (object stream :type t)
      (format stream "~s" (os-pathname-namestring object)))))

(defmethod path-parent ((path pathname) &key (n 1))
  (os-pathname-pathname (path-parent (parse-path (safe-namestring path)) :n n)))

(defmethod path-parent ((path string) &key (n 1))
  (os-pathname-namestring (path-parent (parse-path path) :n n)))

(defmethod path-parent ((path os-pathname) &key (n 1))
  (let ((result (copy-os-pathname path)))
    (check-type n (and fixnum unsigned-byte))
    (cond
      ((zerop n) path)
      (t
       (with-accessors ((p os-pathname-path)) result
	 (when p
	   (setf p (butlast p n))))
       result))))

(defosfun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden.")

(defosfun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\".")

;; (defun safe-truename (pathname)
;;   "This is just like truename, but don't get pointless errors on some systems."
;;   ;; @@@
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File locking

(defosfun with-locked-file ((pathname &key (lock-type :write) (timeout 3)
				      (increment .1))
			    &body body)
  "Evaluate BODY with PATHNAME locked, with a LOCK-TYPE lock, which defaults
to :WRITE. Only wait for TIMEOUT seconds to get a lock, checking at least every
INCREMNT seconds.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Commands?

(defosfun is-executable (path &key user regular)
  "Return true if the PATH is executable by the USER. USER defaults to the
current effective user. If REGULAR is true also check if it's a regular file.")

(defalias 'file-executable-p 'is-executable)

(defun has-directory-p (path)
  "Return true if PATH has a directory part."
  (position *directory-separator* (safe-namestring path)))

;; (defun command-test (test path &optional path2)
;;   "Return true if the command passes the test. Do special platform specific
;; processing, like adding `.exe' on windows. If path2 is provided, test takes
;; two arguments."
;;   (if path2
;;       (or (funcall test path path2)
;; 	  #+windows (funcall test (s+ path ".exe") path2))
;;       (or (funcall test path)
;; 	  #+windows (funcall test (s+ path ".exe")))))

(defosfun command-test (test path &optional path2)
  "Return true if the command passes the test. Do special platform specific
processing, like adding `.exe' on windows. If path2 is provided, test takes
two arguments.")

(defstruct command-cache
  dirs
  (table (make-hash-table :size 2459 :test #'equal)))
;; 2459 is the prime closest to 1.5 times the actual number of commands on my
;; system at the moment, so hopefully it's not too oversized for a small
;; system and only requires one table expansion on my system.

(defun new-command-pathname-cache ()
  (make-command-cache))

(defvar *command-pathname-cache*
  (new-command-pathname-cache))

(defun command-pathname-cache-remove (command)
  "Remove COMMAND from the pathname cache."
  (remhash command (command-cache-table *command-pathname-cache*)))

(defun command-pathname-cache-clear ()
  "Clear the pathname cache."
  (setf *command-pathname-cache* nil))

(defun dir-changed (dir info)
  "Return true if the time changed on DIR in CACHE given INFO."
  (let ((saved-time
	 (cdr (find dir (command-cache-dirs *command-pathname-cache*)
		    :test #'equal :key #'car))))
    (or (not saved-time)
	(os-time> (file-info-modification-time info) saved-time))))

(defun cached-path (cmd)
  (let ((dir (gethash cmd (command-cache-table *command-pathname-cache*))))
    (and dir (path-append dir cmd))))

(defun command-pathname (cmd &key (cached :default))
  "Return the full pathname of the first executable file in the PATH or nil
if there isn't one. If the command has a directory part, just verify that it
exists and is executable, and return true if so, NIL if not.
CACHED can be one of :ONLY, :NO, :BUILD, :DEFAULT.
  :ONLY     Return only cahced results. Don't look on the file system.
  :NO       Only look at the file system not the cache.
  :BUILD    Force rebuilding of the cache.
  :DEFAULT  Look in the cache first, if not there, look in the file system
            and cache the result. Rebuild a directory if it's out of date."
  (assert (member cached '(:only :no :build :default)))
  (when (not *command-pathname-cache*)
    (setf *command-pathname-cache* (new-command-pathname-cache)))
  (cond
    ((has-directory-p cmd)
     (return-from command-pathname
       (and (command-test #'file-exists cmd)
	    (is-executable cmd :regular t)
	    cmd)))
    ((eq cached :only)
     (cached-path cmd))
    (t ;; :no :build :default
     (or (and (eq cached :default) (cached-path cmd))
	 (let ((table (command-cache-table *command-pathname-cache*))
	       result info update)
	   (loop :for dir :in (command-path-list)
	      :do
	      (handler-case
		  (when (eq :directory
			    (file-info-type (setf info (get-file-info dir))))
		    (when (setf update (or (dir-changed dir info)
					   (eq cached :build)))
		      (pushnew (cons dir (file-info-modification-time info))
			       (command-cache-dirs *command-pathname-cache*)
			       :test #'equal))
		    ;; (format t "THE SLOW CODE~%") (finish-output)
		    #|
		    (loop
		       :with full = nil
		       :for f :in (read-directory :dir dir) :do
		       (handler-case
			   (when (is-executable
				  (setf full (s+ dir *directory-separator* f))
				  :regular t)
			     (when (command-test #'equal cmd f)
			       (when (not result)
				 (setf result full
				       (gethash cmd table) dir))
			       (when (not update)
				 (return-from command-pathname result)))
			     (when (and update
					(not (gethash f table)))
			       (setf (gethash f table) dir)))
			 (opsys-error (c) (declare (ignore c)))))
		    |#
		    ;; Unlike the above code which is slow, due to stat-ing
		    ;; every file, this just assumes a regular file can
		    ;; be executable.
		    (loop
		       :with full = nil :and name
		       :for f :in (read-directory :dir dir :full t) :do
		       (handler-case
			   (when (or (eq (dir-entry-type f) :regular)
				     (eq (dir-entry-type f) :link))
			     (setf full (s+ dir *directory-separator*
					    (dir-entry-name f))
				   name (dir-entry-name f))
			     (when (command-test #'equal cmd name)
			       (when (not result)
				 (setf result full
				       (gethash cmd table) dir))
			       (when (not update)
				 (return-from command-pathname result)))
			     (when (and update
					(not (gethash name table)))
			       (setf (gethash name table) dir)))
			 (opsys-error (c) (declare (ignore c))))))
		(opsys-error (c) (declare (ignore c)))))
	   result)))))

(defun command-path-list ()
  "Return the system command path as a list."
  (split-sequence *path-separator* (environment-variable *path-variable*)))

(defun list-to-command-path (path-list)
  "Given a list of pathnames return a suitable system command path value."
  (with-output-to-string (str)
    (write-string (car path-list) str)
    (mapcan (_ (write-char *path-separator* str)
	       (write-string _ str)) (cdr path-list))))

(defun set-command-path-list (path-list)
  "Set the system command path to the elements of PATH-LIST."
  (setf (environment-variable *path-variable*)
	(list-to-command-path path-list)))

(defsetf command-path-list set-command-path-list
  "Set the system command path.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application paths

(defosfun data-dir (&optional app-name)
  "Where user specific data files should be stored.")

(defosfun config-dir (&optional app-name)
  "Where user specific configuration files should be stored.")

(defosfun data-path (&optional app-name)
  "Search path for user specific data files.")

(defosfun config-path (&optional app-name)
  "Search path for user specific configuration files.")

(defosfun cache-dir (&optional app-name)
  "Directory where user specific non-essential data files should be stored.")

(defosfun runtime-dir (&optional app-name)
  "Directory where user-specific non-essential runtime files and other file
objects should be stored.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

;; @@@ 

(defun system-command (cmd &optional args)
  "Run a system command. The command is generally given to whatever the system
shell would be and the output and input are to the standard places. You would
think that the ARGS would end up as separate arguments to the eventual command, 
but because they're passed to the system shell, they may not."
  #+clisp (ext:run-shell-command (format nil "~a~{ ~a~}" cmd args)) ; XXX
;  #+sbcl (sb-ext:process-output (sb-ext:run-program cmd args :search t))
;  #+sbcl (sb-ext:process-exit-code
;	  (sb-ext:run-program cmd args :wait t :pty nil
;			      :search t :output t :input t :error t))
  #+sbcl (system (format nil "~a~{ ~a~}" cmd args))
  #+cmu (ext:process-exit-code (ext:run-program cmd args :output t :input t :error t))
;  #+openmcl (ccl::os-command (format nil "~a~{ ~a~}" cmd args))
; ccl failing for cmds that need a tty
;  #+ccl (ccl:run-program cmd args :input t :output t :input t :wait t)
  #+ccl (nos:system (format nil "~a~{ ~a~}" cmd args))
  ;; @@@ ccl shoud probably use ccl:os-command
;  #+ecl (ext:run-program cmd args)
  #+ecl (ext:system (format nil "~a~{ ~a~}" cmd args))
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args) :wait t)
  #+lispworks (system:call-system-showing-output
	       (format nil "~a~{ ~a~}" cmd args) :prefix "" :show-cmd nil)
  #+abcl (ext:run-shell-command (format nil "~a~{ ~a~}" cmd args))
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl)
  (missing-implementation 'system-command))

;; @@@ Consistently return exit status?
;; @@@ Evironment on other than sbcl and cmu?
(defun run-program (command args &key (environment nil env-p) background)
  "Run COMMAND with arguments ARGS which should be a list. ENVIRONMENT is the
list of environment variables defined. If ENVIRONMENT isn't provided, inherit
it from the current process."
  (declare (ignorable background))
  ;; #+(or clisp sbcl ccl) (fork-and-exec command args)
  #+clisp (declare (ignore environment env-p))
  #+clisp (ext:run-program command :arguments args)
  #+excl (excl:run-shell-command (concatenate 'vector (list command command)
					      args)
				 :wait t)
  #+(and (or openmcl ccl) unix) (apply #'os-unix:fork-and-exec
				       `(,command ,args
					      ,@(when env-p :env environment)))
  #+(and (or openmcl ccl) (not unix))
  (let* ((proc
#|	  (ccl::run-program command args
			    :sharing :external
			    :input t
			    :output t
			    :error t
			    :wait t) |#
	   (apply #'ccl::run-program
		  `(,command ,args
			 ,@(when env-p :env environment)
			 #| :sharing :external |#
			 :input t
			 :output t
			 :error t
			 :wait t))))
    (multiple-value-bind (status code-or-sig)
	(ccl::external-process-status proc)
      (case status
	(:stopped
	 (error "Process stopped. PID = ~d" (ccl::external-process-id proc)))
	(:signaled
	 (error "Process got signal ~d. PID = ~d" code-or-sig
		(ccl::external-process-id proc)))
	(:running
	 (error "Process running. PID = ~d" (ccl::external-process-id proc)))
	(:exited
	 ;; I dunno why it seems to return 71 when it can't exec the
	 ;; program.
	 (if (and (numberp code-or-sig) (= code-or-sig 71))
	     nil
	     code-or-sig))
	(t
	 (error "Process has unknown status ~a" status)))))

  #+(and sbcl (not unix))
  (progn
    (sb-ext:process-exit-code
     (apply #'sb-ext:run-program
	    `(,command ,args
		       ,@(when env-p `(:environment ,environment))
		       :search t :output t :input t :error t
		       ;; :pty nil
		       :wait ,(not background)
		       ))))
  #+(and sbcl unix)
  (apply #'os-unix::forky
	 `(,command ,args
		,@(when env-p `(:environment ,environment))))
  #+cmu (ext:process-exit-code
	 (apply #'ext:run-program
		 `(,command ,args
		   ,@(when env-p :environment environment)
		   :wait t :output t :input t :error t :pty nil)))
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (system:run-shell-command
		   (concatenate 'vector (list command) args)
		   :output :stream
		   #| :wait t |#)
		result)
  #+ecl
  (multiple-value-bind (result ret-code proc)
      (apply #'ext:run-program
	     `(,command ,args
		    ,@(when env-p `(:environ ,environment))
		    :output t :input t))
    (declare (ignore result proc))
    ret-code)

  #+abcl
  (let* ((proc (apply #'sys:run-program
		      `(,command ,args
			     ,@(when env-p :environment environment))))
	 (out (system:process-output proc)))
    (dlib:copy-stream out *standard-output*)
    (finish-output *standard-output*)
    (system:process-exit-code proc))
  #-(or clisp excl openmcl sbcl cmu lispworks ecl abcl)
  (missing-implementation 'run-program))

;; (defun get-groups ()
;;   "Return an array of group IDs for the current process."
;;   ;; @@@@
;;   )

(defosfun suspend-process (&optional id)
  "Suspend the process with the given ID. If ID is NIL or not given, suspend
the current process.")

(defosfun resume-process (id)
  "Resume the suspended process with the given ID.")

(defosfun terminate-process (id)
  "Terminate the process with the given ID.")

(defosfun process-times (who)
  "Get CPU time for WHO, which is either :SELF or :CHILDREN. Return a four
integer values: seconds and microseconds of user time, seconds and microseconds
of system time.")

(defosfun process-list ()
  "Return a list of OS-PROCESS structures that represent the processes active
around the time of the call.")

(defosfun current-process-id ()
  "Return the process identifier of the currrent process.")

(defosvar *system-process-type*
  "The type of a system specific process.")

(defosfun system-process-list ()
  "Return a list of system specific processes.")

(defosfun system-process-info (id)
  "Return system specific process information for ID.")

(defosfun wait-and-chill () ;; @@@ shouldn't really be part of the interface
  "Wait for jobs to do something.")

(defosfun check-jobs () ;; @@@ shouldn't really be part of the interface
  "Check if any sub-processes have changed status. Returns three values.
The PID of the process that changed, and the RESULT and STATUS as returned by
wait. Returns NILs if nothing changed.")

(defosfun os-process-priority (&key user pid group)
  "Get the process priority for USER number, process ID, or process GROUP.
Returns a system dependant priority number. This may be settable if the O/S,
allows it. Only one of USER, PID, or GROUP should be specified.")

(defosvar *os-process-most-favorable-priority*
  "The process priority value for most favorable scheduling.")

(defosvar *os-process-least-favorable-priority*
  "The process priority value for least favorable scheduling.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inter-process communication

(defun environ-to-string-list (env)
  "Convert a keyworded alist environment to a list of strings with #\=.
Just return ENV if it doesn't seem like an alist."
  (or (and env
	   (listp (car env))
	   (keywordp (caadr env))
	   (loop :for (a . b) :in env
	      :collect (format nil "~a=~a" a b)))
      env))

;; Pipes

;; @@@@ We should make sure it's portable!
;; @@@ add environment on others
(defun pipe-program (cmd args &key in-stream (out-stream :stream)
				(environment nil env-p))
  "Return an input stream with the output of the system command. Use IN-STREAM
as an input stream, if it's supplied. If it's supplied, use OUT-STREAM as the
output stream. OUT-STREAM can be T to use *standard-output*.
ENVIRONMENT is a list of strings of the form NAME=VALUE to be used as the
process's environment. If ENVIRONMENT is not provided, it defaults to the
current process's environment."
  #+clisp (declare (ignore environment env-p)) ; XXX
  #+clisp (if in-stream
	      (multiple-value-bind (io i o)
		  (ext:run-shell-command
		   (format nil "~a~{ ~a~}" cmd args) :output out-stream
		   :input :stream :wait nil)
		(declare (ignore io i))
		(alexandria:copy-stream in-stream o)) ; !!!
	      (ext:run-shell-command
	       (format nil "~a~{ ~a~}" cmd args) :output out-stream))
  #+sbcl (sb-ext:process-output
;; @@@ What should we do? Added what version?
;;	      :external-format '(:utf-8 :replacement #\?)
	  (apply #'sb-ext:run-program
		 `(,cmd ,args :output ,out-stream :search t :wait nil
			;;,@(when in-stream `(:input ,in-stream))
			:input ,(or in-stream t)
			,@(when env-p
				`(:environment
				  ,(environ-to-string-list environment))))))
  #+cmu (ext:process-output
	 (if in-stream
	     (ext:run-program cmd args :output out-stream :input in-stream)
	     (ext:run-program cmd args :output out-stream)))
#|  #+openmcl (ccl::external-process-output-stream
	     (if in-stream
		 (ccl::run-program cmd args :output out-stream
				   :input in-stream :wait nil)
		 (ccl::run-program cmd args :output out-stream
				   :wait nil))) |#
  #+(or openmcl ccl)
  (let ((proc (apply #'ccl::run-program
		     `(,cmd ,args :wait nil
			    ,@(when out-stream `(:output ,out-stream))
			    ,@(when in-stream `(:input ,in-stream))
			    ,@(when env-p
				    `(:env
				      ,(environ-to-string-list environment)))))))
    (ccl::external-process-output-stream proc))
  
  #+ecl (multiple-value-bind (result ret-code proc)
	    (apply #'ext::run-program
		   `(,cmd ,args :wait nil :input t
			  ,@(if out-stream
				`(:output ,out-stream)
				'(:output t))
			  ,@(if in-stream
				`(:input ,in-stream)
				'(:input t))
			  ,@(when env-p
				  `(:env
				    ,(environ-to-string-list environment)))))
	  (declare (ignore result ret-code))
	  (ext:external-process-output proc))
  ;;#+ecl (ext:run-program cmd args)

  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args)
				 :output out-stream :wait t)
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (declare (ignore result err-str pid))
		  (system:run-shell-command
		   (concatenate 'vector (list cmd) args)
		   :output out-stream
		   #| :wait t |#)
		  str)
  ;; XXX @@@ This is very bogus! (for what it ignores)
  #+abcl (declare (ignore in-stream out-stream environment env-p))
  #+abcl (sys:process-output (sys:run-program cmd args))
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl)
  (missing-implementation 'pipe-program))

(defmacro with-process-output ((var cmd args) &body body)
  "Evaluate the body with the variable VAR bound to a stream with the output
from the system command CMD with the arguments ARGS."
  `(let (,var)
    (unwind-protect
	 (progn
	   (setf ,var (pipe-program ,cmd ,args))
	   ,@body)
      (if ,var (close ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

(defosfun get-time ()
  "Return the time in seconds and nanoseconds. The first value is seconds in
so-called “universal” time. The second value is nanoseconds.")

(defun os-time ()
  (multiple-value-bind (s ns) (get-time)
    (make-os-time :seconds s :nanoseconds ns)))

;; Deprecated.
(define-alias 'get-os-time 'os-time 'function)

(defosfun set-time (seconds nanoseconds)
  "Set time in seconds and nanoseconds. Seconds are in so-called
“universal” time.")

(defosfun timezone-name ()
  "Return the current timezone name.")

(defosfun timezone-offset ()
  "Seconds west of UTC.")

;; Incredibly bogus hack alert:
;;
;; This tries to get the results of the CL time macro in a form we can better
;; manipulate. It does very implementation dependent hackish things.
;;
;; @@@ Maybe I should scrap this idea, and make something that will return
;; somewhat consistent results for different implementations.

(defvar *time-result* nil
  "A place to dynamically store the timing results.")

#+sbcl
(progn
  (defun gather-time (&rest args
		      &key real-time-ms user-run-time-us system-run-time-us
			gc-run-time-ms processor-cycles eval-calls
			lambdas-converted page-faults bytes-consed
			aborted)
    (declare (ignorable real-time-ms user-run-time-us system-run-time-us
			gc-run-time-ms processor-cycles eval-calls
			lambdas-converted page-faults bytes-consed aborted))
    (setf *time-result* (copy-list args)))

  (defun our-print-time (&rest args
			 &key real-time-ms user-run-time-us system-run-time-us
			   gc-run-time-ms processor-cycles eval-calls
			   lambdas-converted page-faults bytes-consed
			   aborted)
    (declare (ignorable real-time-ms user-run-time-us system-run-time-us
			gc-run-time-ms processor-cycles eval-calls
			lambdas-converted page-faults bytes-consed aborted))
    (setf *time-result*
	  (with-output-to-string (*trace-output*)
	    (apply #'sb-impl::print-time args)))))

#+ccl
(progn
  (defun gather-time (&rest args
		      &key form results elapsed-time user-time system-time
			gc-time bytes-allocated minor-page-faults
			major-page-faults swaps)
    (declare (ignorable form results elapsed-time user-time system-time
			gc-time bytes-allocated minor-page-faults
			major-page-faults swaps))
    (let ((our-results (copy-list args)))
      (remf our-results :results)
      (setf *time-result* our-results))
    (values-list results))

  (defun our-print-time (&rest args
			 &key form results elapsed-time user-time system-time
			   gc-time bytes-allocated minor-page-faults
			   major-page-faults swaps)
    (declare (ignorable form results elapsed-time user-time system-time
			gc-time bytes-allocated minor-page-faults
			major-page-faults swaps))
    (setf *time-result*
	  (with-output-to-string (*trace-output*)
	    (apply #'ccl::standard-report-time args)))
    (values-list results)))

#+clisp
(defun call-with-timing (function as-plist)
  (flet ((tv-to-sec (s us)
	   (+ s (/ us internal-time-units-per-second))))
    (multiple-value-bind
	  (start-real-time-sec start-real-time-ms
	   start-run-time-sec start-run-time-ms
	   start-gc-time-sec start-gc-time-ms
	   start-space-high start-space-low
	   start-gc-count)
	(system::%%time)
      (funcall function)
      (multiple-value-bind
	  (end-real-time-sec end-real-time-ms
	   end-run-time-sec end-run-time-ms
	   end-gc-time-sec end-gc-time-ms
	   end-space-high end-space-low
	   end-gc-count)
	  (system::%%time)
	(let ((timing
	       (list
		;; Time since system start
		:real-time
		(coerce
		 (- (tv-to-sec end-real-time-sec end-real-time-ms)
		    (tv-to-sec start-real-time-sec start-real-time-ms))
		 'float)
		;; Time used since system start
		:run-time
		(coerce
		 (- (tv-to-sec end-run-time-sec end-run-time-ms)
		    (tv-to-sec start-run-time-sec start-run-time-ms))
		 'float)
		;; Time used by GC since system start
		:gc-time
		(coerce
		 (- (tv-to-sec end-gc-time-sec end-gc-time-ms)
		    (tv-to-sec start-gc-time-sec start-gc-time-ms))
		 'float)
		;; Space used since system start, in bytes
		:space
		(- (logior (ash end-space-high 24) end-space-low)
		   (logior (ash start-space-high 24) start-space-low))
		;; Number of garbage collections carried out.
		:gc-count
		(- end-gc-count start-gc-count))))
	(setf *time-result*
	      (if as-plist
		  timing
		  (print-properties timing :stream nil))))))))

#+(or ecl (and (not sbcl) (not ccl) (not clisp)))
(defun call-with-timing (function as-plist)
  "Fallback shabby time function that should work with any implementation."
  (let ((real-start (get-internal-real-time))
	(run-start (get-internal-run-time))
	#+(and ecl (not boehmgc)) gc-start
	#+(and ecl (not boehmgc)) gc-end
	#+(and ecl boehm-gc) gc-count-start
	#+(and ecl boehm-gc) gc-count-end
	#+(and ecl boehm-gc) consed-start
	#+(and ecl boehm-gc) consed-end)
    #+ecl
    (progn
      (si::gc t)
      #+boehm-gc
      (progn
	(when (zerop si::*do-time-level*) (si::gc-stats 0))
	(multiple-value-setq (consed-start gc-count-start) (si::gc-stats t)))
      #-boehm-gc
      (setf gc-start (si::gc-time)))
    (multiple-value-prog1 (funcall function)
      #+(and ecl boehm-gc)
      (progn
	(si:gc t)
	(multiple-value-setq (consed-end gc-count-end) (si::gc-stats nil)))
      (let* ((real-end (get-internal-real-time))
	     (run-end (get-internal-run-time))
	     #+(and ecl (not boehm-gc)) (gc-end (si::gc-time))
	     (timing
	      (list :real-time (coerce (/ (- real-end real-start)
					  internal-time-units-per-second)
				       'float)
		    :run-time  (coerce (/ (- run-end run-start)
					  internal-time-units-per-second)
				       'float)
		    #+(and ecl (not boehm-gc)) :gc-time
		    #+(and ecl (not boehm-gc))
		    (coerce (/ (- gc-end gc-start)
			       internal-time-units-per-second) 'float)
		    #+(and ecl boehm-gc) :gc-count
		    #+(and ecl boehm-gc) (- gc-count-end gc-count-start)
		    #+(and ecl boehm-gc) :bytes-consed
		    #+(and ecl boehm-gc) (- consed-end consed-start))))
	(setf *time-result*
	      (if as-plist
		  timing
		  (print-properties timing :stream nil)))))))

;; Simple portable version:
;; #-(or sbcl ccl ecl)
;; (progn
;;   (defun call-with-timing (function as-plist)
;;     "Fallback shabby time function that should work with any implementation."
;;     (let ((real-start (get-internal-real-time))
;; 	  (run-start (get-internal-run-time)))
;;       (multiple-value-prog1 (funcall function)
;; 	(let ((real-end (get-internal-real-time))
;; 	      (run-end (get-internal-run-time))
;; 	      (timing
;; 	       (list :real-time  (/ (- real-end real-start)
;; 				    internal-time-units-per-second)
;; 		     :run-time  (/ (- real-end real-start)
;; 				   internal-time-units-per-second))))
;; 	  (setf *time-result*
;; 		(if as-plist
;; 		    timing
;; 		    (print-properties timing :stream nil))))))))

;; I'm chosing to use a result variable instead of multiple layers of multiple
;; values, since I think it's easier for the calling function to deal with.
(defmacro fake-time ((&key as-plist) form)
  "Evaluate the FORM and try to return it's values. Store what TIME would have
printed into *time-result*. If AS-PLIST is true, return a implementation
dependent property list of timing data."
  #+sbcl
  (if as-plist
      `(sb-ext:call-with-timing #'gather-time (lambda () ,form))
      `(sb-ext:call-with-timing #'our-print-time (lambda () ,form)))
  #+ccl
  `(let ((ccl::*report-time-function*
	  (if ,as-plist #'gather-time #'our-print-time)))
     (ccl::report-time ',form #'(lambda () (progn ,form))))
  #-(or sbcl ccl)
  `(call-with-timing #'(lambda () ,form) ,as-plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events

(defosfun listen-for (seconds fd)
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available.")

(defosfun %create-event-set ())
(defosfun %destroy-event-set (set))
(defosfun %add-event (event set))
(defosfun %delete-event (event set))
(defosfun %clear-trigggers (set))

(defun create-event-set ()
  (let ((set (make-event-set)))
    (%create-event-set set)
    set))

(defun destroy-event-set (set)
  (%destroy-event-set set))

(defmacro with-event-set ((set) &body body)
  "The usual. Makes sure the O/S specific part is taken care of."
  `(let (,set)
     (unwind-protect
	  (progn
	    (setf ,set (create-event-set))
	    ,@body)
       (when ,set (destroy-event-set ,set)))))

(defun add-event (event &optional (set *event-set*))
  "Add the event to the SET."
  (when (not (or set *event-set*))
    (setf *event-set* (create-event-set)
	  set *event-set*))
  (push event (event-set-list set))
  (%add-event event set))

(defun delete-event (event &optional (set *event-set*))
  "Delete the event from the SET."
  (when (not set)
    (error "Event set has no events."))
  (%delete-event event set)
  (setf (event-set-list set) (delete event (event-set-list set))))

(defun clear-triggers (&optional (set *event-set*))
  "Clear the triggers for the event SET."
  (%clear-triggers set)
  (mapcar (_ (setf (os-event-triggered _) nil)) (event-set-list set)))

(defosfun await-events (&key (event-types t) (event-set *event-set*) timeout
			     (leave-triggers nil))
  "Wait for events of the given EVENT-TYPES, or T for any event. Return if we
don't get an event before TIMEOUT. TIMEOUT can be NIL wait potentially forever,
or T to return immediately, otherwise it's a OS-TIME. The default for TIMEOUT
is NIL. If LEAVE-TRIGGERS it T, it will not clear triggers in the EVENT-SET,
that were set before being invoked.")

(defosfun pick-events (event-types &key (event-set *event-set*) remove timeout)
  "Return any pending events of the types given in EVENT-TYPES. If REMOVE is
true, remove the events from the EVENT-SET. Return if there aren't any events
before TIMEOUT. TIMEOUT can be NIL wait potentially forever, or T to return
immediately, otherwise it's a OS-TIME. The default for TIMEOUT is NIL.")

(defosfun map-events (function &key (event-set *event-set*) (event-types t))
  "Call FUNCTION for each event in EVENT-SET, that is pending or triggered.
EVENT-TYPES restricts the events mapped to those types.")

(defosfun events-pending-p (&key (event-types t) (event-set *event-set*))
  "Return true if there are any events pending in the EVENT-SET. Restrict the
events considered to those in EVENT-TYPES, if it's not T.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-like

;; @@@ Just use bordeaux-threads!
;; locks (mutexes)
;; create thread
;; join thread

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration???
;; is this even a good idea

;; WARNING!: These are low level and should only be the last step in user level
;; shutdown commands. They don't do the complicated system specfic things to
;; shut down. Don't blame me if you get data loss from using these!

(defosfun system-power-off ()
  "Stop the system and remove power.")

(defosfun system-restart ()
  "Reboot the system.")

(defosfun system-suspend ()
  "Suspend or hibernate the system, if possible.")

;; swapon
;; mincore
;; acct
;; adjtime

;; Filesystems:
;; mount/unmount
;; quotactl

;; System independant interface?

(defosfun mounted-filesystems ()
  "Return a list of filesystem info.")

(defosfun get-filesystem-info (file)
  "Try to return the filesystem-info of FILE. This might not always be right.")

(defosfun mount-point-of-file (file)
  "Try to find the mount of FILE. This might not always be right.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminals

(defosfun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal.")

(defosfun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD.")

(defosvar *default-console-device-name* "Name of the default console device.")

(defosfun open-terminal (device-name direction)
  "Open a terminal. Return the system file handle.")

(defosfun close-terminal (terminal-handle)
  "Close a terminal.")

(defosfun read-terminal-char (terminal-handle &key timeout)
  "Return a character read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the character read or NIL if it
the timeout is hit.")

(defosfun read-terminal-byte (terminal-handle &key timeout)
  "Return an unsigned byte read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites. Returns the byte read or NIL if it
the timeout is hit.")

(defosfun read-until (tty stop-char &key timeout)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor.")

(defosfun write-terminal-char (terminal-handle char)
  "Write CHAR to the terminal designated by TERMINAL-HANDLE.")

(defosfun write-terminal-string (terminal-handle string)
  "Write STRING to the terminal designated by TERMINAL-HANDLE.")

(defosfun slurp-terminal (tty &key timeout)
  "Read until EOF. Return a string of the results. TTY is a file descriptor.")

(defosfun set-terminal-mode (tty &key (echo    nil echo-supplied)
					(line    nil line-supplied)
					(raw     nil raw-supplied)
					(timeout nil timeout-supplied)
					(mode    nil mode-supplied))
  "Set the terminal mode. Arguments are:
  ECHO makes input automatically output back, so you can see what you typed.
  LINE makes input wait for a newline until returning.
  RAW ingores normal processing, like interrupt keys.
  TIMEOUT is the time in milliseconds to wait before returning with no input.
  MODE is a TERMINAL-MODE structure to take settings from.
The individual settings override the settings in MODE.")

(defosfun get-terminal-mode (tty)
  "Return a TERMINAL-MODE structure with the current terminal settings.")

(defosfun get-window-size (tty-fd)
  "Get the window size. The first value is columns, second value is rows.")

(defosfun reset-terminal-modes (&key file-descriptor device)
  "Set the terminal modes to a normal starting state.")

(defosfun drain-terminal (fd)
  "Wait for output written to ‘fd’ to be transmitted.")

(defosfun flush-terminal (fd which)
  "Discard data on ‘fd’. ‘which’ can be one of:
 :input     Discard data received but not read.
 :output    Discard data written but not transmitted.
 :both      Discard both of the above.")

(defosfun terminal-time (tty-fd)
  "Return the last modification time for the terminal device or NIL if we can't
get it.")

(defosfun query-terminal (query &key max)
  "Output the string to the terminal and wait for a response. Read up to MAX
characters. If we don't get anything after a while, just return what we got.")

(defalias 'terminal-query 'query-terminal) ;; Remove this some day.

;; @@@ Fix the duplication in termios.lisp
(defmacro with-terminal-mode ((tty) &body body)
  "Evaluate the body, retoring terminal mode changes on exit."
  (with-names (mode)
    `(let ((,mode (get-terminal-mode ,tty)))
       (unwind-protect
	    (progn ,@body)
	 (set-terminal-mode ,tty :mode ,mode)))))

(defosfun with-terminal-signals (() &body body)
  "Evaluate the BODY with signal handlers set appropriately for reading from
a terminal.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication / Network

(defosfun network-host-name ()
  "Return the netowrk host name.")

(defosfun network-domain-name ()
  "Return the network domain name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil

;; Debugging

(defvar *traced-process-id* nil
  "The process currently being traced.")

(defosfun attach-process (process-id)
  "")

(defosfun detach-process (process-id)
  "")

(defmacro with-attached-process ((process-id) &body body)
  `(let ((*traced-process-id* ,process-id))
     (unwind-protect
	  (progn
	    (attach-process *traced-process-id*)
	    ,@body)
	 (detach-process *traced-process-id*))))

(defosfun process-peek (address &key (pid *traced-process-id*))
  "")

(defosfun process-poke (address data &key (pid *traced-process-id*))
  "")

(defosfun process-registers (&key (pid *traced-process-id*))
  "")

(defosfun set-process-registers (register-set &key (pid *traced-process-id*))
  "")

(defosfun process-stop (&key (pid *traced-process-id*))  ;; probably just signal?
  "")

(defosfun process-continue (&key (pid *traced-process-id*))
  "")

(defosfun process-step (&key (pid *traced-process-id*))
  "")

;; Weird/simulation/emulation/API munging:
;; syscall

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(defosfun language ()
  "Return the system's idea of the language for communicating with the user.")

(defun setup-locale-from-environment ()
  "Do appropriate setlocale calls based on the current settings of LC_*
environment variables."
  (loop
    :for f :in libc::+lc-env-type+ :do
       (setlocale (car f) (or (environment-variable (cdr f)) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous

;; Not exactly an operating system function, but implementation specific
(defun exit-lisp (&key code abort timeout)
  "Halt the entire Lisp system." ;; But not necessarily the operating system.
  (declare (ignorable code timeout abort))
  (when (not code)
    (setf code (if abort 1 0)))
  #+openmcl (ccl::quit code)
  #+ccl (ccl::quit code)
  #+cmu (ext:quit)
  ;; #+sbcl (sb-ext:quit)
  #+sbcl (sb-ext:exit :code code :abort abort :timeout timeout)
  #+excl (excl:exit code)
  #+clisp (funcall 'ext:quit)
  #+ecl (ext:quit code)
  #+abcl (ext:quit :status code)
  #-(or openmcl ccl cmu sbcl excl clisp ecl abcl)
  (missing-implementation 'exit-lisp))

;; This isn't really OS specific, but implementation specific.
;(defun stream-file-name

;; Go thru *features* and get rid of all our temporary configuration.
(setf *features*
       (delete-if #'(lambda (x)
 		      (let ((s (princ-to-string x)))
 			(string= s "OS-T-" :end1 (min 5 (length s)))))
 		 *features*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EOF
