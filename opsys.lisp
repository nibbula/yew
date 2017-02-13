;;;
;;; opsys.lisp - Interface to operating systems
;;;

;; This is for system independent functions.
;;
;; System independent functions should choose one of:
;;
;;  - Be fully implemented in the system specific package, and be re-exported
;;    by this package.
;;  - Be partially implemented in this package and use appropriate functions
;;    in the system specific package, likely conditionalized by features.
;;  - Be fully implemented in this package, if there's little variance
;;    between systems
;;  - Be in implementd in a language specific module (e.g. libc.lisp)
;;    if it's something that would be found in a standard library for that
;;    language. We would like these to be optional.
;;  - Be implemented in opsys-base, if they are needed to be used by the
;;    system specific packages, and are generic enough.
;;
;; Conventions:
;;
;;  - Call anything defined by defcstruct like: foreign-<C struct Name> This
;;    hopefully makes it more obvious that you are dealing with a foreign
;;    struct instead of a Lisp struct.
;;
;;  - In foreign-* structs, use the C names, e.g. with underscores, for slot
;;    names, (e.g. "tv_usec").  If the C equivalent would be StudlyCapped,
;;    like on windows, do that.This make it easier to translate from C code.
;;
;;  - If there's a C struct that callers need to access, provide a lisp struct
;;    instead. This avoids having to access it carefully with CFFI macros,
;;    memory freeing issues, and type conversion issues.
;;
;;  - Put +earmuffs+ on constants. Put *earmuffs* on variables.

;; (declaim (optimize (speed 3)) (optimize (safety 0))
;;   	 (optimize (debug 0)) (optimize (space 0))
;;    	 (optimize (compilation-speed 0)))

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(in-package :opsys)

(defmacro defosthing (name type &optional doc)
  "Import a thing from the proper OS specific package and set it's
documenatation."
  (let ((sym (intern (symbol-name name) #+unix :os-unix #+windows :os-ms)))
    `(progn
       (import '(,sym))
       (when ,doc
	 (setf (documentation ',name ,type) ,doc)))))

(defmacro defosfun (name lambda-list &optional doc)
  (declare (ignore lambda-list))
  `(defosthing ,name 'function ,doc))

(defmacro defosvar (name &optional doc)
  `(defosthing ,name 'variable ,doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

(defosfun error-message (error-code)
  "Return a string or something describing the ERROR-CODE. We really make very
little claims about this function, but it should do what it's reasonable to
expect. Like for example on a Unix system it should be like strerror.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental information

;; Are these all the arguements? Maybe not. Maybe it's just the args which
;; wheren't processed. If you give it ALL-P true then hopefully it is.
;; @@@ Actually I think it might be good to have LISP-ARGS be able to return
;; all the arguments.
(defun lisp-args (#| &key all-p |#)
  "Arguments given when starting the Lisp system."
  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  #+cmu ext:*command-line-strings*
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments) 
  #+ecl (ext:command-args)
  #-(or sbcl clisp cmu openmcl excl ecl)
  (missing-implementation 'lisp-args))

;; This is really an obsolescent thing.
(defosfun memory-page-size ()
  "Get the system's memory page size, in bytes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sysconf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User database
;; 

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

(defosfun get-file-info (path &key (follow-links t))
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself.")

;; (defmacro with-temp-file ((var &optional template) &body body)
;;   "Evaluate the body with the variable VAR bound to a POSIX file descriptor with a temporary name. The file is supposedly removed after this form is done."
;;   (unwind-protect (progn @@@

;; I would like to have:
;;
;; (defun get-stream-file-name (stream)
;;   (with-foreign-string (path MAXPATHLEN)
;;     (syscall (fcntl F_GETPATH path)))
;;   )
;;
;; On sbeecil:
;; SB-IMPL::FD-STREAM-PATHNAME
;; SB-IMPL::FD-STREAM-FILE
;; SB-SYS:FD-STREAM-FD (fbound)
;; SB-SYS:FD-STREAM-P (fbound)
;;
;; On linux:
;; (readlink (format nil "/proc/~a/fd/~a" (getpid) fd))
;; ssize_t readlink(const char *path, char *buf, size_t bufsiz);
;; Very unreliable and hackish.
;;
;; Windows:
;;
;; (defcfun GetFileInformationByHandleEx BOOL #| WINAPI |#
;;  (hFile HANDLE) ;; In
;;  (FileInformationClass FILE_INFO_BY_HANDLE_CLASS) ;; In
;;  (lpFileInformation LPVOID)  ;; Out
;;  (dwBufferSize DWORD)  ;; In
;; )
;;
;; GetFileInformationByHandleEx  FileNameInfo,
;; (defcstruct _FILE_NAME_INFO
;;  DWORD FileNameLength;
;;  WCHAR FileName[1];
;; } FILE_NAME_INFO, *PFILE_NAME_INFO;
;;
;; typedef enum _FILE_INFO_BY_HANDLE_CLASS { 
;;   FileBasicInfo                   = 0,
;;   FileStandardInfo                = 1,
;;   FileNameInfo                    = 2,
;;   FileRenameInfo                  = 3,
;;   FileDispositionInfo             = 4,
;;   FileAllocationInfo              = 5,
;;   FileEndOfFileInfo               = 6,
;;   FileStreamInfo                  = 7,
;;   FileCompressionInfo             = 8,
;;   FileAttributeTagInfo            = 9,
;;   FileIdBothDirectoryInfo         = 10, // 0xA
;;   FileIdBothDirectoryRestartInfo  = 11, // 0xB
;;   FileIoPriorityHintInfo          = 12, // 0xC
;;   FileRemoteProtocolInfo          = 13, // 0xD
;;   FileFullDirectoryInfo           = 14, // 0xE
;;   FileFullDirectoryRestartInfo    = 15, // 0xF
;;   FileStorageInfo                 = 16, // 0x10
;;   FileAlignmentInfo               = 17, // 0x11
;;   FileIdInfo                      = 18, // 0x12
;;   FileIdExtdDirectoryInfo         = 19, // 0x13
;;   FileIdExtdDirectoryRestartInfo  = 20, // 0x14
;;   MaximumFileInfoByHandlesClass
;; } FILE_INFO_BY_HANDLE_CLASS, *PFILE_INFO_BY_HANDLE_CLASS;

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
     (ccl::ioblock-device
      (ccl::basic-stream-ioblock
       (slot-value stream (if (eql direction :output)
			      'ccl:output-stream
			      'ccl:input-stream))))))
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
    ((typep stream 'stream)
     (multiple-value-bind (in out) (socket:stream-handles stream)
       (if (eql direction :output)
	   out in))))
  #+lispworks nil
  #+abcl nil
  #+ecl (declare (ignore direction))
  #+ecl (and (typep stream 'file-stream) (ext:file-stream-fd stream))
  #-(or ccl sbcl cmu clisp lispworks abcl ecl)
  (missing-implementation 'stream-system-handle))

;; Sadly I find the need to do this because probe-file might be losing.
(defosfun file-exists (filename)
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long.")

(defosfun simple-delete-file (pathname)
  "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams.")

(defosfun with-os-file ((var filename &key
			     (direction :input)
			     (if-exists :error)
			     (if-does-not-exist :error)) &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

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

(defosfun change-directory (&optional path)
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given.")

(defosfun current-directory ()
  "Return the full path of the current working directory as a string.")

(defosfun make-directory (path &key (mode #o755))
  "Make a directory.")

(defosfun delete-directory (path)
  "Delete a directory.")

(defosfun probe-directory (dir)
  "Something like probe-file but for directories.")

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

;; This is a workaround for not depending on split-sequence.
;; so instead of (split-sequence *directory-separator* p :omit-empty t)
(defun split-path (path)
  "Return a list of components of PATH."
  (let (result (len (length path)))
    (when (and (plusp len)
	       (char= (char path 0) *directory-separator*))
      (setf result '("/")))
    (if (zerop len)
	(list path)
	(append result
		(loop :with i = 0 :and piece
		   :while (< i len) :do
		   (setf piece
			 (with-output-to-string (str)
			   (loop :while (and (< i len)
					     (char/= (char path i)
						     *directory-separator*))
			      :do
			      (princ (char path i) str)
			      (incf i))))
		   :if (and piece (/= (length piece) 0))
		   :collect piece
		   :do (incf i))))))

(defun path-to-absolute (path)
  "Return the PATH converted into an absolute path."
  ;; Make sure path is a string.
  (setf path (etypecase path
	       (null (return-from path-to-absolute nil))
	       (string path)
	       (pathname (safe-namestring path))))
  (let* ((p (if (and (plusp (length path))
		     (char= *directory-separator* (char path 0)))
		path			; already absolute
		(concatenate 'string (current-directory) "/" path)))
	 (pp (split-path p)))
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
      ;; Get rid of relative elemets, "." and ".."
      (get-rid-of "." pos)
      (get-rid-of ".." (1- pos)))
    (if (<= (length pp) 1)
	"/"
	(with-output-to-string (str)
	  (loop :for e :in (cdr pp) :do
	     (write-char *directory-separator* str)
	     (write-string e str))))))

(setf (symbol-function 'abspath) #'path-to-absolute)

(defun clip-path (path side)
  "Return the directory portion of a path."
  ;; Go backwards from the end until we hit a separator.
  (let ((i (1- (length path))))
    (loop :while (and (>= i 0) (char/= *directory-separator* (char path i)))
       :do (decf i))
;    (dlib:dbug "i = ~s~%" i)
    (if (eq side :dir)
	(if (< i 0)
	    (subseq path 0 0)
	    (if (and (zerop i) (char= (char path 0) *directory-separator*))
		(subseq path 0 1)
		(subseq path 0 i)))
	(if (< i 0)
	    path
	    (subseq path (1+ i))))))

(defun path-directory-name (path)
  "Return the directory portion of a PATH. This is similar to DIRECTORY-NAMESTRING."
  (clip-path (or (and (pathnamep path) (safe-namestring path)) path) :dir))
(setf (symbol-function 'dirname) #'path-directory-name)

(defun path-file-name (path)
  "Return the last portion of a PATH. This is similar to FILE-NAMESTRING."
 (clip-path (or (and (pathnamep path) (safe-namestring path)) path) :file))
(setf (symbol-function 'basename) #'path-file-name)

;;*directory-separator*

(defun path-append (first-path &rest paths)
  "Append the elements PATHS to FIRST-PATH. Put a directory separator between
them if there isn't one already."
  (when (not (or (stringp first-path) (pathnamep first-path)))
    (error "FIRST-PATH should be pathname designator."))
  (flet ((trailing-separator-p (s)
	   (char= (aref s (1- (length s))) *directory-separator*)))
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
	     (error "Elements in PATHS should be pathname designators."))
	   (setf ns (safe-namestring p))
	   (when (not (zerop (length ns)))
	     (when (and any (not last-was-separator)
			(char/= (aref ns 0) *directory-separator*))
	       (princ *directory-separator* str))
	     (setf last-was-separator (trailing-separator-p ns))
	     (princ ns str)
	     (setf any t)))))))

(defosfun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden.")

(defosfun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stupid file locking
;;
;; Supposedly making a directory is atomic even on shitty networked
;; filesystems. NOT thread safe, yet.

(defvar *lock-suffix* ".lck"
  "What to append to a path to make the lock name.")

(defun lock-file-name (pathname)
  "Return the name of the lock file for PATHNAME."
  (path-append (or (path-directory-name pathname) "")
	       (concatenate 'string (path-file-name pathname) *lock-suffix*)))

(defun lock-file (pathname lock-type timeout increment)
  "Lock PATHNAME."
  (declare (ignore lock-type))
  ;; @@@ perhaps we should add u+x, even though it's mostly pointless,
  ;; but just so things that traverse the filesystem won't get stupid
  ;; permission errors.
  (let ((mode (file-status-mode (stat (safe-namestring pathname))))
	(filename (lock-file-name pathname))
	(time 0))
    ;; Very lame and slow polling.
    (loop :with wait :and inc = increment
       :do
       (if (not (ignore-errors (make-directory filename :mode mode)))
	   (if (= *errno* +EEXIST+)
	       (setf wait t)
	       (error-check -1 "lock-file: ~s" filename))
	   (setf wait nil))
       ;; (when wait
       ;; 	 (format t "Waiting...~d~%" time))
       :while (and wait (< time timeout))
       :do (sleep inc) (incf time inc))
    (when (>= time timeout)
      (error "Timed out trying to lock file ~s" pathname)))
  t)

(defun unlock-file (pathname)
  "Unlock PATHNAME."
  (let ((filename (lock-file-name (safe-namestring pathname))))
    (when (file-exists filename)
      (delete-directory filename)
      ;; (format t "Unlocked~%")
      (sync))))

(defmacro with-locked-file ((pathname &key (lock-type :write) (timeout 3)
				      (increment .1))
			    &body body)
  ;; @@@ Need to wrap with recursive thread locks
  (with-unique-names (locked)
    `(let ((,locked nil))
       (unwind-protect
	    (progn
	      (setf ,locked
		    (lock-file ,pathname ,lock-type ,timeout ,increment))
	      ,@body)
	 (when ,locked
	   (unlock-file ,pathname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Commands?

(defosfun is-executable (path &optional user)
  "Return true if the PATH is executable by the UID. UID defaults to the
current effective user.")

(defun has-directory-p (path)
  "Return true if PATH has a directory part."
  (position *directory-separator* path))

;; @@@ Maybe this is portable and should be moved to opsys.lisp?
(defun command-pathname (cmd)
  "Return the full pathname of the first executable file in the PATH or nil
if there isn't one."
  (when (has-directory-p cmd)
    (return-from command-pathname cmd))
  (loop :for dir :in (split-sequence *path-separator*
				     (environment-variable *path-variable*))
     :do
     (handler-case
       (when (probe-directory dir)
	 (loop :with full = nil
	    :for f :in (read-directory :dir dir) :do
	    ;; (format t "~s~%" f)
	    (when (and (equal f cmd)
		       (is-executable
			(setf full (format nil "~a~c~a"
					   dir *directory-separator* cmd))))
	      (return-from command-pathname full))))
       (error (c) (declare (ignore c)))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

;; @@@ This is very misleading because you would think that the args
;; would end up as separate arguments, but may not.

(defun system-command (cmd &optional args)
  "Run a system command. The command is generally given to whatever the
 system shell would be and the output and input are to the standard
 places."
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
(defun run-program (cmd args &key (environment nil env-p))
;  #+(or clisp sbcl ccl) (fork-and-exec cmd args)
  #+clisp (declare (ignore environment env-p))
  #+clisp (ext:run-program cmd :arguments args)
  #+excl (excl:run-shell-command (concatenate 'vector (list cmd cmd) args)
				 :wait t)
  #+(and (or openmcl ccl) unix) (apply #'os-unix:fork-and-exec
				       `(,cmd ,args
					      ,@(when env-p :env environment)))
#|  #+(or openmcl ccl)
  (let* ((proc
#|	  (ccl::run-program cmd args
			    :sharing :external
			    :input t
			    :output t
			    :error t
			    :wait t) |#
	   (apply #'ccl::run-program
		  `(,cmd ,args
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
|#
  #+(and sbcl (not unix))
  (sb-ext:process-exit-code
   (apply #'sb-ext:run-program
	  `(,cmd ,args
		 ,@(when env-p `(:environment ,environment))
		 :search t :output t :input t :error t :pty nil)))
  #+(and sbcl unix)
  (apply #'os-unix::forky
	 `(,cmd ,args
		,@(when env-p `(:environment ,environment))))
  #+cmu (ext:process-exit-code
	 (apply #'ext:run-program
		 `(,cmd ,args
		   ,@(when env-p :environment environment)
		   :wait t :output t :input t :error t :pty nil)))
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (system:run-shell-command
		   (concatenate 'vector (list cmd) args)
		   :output :stream
		   #| :wait t |#)
		result)
  #+ecl
  (multiple-value-bind (result ret-code proc)
      (apply #'ext:run-program
	     `(,cmd ,args
		    ,@(when env-p `(:environ ,environment))
		    :output t :input t))
    (declare (ignore result proc))
    ret-code)

  #+abcl
  (let* ((proc (apply #'sys:run-program
		      `(,cmd ,args
			     ,@(when env-p :environment environment))))
	 (out (system:process-output proc)))
    (dlib:copy-stream out *standard-output*)
    (finish-output *standard-output*)
    (system:process-exit-code proc))
  #-(or clisp excl openmcl sbcl cmu lispworks ecl abcl)
  (missing-implementation 'run-program)
)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC

(defun environ-to-string-list (env)
  "Convert a keyworded alist environment to a list of strings with #\=.
Just return ENV if it doesn't seem like an alist."
  (or (and env
	   (listp (car env))
	   (keywordp (caadr env))
	   (loop :for (a . b) :in env
	      :collect (format nil "~a=~a" a b)))
      env))

;; pipes

;; @@@@ We should make sure it's portable!
;; @@@ add environment on other than sbcl
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
			,@(when in-stream `(:input ,in-stream))
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
		     `(,cmd ,args :wait nil :input t
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

;; This is probably best provided in relation to some kind of "event loop"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poll

(defosfun listen-for (seconds &optional (fd 0))
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-like

;; @@@ Just use bordeaux-threads!
;; locks (mutexes)
;; create thread
;; join thread

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration???
;; is this even a good idea

;; reboot
;; swapon
;; mincore
;; acct
;; settimeofday
;; adjtime

;; Filesystems:
;; mount/unmount
;; quotactl
;; fsstat?

;; System independant interface?

(defosfun mounted-filesystems ()
  "Return a list of filesystem info.")

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

(defosfun open-terminal (device-name)
  "Open a terminal. Return the system file handle.")

(defosfun close-terminal (terminal-handle)
  "Close a terminal.")

(defosfun read-terminal-char (terminal-handle &key timeout)
  "Return a character read from the terminal TERMINAL-HANDLE.
If there's a problem, it will signal a READ-CHAR-ERROR. If the terminal is
resized it will signal an OPSYS-RESIZED. If the program is continued from
being suspended, it will signal an OPSYS-RESUMED. Usually this means the
caller should handle these possibilites.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil
;; ptrace

;; Weird/simulation/emulation/API munging:
;; syscall

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
  (loop :with e = nil
	:for f :in +lc-env-type+
	:do
	(when (setf e (getenv (cdr f)))
	  (setlocale (car f) e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Not exactly an operating system function, but implementation specific
(defun exit-lisp ()
  "Halt the entire Lisp system." ;; But not necessarily the operating system.
  #+openmcl (ccl::quit 0)
  #+cmu (ext:quit)
;  #+sbcl (sb-ext:quit)
  #+sbcl (sb-ext:exit)
  #+excl (excl:exit)
  #+clisp (funcall 'ext:quit)
  #+ecl (ext:quit)
  #-(or openmcl cmu sbcl excl clisp ecl) (missing-implementation 'exit-lisp)
  )

;; This isn't really OS specific, but implementation specific.
;(defun stream-file-name

;; Go thru *features* and get rid of all our temporary configuration.
; (setf *features*
;       (delete-if #'(lambda (x)
; 		     (let ((s (string x)))
; 		       (string= s "OS-T-" :end1 (min 5 (length s)))))
; 		 *features*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @@@ Should get rid of temporary features :os-t-*

;; EOF
