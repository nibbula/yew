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
;;    between systems, such as in C stdlib.
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

;; The without-warning is overkill, so don't screw up, or comment it out to
;; check for real problems. Otherwise, certain complainy implementatations,
;; don't take kindly to us re-exporting things from opsys-base.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (without-warning
(defpackage :opsys
  (:documentation "Generic interface to operating system functionality.")
  (:nicknames :nos)
  (:use :cl :cffi :dlib :opsys-base
	#+unix :os-unix #+unix :termios
	#+(and windows (not unix)) :os-ms)
  (:export
   ;; errors
   #:error-message
   
   ;; info
   #:environment
   #:environment-variable
   #:lisp-args
   #:memory-page-size

   #:user-home
   #:user-name-char-p
   #:valid-user-name
   #:user-name
   #:user-id
   #:user-full-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   #:users-logged-in

   #:group-name
   #:group-id
   #:get-next-group
   #:group-list
   #:refresh-group-list

   ;; directories
   #:change-directory
   #:current-directory
   #:in-directory
   #:make-directory
   #:delete-directory
   #:read-directory
   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode
   #:without-access-errors
   #:probe-directory
   #:path-to-absolute #:abspath
   #:path-directory-name #:dirname
   #:path-file-name #:basename
   #:path-append
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:command-pathname
   #:quote-filename
   #:safe-namestring

   ;; files
   #:get-file-info
   #:stream-system-handle
   #:file-exists

   ;; Stupid cooperative locking
   #:with-locked-file
   
   ;; processes
   #:system-command
   #:run-program
   #:pipe-program
   #:with-process-output
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:is-executable
   #:command-pathname
   #:process-times
   #:process-list

   ;; polling
   #:listen-for

   ;; filesystems
   #:mounted-filesystems
   #:mount-point-of-file

   ;; terminals
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:open-terminal
   #:close-terminal
   #:slurp-terminal
   #:read-terminal-char
   #:write-terminal-char
   #:write-terminal-string
   #:set-terminal-mode
   #:get-terminal-mode
   #:get-window-size
   #:*default-console-device-name*
   
   ;; character coding / localization (or similar)
   #:char-width
   #:setlocale
   #:setup-locale-from-environment

   ;; misc
   #:exit-lisp
   #:missing-implementation

   ;; stdio
   #:*stdin* #:*stdout* #:*stderr*
   #:fopen #:fclose #:fileno #:fflush
   #:fgetc #:getc #:getchar #:fgets #:gets
   #:printf #:fprintf #:sprintf #:snprintf
   #:fputc #:putc #:putchar #:fputs #:puts
   #:fread #:fwrite
   #:fscanf #:scanf #:sscanf
   #:fsetpos #:fgetpos #:fseek #:ftell
   #:perror #:setbuf #:ungetc

   ;; ctype
   #:iswalnum #:iswalpha #:iswascii #:iswblank #:iswcntrl #:iswdigit
   #:iswgraph #:iswhexnumber #:iswideogram #:iswlower #:iswnumber
   #:iswphonogram #:iswprint #:iswpunct #:iswrune #:iswspace #:iswspecial
   #:iswupper #:iswxdigit

   #:isalnum #:isalpha #:isascii #:isblank #:iscntrl #:isdigit #:isgraph
   #:ishexnumber #:isideogram #:islower #:isnumber #:isphonogram #:isprint
   #:ispunct #:isrune #:isspace #:isspecial #:isupper #:isxdigit

   ;; stdlib
   #:system
   ))
)) ;; without-warning
(in-package :opsys)

;; Re-export things from opsys-base

(do-external-symbols (sym :opsys-base)
  (export sym :opsys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

#+unix (import '(os-unix:error-message))
#+windows (import '(ms:error-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental information

(defun lisp-args ()
  "Arguments given to when starting the lisp system."
  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  #+cmu ext:*command-line-strings*
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments) 
  #+ecl (ext:command-args)
  #-(or sbcl clisp cmu openmcl excl ecl)
  (missing-implementation 'lisp-args))

#+unix (import '(os-unix:memory-page-size))
#+windows (import '(ms:memory-page-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sysconf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User database
;; 

#+unix (import '(os-unix:user-home
		 os-unix:user-name
		 os-unix:user-id
		 os-unix:user-full-name
		 os-unix:user-name-char-p
		 os-unix:valid-user-name
		 os-unix:get-next-user
		 os-unix:user-list
		 os-unix:refresh-user-list
		 os-unix:is-administrator
		 os-unix:users-logged-in
		 ))

#+ms (import '(ms:user-home
	       ms:user-name
	       ms:user-id
	       ms:user-full-name
	       ms:user-name-char-p
	       ms:valid-user-name
	       ms:get-next-user
	       ms:user-list
	       ms:refresh-user-list
	       ms:is-administrator
	       ms:users-logged-in
	       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group database
;; 

#+unix (import '(os-unix:group-name
		 os-unix:group-id
		 os-unix:get-next-group
		 os-unix:group-list
		 os-unix:refresh-group-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

#+unix (import '(os-unix:get-file-info))
#+ms (import '(ms:get-file-info))

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
  #+ecl (and (typep stream 'file-stream) (ext:file-stream-fd stream))
  #-(or ccl sbcl cmu clisp lispworks abcl ecl)
  (missing-implementation 'stream-system-handle))

;; Sadly I find the need to do this because probe-file might be losing.
#+unix (import 'os-unix:file-exists)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

#+unix (import '(os-unix:read-directory
		 os-unix:change-directory
		 os-unix:current-directory
		 os-unix:make-directory
		 os-unix:delete-directory
		 os-unix:probe-directory
		 os-unix:without-access-errors))

(defmacro in-directory ((dir) &body body)
  "Evaluate the body with the current directory set to DIR."
  (let ((%old-dir (gensym "old-dir")))
  `(let ((,%old-dir (current-directory)))
     (unwind-protect
       (progn
         (change-directory ,dir)
         ,@body)
       (change-directory ,%old-dir)))))

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
	  (if (or ;; They keep changing this shit!!
	       (and (function-defined '#:make-directory :posix)
		    (function-defined '#:delete-directory :posix))
	       (and (function-defined '#:make-directory :ext)
		    (function-defined '#:delete-directory :ext)))
	      (config-feature :os-t-has-new-dir)))

;; This is a workaround for not depending on split-sequence.
;; so instead of (split-sequence *directory-separator* p :omit-empty t)
(defun split-path (p)
  (loop :with i = 0 :and piece
     :while (< i (length p))
     :do (setf piece nil)
     (setf piece
	   (with-output-to-string (str)
	      (loop :while (and (< i (length p))
				(char/= (char p i) *directory-separator*))
		 :do
		 (princ (char p i) str)
		 (incf i))))
     :if (and piece (/= (length piece) 0))
     :collect piece
     :do (incf i)))

(defun path-to-absolute (path)
  "Return the PATH converted into an absolute path."
  ;; Make sure path is a string.
  (setf path (etypecase path
	       (null (return-from path-to-absolute nil))
	       (string path)
	       (pathname (safe-namestring path))))
    (let* ((p (if (char= *directory-separator* (char path 0))
		 path			; already absolute
		 (concatenate 'string (current-directory) "/" path)))
	   ;; (pp (split-sequence *directory-separator* p :omit-empty t)))
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
	;; Get rid of ".."
;	(dbug "starting with ~s~%" pp)
	(get-rid-of "." pos)
;	(dbug "after . ~s~%" pp)
	(get-rid-of ".." (1- pos))
;	(dbug "after .. ~s~%" pp)
	)
      (if (zerop (length pp))
	  "/"
	  (apply #'concatenate 'string
		 (loop :for e :in pp :collect "/" :collect e)))))

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

#+unix (import '(os-unix:hidden-file-name-p
		 os-unix:superfluous-file-name-p))

#+ms (import '(ms:hidden-file-name-p
	       ms:superfluous-file-name-p))

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

#+unix (import 'os-unix:is-executable)

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
  #+clisp (ext:run-shell-command (format nil "~a~{ ~a~}" cmd args))
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
(defun run-program (cmd &optional args (environment nil env-p))
;  #+(or clisp sbcl ccl) (fork-and-exec cmd args)
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
  #+sbcl (sb-ext:process-exit-code
	  (apply #'sb-ext:run-program
		 `(,cmd ,args
		   ,@(when env-p :environment environment)
		   :search t :output t :input t :error t :pty nil)))
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
  #+abcl
  (let* ((proc (apply #'sys:run-program
		      `(,cmd ,args
			     ,@(when env-p :environment environment))))
	 (out (system:process-output proc)))
    (dlib:copy-stream out *standard-output*)
    (finish-output *standard-output*)
    (system:process-exit-code proc))
  #-(or clisp excl openmcl sbcl cmu lispworks abcl)
  (missing-implementation 'run-program)
)

;; (defun get-groups ()
;;   "Return an array of group IDs for the current process."
;;   ;; @@@@
;;   )

#+unix (import '(os-unix:suspend-process
		 os-unix:resume-process
		 os-unix:terminate-process
		 os-unix:process-times
		 os-unix:process-list))

#+ms (import '(ms:suspend-process
	       ms:resume-process
	       ms:terminate-process
	       ms:process-list
	       ms:process-times))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC

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
			,@(when env-p `(:environment ,environment)))))
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
			    ,@(when env-p `(:env ,environment))))))
    (ccl::external-process-output-stream proc))
  
  #+ecl (ext:run-program cmd args)
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

;; It might be nice if could do this on a Lisp stream.
(defun listen-for (seconds &optional (fd 0))
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available."
;  (lame-poll `((,fd :read)) (truncate (* 1000 seconds)))
  (lame-select `((,fd :read)) seconds)
  )

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

#+unix (import '(os-unix:mounted-filesystems
		 os-unix:mount-point-of-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ttys

#+unix (import '(os-unix:file-handle-terminal-p
		 os-unix:file-handle-terminal-name
		 os-unix:*default-console-device-name*
		 os-unix:open-terminal
		 os-unix:close-terminal
		 os-unix:read-terminal-char
		 os-unix:write-terminal-char
		 os-unix:write-terminal-string
		 termios:slurp-terminal
		 termios:set-terminal-mode
		 termios:get-terminal-mode
		 termios:get-window-size))

#+windows (import '(ms:file-handle-terminal-p
		    ms:file-handle-terminal-name
		    ms:*default-console-device-name*
		    ms:open-terminal
		    ms:close-terminal
		    ms:slurp-terminal
		    ms:read-terminal-char
		    ms:set-terminal-mode
		    ms:get-terminal-mode
		    ms:get-window-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil
;; ptrace

;; Weird/simulation/emulation
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
;;
;; limited stdio & standard C library support
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
(defcfun iswascii :int (wc wint-t))
(defcfun iswblank :int (wc wint-t))
(defcfun iswcntrl :int (wc wint-t))
(defcfun iswdigit :int (wc wint-t))
(defcfun iswgraph :int (wc wint-t))
(defcfun iswhexnumber :int (wc wint-t))
(defcfun iswideogram :int (wc wint-t))
(defcfun iswlower :int (wc wint-t))
(defcfun iswnumber :int (wc wint-t))
(defcfun iswphonogram :int (wc wint-t))
(defcfun iswprint :int (wc wint-t))
(defcfun iswpunct :int (wc wint-t))
(defcfun iswrune :int (wc wint-t))
(defcfun iswspace :int (wc wint-t))
(defcfun iswspecial :int (wc wint-t))
(defcfun iswupper :int (wc wint-t))
(defcfun iswxdigit :int (wc wint-t))

(defcfun isalnum :int (c :int))
(defcfun isalpha :int (c :int))
(defcfun isascii :int (c :int))
(defcfun isblank :int (c :int))
(defcfun iscntrl :int (c :int))
(defcfun isdigit :int (c :int))
(defcfun isgraph :int (c :int))
(defcfun ishexnumber :int (c :int))
(defcfun isideogram :int (c :int))
(defcfun islower :int (c :int))
(defcfun isnumber :int (c :int))
(defcfun isphonogram :int (c :int))
(defcfun isprint :int (c :int))
(defcfun ispunct :int (c :int))
(defcfun isrune :int (c :int))
(defcfun isspace :int (c :int))
(defcfun isspecial :int (c :int))
(defcfun isupper :int (c :int))
(defcfun isxdigit :int (c :int))

;; @@@ Should get rid of temporary features :os-t-*

;; EOF
