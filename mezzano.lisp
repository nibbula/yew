;;
;; mezzano.lisp - Interface to Mezzano
;;

(defpackage :mezzano
  (:documentation "Interface to the Mezzaon operating system.")
  (:use :cl :dlib :opsys-base)
  (:export
   ;; Things that opsys imports:
   #:error-message
   #:environment
   #:environment-variable
   #:env
   #:memory-page-size
   #:processor-count
   #:get-user-info
   #:user-name
   #:user-home
   #:user-id
   #:user-full-name
   #:user-name-char-p
   #:valid-user-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   #:users-logged-in
   #:get-file-info
   #:file-exists
   #:os-delete-file
   #:with-os-file
   #:set-file-time
   #:make-symbolic-link
   #:read-directory
   #:map-directory
   #:change-directory
   #:current-directory
   #:make-directory
   #:delete-directory
   #:probe-directory
   #:directory-p
   #:without-access-errors
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:%path-absolute-p
   #:lock-file
   #:unlock-file
   #:with-locked-file
   #:is-executable
   #:config-dir
   #:data-path
   #:data-dir
   #:config-path
   #:cache-dir
   #:runtime-dir
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:process-times
   #:process-list
   #:system-process-list
   #:system-process-type
   #:wait-and-chill
   #:check-jobs
   #:get-time
   #:set-time
   #:listen-for
   #:%create-event-set
   #:%destroy-event-set
   #:%add-event
   #:%delete-event
   #:%clear-triggers
   #:await-events
   #:pick-events
   #:map-events
   #:events-pending-p
   #:mounted-filesystems
   #:mount-point-of-file
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:*default-console-device-name*
   #:open-terminal
   #:close-terminal
   #:read-terminal-char
   #:read-terminal-byte
   #:read-until
   #:listen-for-terminal
   #:write-terminal-char
   #:write-terminal-string
   #:slurp-terminal
   #:set-terminal-mode
   #:get-terminal-mode
   #:reset-terminal-modes
   #:terminal-query
   #:with-terminal-signals
   #:get-console-info
   #:get-window-size
   #:get-cursor-position
   #:get-cursor-info
   #:set-cursor-state
   #:set-cursor-position
   #:scroll-console
   #:fill-console-char
   #:fill-console-attribute
   #:get-attributes
   #:set-console-attribute
   #:+FOREGROUND-BLUE+ #:+FOREGROUND-GREEN+ #:+FOREGROUND-RED+
   #:+FOREGROUND-INTENSITY+ #:+BACKGROUND-BLUE+ #:+BACKGROUND-GREEN+
   #:+BACKGROUND-RED+ #:+BACKGROUND-INTENSITY+
   #:get-console-title #:set-console-title
   #:get-computer-name
   #:os-machine-instance
   #:os-machine-type
   #:os-machine-version
   #:os-software-type
   #:os-software-version
   ))
(in-package :mezzano)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mz/environment.lisp

;; Just make up our own environment for the sake of compatibility.
;; It seems like a pointless thing on a Lisp OS.

(defvar *fake-environment* nil
  (make-hash-table :test #'equal))

(defun environment ()
  (loop :for key :being :the :hash-keys :of hh
     :collect (list key (gethash key *fake-environment*))))

(defun environment-variable (name)
  (gethash name *fake-environment*))

(defalias 'env 'environment-variable)

(defun set-environment-variable (var value)
  (setf (gethash var *fake-environment*) value))

(defsetf environment-variable set-environment-variable
    "Set the environtment variable named VAR to the string VALUE.")

(defsetf env set-environment-variable
    "Set the environtment variable named VAR to the string VALUE.")

;; Just guessing for most of these.

(defun memory-page-size ()
  mezzano.supervisor::+4k-page-size+)

(defun processor-count ()
  (mezzano.supervisor::logical-core-count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mzos/users.lisp

;; Fake user stuff, from Windows for now.

(defun get-user-info (&key name id)
  (declare (ignore name id))
  (make-user-info :name "you"
		  :id 2001
		  :full-name "Y. Random Hacker"
		  :home-directory "e:\\"
		  :shell "lish"
		  :primary-group-id 1
		  :guid "123-456-789"
		  :picture "D E R P!"))

(defun user-name (&optional id)
  (declare (ignore id))
  "you")

(defun user-home (&optional (user (user-name)))
  (declare (ignore user))
  "e:\\")

(defun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user."
  (declare (ignore name effective))
  1024)

(defun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (declare (ignore id))
  "Y. Random Hacker")

(defun user-name-char-p (c)
  "Return true if C is a valid character in a user name."
  (alphanumericp c))

(defun valid-user-name (username)
  "Return true if USERNAME could be a valid user name, but not that the user
actually exists."
  (declare (ignore username))
  t)

(defun get-next-user ()
  "Return the next user structure from the user database."
  nil)

(defun user-list ()
  "Return the list of all users."
  (list (get-user-info)))

(defun refresh-user-list ()
  "Make GET-NEXT-GROUP or GROUP-LIST return potentially updated data."
  )

(defun is-administrator ()
  "Return true if you are root, or effectively root."
  t) 					; Ha! Yes!

(defun users-logged-in ()
  "Return a list of names of logged in users."
  "you")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mz/filesystem.lisp

;; So, you'll notice that using this metadata system can be lossy. I hate to
;; tell you, but that's just already the case with any files on almost every
;; operating system. The only way around that is to make sure every file
;; system storage written in the future allow for arbitrary metadata, like
;; this interface supports. I don't think the filesystems of the past can even
;; be patched to do so. I'm sure you can guess that won't happen. The best we
;; can do is make sure any storage system we create, supports arbitrary
;; metadata, and try deal with the differences. We should probably also be
;; able to give warnings or errors when metadata would be lost. In some cases
;; upper level software can work around this by putting metadata into the
;; normal file data, if the format supports it.

(defclass file-metadata-template ()
  ()
  (:documentation "Generic metadata."))

;; TYPE and FLAGS slots should only have things which can be reliably detected
;; on all systems and have nearly the same meaning and are useful.
(defclass base-file-metadata-template ()
  ((type
    :initarg :type :accessor metadata-template-type :initform nil :type file-type
    :documentation "Most general category of a file.")
   (size
    :initarg :size :accessor metadata-template-size :initform 0 :type integer
    :documentation "Size in bytes.")
   (flags ;; :hidden :immutable :compressed
    :initarg :flags :accessor metadata-template-flags :initform nil :type list
    :documentation "A list of keyword flags.")
   (creation-time
    :initarg :creation-time :accessor template-creation-time
    :initform nil :type dtime
    :documentation "When the file was created.")
   (access-time
    :initarg :access-time :accessor metadata-template-access-time
    :initform nil :type dtime
    :documentation "When the file was last accessed.")
   (modification-time
    :initarg :modification-time :accessor metadata-template-modification-time
    :initform nil :type dtime
    :documentation "When the file was last modified."))
  (:documentation "Least common denominator metadata, as in opsys:file-info."))

;; Supporting the things in uos:file-status
(defclass unix-file-metadata-template ()
  (device
   :initarg :device :accessor metadata-template-device
   :documentation "Device number.")
  (inode
   :initarg :inode :accessor metadata-template-inode :initform  :type 
   :documentation "Unix inode, which us usually an unique index number.")
  (mode
   :initarg :mode :accessor metadata-template-mode :initform 0 :type integer
   :documentation "Unix mode.")
  (links
   :initarg :links :accessor metadata-template-links :initform nil :type 
   :documentation "Number of hard links to the file.")
  (uid
   :initarg :uid :accessor metadata-template-uid
   :initform -1 :type integer
   :documentation "User identification number of the owner of the file.")
  (gid
   :initarg :gid :accessor metadata-template-gid
   :initform -1 :type integer
   :documentation "Group identification number of the group owning the file.")
  (device-type
   :initarg :device-type :accessor metadata-template-device-type
   :documentation "Device type code.")
  (access-time
   :initarg :access-time :accessor metadata-template-access-time
   ;;:initform nil :type uos:timespec
   :documentation "Time of last access.")
  (modify-time
   :initarg :modify-time :accessor metadata-template-modify-time
   ;;:initform nil :type uos:timespec
   :documentation "Time of last modification.")
  (change-time
   :initarg :change-time :accessor metadata-template-change-time
   ;;:initform nil :type uos:timespec
   :documentation "Time of last status change.")
  (birth-time
   :initarg :birth-time :accessor metadata-template-birth-time
   ;;:initform nil :type uos:timespec
   :documentation "Time the file was first created.")
  (size
   :initarg :size :accessor metadata-template-size
   :initform -1 :type integer
   :documentation "Size of the file in bytes.")
  (blocks
   :initarg :blocks :accessor metadata-template-blocks
   :initform -1 :type integer
   :documentation "Number of 512 byte blocks allocated to the file.")
  (block-size
   :initarg :block-size :accessor metadata-template-block-size
   :initform -1 :type integer
   :documentation "Size in bytes of file system blocks.")
  (flags
   :initarg :flags :accessor metadata-template-flags
   ;; :initform nil :type
   :documentation "Unix flags")
  (generation
   :initarg :generation :accessor metadata-template-generation
   ;; :initform nil :type ???
   :documentation "Generation number?"))
  (:documentation "Metadata typical on Unix filesystems."))

(defclass windows-file-metadata-template ()
  ()
  (:documentation "Generic metadata."))

(defclass file-metadata ()
  ((template
    :initarg :template :accessor file-metadata-template :initform nil
    :type (or file-metadata-template null)
    :documentation
    "Class describing the the metadata expected. NIL means, it is what it is.")
   (table
    :initarg :table :accessor file-metadata-table
    :documentation "Table of names and values."))
  (:documentation "Generic file metadata."))

(defun %rudimentary-get-file-info (pathname &key (follow-links t))
  (declare (ignore follow-links))
  (with-open-file (stream pathname :direction :input)
    (make-file-info :size (file-lenth stream)
		    :modification-time (file-write-date stream))))

(defmethod get-file-info-using-host ((host local-file-host) path
				      &key (follow-links t))
  (%rudimentary-get-file-info pathname :follow-links follow-links))

(defmethod get-file-info-using-host ((host remote-file-host) path
				     &key (follow-links t))
  (%rudimentary-get-file-info pathname :follow-links follow-links)))

(defun get-file-info (path &key (follow-links t))
  "Return information about the file described by PATH in a FILE-INFO
structure. If FOLLOW-LINKS is true (the default), then if PATH is a symbolic
link, return information about the file it's linked to, otherwise return
information about the link itself."
  (let ((path (translate-logical-pathname (merge-pathnames pathspec))))
    (get-file-info-using-host (pathname-host path) path
			      :follow-links follow-links)))

(defun file-exists (filename)
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."
  )

(defun os-delete-file (pathname)
  "Delete a file. Doesn't monkey with the name, which should be a string.
Doesn't operate on streams."
  )

(defmacro with-os-file ((var pathname &key
			     (direction :input)
			     if-exists
			     if-does-not-exist) &body body)
  "Evaluate the body with the variable VAR bound to a Windows file handle
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
"
  )

(defun set-file-time (path &key access-time modification-time)
  "Set the given times on PATH. The times are OS-TIME structures. Either
time can be :NOW to use the current time."
  )

(defun make-symbolic-link (from to)
  "Make a symbolic link from FROM to TO."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make some stuff up.

(defun os-machine-instance ()
  "bogozano")

(defun os-machine-type ()
  (machine-type))

(defun os-machine-version ()
  (machine-version))

(defun os-software-type ()
  "Mezzano")

(defun os-software-version ()
  (software-version))

;; EOF
