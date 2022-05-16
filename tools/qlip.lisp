;;;
;;; qlip.lisp - Quicklisp packaginator.
;;;

(defpackage :qlip
  (:documentation "Quicklisp packaginator.")
  (:use :cl :dlib :opsys :inator :fui)
  (:export
   #:generate
   #:qlip
   #:!qlip
   ))
(in-package :qlip)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *system-cache-file* (nos:path-append (nos:data-dir "qlip")
						   "package-cache")
  "File name of the package cache.")

;; The first two elements must be changed for every incompatible change to the
;; mini-sys structure.
(defparameter *version* '(0 2 0)
  "The version of the system data.")

;; The choices that I can think of at the moment, seem to be that either I
;; exclude packages that behave in ways that cause errors, OR I could write an
;; .asd loader that doesn't evaluate forms except for defsystem and doesn't
;; act on things ‘defsystem-depends-on’, but can still maybe somehow evaluate
;; necessary "safe" forms.
;;
;; Another possibility which would require cooperation from authors, is I
;; could push a *feature* that people could #- around troublesome things in
;; their system.
;;
;; It's obviously much easier to do it the first way, but of course there are
;; things that are both useful and might cause hard to avoid problems, such as
;; things that need to do ffi groveling in the .asd file.
;;
;; For now I'm just doing the exclude list, but probably in the future we should
;; try a non-evaluating ‘load-asd’.
;;
;; @@@ figure out why iolib & clsql are failing, and how to fix

(defparameter *excluded-systems*
  (alist-to-hash-table (mapcar (_ (cons _ t))
			       '("fast-io-test.asd"
				 "gsll.asd"
				 "hu.dwim.asdf.documentation.asd"
				 "hu.dwim.sdl.asd"
				 "inquisitor-flexi-test.asd"
				 "inquisitor-test.asd"
				 "iolib.asd" ;;; <<-- this seems important
				 "iolib.base.asd"
				 "iolib.common-lisp.asd"
				 "iolib.examples.asd"
				 "iolib.tests.asd"
				 "opticl-doc.asd"
				 "clsql.asd" ;; @@@ this is a bad situation
				 ;;"asdf-finalizers-test.asd"
				 "zaserve.asd"
				 "cl-gamepad-visualizer.asd"
				 ))
		       :table (make-hash-table :test #'equal))
  "Systems that mess things up. Usually extra code in the ASD file.")

(defstruct sw
  "A software." ; [sic]
  release
  systems)

(defstruct mini-sys
  "An abbreviated version of an ASDF system."
  category		; Addition
  name
  version
  description
  author
  license
  maintainer
  homepage
  bug-tracker
  mailto
  source-control
  long-name
  long-description
  tags			; Addition
  source-file
  entry-point
  defsystem-depends-on
  depends-on
  weakly-depends-on)

(defun as-string (thing)
  (typecase thing
    (string thing)
    (null "")))

(defun mini-sys-ify (system)
  "Return a mini-sys from a system."
  (make-mini-sys
   :name                  (asdf:component-name                     system)
   ;;:version               (asdf:system-version                     system)
   :version               (slot-value system 'asdf:version)
   :description           (as-string (asdf:system-description      system))
   :author                (as-string (asdf:system-author           system))
   :maintainer            (as-string (asdf:system-maintainer       system))
   :license               (as-string (asdf:system-license          system))
   :homepage              (as-string (asdf:system-homepage         system))
   :bug-tracker           (as-string (asdf:system-bug-tracker      system))
   :mailto                (as-string (asdf:system-mailto           system))
   :source-control        (as-string (asdf:system-source-control   system))
   :long-name             (as-string (asdf:system-long-name        system))
   :long-description      (as-string (asdf:system-long-description system))
   :source-file           (asdf:system-source-file                 system)
   :entry-point           (asdf::component-entry-point             system)
   :defsystem-depends-on  (asdf:system-defsystem-depends-on        system)
   :depends-on            (asdf:system-depends-on                  system)
   :weakly-depends-on     (asdf:system-weakly-depends-on           system)))

(defstruct mini-dist
  "A minimal but hopefully unique DIST."
  name
  version
  archive-base-url)

(defun mini-dist-ify (dist)
  (make-mini-dist :name             (ql-dist:name dist)
		  :version          (ql-dist:version dist)
		  :archive-base-url (ql-dist::archive-base-url dist)))


(defstruct mini-release
  "A minimal but hopefully unique RELEASE."
  project-name
  dist
  archive-md5
  archive-content-sha1)

(defun mini-release-ify (release)
  (make-mini-release
   :project-name          (ql-dist:project-name release)
   :dist                  (mini-dist-ify (ql-dist:dist release))
   :archive-md5           (ql-dist:archive-md5 release)
   :archive-content-sha1  (ql-dist:archive-content-sha1 release)))

#|
(defun load-asd (pathname &key name)
  "Load system definitions from PATHNAME.
NAME if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:LOAD-ASD."
  (with-asdf-session ()
    ;; TODO: use OPERATE, so we consult the cache and only load once per session.
    (flet ((do-it (o c) (operate o c)))
      (let ((primary-name (primary-system-name (or name (pathname-name pathname))))
	    (operation (make-operation 'define-op)))
	(if-let (system (registered-system primary-name))
            (progn
              ;; We already determine this to be obsolete ---
              ;; or should we move some tests from find-system to check for up-to-date-ness here?
              (setf (component-operation-time operation system) t
                    (definition-dependency-list system) nil
                    (definition-dependency-set system) (list-to-hash-set nil))
              (do-it operation system))
	  (let ((system (make-instance 'undefined-system
				       :name primary-name :source-file pathname)))
	    (register-system system)
	    (unwind-protect (do-it operation system)
	      (when (typep system 'undefined-system)
		(clear-system system)))))))))
|#

(defparameter *system-log* nil
  "Capture systems being created.")

;; (defmethod initialize-instance
;;     :after ((o asdf:system) &rest initargs &key &allow-other-keys)
;;   "Initialize a asdf:system."
;;   (declare (ignore initargs))
;;   (push o *system-log*))

(defvar *saved-register-system-definition* nil)

(defun capture-register-system-definition
    (name &rest options &key pathname (class 'asdf::system)
			  (source-file () asdf::sfp)
			  defsystem-depends-on &allow-other-keys)
  (declare (ignorable pathname class source-file asdf::sfp
		      defsystem-depends-on))
  (let ((result
	 (apply *saved-register-system-definition* name options)))
    (push result *system-log*)
    result))

;; This sucks.
(defmacro with-sysdef-capture (() &body body)
  `(let ((*saved-register-system-definition*
	  (symbol-function 'asdf::register-system-definition)))
     (unwind-protect
	  (progn
	    (setf (symbol-function 'asdf::register-system-definition)
		  #'capture-register-system-definition)
	    ,@body)
       (setf (symbol-function 'asdf::register-system-definition)
	     *saved-register-system-definition*))))

;; ql-dist:install

(defun gather (&key all)
  (when all
    ;; Install all quicklisp software
    (format t "Installing all the things...~%")
    (finish-output)
    (loop :for r :in (ql-dist:provided-releases t)
       :do
       (format t "~s :" (ql-dist:name r))
       (ql-dist:ensure-installed r)
       (terpri)
       (finish-output)))

  ;; Collect all systems
  (format t "Collecting system info...~%")
  (loop :for release :in (ql-dist:provided-releases t)
     :collect
     (make-sw
      :release (mini-release-ify release)
      :systems
      (let (*system-log*)
	(loop :for system-file :in (ql-dist:system-files release)
	   :do
	   (with-simple-restart (next "Go on to the next package.")
	     (setf *system-log* nil)
	     (when (not (gethash system-file *excluded-systems*))
	       (format t ">>> ROADING ~s" system-file) (finish-output)
	       ;; (read-line)
	       (with-sysdef-capture ()
		 (handler-case
		     (asdf:load-asd (ql-dist:relative-to release system-file))
		   (error ()
		     (invoke-restart 'next))))))
	   (format t " ~s~%" *system-log*)
	   :when *system-log*
	   :nconc (mapcar #'mini-sys-ify *system-log*))))))

(defun write-system-data (dists data file)
  "Write the system DATA to FILE."
  (ensure-directories-exist file)
  (with-open-file (stream file
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (with-standard-io-syntax
      (write `(:qlip ,*version*
		     ,(mapcar #'mini-dist-ify dists)
		     ,(coerce data 'vector))
	     :stream stream
	     :circle t
	     :readably t :pretty nil :level nil :length nil :array t))))

(defun generate (&key all)
  (let ((data (gather :all all)))
    ;; (break)
    (write-system-data (ql-dist:enabled-dists) data *system-cache-file*))
  (format t "Wrote ~s.~%" *system-cache-file*))

(defun check-dist (mini-dist)
  (let ((d (ql-dist:find-dist (mini-dist-name mini-dist))))
    (and (equal (mini-dist-version mini-dist) (ql-dist:version d))
	 (equal (mini-dist-archive-base-url mini-dist)
		(ql-dist::archive-base-url d)))))

(defun read-system-data (file)
  "Read the system data cache from FILE and return it."
  (with-open-file (stream file :direction :input)
    (with-standard-io-syntax
      (let* ((result (safe-read stream))
	     (magic   (elt result 0))
	     (version (elt result 1))
	     (dists   (elt result 2))
	     (systems (elt result 3)))
	(when (not (listp result))
	  (error "Package cache shell should be a list."))
	(when (not (eq :qlip magic))
	  (error "Bad magic key in in package cache ~s." magic))
	(when (not (and (eq (first version) (first *version*))
			(eq (second version) (second *version*))))
	  (error "Package cache Version mismatch. Got ~s, exepected ~s."
		 version *version*))
	(map nil (_ (when (not (check-dist _))
		      (error #.(s+ "Cached dist doesn't match one in the "
				   "currently enabled Quicklisp."))))
	     dists)
	(when (not (and (vectorp systems)
			(plusp (length systems))))
	  (error "Package cahce systems should be a non-empty vector."))
	systems))))

(defun confirm-box (title message)
  (char-equal
   #\y
   (loop :with c
      :do
	(setf c (fui:display-text title message :y 9 :x 10))
      :until (member c '(#\y #\n) :test #'char-equal)
      :finally (return c))))

(defparameter *cache* nil
  "System data cache.")

(defun qlip ()
  "Quicklisp interface program."
  (when (not (file-exists *system-cache-file*))
    (error "The system cache file ~s isn't there." *system-cache-file*))
  (when (not *cache*)
    (setf *cache* (read-system-data *system-cache-file*)))
  (let (systems)
    (loop :for sw :across *cache*
       :do (mapcar (_ (push _ systems)) (sw-systems sw)))
    (setf systems (nreverse systems))
    (table-viewer:view-table (table:make-table-from systems))
    ;; (tree-viewer:view-tree systems)
    ))

#+lish
(lish:defcommand qlip
  ((generate boolean :short-arg #\g :help "Generate the system data cache.")
   (all boolean :short-arg #\a
    :help "Download all quicklisp systems when generating."))
  "Quicklisp interface program."
  (cond
    (generate (generate :all all))
    (t (qlip))))

;; End
