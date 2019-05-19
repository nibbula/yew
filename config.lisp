;;
;; config.lisp - Package build configuration.
;;

(defpackage :config
  (:documentation "Package build configuration.

Provides a way to configure systems before loading and compiling them.
Provide for automatic default configuration.
Provide a way the user can customize the configuration.
Provide a way other programs can customize the configuration.

This is basically just an alternative to putting code in your ‘.asd’ file, in
an effort to keep ‘asd’ files declarative without side-effects. The way to use
this is:

1. Make 2 files in your project:
     <system-name>-config.asd

       The system in the ‘.asd’ should depend on this package ‘:config’ and
       your <system-name>-config.lisp.

     <system-name>-config.lisp
 
       This should have a ‘defconfiguration’ form, or some ‘defconfig’ forms
       to define the configuration variables. Then it should have a call to
       ‘configure’ which will actually set the values.

   The easiest way to understand or do this, is to copy an already existing
   project or use a template.

2. In the ‘.asd’ for your system, use whatever ‘*features*’ or other things
   that were set up. Your code can also consult the settings in
   ‘<x>-config:*config*’.

3. Load or compile, say with ‘asdf:load-system’, or ‘ql:quickload’ as normal.

4. If someone wants to do custom configuration to your package, they can
   either:

   (asdf:load-system :<x>-config)

   ;; To set the whole configuration:
   (setf <x>-conifg:*config* '(:foo \"bar\" :bar 2.3 :optimize :fast))

   ;; To set a specific variable:
   (setf (getf <x>-conifg:*config* :use-backend) :smolder-db)

   This is also how a program can configure your package.
   OR [@@@ not done yet]

   (config :<package>)

   This will do some stupid thing where it interactively prompts you
   for configuation variables to change. Try to avoid this.

In general, please don't conflate this with user preferences or options, which
should be handled in an appropriate way after your software is compiled and
loaded, or with software installation, which is very different and dependant
on the operating system, package manager, etc. This is just for things to
tweak about the build process.
")
  (:use :cl)
  (:export
   ;; things for custom variable types
   #:configuration-variable
   #:package-config-name
   #:set-var
   #:temporary-feature-name
   #:cv-boolean-feature
   ;; utility things
   #:library-loadable-p
   ;; normal stuff
   #:defconfig
   #:defconfiguration
   #:configure
   #:configure
   ))
(in-package :config)

;; (declaim (optimize (debug 2)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; @@@ I'm really not sure if this entire thing makes sense. Is it just adding
;; needless complexity?

;; We had better keep dependencies of this package simple. Anything that
;; this uses, can't use this for configuration. But,?

;; (deftype cv-value ()
;;   "Configuration variable type"
;;   (member boolean number integer float double-float
;; 	  string keyword character))

(defclass configuration-variable ()
  ((name
    :initarg :name :accessor %name :initform nil :type symbol
    :documentation "Name of the variable.")
   (value-type
    :initarg :value-type :accessor %value-type
    :documentation "Type of the value.")
   (default-value
    :initarg :default-value :accessor %default-value :initform nil
    :documentation "Value initially, if not otherwise set.")
   (value-function
    :initarg :value-function :accessor %value-function
    :initform nil :type (or function null)
    :documentation "Function to call to configure the value.")
   (documentation
    :initarg :documentation :accessor %documentation :initform "" :type string
    :documentation "A description of the variable."))
  (:documentation "A generic configuration variable."))

(defun package-config-name ()
  "Name of the package configuration."
  (intern "*CONFIG*" *package*))

(defgeneric set-var (var value)
  (:documentation "Set a configuration value.")
  (:method (var value)
    (setf (getf (symbol-value (package-config-name))
		(intern (string-upcase (%name var)) :keyword))
	  value)))

(defun variable-class-name (name)
  "Return the real name of the variable class, given the one we use in
defconfig* forms."
  (let ((str (concatenate 'string "CV-" (string-upcase name))))
    (if (not (find-symbol str :config))
	'configuration-variable
	(intern str :config))))

(defgeneric value-type (type)
  (:documentation "Return the configuration variable value type for type.")
  (:method (type)
    (let ((tt (variable-class-name type)))
      (if (eq tt 'configuration-variable)
	  type
	  tt))))

;; @@@ Maybe this should be somewhere else? opsys?
(defun library-loadable-p (library-spec)
  "Return true if we can load the operating system library, which usually
means 'C' language interface, dynamic library."
  ;; We just use CFFI
  (prog (library (library-symbol (gensym "llp")))
     (when (stringp library-spec)
       (setf library-spec `((t ,library-spec))))
     (cffi::register-foreign-library library-symbol library-spec)
     (when (cffi:foreign-library-loaded-p library-symbol)
       (return t))
     (unwind-protect
	  (handler-case
	      (setf library (cffi:load-foreign-library library-symbol))
	    (cffi:load-foreign-library-error ()
	      (return nil)))
       ;; I know it's somewhat stupid, since we're probably going to load it
       ;; again soon?
       (when library
	 (cffi:close-foreign-library library)))
     (return t)))

(defun temporary-feature-name (var)
  "Return the name we should use for a temporary configuration feature."
  (intern
   (concatenate 'string "T-" (package-name *package*) "-"
		(string-upcase (%name var)))
   :keyword))

(defclass cv-class-type (configuration-variable)
  ()
  (:documentation "Configuration variable types that are classes."))

(defmethod value-type ((type cv-class-type))
  "Return the value type for generic configuration variable that are classes,
which value-type slot."
  (%value-type type))

(defclass cv-boolean-feature (cv-class-type)
  ()
  (:default-initargs
   :value-type 'boolean)
  (:documentation
   "A true or false value that sets a temporary feature in *features*.
The feature will be named :t-<package>-<var name>. If you want a non-temporary
feature that other software can use, just set it the old-fashioned way in
your software."))

(defmethod set-var ((var cv-boolean-feature) value)
  "Also add or remove a feature based on the value."
  (call-next-method)
  (if value
      ;; (d-add-feature (temporary-feature-name var)))
      ;; (d-remove-feature (temporary-feature-name var)))
      (progn
	(format t "Adding feature ~s~%" (temporary-feature-name var))
	(pushnew (temporary-feature-name var) *features*))
      (progn
	(format t "Removing feature ~s~%" (temporary-feature-name var))
	(setf *features* (delete (temporary-feature-name var) *features*)))))

(defmacro defconfig (name type documentation &body body)
  "Define a single configuration variable."
  (let ((func (when body
		`(lambda (var) (declare (ignorable var)) ,@body)))
	(conf (intern "*CONFIGURATION*" *package*)))
    `(progn
       (when (not (boundp ',conf))
	 (defvar ,conf nil
	   ,(format nil "Configuration for ~a" (package-name *package*))))
       (push (make-instance ',(variable-class-name type)
	      :name ',name
	      :documentation ,documentation
	      :value-function ,func)
	     ,conf))))

(defmacro defconfiguration (vars)
  "Define the set of configuration variables."
  (let ((conf (intern "*CONFIGURATION*" *package*)))
    `(defparameter ,conf
       (list
	,@(loop :for v :in vars
	     :collect `(make-instance
			',(variable-class-name (second v))
			:name ',(first v)
			:documentation ,(third v)
			:value-function ,(when (fourth v)
					   `(lambda (var)
					      (declare (ignorable var))
					      ,(fourth v))))))
       ,(format nil "Configuration variables for ~s" *package*))))

;; @@@ The verbose here should be dynamic
(defmacro configure (&key (package *package*) verbose)
  "Set the default configuration values."
  (let* ((*package* package)
	 (conf (intern "*CONFIGURATION*" *package*)))
    `(progn
       (defvar ,(intern "*CONFIG*" package) nil
	 ,(format nil "Configuration for ~a" (package-name *package*)))
       (when ,verbose
	 (format t ";; Configuring ~a…" *package*) (finish-output))
       (loop :with value
	  :for v :in ,conf
	  :do (setf value (or (and (%value-function v)
				   (funcall (%value-function v) (%name v)))
			      (%default-value v)))
	  (set-var v value)
	  (when ,verbose
	    (write-char #\.)
	    (finish-output)))
       (when ,verbose
	 (format t ".~%")))))

;; End
