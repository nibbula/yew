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
an effort to keep ‘asd’ files declarative without side-effects.

@@@ Or maybe yet again I am not knowing about some ASDF feature that already
@@@ does this, and this whole thing is completely useless.
@@@ I guess you can use :around-compile in the ‘.asd’ to do this.
@@@ I'm not sure what's worse.

The way to use this is:

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

   <x>-config:*configuration*
     is a list of configuration-variable objects that have been defined.

   <x>-config:*config*
     is a plist of values after configuration in done.

3. Load or compile, say with ‘asdf:load-system’, or ‘ql:quickload’ as normal.

4. If someone wants to do custom configuration to your package, they can
   set individual values:

     (config :<package> :option value ...)

   Or set the whole configuration:

     (config :<package> '(:option1 value1 :option2 value2 ...))

   These are equivalent to:
     (asdf:load-system :<x>-config)
     (setf (getf <x>-conifg:*config* :option) value)

     (asdf:load-system :<x>-config)
     (setf <x>-conifg:*config* '(:option1 value1 :option2 value2))

   This is also how a program can configure your package.

   OR [@@@ not done yet]

   (config :<package>)

   This will do some stupid thing where it interactively prompts you
   for configuation variables to change. Try to avoid this. In other words,
   try to make everything work right automatically, instead of having a bunch
   of confusing build options.

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
   #:config
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

(defun package-config-name (&optional (package *package*))
  "Name of the package configuration."
  (intern "*CONFIG*" package))

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
  #-mezzano
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
     (return t))
  #+mezzano (declare (ignore library-spec))
  #+mezzano nil
  )

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
	   ,(format nil "Configuration for ~a" (package-name *package*)))
	 ;; (export '(#:*configuration*))
	 )
       (push (make-instance ',(variable-class-name type)
	      :name ',name
	      :documentation ,documentation
	      :value-function ,func)
	     ,conf))))

;; @@@ Shouldn't there be a way to set the default value, instead of just
;; the value function?
(defmacro defconfiguration (vars)
  "Define the set of configuration variables."
  (let ((conf (intern "*CONFIGURATION*" *package*)))
    `(progn
       (defparameter ,conf
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
	 ,(format nil "Configuration variables for ~s" *package*))
       ;; (export '(#:*configuration*))
       )))

;; @@@ The verbose here should be dynamic
(defmacro configure (&key (package *package*) verbose)
  "Set the default configuration values."
  (let* ((*package* package)
	 (conf (intern "*CONFIGURATION*" *package*)))
    `(progn
       (defvar ,(intern "*CONFIG*" package) nil
	 ,(format nil "Configuration for ~a" (package-name *package*)))
       ;; (export '(#:*config*))
       (when ,verbose
	 (format t ";; Configuring ~a..." *package*) (finish-output))
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

(defun print-config (var &key (stream t))
  (let ((label-length 0))
    ;; I do the whole load, in two nostril mode.
    (loop :with l = var
       :while l
       :do
       (setf label-length (max (length (string (pop l))) label-length))
       (pop l))
    (loop :with l = var
       :while l
       :do
       (format stream "~(~v@a~): ~a~%" label-length (pop l) (pop l)))))

(defun config (package &rest args)
  "Set or print the configuration variables for PACKAGE. If ARGS is a single
plist, set the entire configuration. Otherwise ARGS are individual keywords
and values to set. When setting values, this loads the <PACKAGE>-CONFIG
package, and sets it's *config* variable."
  (let ((config-package-name
	 (intern (concatenate 'string (string package) "-CONFIG") :keyword))
	(len (length args)))
    (and (asdf:load-system config-package-name)
	 (cond
	   ((zerop len)
	    (print-config
	     (symbol-value (package-config-name config-package-name))))
	   ((and (= len 1) (consp (car args)))
	    (setf (symbol-value (package-config-name config-package-name))
		  args))
	   ((evenp len)
	    (loop :with l = args
	       :while l
	       :do
	       (setf (getf (symbol-value
			    (package-config-name config-package-name))
			   (pop l))
		     (pop l))))
	   (t
	    (error "Malformed property list"))))))

;; End
