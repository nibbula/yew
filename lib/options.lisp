;;;
;;; options.lisp - Options “pattern”.
;;;

;;; I'm not sure this is actually useful.
;;;
;;; Why would I want some fake Javascript/Smalltalk like prototyping?
;;; Perhaps it's just paranoia about having to reboot my nonexistent Lisp
;;; machine when class versioning somehow fails?
;;;
;;; Options look like a slot, but you're not supposed to really give a damn if
;;; they're there or not. And they don't change the type graph or method
;;; dispatching.

(defpackage :options
  (:documentation
   "Options “pattern”. For when you might want a new slot in your class, but
maybe you don't really want to commit to it, or you'd like it to fail in a
different way when you change your mind.

To use this, define a subclass of options-mixin, then define your options with:
  (defoption class option-name type &rest args)

For example:

  (defclass lorry (vehicle options-mixin) ((chassis) (wheels) (motor)))
  (defoption lorry winch option :load-capacity '(2000 kg))
  (defoption lorry plow  option :height '(125 cm))")
  (:use :cl :dlib)
  (:export
   #:option #:option-name #:option-value
   #:options-mixin #:options #:options-prototypes
   #:find-option #:set-option #:get-option
   #:defoption))
(in-package :options)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defclass option ()
  ((name
    :initarg :name :accessor option-name
    :documentation "Name of the option.")
   (value
    :initarg :value :accessor option-value  
    :documentation "Value of the option.")
   (documentation
    :initarg :documentation :accessor option-documentation
    :documentation "Documentation for the option."))
  (:documentation "Something like a slot, but more malleable."))

(defclass options-mixin ()
  ((options
    :initarg :options :accessor options :initform nil
    :documentation "Optional properties.")
   ;; (option-prototypes
   ;;  :allocation :class
   ;;  :initarg :option-prototypes
   ;;  :accessor options-prototypes
   ;;  :initform nil :type list
   ;;  :documentation "List of options to be created on a new object.")
   )
  (:documentation "Options mixin."))

(defmethod initialize-options ((o options-mixin))
  (let* ((package (symbol-package (class-name (class-of o))))
	 (prototype-name (symbolify
			  (s+ "*" (class-name (class-of o)) "-prototype*")
			  :package package))
	 (prototype (and (boundp prototype-name)
			 (symbol-value prototype-name))))
    (loop :for option :in prototype :do
       (when (not (find (getf (cddr option) :name) (options o)
			:key #'option-name :test #'equalp))
	 (push (apply #'make-instance (second option)
		      (cddr option))
	       (options o))))))

(defmethod initialize-instance
    :after ((o options-mixin) &rest initargs &key &allow-other-keys)
  "Initialize a options-mixin."
  (declare (ignore initargs))
  ;; Instantiate options from the defined option list.
  (initialize-options o))

(defgeneric find-option (obj name)
  (:documentation
   "Find the option of the object OBJ, named NAME. Error if there is none.")
  (:method ((obj options-mixin) name)
    (when (not (options obj))
      (initialize-options obj))
    (or (find name (options obj) :key #'option-name :test #'equalp)
	(progn
	  ;; Initialize and try again.
	  (initialize-options obj)
	  (or (find name (options obj) :key #'option-name :test #'equalp)
	      (error "No such option ~s" name))))))

(defgeneric set-option (obj name value)
  (:documentation "Set the option named NAME, for OBJ, to VALUE.")
  (:method ((obj options-mixin) name value)
    (when (not (options obj))
      (initialize-options obj))
    (setf (option-value (find-option obj name)) value)))

(defgeneric get-option (obj name)
  (:documentation "Get the option named NAME, for OBJ.")
  (:method ((obj options-mixin) name)
    (when (not (options obj))
      (initialize-options obj))
    (option-value (find-option obj name))))

;; This is pretty messed up.
;;
;; When we define an option we squirrel away it's details in a *global*
;; variable for it's containing class. Then, when we create an object with
;; options, we intuit it's varible name from the object's class name and
;; package, and instantiate the options, with the initargs given here.
;;
;; I would like to hang these on a class allocated slot, but there's no
;; instance around at the time we're defining them.
;;
;; The stupid thing is this: if the defoption is done after the object is
;; created, then the object doesn't have the options, so we have to make them,
;; when we first try to access them.

(defmacro defoption (class name type &rest args)
  "Define an option of type TYPE, named NAME for THING, with the initargs ARGS."
  (let* ((package (symbol-package class))
	 (prototype (symbolify (s+ "*" class "-prototype*")
			       :package package))
	 (sym (symbolify (s+ class "-" name)))
	 (name-string (string-downcase name)))
    `(progn
       ;; Access options as if they were in the containing object.
       (defgeneric ,sym (obj)
	 (:documentation ,(s+ "Return the value of " name-string ".")))
       (defmethod ,sym ((obj ,class))
	 (get-option obj ,name-string))
       (defgeneric (setf ,sym) (value obj)
	 (:documentation ,(s+ "Set the value of " name-string ".")))
       (defmethod (setf ,sym) (value (obj ,class))
	 (set-option obj ,name-string value))
       (push '(,*package* ,type :name ,name-string ,@args) ,prototype))))

;; “I have run out of options patterns.”

;; EOF
