;;
;; options.lisp - Options “pattern”.
;;

;; I'm not sure this is actually useful.

(defpackage :options
  (:documentation "Options “pattern”.")
  (:use :cl :dlib)
  (:export
   #:option #:option-name #:option-value
   #:options-mixin #:options #:options-prototypes
   #:find-option #:set-option #:get-option
   #:defoption))
(in-package :options)

(defclass option ()
  ((name
    :initarg :name :accessor option-name
    :documentation "Name of the option.")
   (value
    :initarg :value :accessor option-value  
    :documentation "Value of the option."))
  (:documentation "Something like a slot, but more malleable."))

(defclass options-mixin ()
  ((options
    :initarg :options :accessor options :initform nil
    :documentation "Optional properties.")
   (option-prototypes
    :allocation :class
    :initarg :option-prototypes
    :accessor options-prototypes
    :initform nil :type list
    :documentation "List of options to be created on a new object."))
  (:documentation "Options mixin."))

(defmethod initialize-instance
    :after ((o options-mixin) &rest initargs &key &allow-other-keys)
  "Initialize a options-mixin."
  (declare (ignore initargs))
  ;; Copy the objetcs from the defined option list, and set the default values.
  (loop :for option :in (options-prototypes o) :do
     (push (shallow-copy-object option) (options o))))

(defgeneric find-option (obj name)
  (:documentation
   "Find the option of the object OBJ, named NAME. Error if there is none.")
  (:method ((obj options-mixin) name)
    (or (find name (options obj) :key #'option-name :test #'equalp)
	(error "No such option ~s" name))))

(defgeneric set-option (obj name value)
  (:documentation "Set the option named NAME, for OBJ, to VALUE.")
  (:method ((obj options-mixin) name value)
    (setf (option-value (find-option obj name)) value)))

(defgeneric get-option (obj name)
  (:documentation "Get the option named NAME, for OBJ.")
  (:method ((obj options-mixin) name)
    (option-value (find-option obj name))))

(defmacro defoption (thing name type &rest args)
  "Define an option of type TYPE, named NAME for THING, with the initargs ARGS."
  (let* ((class (class-name (class-of (symbol-value thing))))
	 (sym (symbolify (s+ class "-" name)))
	 (name-string (string-downcase name)))
    `(progn
       ;; Access options as if they were in the shell object.
       (defgeneric ,sym (obj)
	 (:documentation ,(s+ "Return the value of " name-string ".")))
       (defmethod ,sym ((obj ,class))
	 (get-option obj ,name-string))
       (defgeneric (setf ,sym) (value obj)
	 (:documentation ,(s+ "Set the value of " name-string ".")))
       (defmethod (setf ,sym) (value (obj ,class))
	 (set-option obj ,name-string value))
       (push (make-instance ',type :name ,name-string ,@args)
	     (options-prototypes ,thing)))))

#|

(defclass carg (options:options-mixin) ((body) (wheels) (doors)))
(defvar cc)
(setf cc (make-instance 'carg :body 'suby :wheels 4 :doors 4))
(options:defoption cc "tint" options:option :value "dark")
(defvar zz)
(setf zz (make-instance 'carg :body 'suby :wheels 4 :doors 4))

|#

;; EOF
