;;
;; keymap.lisp - Associate functions with keys.
;;

;; $Revision: 1.3 $

(defpackage :keymap
  (:documentation "Associate functions with keys.")
  (:use :cl :dlib :char-util)
  (:export
   #:keymap
   #:keymap-map
   #:keymap-default-binding
   #:keymap-p
   #:dump-keymap
   #:defkeymap
   #:map-keymap
   #:define-key
   #:push-keymap
   #:remove-keymap
   #:set-key
   #:key-binding
   #:key-definition
   #:key-sequence-binding
   #:key-sequence-string
   #:add-keymap
   #:build-escape-map
   ))
(in-package :keymap)

(defclass keymap ()
  ((map
    :accessor		keymap-map
    :initarg		:map
    :documentation	"Hash table of key to binding.")
   (default-binding
    :accessor		keymap-default-binding
    :initarg		:default-binding
    :initform		nil
    :documentation	"Binding if there is none in map."))
  (:documentation "Relates input events to commands."))

(defun keymap-p (object)
  "Return true if OBJECT is a keymap."
;  (eq (type-of object) 'keymap))
  (typep object 'keymap))

(defmethod initialize-instance :after ((k keymap) &rest initargs)
  "Initialize a keymap. If the map is an alist convert it into a hash table."
  (declare (ignore initargs))
  (if (slot-boundp k 'map)
      (when (consp (slot-value k 'map))
	(setf (keymap-map k)
	      (alist-to-hash-table (keymap-map k) (make-hash-table))))
      (setf (slot-value k 'map) (make-hash-table))))

(defun dump-keymap (map &key (stream *standard-output*) prefix raw)
  "Show the bindings of a keymap MAP on STREAM. If PREFIX is given it is assumed to be a prefix for all bindings in the keymap."
;  (format stream "~:@(~a~):~%" (named-name map))
  (map-keymap
   #'(lambda (key action)
       (if raw
	   (format stream "  ~@[~a ~]~5a ~3d ~3x~20t ~(~a~)~%"
		   prefix (nice-char key)
		   (or (and (characterp key) (char-code key)) "")
		   (or (and (characterp key) (char-code key)) "")
		   action)
	   (format stream "  ~@[~a ~]~a~20t ~(~a~)~%"
		   prefix (nice-char key) action))
       (if (and (symbolp action) (boundp action)
		(typep (symbol-value action) 'keymap))
	   (dump-keymap (symbol-value action)
			:stream stream :prefix (nice-char key))))
   map)
  (when (and (slot-boundp map 'default-binding)
	     (keymap-default-binding map))
    (format stream "Default: ~(~a~)~%" (keymap-default-binding map)))
  map)

(defmethod describe-object ((o keymap) stream)
  "Output a description of a keymap."
  (format stream "~a is a keymap with ~a bindings:~%" o
	  (hash-table-count (keymap-map o)))
  (dump-keymap o :stream stream))

(defmethod print-object ((obj keymap) stream)
  "Print a KEYMAP on a STREAM."
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream " [~d]"
	    (hash-table-count (keymap-map obj)))))

;(defmacro defkeymap (name map &key default-binding)
(defmacro defkeymap (name &body body)
  "Define a keymap. DEFAULT-BINDING is what keys that aren't defined are bound to."
  (let* (docstring map default-binding)
    ;; See if there's a docstring
    (if (stringp (first body))
	(setf docstring (pop body)))
    (setf map (pop body))
    ;; See if there's a default-binding keyword
    (when (eql (first body) :default-binding)
      (pop body)
      (setf default-binding (pop body)))
    `(defparameter ,name
       (make-instance 'keymap:keymap
		      #| :name ,(string-downcase name) |#
		      :map ,map
		      ,@(when default-binding
			      `(:default-binding ,default-binding)))
       ,@(when docstring `(,docstring)))))

;; semi-bogus wrapper for named-name
;; (defun keymap-name (map)
;;   "Return the name of the keymap."
;;   (named-name map))

(defun map-keymap (func keymap)
  "Call FUNC on each binding in KEYMAP."
  (maphash func (keymap-map keymap)))

(defun define-key (keymap key def)
  "Define the KEY as performing the action in the KEYMAP."
  (setf (gethash key (keymap-map keymap)) def))

(defmacro push-keymap (new current)
  "Push a new keymap on the list of currently searched keymaps."
  `(progn
    (when (not (listp ,current))
      (setf ,current (list ,current)))
    (pushnew ,new ,current)))

(defmacro remove-keymap (new current)
  "Remove keymap from the list of currently searched keymaps."
  `(if (listp ,current)
    (setf ,current (delete ,new ,current))
    (when (eql ,current ,new)
      (setf ,current '()))))

(defun set-key (keyseq def map)
  "Define a key or sequence of keys in a keymap." ;; @@@ decribe me better
  (if (or (vectorp keyseq) (listp keyseq))
      (let* ((key (elt keyseq 0))
	     (action (key-definition key map)))
	;; If the key has a keymap binding
	(if (and (symbolp action) (boundp action)
		 (keymap-p (symbol-value action)))
	    ;; look that up
;	    (set-key (subseq keyseq 1) :map (symbol-value action))
	    (set-key (subseq keyseq 1) def (symbol-value action))
	    ;; simple binding
	    (typecase map
	      (cons (define-key (car map) key action))
	      (keymap (define-key map key def)))))
      ;; Just one key
      (typecase map
	(cons (define-key (car map) keyseq def))
	(keymap (define-key map keyseq def)))))

(defun key-binding (key keymap)
  "Return the binding of single key in single keymap. Return the default
binding if it is set and no other binding has been specified or NIL if
there is no binding."
  (or (gethash key (keymap-map keymap))
      (keymap-default-binding keymap)))

(defun key-definition (key keymap-stack)
  "Return the definition of KEY in KEYMAP-STACK."
  (cond
    ((consp keymap-stack)    ;; a list of keymaps
     (loop
	:with binding = nil
	:for m :in keymap-stack
	:when (setf binding (key-binding key m))
	:return binding))
;		(multiple-value-bind (binding is-bound)
;		    (gethash key (keymap-map m))
;		  (when is-bound (return binding)))))
    ;; handle a single keymap
    ((typep keymap-stack 'keymap)
     (key-binding key keymap-stack))
    (t
     (error "Key definition must be looked up in either a keymap or a list
 of keymaps."))))

(defun key-sequence-binding (keyseq map)
  "Return the binding for the given key sequence."
  (if (or (vectorp keyseq) (listp keyseq))
      (let* ((key (elt keyseq 0))
	     (action (key-definition key map)))
	;; If the key has a keymap binding
	(if (and (symbolp action) (boundp action)
		 (keymap-p (symbol-value action)))
	    ;; look that up
	    (key-sequence-binding (subseq keyseq 1) (symbol-value action))
	    ;; simple binding
	    action))
      ;; Just one key
      (key-definition keyseq map)))

(defun key-sequence-string (keyseq)
  "Covert a key sequence to a human readable string."
  (cond
    ((vectorp keyseq)
     (format nil "~{~A ~}"
	     (loop :for i :across keyseq :collect (nice-char i))))
    ((listp keyseq)
     (format nil "~{~A ~}"
	     (loop :for i :in keyseq :collect (nice-char i))))
    (t
     (nice-char keyseq))))

(defun add-keymap (source dest)
  "Add all the key definitions from the SOURCE keymap to the DEST keymap.
If there is already a binding for a key in DEST, it gets overwritten by the
one in source."
  (loop :for k :being :the :hash-keys :of (keymap-map source)
     :do
     (define-key dest k (gethash k (keymap-map source))))
  dest)

;; This is very useful if you want Escape and Meta to be equivalent.
(defun build-escape-map (keymap)
  "Return a new keymap containing only the meta character bindings in KEYMAP
 converted to non-meta character bindings. This is useful for making a keymap
to bind to the escape key, so you have escape key equivalents to meta keys."
;   (loop :for (c . f) :in (keymap-map keymap)
; 	:when (and (characterp c) (meta-char-p (char-code c)))
; 	:collect (cons (un-meta (char-code c)) f)))
  (let ((new-keymap (make-instance (type-of keymap))))
    (map-keymap
     (lambda (k f)
       (when (and (characterp k) (meta-char-p (char-code k)))
	 (define-key new-keymap (un-meta (char-code k)) f)))
     keymap)
    new-keymap))

;; EOF
