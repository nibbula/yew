;;
;; keymap.lisp - Associate functions with keys.
;;

(defpackage :keymap
  (:documentation
   "Associate functions with keys.

This defines a KEYMAP class which contains mappings of keys to actions.
The KEYMAP class has slots:
  MAP             - The the actual table of bindings, accessed by KEYMAP-MAP.
  DEFAULT-BINDING - The binding that is invoked if the key doesn't match,
                    accessed by KEYMAP-DEFAULT-BINDING.

Keys are customarily a character or a keyword. Keywords are used to indicate
keys or events that aren't characters, like :F1 or :RESIZE.
Key bindings are customarily a function designator (so therefore a symbol or a
function), a list which represents function application, or another keymap,
where the next element in a key sequence is looked up.

Keymaps:
  macro DEFKEYMAP name [docstring] alist [:default-binding key]
  function KEYMAP-P object -> boolean
  function SET-KEYMAP keymap alist -> keymap
  function MAP-KEYMAP function keymap -> NIL
  function ADD-KEYMAP source-keymap destination-keymap -> destination-keymap
  function COPY-KEYMAP keymap -> new-keymap
  function BUILD-ESCAPE-MAP keymap -> new-keymap
  function DESCRIBE-KEYMAP keymap &key (stream *standard-output*) prefix raw
    -> keymap

Key bindings:
  function DEFINE-KEY keymap key definition -> definition
  function SET-KEY keymap definition key-sequece -> definition
  function KEY-BINDING key keymap -> definition
  function KEY-DEFINITION key keymap-stack -> definition
  function KEYS-BOUND-TO definition keymap -> sequence
  function SUBSTITUTE-KEY-DEFINITION new-definition old-definition keymap
    -> keymap

Key sequences are sequences of keys that can trace a path through multiple
keymaps.

Key sequences:
  function KEY-SEQUENCE-BINDING key-sequence keymap -> definition
  function KEY-SEQUENCE-STRING key-sequence -> string
  function GET-KEY-SEQUENCE function keymap -> key-sequence OR key

Keymap stacks are sequences of keymaps that are searched in order for a key
definition.

Keymap stacks:
  macro PUSH-KEYMAP new current -> current
  macro REMOVE-KEYMAP new current -> current
")
  (:use :cl :dlib :char-util)
  (:export
   #:keymap
   #:keymap-map
   #:keymap-default-binding
   #:keymap-p
   #:set-keymap
   #:describe-keymap
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
   #:get-key-sequence
   #:add-keymap
   #:build-escape-map
   #:copy-keymap
   #:substitute-key-definition
   #:keys-bound-to
   ))
(in-package :keymap)

(declaim (optimize (debug 3)))

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
  (typep object 'keymap))

(defun set-keymap (keymap map)
  "Initialize the map of KEYMAP from the alist MAP."
  (when (and map (consp map))
    (setf (keymap-map keymap)
	  (alist-to-hash-table map (make-hash-table))))
  keymap)

;; @@@ Make the map be an alist under a certain threshold of population, but
;; allow a hint to make a big one when creating.
(defmethod initialize-instance :after ((k keymap) &rest initargs)
  "Initialize a keymap. If the map is an alist convert it into a hash table."
  (declare (ignore initargs))
  (if (and (slot-boundp k 'map) (consp (slot-value k 'map)))
      (set-keymap k (keymap-map k))
      (setf (slot-value k 'map) (make-hash-table))))

(defmethod print-object ((obj keymap) stream)
  "Print a KEYMAP on a STREAM."
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream " [~d]"
	    (hash-table-count (keymap-map obj)))))

(defmacro defkeymap (name &body body)
  "Define a keymap. DEFAULT-BINDING is what keys that aren't defined are
bound to."
  (let (docstring map default-binding)
    (when body
      ;; See if there's a docstring
      (when (stringp (first body))
	(setf docstring (pop body)))
      (setf map (pop body))
      ;; See if there's a default-binding keyword
      (when (eql (first body) :default-binding)
	(pop body)
	(setf default-binding (pop body))))
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

(defun define-key (keymap key definition)
  "Define the KEY as DEFINITION in the KEYMAP. KEYMAP must be a keymap object.
KEYs are compared with EQL. See also SET-KEY."
  (setf (gethash key (keymap-map keymap)) definition))

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

(defun set-key (key-sequence definition keymap)
  "Define a key or sequence of keys in a keymap.
  KEY-SEQUENCE    A single key, or a sequence of keys. Keys currently have
                  the restriction that they have to be EQL.
  DEFINITION      Customarily a function designator or a list to be applied,
                  but has no formal restriction.
  KEYMAP          A keymap object or keymap stack, which is a list of keymap
                  objects. The keys are defined in the first keymap."
  (when (not (or (keymap-p keymap) (consp keymap)))
    (error "KEYMAP should be a keymap or a keymap list."))
  (labels ((real-keymap (map)
	     (etypecase map
	       (cons (car map))
	       (keymap map)))
	   (set-single-key (map key def)
	     (define-key (real-keymap map) key def)))
    (if (or (vectorp key-sequence) (listp key-sequence))
	(let* ((key (elt key-sequence 0))
	       (action (key-definition key keymap)))
	  (cond
	    ((= (length key-sequence) 1)
	     ;; simple single binding
	     (set-single-key keymap key definition))
	    ;; If the key has a symbolic keymap binding
	    ((and action (symbolp action) (boundp action)
		  (keymap-p (symbol-value action)))
	     ;; look that up and try to set it
	     (set-key (subseq key-sequence 1) definition (symbol-value action)))
	    ((keymap-p action)
	     (set-key (subseq key-sequence 1) definition action))
	    (t ;; We have to make a new keyamp for this key
	     (let ((new-map (make-instance 'keymap)))
	       (set-single-key keymap key new-map)
	       (set-key (subseq key-sequence 1) definition new-map)))))
	;; Just one key
	(set-single-key keymap key-sequence definition))))

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
    ;; handle a single keymap
    ((typep keymap-stack 'keymap)
     (key-binding key keymap-stack))
    (t
     (error "Key definition must be looked up in either a keymap or a list
 of keymaps."))))

(defun key-sequence-binding (keyseq map)
  "Return the binding for the given key sequence."
  (if (and (or (vectorp keyseq) (listp keyseq)) (not (zerop (length keyseq))))
      (let* ((key (elt keyseq 0))
	     (action (key-definition key map)))
	(cond
	  ;; If the key has a symbol keymap binding
	  ((and (symbolp action) (boundp action)
		(keymap-p (symbol-value action)))
	    ;; look that up
	   (key-sequence-binding (subseq keyseq 1) (symbol-value action)))
	  ((keymap-p action)
	   (key-sequence-binding (subseq keyseq 1) action))
	  (t
	   ;; simple binding
	   action)))
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

(defun get-key-sequence (get-key-function keymap)
  "Read a sequence of keys returned by GET-KEY-FUNCTION, starting with keymap.
Descend into keymaps. Return a key or sequence of keys."
  (let* ((c (funcall get-key-function))
	 (action (key-definition c keymap)))
    (flet ((sub-map (map)
	     (let ((result-seq (get-key-sequence get-key-function map)))
	       (if (listp result-seq)
		   (append (list c) result-seq)
		   (list c result-seq)))))
      (cond
	((and (symbolp action) (boundp action) (keymap-p (symbol-value action)))
	 (sub-map (symbol-value action)))
	((keymap-p action)
	 (sub-map action))
	(t c)))))

(defun add-keymap (source dest)
  "Add all the key definitions from the SOURCE keymap to the DEST keymap.
If there is already a binding for a key in DEST, it gets overwritten by the
one in source."
  (check-type source keymap)
  (check-type dest keymap)
  (loop :for k :being :the :hash-keys :of (keymap-map source)
     :do
     (define-key dest k (gethash k (keymap-map source))))
  dest)

(defun describe-keymap (map &key (stream *standard-output*) prefix raw)
  "Show the bindings of a keymap MAP on STREAM. If PREFIX is given it is
assumed to be a prefix for all bindings in the keymap."
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
	   (describe-keymap (symbol-value action)
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
  (describe-keymap o :stream stream))

(defun copy-keymap (keymap)
  "Return a new copy of KEYMAP."
  (check-type keymap keymap)
  (let ((new-keymap (make-instance
		     (type-of keymap)
		     :default-binding (keymap-default-binding keymap))))
    (map-keymap #'(lambda (k f) (define-key new-keymap k f))
		keymap)
    new-keymap))

;; This is very useful if you want Escape and Meta to be equivalent.
(defun build-escape-map (keymap)
  "Return a new keymap containing only the meta character bindings in KEYMAP
 converted to non-meta character bindings. This is useful for making a keymap
to bind to the escape key, so you have escape key equivalents to meta keys."
  (check-type keymap keymap)
  (let ((new-keymap (make-instance (type-of keymap))))
    (map-keymap
     #'(lambda (k f)
	 (when (and (characterp k) (meta-char-p (char-code k)))
	   (define-key new-keymap (un-meta (char-code k)) f)))
     keymap)
    new-keymap))

(defun substitute-key-definition (new-definition old-definition keymap)
  "Substitute NEW-DEFINITION for every binding of OLD-DEFINITION in KEYMAP."
  (map-keymap #'(lambda (key def)
		  (when (eq def old-definition)
		    (define-key keymap key new-definition))) keymap)
  keymap)

(defun keys-bound-to (definition keymap &key test)
  "Return a list of keys bound to DEFINITION in KEYMAP. Definitions are
compared with TEST, which defaults to EQ."
  (let ((results))
    (map-keymap (if test
		    #'(lambda (key def)
			(when (funcall test def definition)
			  (push key results)))
		    #'(lambda (key def)
			(when (eq def definition)
			  (push key results))))
		    keymap)
    (remove-duplicates results)))

;; EOF
