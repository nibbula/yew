;;;
;;; keymap.lisp - Associate functions with keys.
;;;

(defpackage :keymap
  (:documentation
   "Associate functions with keys.

This defines a ‘keymap’ class which contains mappings of keys to actions.
The KEYMAP class has slots:
  map             - The the actual table of bindings, accessed by KEYMAP-MAP.
  default-binding - The binding that is invoked if the key doesn't match,
                    accessed by ‘keymap-default-binding’.

Keys are customarily a character or a keyword. Keywords are used to indicate
keys or events that aren't characters, like :F1 or :RESIZE.
Key bindings are customarily a function designator (so therefore a symbol or a
function), a list which represents function application, or another keymap,
where the next element in a key sequence is looked up.

Keymaps:
  macro DEFKEYMAP name (&key name default-binding) [docstring] alist
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
   #:key-definition
   #:key-definition-action
   #:key-definition-category
   #:key-definition-equal
   #:keymap
   #:keymap-map
   #:keymap-name
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

;; (declaim (optimize (debug 3)))

(defclass key-definition ()
  ((action
    :initarg :action :accessor key-definition-action  :initform nil
    :documentation
    "Ususally a function to call, or form to eval, or a keymap.")
   (category
    :initarg :category :accessor key-definition-category  :initform nil
    :documentation "A descriptive category, mostly for documentation."))
  (:documentation "The value of a key binding."))

(defgeneric key-definition-equal (k1 k2)
  (:documentation
   "Return true if key-definitions ‘k1’ and ‘k2’ are considered equal. This
ignores the category.")
  (:method (k1 k2)
    (let ((a1 (if (typep k1 'key-definition) (key-definition-action k1) k1))
	  (a2 (if (typep k2 'key-definition) (key-definition-action k2) k2)))
      (equal a1 a2))))

(defclass keymap ()
  ((map
    :accessor		keymap-map
    :initarg		:map
    :documentation	"Hash table of key to binding.")
   (name
    :initarg :name :accessor keymap-name :initform nil
    :documentation "A name for this keymap when printing.")
   (default-binding
    :accessor		keymap-default-binding
    :initarg		:default-binding
    :initform		nil
    :documentation	"Binding if there is none in map."))
  (:documentation "Relates input events to commands."))

(defun keymap-p (object)
  "Return true if OBJECT is a keymap."
  (typep object 'keymap))

(defun binding-list-to-table (list)
  "Take a list of items which can be either (key . action) or a category, and
return a hash table of key-definitions. Items which are atoms set the category
for subsequent definitions."
  (let ((table (make-hash-table))
	(category))
    (loop :for item :in list
	  :do
	  (typecase item
	    (cons
	     (setf (gethash (car item) table)
		   (make-instance 'key-definition
				  :action (cdr item)
				  :category category)))
	    (t
	     (setf category item))))
    table))

(defun set-keymap (keymap map)
  "Initialize the map of ‘keymap’ from the list ‘map’ as described in
#'binding-list-to-table."
  (when (and map (consp map))
    (setf (keymap-map keymap)
	  ;; (alist-to-hash-table map :table (make-hash-table))
	  (binding-list-to-table map)
	  ))
  keymap)

;; @@@ Make the map be an alist under a certain threshold of population, but
;; allow a hint to make a big one when creating.
(defmethod initialize-instance :after ((k keymap) &rest initargs)
  "Initialize a keymap. If the map is a list convert it into a hash table,
with binding-list-to-table."
  (declare (ignore initargs))
  (if (and (slot-boundp k 'map) (consp (slot-value k 'map)))
      (set-keymap k (keymap-map k))
      (setf (slot-value k 'map) (make-hash-table))))

(defmethod print-object ((obj keymap) stream)
  "Print a KEYMAP on a STREAM."
  (print-unreadable-object (obj stream :type t #|:identity t|#)
    (format stream "~a [~d]" (keymap-name obj)
	    (and (hash-table-p (keymap-map obj))
		 (hash-table-count (keymap-map obj))))))

(defmacro defkeymap (var-name (&key name default-binding) &body body)
  "Define a keymap.
 ‘var-name’ is a variable the keymap is bound to.
 ‘name’ is a descriptive name for printing. It defaults to ‘var-name’.
 ‘default-binding’ is what keys that aren't defined are bound to.
 ‘body’ is of the form:
    [docstring] binding-list
 where binding-list is an alist of (key . binding) or category.
  ‘key’ is a character or keyword.
  ‘binding’ is a symbol or a function to be called or a form to be evaluated.
  ‘category’ describes the category of subsequent bindings."
  (let (docstring map)
    (when body
      ;; See if there's a docstring
      (when (stringp (first body))
	(setf docstring (pop body)))
      (setf map (pop body))
      ;; See if there's a default-binding keyword
      ;; (when (eql (first body) :default-binding)
      ;; 	(pop body)
      ;; 	(setf default-binding (pop body)))
      )
    `(defparameter ,var-name
       (make-instance 'keymap:keymap
		      :name ',(or name var-name)
		      :map ,map
		      ,@(when default-binding
			  `(:default-binding ,default-binding)))
       ,@(when docstring `(,docstring)))))

(defun map-keymap (function keymap)
  "Call ‘function’ on each binding in ‘keymap’."
  (maphash function (keymap-map keymap)))

(defun define-key (keymap key definition)
  "Define the ‘key’ as ‘definition’ in the ‘keymap’. ‘keymap’ must be a keymap
object. ‘key's are compared with ‘eql’. See also #'set-key."
  (setf (gethash key (keymap-map keymap))
	(if (typep definition 'key-definition)
	    definition
	    (make-instance 'key-definition :action definition))))

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
  ‘key-sequence’  A single key, or a sequence of keys. Keys currently have
                  the restriction that they have to be EQL.
  ‘definition’    Customarily a function designator or a list to be applied,
                  but has no formal restriction.
  ‘keymap’        A keymap object or keymap stack, which is a list of keymap
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

(defun key-binding (key keymap &key use-default)
  "Return the binding of single key in single keymap. Return the default
binding if it is set and no other binding has been specified or NIL if
there is no binding."
  (let ((def (or (gethash key (keymap-map keymap))
		 (and use-default
		      (keymap-default-binding keymap)))))
    (typecase def
      (key-definition (key-definition-action def))
      (t def))))

;; @@@ clean up keymap traversal code in here key-sequence-binding and
;; inator:process-event.
;; @@@ This is confusing with the ‘key-definition’ class.
(defun key-definition (key keymap-stack &key use-default)
  "Return the definition of ‘key’ in ‘keymap-stack’. If ‘use-default’ is true,
use the default binding in keymaps."
  (cond
    ((typep keymap-stack 'sequence)
     (let (binding)
       (or (map nil
		(lambda (m)
		  (when (setf binding (key-binding key m))
		    (return-from key-definition binding)))
		keymap-stack)
	   ;; Check for default bindings in order.
	   (and use-default
		(map nil
		     (lambda (m)
		       (when (setf binding (keymap-default-binding m))
			 (return-from key-definition binding)))
		     keymap-stack)))))
    ;; handle a single keymap
    ((typep keymap-stack 'keymap)
     (key-binding key keymap-stack :use-default use-default))
    (t
     (error "Key definition must be looked up in either a keymap or a list
 of keymaps."))))

(defun key-sequence-binding (keyseq keymap-stack)
  "Return the binding for the key sequence ‘keyseq’ in ‘keymap-stack’ which can
be a keymap or a keymap stack."
  (labels ((try-map (keyseq map use-default)
	     (if (and (or (vectorp keyseq) (listp keyseq))
		      (not (zerop (length keyseq))))
		 ;; More than one key in the sequence
		 (let* ((key (elt keyseq 0))
			(action (key-definition key map
						:use-default use-default)))
		   (cond
		     ;; If the key has a symbol keymap binding
		     ((and (symbolp action) (boundp action)
			   (keymap-p (symbol-value action)))
		      ;; look that up
		      (try-map (subseq keyseq 1) (symbol-value action)
			       use-default))
		     ((keymap-p action)
		      (try-map (subseq keyseq 1) action use-default))
		     (t
		      ;; simple binding
		      action)))
		 ;; Just one key
		 (key-definition keyseq map :use-default use-default))))
    (let (binding)
      (etypecase keymap-stack
	(sequence
	 (or (map nil (_ (when (setf binding (try-map keyseq _ nil))
			   (return-from key-sequence-binding binding)))
		  keymap-stack)
	     ;; Check for default bindings in order.
	     (map nil (_ (when (setf binding (try-map keyseq _ t))
			   (return-from key-sequence-binding binding)))
		  keymap-stack)))
	(keymap
	 (setf binding (try-map keyseq keymap-stack t))))
      binding)))

(defun key-sequence-string (keyseq)
  "Covert a key sequence to a human readable string."
  (cond
    ((vectorp keyseq)
     (format nil "~{~A~^ ~}"
	     (loop :for i :across keyseq :collect (nice-char i))))
    ((listp keyseq)
     (format nil "~{~A~^ ~}"
	     (loop :for i :in keyseq :collect (nice-char i))))
    (t
     (nice-char keyseq))))

(defun get-key-sequence (get-key-function keymap)
  "Read a sequence of keys returned by ‘get-key-function’, starting with
‘keymap’. Descend into keymaps. Return a key or sequence of keys."
  (let* ((c (funcall get-key-function))
	 (action (key-definition c keymap :use-default t)))
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
  "Add all the key definitions from the ‘source’ keymap to the ‘dest’ keymap.
If there is already a binding for a key in ‘dest’, it gets overwritten by the
one in ‘source’."
  (check-type source keymap)
  (check-type dest keymap)
  (loop :for k :being :the :hash-keys :of (keymap-map source)
     :do
     (define-key dest k (gethash k (keymap-map source))))
  dest)

(defun describe-keymap (map &key (stream *standard-output*) prefix raw)
  "Show the bindings of a keymap ‘map’ on ‘stream’. If ‘prefix’ is given it is
assumed to be a prefix for all bindings in the keymap. ‘stream’ defaults to
‘*standard-output*'."
;  (format stream "~:@(~a~):~%" (named-name map))
  (map-keymap
   #'(lambda (key def)
       (let ((action (key-definition-action def)))
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
			      :stream stream :prefix (nice-char key)))))
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

(defun copy-keymap (keymap &key name)
  "Return a new copy of KEYMAP."
  (check-type keymap keymap)
  (let* ((new-name (or name (format nil "copy of ~s" (keymap-name keymap))))
	 (new-keymap (make-instance
		     (type-of keymap)
		     :name new-name
		     :default-binding (keymap-default-binding keymap))))
    (map-keymap #'(lambda (k f) (define-key new-keymap k f))
		keymap)
    new-keymap))

;; This is very useful if you want Escape and Meta to be equivalent.
(defun build-escape-map (keymap &key name)
  "Return a new keymap containing only the meta character bindings in ‘keymap’
 converted to non-meta character bindings. This is useful for making a keymap
to bind to the escape key, so you have escape key equivalents to meta keys."
  (check-type keymap keymap)
  (let* ((new-name (or name (format nil "ESC-~s" (keymap-name keymap))))
	 (new-keymap (make-instance (type-of keymap) :name new-name)))
    (map-keymap
     #'(lambda (k f)
	 (when (and (characterp k) (meta-char-p (char-code k)))
	   (define-key new-keymap (un-meta (char-code k)) f)))
     keymap)
    new-keymap))

(defun substitute-key-definition (new-definition old-definition keymap)
  "Substitute ‘new-definition’ for every binding of ‘old-definition’ in
‘keymap’. Definitions are compared with ‘key-definition-equal’."
  (map-keymap #'(lambda (key def)
		  ;; (when (eq def old-definition)
		  (when (key-definition-equal def old-definition)
		    (define-key keymap key new-definition))) keymap)
  keymap)

(defun keys-bound-to (definition keymap &key test)
  "Return a list of keys bound to ‘definition’ in ‘keymap’. Definitions are
compared with ‘test’, which defaults to ‘key-definition-equal’."
  (let ((results))
    (map-keymap (if test
		    #'(lambda (key def)
			(when (funcall test def definition)
			  (push key results)))
		    #'(lambda (key def)
			(when (key-definition-equal def definition)
			  (push key results))))
		    keymap)
    (remove-duplicates results)))

;; EOF
