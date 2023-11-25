;;;
;;; inator.lisp - Generic UI applet
;;;

(defpackage :inator
  (:documentation
"This is a little scaffolding for making a certain style of applet. The style
that's encouraged is what one might call “emacs-like”. It encapsulates a event
loop, a set of key bindings, generic functions for viewing and editing, with
one or many editing locations, and minimal user interaction with messages,
prompts, and help.

To make an app, you subclass ‘inator’ and provide movement, optional editing,
input, and display methods. You can provide a custom keymap in ‘default-keymap’.
You can probably get by with just providing methods for ‘update-display’ and
‘await-event’, and then calling ‘invoke’.")
  (:use :cl :dlib :keymap :char-util)
  (:export
   ;; Inator classes
   #:editing-location
   #:editing-context
   #:copy-editing-context
   #:base-inator
   #:inator
   #:inator-keymap
   #:inator-default-keymap
   #:inator-local-keymap
   #:inator-point
   #:inator-mark
   #:inator-clipboard
   #:inator-contexts
   #:inator-quit-flag
   #:inator-command
   #:inator-last-command
   #:inator-event-sequence
   #:inator-views
   #:multi-inator-mixin
   #:multi-inator

   ;; Commands
   #:next
   #:previous
   #:forward-unit
   #:backward-unit
   #:forward-multiple
   #:backward-multiple
   #:next-page
   #:previous-page
   #:move-to-beginning
   #:move-to-end
   #:move-to-top
   #:move-to-bottom
   #:search-command
   #:sort-command
   #:jump-command
   #:quit
   #:accept
   #:redraw
   #:cut
   #:copy
   #:paste
   #:select
   #:default-action
   #:describe-key-briefly
   #:show-menu

   ;; Output
   #:message
   #:prompt
   #:help

   ;; Events
   #:event-loop
   #:update-display
   #:await-event
   #:process-event
   #:call-command
   #:resize
   #:*default-inator-keymap*
   #:*default-inator-escape-keymap*
   #:read-key-sequence

   ;; Inator management
   #:start-inator
   #:finish-inator
   #:with-inator
   #:*inator*
   #:run
   #:invoke

   ;; Misc
   #:*clipboard*
   ))
(in-package :inator)

(defclass editing-location ()
  ()
  (:documentation "A generic location for edits to happen."))

(defclass editing-context ()
  ((point
    :initarg :point :accessor inator-point
    ;; :type editing-location
    :documentation "The location that edits happen at.")
   (mark
    :initarg :mark :accessor inator-mark
    ;; :type editing-location
    :documentation "A location that defines the selection.")
   (clipboard
    :initarg :clipboard :accessor inator-clipboard
    :documentation "A saved piece of the edited thing for copying."))
  (:documentation "A context for editing operations."))

(defvar *clipboard* nil
  "A place to copy and paste, if you'd like.")

;; Some struct-like generics:

(defgeneric copy-editing-context (editing-context)
  (:documentation "Copy an editing context.")
  (:method (editing-context)
    (make-instance (type-of editing-context)
		   :point     (inator-point     editing-context)
		   :mark      (inator-mark      editing-context)
		   :clipboard (inator-clipboard editing-context))))

;; (defgeneric make-editing-context (&rest keys &key point mark clipboard
;; 				    &allow-other-keys)
;;   (apply #'make-instance 'editing-context keys))

#|──────────────────────────────────────────────────────────────────────────┤#
 │Inators and keymaps.
 │
 │There are three slots related to keymaps:
 │   ‘keymap’
 │     The current keymap or keymap sequence that is actually used.
 │   ‘default-keymap’
 │     A keymap for the specific inator class.
 │   ‘local-keymap’
 │     A keymap for making customizations to a specific inator instance.
 │
 │Inator implementors should set the default keymap with customizations that
 │they want for that inator class. The ‘default-keymap’ will get pushed on the
 │‘keymap’ by the default inator initialization if ‘keymap’ is not explicitly
 │provided to ‘make-instance’.
 │
 │Users can create bindings in the default keymap of the class to customize the
 │keymap for all instances of that class.
 │
 │Users can create bindings in the ‘local-keymap’ to customize a specific
 │instance. A key setting command should probably create the ‘local-keymap’ if
 │there isn't one.
 │
 │The ‘keymap’ slot should probably not be set directly except in unusual
 │circumstances.
 │
 │Be aware that keymaps with a ‘default-binding’ can block other keymaps in a
 │keymap sequence, so they should rarely be used.
 ╰|#

;;(defkeymap *default-inator-keymap* (:default-binding 'default-action)
;; @@@ The problem is if there is default binding then it stops other
;; keymaps from inheriting an event. I think if you want a default binding
;; you should probably add it explicitly.
(defkeymap *default-inator-keymap* ()
  `((,(ctrl #\n)	. next)
    (,(ctrl #\p)	. previous)
    (,(ctrl #\f)	. forward-unit)
    (,(ctrl #\b)	. backward-unit)
    (,(meta-char #\f)	. forward-multiple)
    (,(meta-char #\b)	. backward-multiple)
    (,(ctrl #\v)	. next-page)
    (:page-down		. next-page)
    (,(meta-char #\v)	. previous-page)
    (:page-up		. previous-page)
    (,(ctrl #\a)	. move-to-beginning)
    (,(ctrl #\e)	. move-to-end)
    (:home		. move-to-beginning)
    (:end		. move-to-end)
    (,(meta-char #\<)	. move-to-top)
    (,(meta-char #\>)	. move-to-bottom)
    (:c-home		. move-to-top)
    (:c-end		. move-to-bottom)
    (,(ctrl #\s)	. search-command)
    (,(meta-char #\s)	. sort-command)	; ?
    (,(meta-char #\j)	. jump-command)	; ?
    (,(meta-char #\=)   . describe-key-briefly)
    (,(meta-char #\m)	. show-menu)
    (#\return		. accept)
    (,(ctrl #\l)	. redraw)
    (,(ctrl #\g)	. quit)
    (:resize		. resize)
    (#\escape		. *default-inator-escape-keymap*)
    ))

(defparameter *default-inator-escape-keymap*
  (build-escape-map *default-inator-keymap*))

(defclass base-inator ()
  ((keymap
    :initarg :keymap :accessor inator-keymap :initform nil
    :documentation "What to do when a key is pressed.")
   (default-keymap
    :initarg :default-keymap :accessor inator-default-keymap :initform nil
    :documentation "Keymap to push if the keymap isn't given.")
   (local-keymap
    :initarg :local-keymap :accessor inator-local-keymap :initform nil
    :documentation "Keymap for instance specific bindings.")
   (contexts
    :initarg :contexts :accessor inator-contexts
    :documentation "A set of contexts for editing.")
   (quit-flag
    :initarg :quit-flag :accessor inator-quit-flag :initform nil :type boolean
    :documentation "True to quit the inator.")
   (command
    :initarg :command :accessor inator-command :initform nil
    :documentation "The current command.")
   (last-command
    :initarg :last-command :accessor inator-last-command :initform nil
    :documentation "The last command.")
   (event-sequence
    :initarg :event-sequence :accessor inator-event-sequence
    :type list :initform nil
    :documentation "The sequence of events that invoked the command.")
   (views
    :initarg :views :accessor inator-views
    :documentation "A set of views of the inator."))
  (:default-initargs
   :default-keymap *default-inator-keymap*)
  (:documentation
   "Have some kind of editing style interaction."))

(defclass inator (base-inator editing-context)
  ()
  (:documentation
   "An editor with a single editing context."))

(defclass multi-inator-mixin ()
  ((contexts
    :initarg :contexts :accessor inator-contexts
    :documentation "A set of contexts for editing."))
  (:documentation
   "Mixin for multiple editing contexts."))

(defclass multi-inator (base-inator multi-inator-mixin)
  ()
  (:documentation "An editor with multiple editing contexts."))

;; Navigation
(defgeneric next (inator))
(defgeneric previous (inator))
(defgeneric forward-unit (inator))
(defgeneric backward-unit (inator))
(defgeneric forward-multiple (inator))
(defgeneric backward-multiple (inator))
(defgeneric next-page (inator))
(defgeneric previous-page (inator))
(defgeneric move-to-beginning (inator))
(defgeneric move-to-end (inator))
(defgeneric move-to-top (inator))
(defgeneric move-to-bottom (inator))
(defgeneric search-command (inator))
(defgeneric sort-command (inator))
(defgeneric jump-command (inator)) ; or maybe leap?

;; Action
(defgeneric quit (inator)
  (:documentation "Quit the Inator.")
  (:method ((i inator)) (setf (inator-quit-flag i) t)))

(defgeneric accept (inator)
  (:documentation "Accept the data and usually exit."))

(defgeneric redraw (inator)
  (:documentation "Redraw the screen."))

;; Edit?
(defgeneric cut (inator))		; kill
(defgeneric copy (inator))
(defgeneric paste (inator))		; yank
(defgeneric select (inator))		; mark
(defgeneric default-action (inator &optional event))

;; Help / Output

(defgeneric message (inator format-string &rest args)
  (:documentation "Display a short message. Usually one line."))

(defgeneric prompt (inator format-string &rest args)
  (:documentation "Display a short message, asking the user for input."))

(defgeneric help (inator)
  (:documentation "Display help, usually describing what keys do."))

(defgeneric describe-key-briefly (inator)
  (:documentation "Describe what a key does very briefly."))

(defgeneric show-menu (inator)
  (:documentation "Show a menu of commands."))

;; Whole Inator functions
(defgeneric event-loop (inator)
  (:documentation "Start the inator, then continue updating the display,
waiting for events, and processing events, util the quit-flag is set.
Finish the inator when done."))

(defgeneric update-display (inator)
  (:documentation "Make changes to the inator state observable."))

(defgeneric await-event (inator)
  (:documentation "Wait for an event. Return NIL if there was no event, for
example if reading timed out, in which case the event-loop normally does
not call ‘process-event’ with it."))

(defgeneric process-event (inator event &optional keymap-in)
  (:documentation
  "Call commands from the keyamps for event. Can call ‘await-event’ to get more
events if needed. This is usually called with an event from ‘await-event’ inside
the ‘event-loop’. This should set ‘last-command’ if applicable. If ‘keymap-in’
is specified, use it instead of the inator's keymaps."))

(defgeneric start-inator (inator)
  (:documentation "Prepare the inator for entering the event-loop.
Paired with ‘finiish-inator’"))

(defgeneric finish-inator (inator)
  (:documentation
   "Called when the event-loop is exiting to deactivate the inator. Paired
with ‘start-inator’."))

(defgeneric resize (inator)
  (:documentation "Called when the inator gets a resize event."))

(defgeneric read-key-sequence (inator)
  (:documentation "Read a key sequence from the inator input."))

;; Default methods

(defmethod default-action ((inator inator) &optional event)
  "Default method which does nothing."
  (message inator "Event ~{~a ~}is not bound."
	   (if (consp event)
	       (mapcar #'nicer-event event)
	       (list (nicer-event event)))))

(defmethod initialize-instance
    :after ((o inator) &rest initargs &key &allow-other-keys)
  "Initialize a inator."
  ;; Add all the default keymaps from superclasses if keymap isn't given in an
  ;; initarg.
  (when (not (getf initargs :keymap))
    (let ((c (class-of o)) val)
      (labels
	((push-keymap (class keymap-init-arg &optional no-ii)
	   (cond
	     ((setf val (assoc keymap-init-arg
			       (mop:class-direct-default-initargs class)))
	      (pushnew (funcall (third val)) (slot-value o 'keymap)))
	     ((and (not no-ii) (setf val (getf initargs keymap-init-arg)))
	      (pushnew val (slot-value o 'keymap)))))
	 (push-supers (class)
	   (loop :for cc :in (mop:class-precedence-list class)
		 :do (push-keymap cc :default-keymap t))))
	(push-keymap c :default-keymap)
	(push-supers c)
	;; Just reversing the order isn't perfect, but it's better than nothing.
	(setf (slot-value o 'keymap) (nreverse (slot-value o 'keymap)))
	(push-keymap c :local-keymap)))))

(defmethod start-inator ((inator inator))
  "Default method which does nothing."
  (declare (ignore inator)))

(defmethod finish-inator ((inator inator))
  "Default method which does nothing."
  (declare (ignore inator)))

(defmethod resize ((inator inator))
  "Default method which calls redraw."
  (redraw inator))

(defmethod call-command ((inator inator) function args)
  "Default method to invoke inator commands. This can be useful to extend
command invocation, or have something done on every command. The default
is just to call ‘function’ with the ‘inator’ as the first argument and the
list ‘args’ as the subsequent arguments."
  (apply function inator args))

(defgeneric nicer-event (event)
  (:documentation
   "Return a version of event which is nicer for a person to read.")
  (:method (e) e)
  (:method ((e character))
    (nice-char e)))

;; @@@ This is quite hairy and not really reflected in keymap.lisp
;; @@@ clean up keymap traversal in here and keymap.lisp
(defmethod process-event ((inator inator) event &optional keymap-in)
  "Default way to process an event. If ‘keymap-in’ is specified, use it instead
of the inator's keymap."
  (with-slots (command last-command event-sequence keymap) inator
    (setf last-command command
	  event-sequence nil)
    (let ((outer-map (or keymap-in keymap))
	  event-list result saved-list use-default)
      (labels
	  ((apply-symbol (s &optional args)
	     (if (typep (symbol-function s) 'generic-function)
		 (if (compute-applicable-methods (symbol-function s)
						 (cons inator args))
		     (call-command inator s args)
		     (message inator "(~S) has no applicable methods." s))
		 (call-command inator s args)))
	   (get-event ()
	     "Get a event from L, or if not L then push on event-list."
	     (dbugf :event "get-event ~s~%" saved-list)
	     (if saved-list
		 (pop saved-list)
		 (let ((ev (await-event inator)))
		   (push ev event-list)
		   ev)))
	   (sub-process (ev map)
	     "Look up the definition in keymap ‘map’ and try to invoke it."
	     (dbugf :event "sub-process ~s ~s ~s~%" ev saved-list map)
	     (when (setf command (key-definition ev map
						 :use-default use-default))
	       (invoke)))
	   (invoke ()
	     "Try to invoke command."
	     (dbugf :event "invoke ~s ~s~%" saved-list command)
	     (setf event-sequence (append (list event) event-list))
	     (cond
	       ;; a list to apply
	       ((consp command)
		(if (fboundp (car command))
		    (progn
		      (apply-symbol (car command) (cdr command))
		      t)
		    (progn
		      (message inator "(~S) is not defined." (car command))
		      (return-from process-event))))
	       ;; something represted by a symbol
	       ((symbolp command)
		(cond
		  ; a function
		  ((fboundp command)
		   (apply-symbol command)
		   t)
		  ; a keymap
		  ((keymap-p (symbol-value command))
		   (sub-process (get-event) (symbol-value command)))))
	       ;; a plain keymap
	       ((keymap-p command)
		(sub-process (get-event) command))
	       ;; a function object
	       ((functionp command)
		(call-command inator command nil)
		t)
	       (t ; anything else
		(message inator "Key binding ~S is not a function or a keymap."
			 command)
		(return-from process-event nil))
	       ;; ((not command)
	       ;; 	nil)
	       ;; (t ;; anything else is an error
	       ;; 	(error "Weird thing in keymap: ~s." command))
	       )))
	;; (push event event-list)
	(if (typep outer-map 'sequence) ;(listp outer-map)
	    ;; Try all the keymaps in outer-map, saving events in event-list,
	    ;; until one invocation returns true. Re-pull events from
	    ;; event-list for subsequent lookups.
	    (catch 'out
	      (map nil (lambda (keymap)
			 (when (setf result (sub-process event keymap))
			   (throw 'out nil))
			 (setf saved-list (reverse event-list)))
		   outer-map)
	      (setf use-default t)
	      (map nil (lambda (keymap)
			 (when (setf result (sub-process event keymap))
			   (throw 'out nil))
			 (setf saved-list (reverse event-list)))
		   outer-map))
	    ;; Just one to try.
	    (setf use-default t
		  result (sub-process event outer-map)))
	(when (not result)
	  (default-action inator (append (list event) event-list)))))))

(defmethod event-loop ((inator inator))
  "The default event loop. Using this loop a sub-class only has to supply the
UPDATE-DISPLAY and and AWAIT-EVENT methods."
  (unwind-protect
       (progn
	 (start-inator inator)
	 (update-display inator)
	 (loop :with event
	    :do
	    (when (setf event (await-event inator))
	      (process-event inator event))
	    :while (not (inator-quit-flag inator))
	    :do
	    (update-display inator)))
    (finish-inator inator)))

;; Yet another thin defclass wrapper.
#|
(defmacro definator (name superclasses slots &rest options)
  "Define an INATOR with class"
  (let* ((var-name (intern (s+ #\* name #\*)))
	 (with (intern (s+ "WITH-" (string-upcase name))))
	 keymap
	 real-options
	 initargs)
    (setf real-options
	  ;; round 1 - pick out a :keymap option
	  (loop :for o :in options
	     :if (and (consp o) (eq (car o) :keymap))
	     :do (setf keymap (second o))
	     :else
	     :collect o)
	  ;; round 2 - put it back in a :default-initargs for the class
	  real-options
	  (if (setf initargs (find :default-initargs real-options #'car))
	      ;;
	      ))
    `(progn
       (defvar ,var-name nil ,(s+ "The current " name "."))
       (defclass ,name ,@superclasses ,@slots ,@real-options)
       (defmacro ,with (var)
	 `(let ((,var (make-ins
       )))
|#

(defmacro with-inator ((var type &rest args) &body body)
  "Evaluate ‘body’ with a new inator of type ‘type’, made with ‘args’ passed to
MAKE-INSTANCE, and with ‘var’ bound to the new instance."
  `(let ((,var (make-instance ,type ,@args)))
     ,@body))

;; This encourages a one current nested inator per thread model. But we should
;; probably allow for different models.
(defvar *inator* nil
  "The current inator.")

(defgeneric run (inator &rest keys &key &allow-other-keys)
  (:documentation "Enter the event loop of ‘inator’. Return when it exits."))

(defmethod run ((inator inator) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (event-loop inator))

(defgeneric invoke (type &rest keys &key &allow-other-keys)
  (:documentation "Create and run an inator of ‘type’."))

(defmethod invoke (type &rest keys &key &allow-other-keys)
  (let ((*inator* (apply 'make-instance type keys)))
    (run *inator*)))

;; End
