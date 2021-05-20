;;;
;;; dired.lisp - Directory editor.
;;;

(defpackage :dired
  (:documentation "Directory editor.")
  (:use :cl :dlib :opsys :collections :keymap :char-util :fatchar :terminal
	:inator :terminal-inator :terminal-table :table-viewer :lish
	:view-generic :ochar :ostring)
  (:export
   #:dired
   ))
(in-package :dired)

(defstruct mark
  "A thing to mark objects with."
  name
  display)

(defparameter *delete-mark* (make-mark :name 'D  :display (ß `(:red "D")))
  "A mark for things to be deleted.")

(defparameter *select-mark* (make-mark :name 'M  :display (ß `(:white "M")))
  "A generic selection mark.")

(defun mark-equal (a b)
  "Return true if we consider the marks equal."
  (flet ((prepare (x)
	  (etypecase x
	    ((or string vector fat-string)
	     (symbolify (osimplify x) :package :dired))
	    (symbol x)
	    (mark
	     (mark-name x)))))
    (equal (prepare a) (prepare b))))

(defkeymap *dired-keymap* ()
  `((#\q			. quit)
    (#\Q			. quit-all)
    (#\?			. help)
    (:home			. move-to-top)
    (:end			. move-to-bottom)
    (:up			. previous)
    (:down			. next)
    (:left			. backward-unit)
    (:right			. forward-unit)
    (#\<			. move-to-top)
    (#\>			. move-to-bottom)
    (,(char-util:meta-char #\<)	. move-to-top)
    (,(char-util:meta-char #\>)	. move-to-bottom)
    (#\d			. mark-for-delete)
    (#\r			. rename)
    (#\m			. mark-item)
    (#\u			. unmark-item)
    (#\g			. refresh)
    (#\x			. execute)
    ;; (:right		        . move-right)
    ;; (:left		        . move-left)
    ;;(,(meta-char #\i)	        . table-info)
    ;; (#\escape		. *lazy-menu-escape-keymap*)
    ))

(defvar *dired* nil
  "The current directory editor.")

(defclass directory-editor (table-viewer)
  ((directory
    :initarg :directory :accessor dired-directory
    :initform (nos:current-directory)
    :type (or pathname string)
    :documentation "The pathname of the directory we're editing.")
   (last
    :initarg :last :accessor directory-editor-last :initform nil
    :documentation "The editor that invoked this one."))
  (:default-initargs
   :keymap `(,*dired-keymap* ,table-viewer::*table-viewer-keymap*
	     ,*default-inator-keymap*))
  (:documentation "A directory editor."))

(defun file-cell (row)
  "Return the file cell of a table ‘row’."
  (oelt row (1- (olength row))))

(defun set-file-cell (row value)
  "Set the file cell of a table ‘row’ to ‘value’."
  (setf (oelt row (1- (olength row))) value))

(defsetf file-cell set-file-cell
  "Set the file cell of a table ‘row’ to ‘value’.")

;; (defmacro %file-cell (row)
;;   `(oelt ,row (1- (olength ,row)))) ;; @@@ double eval, so watch out!

(defun current-file-cell (o)
  "Return the file cell of the current row."
  (with-accessors ((table table-viewer-table)
		   (renderer table-viewer-renderer)) o
    (with-slots (table-viewer::current-position) renderer
      (let ((row (oelt table (table-point-row table-viewer::current-position))))
	(file-cell row)))))

(defun mark-cell (row)
  (oelt row 0))

(defun set-mark-cell (row value)
  (setf (oelt row 0) value))

(defsetf mark-cell set-mark-cell)

(defgeneric current-mark-cell (dired)
  (:documentation "Accessor for the mark cell."))

(defmethod current-mark-cell ((o directory-editor))
  "Return the mark cell of the current table row."
  (with-accessors ((table table-viewer-table)
		   (renderer table-viewer-renderer)) o
    (with-slots (table-viewer::current-position) renderer
      (let ((row (oelt table (table-point-row table-viewer::current-position))))
	(mark-cell row)))))

(defmethod (setf current-mark-cell) (value (o directory-editor))
  "Set the mark cell of the current table row."
  (with-accessors ((table table-viewer-table)
		   (renderer table-viewer-renderer)) o
    (with-slots (table-viewer::current-position) renderer
      (let ((row (oelt table (table-point-row table-viewer::current-position))))
	(setf (mark-cell row) value)))))

(defmethod view-cell ((o directory-editor))
  (with-slots (directory last) o
    (handler-bind
	((error (lambda (c)
		  (if (fui:popup-y-or-n-p
		        (span-to-fat-string
			 `((:red "Error: ") ,(apply #'format nil "~a" (list c))
			   #\newline #\newline "Enter the debugger?"))
			:default #\N)
		      (invoke-debugger c)
		      (continue c)))))
      (let* ((name (osimplify (current-file-cell o)))
	     (full (nos:path-append directory name)))
	(cond
	  ;; If we're going back up to the previous editor, just quit.
	 ((and last (equal name "..")
	       (equal (nos:path-to-absolute full)
		      (dired-directory last)))
	  (quit o))
	 ;; Otherwise, make a new one.
	 (t
	  (view:view full)))))))

(defun quit-all (o)
  "Quit all recursive directory editors."
  (declare (ignore o))
  (throw 'quit-all nil))

(defmethod accept ((o directory-editor))
  "View the file."
  (view-cell o))

#|
(defun rename (o)
  "Change the name of the file."
  (with-slots ((renderer table-viewer::renderer)
	       (point inator::point)
	       (table table-viewer::table)
	       directory) o
    (with-slots ((x table-viewer::x)
		 (y table-viewer::y)
		 (start table-viewer::start)
		 (rows table-viewer::rows)
		 (cursor table-viewer::cursor)) renderer
      (let* ((old-name (ochar:osimplify (current-file-cell o)))
	     (new-name
	     (rl-widget:widget-read
	       :x (table-point-col cursor)
	       :y (table-point-row cursor)
	       :width (- (tt-width) (table-point-col cursor))
	       :buf (make-fat-string
		     :string (copy-seq old-name)))))
	(when (not (string= old-name new-name))
	  (rename-file (path-append old-name directory)
		       (path-append new-name directory)))))))
|#

(defun mark-for-delete (o)
  "Mark the current item for deleting."
  (setf (current-mark-cell o) (mark-display *delete-mark*))
  (next o))

(defun mark-item (o)
  "Mark the current item."
  (setf (current-mark-cell o) (mark-display *select-mark*))
  (next o))

(defun unmark-item (o)
  "Mark the current item."
  (setf (current-mark-cell o) " ")
  (next o))

(defun mark-file (o file-name mark)
  "Mark the row of ‘file-name’ in the table, with ‘mark’."
  (let ((row (ofind file-name (table-viewer-table o) :key #'file-cell)))
    (when row
      (setf (file-cell row) (mark-display mark)))))

(defun mark-files (o file-list mark)
  "Mark the files in ‘file-list’ with ‘mark’."
  (omap (lambda (row)
	  (when (member (file-cell row) file-list :test #'ostring=)
	    (setf (mark-cell row) (mark-display mark))))
	(table-viewer-table o)))

(defun input-window (title text-lines)
  (prog1
      (fui:display-text
       title text-lines
       :input-func #'(lambda (w)
			(rl-widget:widget-read
			 :x (+ (fui:fui-window-x w) 20)
			 :y (+ (fui:fui-window-y w)
			       (- (fui:fui-window-height w) 1))
			 :width 10 :height 1
			 :prompt ""
			 :rendition (make-fatchar :bg :blue :fg :white))))
    (tt-clear)
    (refresh *dired*)
    (tt-finish-output)))

(defun dired-yes-or-no-p (&optional format &rest arguments)
  (equalp "Yes" (input-window
		 nil ;;"Yes or No?"
		 (list
		  (if format
		      (apply #'format nil format arguments)
		      "Yes or No?")))))

(defun ask-delete-files (to-delete directory)
  (when (dired-yes-or-no-p
	 "Are you sure you want to delete these ~d files from storage?~%~%~
         ~{~a~%~}~%~
         (yes or no)? "
	 (length to-delete) to-delete)
    (loop :for file :in to-delete :do
      (delete-file (nos:path-append directory (osimplify file))))
    t))

(defun execute (o)
  "Execute the actions."
  (with-accessors ((table table-viewer-table) (directory dired-directory)) o
    (let (to-delete did-it)
      (omap (lambda (row)
	      (when (mark-equal (mark-cell row) *delete-mark*)
		 (push (file-cell row) to-delete)))
	    table)
      (if (null to-delete)
	  (fui:show-text "There is nothing to delete.")
	  (setf did-it (ask-delete-files to-delete directory)))
      ;; Update the file list and resize.
      (refresh o)
      ;; Replace the marks if we didn't delete them.
      (when (not did-it)
	(mark-files o to-delete *delete-mark*)))))

(defun update-items (o)
  (with-accessors ((directory dired-directory)
		   (table table-viewer-table)) o
    (setf table (ls:list-files :files `(,directory) :collect t :nice-table t
			       :long t :quiet t :hidden t)
	  table (table:insert-column 0 "Mark" table :value " "))
    (resize o)))

(defun refresh (o)
  "Refresh the directory listing."
  (update-items o)
  (tt-clear)
  (redraw o))

(defmethod update-display ((o directory-editor))
  (with-slots ((renderer table-viewer::renderer)
	       (point inator::point)
	       (table table-viewer::table)
	       (long-titles table-viewer::long-titles)
	       message directory) o
    (with-slots ((x table-viewer::x)
		 (y table-viewer::y)
		 (start table-viewer::start)
		 (rows table-viewer::rows)
		 (cursor table-viewer::cursor)) renderer
      (tt-home)
      (tt-erase-below)
      (tt-write-span `(:white ,directory ,nos:*directory-separator* #\newline))
      (setf y 1)

      ;; Adjust the view to the point
      (when (>= (table-point-row point) (+ (table-point-row start) rows))
	(setf (table-point-row start) (1+ (- (table-point-row point) rows))))
      (when (< (table-point-row point) (table-point-row start))
	(setf (table-point-row start) (table-point-row point)))

      ;; Unset the cursor
      (setf (table-point-row cursor) nil)

      ;; Tell the renderer where point is.
      (setf (table-viewer::current-position renderer) point)

      ;; Output the table rows
      ;; (dbugf :tv "Before output table ~s~%" long-titles)
      (table-print:output-table table renderer *terminal*
				:long-titles long-titles)

      ;; Show the message temporarily.
      (when message
	(message o (quote-format message))
	(setf message nil))

      ;; Make the cursor show up in the right spot.
      (when (table-point-row cursor) ;; @@@ XXX workaround
	;;(tt-move-to (table-point-row cursor) 0)
	(tt-move-to (+ y (table-point-row cursor)) x)))))

(defun dired (&optional (directory (nos:current-directory)))
  (let ((top-level (not *dired*)))
    (flet ((body ()
	    (with-terminal-inator (*dired* 'directory-editor
				   :directory directory
				   :last *dired*)
	      (update-items *dired*)
	      (inator:resize *dired*)
	      (table-print:table-output-sizes (table-viewer-renderer *dired*)
					      (table-viewer-table *dired*))
	      (event-loop *dired*)
	      (tt-move-to (1- (tt-height)) 0))))
      (if top-level
	  (with-terminal ()
	    (catch 'quit-all
	      (body))
	    (tt-move-to (1- (tt-height)) 0))
	  (body)))))

#+lish
(lish:defcommand dired
  ((directory directory :default '(nos:current-directory) :optional t
   :help "The directory to edit."))
  "Edit directories."
  (dired directory))

;; End
