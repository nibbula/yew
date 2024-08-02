;;;
;;; dired.lisp - Directory editor.
;;;

(defpackage :dired
  (:documentation "Directory editor.")
  (:use :cl :dlib :opsys :collections :keymap :char-util :fatchar :terminal
	:inator :terminal-inator :terminal-table :table-viewer :lish
	:view-generic :ochar :ostring :fancy-inator)
  (:export
   #:dired
   #:!dired
   #:*no-warning*
   #:*pause-after-running*
   ))
(in-package :dired)

(defvar *no-warning* nil
  "Set to squelch the warning.")

(defparameter *warning*
  (ß '((:red "Warning: ") "USE AT YOUR OWN RISK!!"
       (:yellow :blink "   *Warning*")
       #\newline
       #\newline (:cyan "dired")
       " does dangerous things like deleting files" #\newline
"    " (:bold "AND") " has not been tested very well.
        There is no undo! No trashcan!
           Please exert caution.

  [Set dired:*no-warning* to T to supress this].
")))

(defstruct mark
  "A thing to mark objects with."
  name
  display)

(defparameter *delete-mark* (make-mark :name 'D  :display (ß `(:red "D")))
  "A mark for things to be deleted.")

(defparameter *select-mark* (make-mark :name 'M  :display (ß `(:white "M")))
  "A generic selection mark.")

(define-constant +empty+ " ")
(defparameter *empty-mark* (make-mark :name nil :display +empty+)
  "The empty mark, or no mark.")

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

(defstruct clipboard
  "A clipboard for dired objects."
  items			; A sequence of files.
  operation)		; The operation to perform when pasting the clipboard.

(defparameter *clip* (make-clipboard)
  "The shared clipboard struct for directory-editors.")

(defkeymap *dired-keymap* ()
  `((#\q			. quit)
    (#\Q			. quit-all)
    (#\?			. help)
    "Movement"
    (:home			. move-to-top)
    (:end			. move-to-bottom)
    (:up			. previous)
    (:down			. next)
    (:left			. backward-unit)
    (:right			. forward-unit)
    (#\j			. next)
    (#\k			. previous)
    (#\h			. backward-unit)
    (#\l			. forward-unit)
    (#\space			. next-page)
    (#\b			. previous-page)
    (#\<			. move-to-top)
    (#\>			. move-to-bottom)
    (,(meta-char #\<)		. move-to-top)
    (,(meta-char #\>)		. move-to-bottom)
    (#\/			. search-command)
    "Editing"
    (,(ctrl #\@)		. select)
    (,(ctrl #\w)		. cut)
    (,(meta-char #\w)		. copy)
    (,(ctrl #\y)		. paste)
    ;;(,(char-util:ctrl #\x)	. cut)
    ;;(,(char-util:ctrl #\c)	. copy)
    ;;(,(char-util:ctrl #\v)	. paste)
    (#\d			. mark-for-delete)
    (#\r			. rename)
    (:f2                        . rename-in-place)
    (#\n			. new-directory)
    (#\+                        . new-directory)
    (#\m			. mark-item)
    (#\M			. mark-region)
    (#\u			. unmark-item)
    (#\U			. unmark-region)
    (#\V			. view-raw-command)
;    (#\C                        . copy-file-command)
;    (#\G                        . change-group-command)
;    (#\M                        . change-mode-command)
;    (#\O                        . change-owner-command)
;    (#\T                        . touch-command)
;    (#\Z                        . compress-command)
    "Other"
    (,(ctrl #\L)		. refresh)
    (#\g                        . refresh)
    (#\x			. execute)
    (#\!			. dired-run)
    (#\&			. dired-run-in-background)
    (,(meta-char #\x)           . shell-command)
    (#\^			. dired-up)
    (,(meta-char #\u)           . dired-up)
    (#\c			. dired-cd)
    (#\:			. shell-command)
    (#\X			. shell-command)
    (:mouse-1			. handle-click)
    (:mouse-3			. handle-menu)
    (#\escape			. *dired-escape-keymap*)
    (,(ctrl #\X)		. *dired-ctrl-x-keymap*)
    ))

(defparameter *dired-escape-keymap*
  (build-escape-map *dired-keymap*))

(add-keymap table-viewer::*table-viewer-escape-keymap* *dired-escape-keymap*)

(defkeymap *dired-ctrl-x-keymap* ()
  `((#\c		. show-clipboard)
    (,(ctrl #\X)	. toggle-region)))

(defvar *dired* nil
  "The current directory editor.")

(defclass fancy-table-viewer (table-viewer-base fancy-inator)
  ()
  (:documentation "A fancy table viewer."))

(defclass directory-editor (fancy-table-viewer)
  ((directory
    :initarg :directory :accessor dired-directory
    :initform (nos:current-directory)
    :type (or pathname string)
    :documentation "The pathname of the directory we're editing.")
   (last
    :initarg :last :accessor directory-editor-last :initform nil
    :documentation "The editor that invoked this one.")
   (click-pos
    :initarg :click-pos :accessor click-pos :initform nil
    :documentation
    "Cons of X and Y position of the start of the pointer click."))
  (:default-initargs
   :default-keymap *dired-keymap*)
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

(defun marked-files (o)
  "Return a list of file names marked as selected."
  (with-accessors ((table table-viewer-table)) o
    (with-collecting ()
      (omapn (_ (when (mark-equal (mark-cell _) *select-mark*)
		  (collect (osimplify (file-cell _))))) table))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-error-handling (() &body body)
    "Handle errors in ‘body’ and allow picking a restart."
    `(handler-bind
       ((serious-condition
          (lambda (c)
            (let ((restarts (compute-restarts c)))
              (fui:show-text
	       (list-restarts restarts c)
	       :input-func
	       (lambda (w)
                 (declare (ignore w))
                 (loop
                   :with key = (tt-get-key) :and digit
                   :do
                      (setf digit (and (characterp key) (digit-char-p key)))
                      (cond
			((or (eql key #\q)
                             (eql key #\a))
			 (invoke-restart (find-restart 'abort)))
			((and digit (< 0 digit (length restarts)))
			 ;; (invoke-restart (nth digit (cdr restarts))))
			 (invoke-restart (nth digit restarts)))
			(t (message *dired* "What? ~s" key))))))))))
       (with-simple-restart (continue "Continue with the directory editor.")
         (handler-case
             (progn ,@body)
           (dired-error (c)
             (tt-format-at 0 0 "~a~%" c)
             (tt-get-key)
	     ;; (message *dired* "~a" c)
             (continue c)
             )))))

  (defmacro with-simple-error-handling (() &body body)
    "Handle errors in ‘body’ and allow entering the debugger or ignoring."
    `(with-simple-restart (continue "Continue with the directory editor.")
       (handler-bind
	   ((error (lambda (c)
		     (if (fui:popup-y-or-n-p
		          (span-to-fat-string
			   `((:red "Error: ")
			     ,(apply #'format nil "~a" (list c))
			     #\newline #\newline "Enter the debugger?"))
			  :default #\N)
			 (invoke-debugger c)
			 (continue c)))))
	 ,@body)))

  (defmacro with-outside-terminal (() &body body)
    "Evaluate ‘body’ with the terminal set up for external commsnds. Make sure
the terminal is reset when comming back."
    `(unwind-protect
	  (progn
	    (tt-clear)
	    (tt-finish-output)
	    (terminal-end *terminal*)
	    ,@body)
       (terminal-reinitialize *terminal*)
       (tt-clear)
       (redraw *dired*))))

(defun full-path (o name)
  (nos:path-append (dired-directory o) (osimplify name)))

(defun view-thing (o &key raw)
  (with-slots (directory last) o
    (with-simple-error-handling ()
      (let* ((name (osimplify (current-file-cell o)))
	     (full (full-path o name)))
	(flet ((view-one (file)
		 (if raw
		     (view:view-raw file)
		     (view:view file)))
	       (view-multiple (files)
		 ;; @@@@ Work this out
		 ;; (let ((viewer (view:all-same-viewer files)))
		 ;;   (if (and viewer (view:can-view-multiple-p viewer))
		 ;; 	 (view:view-multiple files)
		 ;; 	 (if raw
		 ;; 	     (view:view-raw-things file)
		 ;; 	     (view:view-things file))))
		 (let ((files (mapcar (_ (full-path o _)) files)))
		   (if raw
		       (view:view-raw-things files)
		       (view:view-things files)))))
	  (cond
	    ;; If we're going back up to the previous editor, just quit.
	    ((and last (equal name "..")
		  (equal (nos:path-to-absolute full)
			 (dired-directory last)))
	     (quit o))
	    #| This shouldn't be on a view command!!
	      ;; Try to execute regular executable files? ;
	    ((and (is-executable full)	;
	    (eq :regular (file-info-type (file-info full)))) ;
	        ;; (!= full)		;
	    (lish:shell-eval (lish::expr-from-args (list full)) ;
	    :no-expansions t)		;
	    (tt-clear)			;
	    (redraw o)) |#
	    (t
	     ;; Otherwise, make a new one.
	     (let ((files (marked-files o)))
	       (if files
		   (view-multiple files)
		   (view-one full))
	       (tt-clear)
	       (redraw o)))))))))

(defmethod view-cell ((o directory-editor))
  (view-thing o))

(defun quit-all (o)
  "Quit all recursive directory editors."
  (declare (ignore o))
  (throw 'quit-all nil))

(defmethod accept ((o directory-editor))
  "View the file."
  (view-cell o))

(defun run-program-here (o &key pause background)
  (with-slots (directory) o
    (let* ((name (osimplify (current-file-cell o)))
	   (full (nos:path-append directory name)))

      ;; Try to execute regular executable files.
      (cond
	((and (is-executable full)
	      (eq :regular (file-info-type (file-info full))))
	 ;; (!= full)
	 (with-simple-error-handling ()
	   (with-outside-terminal ()
	     (with-working-directory (directory)
	       (lish:shell-eval (lish::expr-from-args (list full))
				:no-expansions t
				:context
				(if background
				    (lish::modified-context
				     lish::*context*
				     :background background)
				     lish::*context*))
	       (when pause
		 (write-line "[Press Return]")
		 (finish-output)
		 (read-line)))))))
      (values)))) ;; suppress sbcl note

(defvar *pause-after-running* t
  "Whether to pause after runninng a program.")

(defgeneric dired-run (dired)
  (:documentation "Run a file.")
  (:method ((o directory-editor))
    (run-program-here o :pause *pause-after-running*)))

(defgeneric dired-run-pause (dired)
  (:documentation "Run a file.")
  (:method ((o directory-editor))
    (run-program-here o :pause t)))

(defgeneric dired-run-in-background (dired)
  (:documentation "Run a file in the background.")
  (:method ((o directory-editor))
    (run-program-here o :background t)))

#|
(defun item-at (o x y)
  (let ((r (table-viewer-renderer o)))
    (with-slots (x y width height) r
      )))

(defun handle-click (o)
  (with-slots (click-pos) o
    (destructuring-bind (x . y) click-pos
      (
  )

(defun handle-menu (o)
  (declare (ignore o))
  ;; @@@
  )
|#

(defun dired-up (o)
  "Go to the parent directory."
  (with-slots (directory) o
    (let ((parent (path-parent directory)))
      (cond
	((not (equal parent directory))
	 (setf directory parent))
	(t
	 (let ((up (path-append directory "..")))
	   (if (file-exists up)
	       (setf directory (namestring (truename up)))
	       (notify o "I don't know how to go up from ~s." directory))))))
    (refresh o)))

(defun dired-cd (o)
  "Go to a directory."
  (with-slots (directory) o
    (let ((new-dir (input-window nil '("Go to directory:" ""))))
      (cond
	((not (file-exists new-dir))
	 (notify o "The directory ~s doesn't exist." new-dir))
	((not (equal new-dir directory))
	 (setf directory new-dir))))
    (refresh o)))

;; As an alternative to rename, it seems like it would be cool to do
;; multi-cursor editing of file names. Unfortunately it seems like it would
;; require some complicated inator/editor machinery that we don't have yet.
;; Some possibilities might be: an abstract editor over file names,
;; making a bunch of widgets and feeding them events, a one widget overlay
;; editor with disjoint lines?

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
      ;; we need to go to the file name cell, so we can get it's point
      (let* ((old-name (ochar:osimplify (current-file-cell o)))
	     new-name
	     (rl:*entry-hook*
	       `(,(lambda (e)
		    (rl::without-undo (e)
		      (rl::buffer-insert e 0 old-name 0)
		      ;; (setf (inator-point e) (length string))
		      (rl::set-all-points e (olength old-name)))))))

	(multiple-value-bind (x y width #|height|#)
	    (cell-box o (table-point-row cursor)
		      (table:table-column-number "Name" table))
	  (setf new-name
		(rl-widget:widget-read
		 :x x :y y :width width
		 ;; :buf (make-fatchar-string old-name
		 ;;       :fill-pointer (olength old-name))
		 :rendition (style:styled-char
			     (theme:value '(:program :input :style)) #\X)
		 :flex-width t
		 ;; :box-p t
		 ))
	  (when (not (string= old-name new-name))
	    (dired-rename-file (path-append directory old-name)
			       (path-append directory new-name))))))
    (refresh o)))

(defun new-directory (o)
  (let ((new-name (input-window "Create a new directory"
				'("Enter a for the new directory:"
				  ""))))
    (mkdir:mkdir :directories `(,new-name))
    (refresh o)))

(define-condition dired-error (simple-error)
  ()
  (:documentation "An error in dired."))

(defun mark-set-p ()
  (and (slot-boundp *dired* 'inator::mark)
       (slot-value *dired* 'inator::mark)))

(defun check-mark ()
  "Give an error and continue if ‘mark’ is not set."
  (unless (mark-set-p)
    (error 'dired-error :format-control "The mark is not set.")))

(defmethod select ((o directory-editor))
  "Set the start of the region."
  (setf (inator-mark o) (table-point-row (inator-point o))))

(defun map-region (o function)
  "Call ‘function’ with each row between the current point and mark."
  (check-mark)
  (with-slots ((point inator::point)
	       (mark inator::mark)
	       (table table-viewer::table)
	       directory) o
    (loop
      :with low = (min mark (table-point-row point))
      :and high = (max mark (table-point-row point))
      :for row :from low :to high
      :collect (funcall function (oelt table row)))))

(defun region-files (o)
  "Return a list of files in the region."
  (with-slots (directory) o
    (let ((result '()))
      (map-region o
       (lambda (row)
	 (push (truename (path-append directory
				      (osimplify (file-cell row))))
	       result)))
      (nreverse result))))

(defun grab-region-files (o)
  "Return a list of files in the region."
  (with-slots (directory) o
    (let ((result '()))
      (map-region o
       (lambda (row)
	 (push (truename (path-append directory
				      (osimplify (file-cell row))))
	       result)))
      (nreverse result))))

(defmethod cut ((o directory-editor))
  "Mark the files in the region."
  (with-slots ((mark inator::mark)) o
    (cond
      (mark
       (setf (clipboard-items *clip*) (region-files o)
	     (clipboard-operation *clip*) :cut))
      (t (notify o "The mark is not set.")))))

(defmethod copy ((o directory-editor))
  "Delete the files in the region."
  (with-slots ((mark inator::mark)) o
    (cond
      (mark
       (setf (clipboard-items *clip*) (region-files o)
	     (clipboard-operation *clip*) :copy))
      (t (notify o "The mark is not set.")))))

(defun toggle-region (o)
  (rotatef (inator-point o) (inator-mark o)))

(defun show-clipboard (o)
  (declare (ignore o))
  (let ((title "Clipboard Contents"))
    (fui:display-text title
      `(,@(map 'list #'princ-to-string (clipboard-items *clip*))
	""
	,(format nil "~d items" (olength (clipboard-items *clip*))))
      :min-width (olength title))))

(defun list-restarts (restarts condition)
  (ß `((:fg-white "Condition: ")
       (:fg-red (:underline ,(princ-to-string (type-of condition))) #\newline
		 ,(princ-to-string condition)) #\newline
       (:underline "Restarts") " are:" #\newline
       ,(loop :with i = 0
          :for r :in restarts
	  :collect
	   `((:fg-cyan ,(princ-to-string i)) ": "
	     ,(let (rs)
		(cond
		  ((not (ignore-errors
			 (and (setf rs (format nil "~s ~a"
					       (restart-name r) r)) t)))
		   (with-output-to-string (str)
		     (format str "Error printing restart ")
		     (print-unreadable-object (r str :type t :identity t)
		       (format str "~a" (restart-name r)))))
		  (t rs)))
	     #\newline)
	   :do (incf i)))))

(defun restart-number (n restarts)
  "Return the restart number N in RESTARTS or NIL if it's not in RESTARTS."
  (when (and restarts (>= n 0) (< n (length restarts)))
    (nth n restarts)))

;; @@@ These are BOGUS!!
;; We should make good versions suitable for "cp" and "mv"
(defun dired-copy-file (from to)
  "Copy contents of file ‘from’ to ‘to’. Error if either ‘from’ doesn't exist,
or ‘to’ exists."
  (with-open-file (output to :element-type '(unsigned-byte 8)
			       :direction :output :if-exists :error)
    (with-open-file (input from :element-type '(unsigned-byte 8)
				:direction :input :if-does-not-exist :error)
      (copy-stream input output :element-type '(unsigned-byte 8)))))

(defun dired-rename-file (from to)
  "Rename from ‘from’ to file ‘to’. Overwrites ‘to’ if it exists. Errors if
‘from’ doesn't exist."
  (nos:os-rename-file from to))

(defun apply-file-op (o files operation)
  (with-error-handling ()
    (with-simple-restart (abort "Stop moving files.")
      (loop
	:with dir = (dired-directory o) :and to
	:for f :in files
	:do
	   ;; (setf to (truename (nos:path-append dir (nos:path-file-name f))))
	   (setf to (nos:path-append dir (nos:path-file-name f)))
	   (with-simple-restart (skip "Skip this file and continue.")
	     (with-simple-restart (overwite "Overwrite the file.")
	       (cond
		 ((equal f to)
		  (error "Can't move ~s to itself." f))
		 ((nos:file-exists to)
		  (error "The file ~s already exits in ~s"
			 (nos:path-file-name f) dir))))
	     (case operation
	       (:move (dired-rename-file f to))
	       (:copy (dired-copy-file f to))
	       (otherwise
		(notify o "Unknown clipboard operation ~s" operation)))))))
  t)

(defmethod paste ((o directory-editor))
  "Paste the files from the clipboard here."
  (with-slots (items operation) *clip*
    (cond
      ((not items)
       (notify o "There is nothing to paste on the clipboard."))
      (t
       (apply-file-op o items operation)))))

(defun mark-for-delete (o)
  "Mark the current item for deleting."
  (setf (current-mark-cell o) (mark-display *delete-mark*))
  (next o))

(defun mark-item (o)
  "Mark the current item."
  (setf (current-mark-cell o) (mark-display *select-mark*))
  (next o))

(defun mark-row (row &optional (mark *select-mark*))
  "Mark ‘row’ with ‘mark’, which defaults to *select-mark*."
  (setf (mark-cell row) (mark-display mark)))

(defun mark-region (o)
  "Mark the current region with the *select-mark*."
  (map-region o (_ (mark-row _))))

(defun unmark-item (o)
  "Remove the mark on the current item."
  (setf (current-mark-cell o) +empty+)
  (next o))

(defun unmark-region (o)
  "Un-mark the region or all marks if no region."
  (if (mark-set-p)
      (map-region o (_ (setf (mark-cell _) +empty+)))
      (omapn (_ (setf (mark-cell _) +empty+)) (table-viewer-table o))))

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

(defun input-window (title text-lines #| &key widget |#)
  (prog1
      (fui:display-text
       title text-lines
       :input-func
       #'(lambda (w)
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
      (nos:os-delete-file (nos:path-append directory (osimplify file))))
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

(defun shell-command (o)
  (tt-move-to (1- (tt-height)) 0)
  (tt-finish-output)
  ;; @@@ This is not good, since we won't get the shell completion and features.
  ;; We should add a way to exit after one prompt to lish.
  (nos:with-working-directory ((dired-directory o))
    (let ((command (rl:rl :prompt "$ ")))
      (lish:lish :command command))
    (tt-format "~&[Press Enter]")
    (tt-get-key))
  (refresh o))

(defun view-raw-command (o)
  (view-thing o :raw t))

(defun update-items (o)
  (with-accessors ((directory dired-directory)
		   (table table-viewer-table)) o
    (setf table (ls:list-files :files `(,directory) :collect t :nice-table t
			       :long t :quiet t :hidden t)
	  table (table:insert-column 0 "Mark" table :value +empty+))
    (resize o)))

(defun refresh (o)
  "Refresh the directory listing."
  (update-items o)
  (table-viewer-recalculate-sizes o)
  (tt-clear)
  (redraw o))

(defmethod await-event ((i directory-editor))
  "Get an event."
  (with-slots (click-pos) i
    (let ((result (call-next-method)))
      (typecase result
	(tt-mouse-button-press
	 (setf click-pos (cons (tt-mouse-event-x result)
			       (tt-mouse-event-y result))))
	(tt-mouse-button-release
	 (case (tt-mouse-button result)
	   (1 (setf result :mouse-1))
	   (3 (setf result :mouse-3)))))
      result)))

(defmethod update-display ((o directory-editor))
  (with-slots ((renderer table-viewer::renderer)
	       (point inator::point)
	       (table table-viewer::table)
	       (long-titles table-viewer::long-titles)
	       (message inator::message)
	       directory) o
    (with-slots ((x table-viewer::x)
		 (y table-viewer::y)
		 (start table-viewer::start)
		 (rows table-viewer::rows)
		 (cursor table-viewer::cursor)) renderer
      (tt-home)
      (tt-erase-below)
      (tt-write-span `(:white ,directory ,nos:*directory-separator* #\newline))
      (setf y 1)

      ;; @@@ Stupid hack to make it work. The rows should really set based on
      ;; display size of the content.
      (setf rows (min (table-viewer::table-length o) (- (tt-height) 3)))

      ;; Adjust the view to the point
      (when point
	(when (>= (table-point-row point) (+ (table-point-row start) rows))
	  (setf (table-point-row start) (1+ (- (table-point-row point) rows))))
	(when (< (table-point-row point) (table-point-row start))
	  (setf (table-point-row start) (table-point-row point)))

	;; Unset the cursor
	(setf (table-point-row cursor) nil))

      ;; Tell the renderer where point is.
      (setf (table-viewer::current-position renderer) point)

      ;; Output the table rows
      (table-print:output-table table renderer *terminal*
				:long-titles long-titles)

      ;; Show the message temporarily.
      (when message
	(message o (quote-format message))
	(setf message nil))

      ;; Make the cursor show up in the right spot.
      (when point
	(when (table-point-row cursor) ;; @@@ XXX workaround
	  ;;(tt-move-to (table-point-row cursor) 0)
	  (tt-move-to (+ y (table-point-row cursor)) x))))))

;; @@@ Consider how we could add easily customizable error handling to inators.
;; Because it's stupid to have to duplicate the generic event-loop here.

(defmethod event-loop ((inator directory-editor))
  (unwind-protect
       (progn
	 (start-inator inator)
	 (update-display inator)
	 (loop :with event
	   :do
           (with-simple-restart (continue "Continue with the directory editor.")
             (handler-case
	       (when (setf event (await-event inator))
	         (process-event inator event))
               (dired-error (c)
                 ;; (tt-format-at 0 0 "~a~%" c)
                 ;; (tt-get-key)
	         (notify *dired* "~a" c)
                 (continue c))))
	    :while (not (inator-quit-flag inator))
	    :do
	    (update-display inator)))
    (finish-inator inator)))

(defun dired (&optional (directory (nos:current-directory)))
  (with-terminal ()
    (when (not *no-warning*)
      (fui:show-text *warning* :justify nil
                               :y (round (- (/ (tt-height) 2) 11/2))))
    (let ((top-level (not *dired*))
          (*no-warning* t))
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
            (catch 'quit-all
              (body))
            (body))))
    (tt-move-to (1- (tt-height)) 0)))

#+lish
(lish:defcommand dired
  ((directory directory :default '(nos:current-directory) :optional t
   :help "The directory to edit."))
  "Edit directories."
  (dired directory))

;; End
