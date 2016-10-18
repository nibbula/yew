;;
;; puca.lisp - Putative Muca (A very simple(istic) interface to CVS/git/svn)
;;
;; (a.k.a Muca-ing for fun)
;; This was translated from puca.pl, written in Perl, which is my excuse for
;; why it's poorly organized.
;;
;; TODO:
;;  - way to provide command options?
;;  - way to invoke unbound subcommand?
;;  - improve git mode
;;    - show things to pull? (e.g. changes on remote)
;;  - Consider making a branch editing mode
;;  - Consider making a version editing mode
;;  - Consider configuration / options editing
;;     like for "git config ..." or whatever the equivalent is in other systems

(defpackage :puca
  (:documentation "Putative Muca")
  (:use :cl :dlib :dlib-misc :opsys :keymap :char-util :curses :tiny-rl
	:completion :inator :fui)
  (:export
   ;; Main entry point
   #:puca
   #:!puca
   #:make-standalone
   ))
(in-package :puca)

(defstruct goo
  "A file/object under version control."
  selected
  modified
  filename
  extra-lines)

(defvar *puca* nil
  "The current puca instance.")

(defclass puca (fui-inator)
  ((goo
    :initarg :goo :accessor puca-goo :initform nil
    :documentation "A list of goo entries.")
   (maxima
    :initarg :maxima :accessor puca-maxima :initform 0
    :documentation "Number of items.")
   (mark
    :initarg :mark :accessor puca-mark :initform nil
    :documentation "One end of the region")
   (top
    :initarg :top :accessor puca-top :initform 0
    :documentation "Top item")
   (bottom
    :initarg :bottom :accessor puca-bottom :initform nil
    :documentation "Bottom item.")
   (errors
    :initarg :errors :accessor puca-errors :initform nil
    :documentation "Error output")
   (extra
    :initarg :extra :accessor puca-extra :initform nil
    :documentation "extra lines")
   (message
    :initarg :has-message :accessor puca-message :initform nil
    :documentation "A message to show."))
  (:default-initargs
   :point 0)
  (:documentation "An instance of a version control frontend app."))

(defvar *backend* nil
  "The current backend.")

(defclass backend ()
  ((name
    :initarg :name :accessor backend-name :type string
    :documentation "The name of the back-end.")
   (list-command
    :initarg :list-command :accessor backend-list-command  
    :documentation "Command to list the things.")
   (add
    :initarg :add :accessor backend-add :type string
    :documentation "Command to add a file to the repository.")
   (reset
    :initarg :reset :accessor backend-reset :type string
    :documentation "Command to do something like whatever git reset does.")
   (diff
    :initarg :diff :accessor backend-diff :type string
    :documentation "Command to show the difference vs the last change.")
   (diff-repo
    :initarg :diff-repo :accessor backend-diff-repo :type string
    :documentation "Command to show the some kind of more differences.")
   (commit
    :initarg :commit :accessor backend-commit :type string
    :documentation "Commit the changes.")
   (update
    :initarg :update :accessor backend-update :type string
    :documentation "Update the file from the remote or repository.")
   (update-all
    :initarg :update-all :accessor backend-update-all :type string
    :documentation "Update the whole directory from the remote or repository.")
   (push
    :initarg :push :accessor backend-push :type string
    :documentation "Push the changes to the remote in a distributed RCS.")
   (ignore-file
    :initarg :ignore-file :accessor backend-ignore-file  :type string
    :documentation "File which contains a list of files to ignore."))
  (:documentation "A generic version control back end."))

(defgeneric check-existence (type)
  (:documentation
   "Return true if we guess we are in a directory under this type."))

(defgeneric parse-line (backend line i)
  (:documentation "Take a line and add stuff to goo and/or *errors*."))

(defgeneric add-ignore (backend file)
  (:documentation "Add FILE to the list of ignored files."))

(defmethod parse-line ((backend backend) line i)
  "Parse a status line LINE for a typical RCS. I is the line number."
  (with-slots (goo errors extra) *puca*
    (let ((words (split-sequence " " line
				 :omit-empty t
				 :test #'(lambda (a b)
					   (declare (ignore a))
					   (or (equal b #\space)
					       (equal b #\tab))))))
      ;;(debug-msg "~s" words)
      (cond
	;; If the first word is more than 1 char long, save it as extra
	((> (length (car words)) 2)
	 (push line extra)
	 (push (format nil "~d: ~a" i line) errors))
	;; skip blank lines
	((or (not words)
	     (and (= (length words) 1)
		  (= (length (car words)) 0))))
	(t
	 (push (make-goo :modified (subseq (elt words 0) 0 1)
			 :filename (elt words 1)) goo)
	 ;; If we've accumulated extra lines add them to this line.
	 (when extra
	   (setf (goo-extra-lines (car goo)) (nreverse extra))
	   (setf extra nil)))))))

(defmethod add-ignore ((backend backend) file)
  "Add FILE to the list of ignored files."
  (when (not (backend-ignore-file backend))
    (info-window
     "Problem"
     (list (format nil "I don't know how to ignore with ~a."
		   (backend-name backend))))
    (return-from add-ignore nil))
  (with-open-file (stream (backend-ignore-file backend)
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
    (write-line file stream))
  (get-list)
  (draw-screen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVS

(defclass cvs (backend)
  ()
  (:default-initargs
   :name	 "CVS"
   :list-command '("cvs" "-n" "update")
   :add		 "cvs add ~{~a ~}"
   :reset	 "echo 'No reset in CVS'"
   :diff	 "cvs diff ~{~a ~} | pager"
   :diff-repo	 "cvs diff -r HEAD ~{~a ~} | pager"
   :commit	 "cvs commit ~{~a ~}"
   :update	 "cvs update ~{~a ~}"
   :update-all	 "cvs update"
   :push	 "echo 'No push in CVS'"
   :ignore-file	 ".cvsignore")
  (:documentation "CVS."))

(defparameter *backend-cvs* (make-instance 'cvs))

(defmethod check-existence ((type (eql :cvs)))
  (probe-directory "CVS"))

(defmethod parse-line ((backend cvs) line i)
  (with-slots (goo errors extra) *puca*
    (let ((words (split-sequence " " line
				 :omit-empty t
				 :test #'(lambda (a b)
					   (declare (ignore a))
					   (or (equal b #\space)
					       (equal b #\tab))))))
      (dbug "~s~%" words)
      (cond
	;; If the first word is more than 1 char long, save it as extra
	((> (length (first words)) 1)
	 (push line extra)
	 (push (format nil "~d: ~a" i line) errors))
	;; skip blank lines
	((or (not words)
	     (and (= (length words) 1)
		  (= (length (first words)) 0))))
	(t
	 (push (make-goo :modified (elt words 0)
			 :filename (elt words 1)) goo)
	 ;; If we've accumulated extra lines add them to this line.
	 (when extra
	   (setf (goo-extra-lines (first goo)) (nreverse extra))
	   (setf extra nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT

(defclass git (backend)
  ()
  (:default-initargs
   :name	 "git"
   :list-command '("git" "status" "--porcelain")
   :add		 "git --no-pager add ~{~a ~}"
   :reset	 "git --no-pager reset ~{~a ~}"
   :diff	 "git diff --color ~{~a ~} | pager"
   :diff-repo	 "git diff --color --staged | pager"
   :commit	 "git --no-pager commit ~{~a ~}"
   :update	 "git --no-pager pull ~{~a ~}"
   :update-all	 "git --no-pager pull"
   :push	 "git --no-pager push"
   :ignore-file	 ".gitignore")
  (:documentation "Backend for git."))

(defmethod check-existence ((type (eql :git)))
  (equal "true" (shell-line "git" "rev-parse" "--is-inside-work-tree")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVN

(defclass svn (backend)
  ()
  (:default-initargs
   :name		"SVN"
   :list-command	'("svn" "status")
   :add			"svn add ~{~a ~}"
   :reset		"svn revert ~{~a ~}"
   :diff		"svn diff ~{~a ~} | pager"
   :diff-repo		"svn diff -r HEAD ~{~a ~} | pager"
   :commit		"svn commit ~{~a ~}"
   :update		"svn update ~{~a ~}"
   :update-all		"svn update"
   :push		"echo 'No push in SVN'")
  (:documentation "Backend for SVN."))

(defmethod check-existence ((type (eql :svn)))
  (probe-directory ".svn"))

;; @@@ I haven't tested this.
(defmethod add-ignore ((backend svn) file)
  "Add FILE to the list of ignored files."
  (do-literal-command "svn propset svn:ignore \"~a\" ." (list file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mercurial (hg)

(defclass hg (backend)
  ()
  (:default-initargs
   :name		"hg"
   :list-command	'("hg" "status")
   :add			"hg add ~{~a ~}"
   :reset		"hg revert ~{~a ~}"
   :diff		"hg diff ~{~a ~} | pager"
   :diff-repo		"hg diff ~{~a ~} | pager"
   :commit		"hg commit ~{~a ~}"
   :update		"hg pull ~{~a ~}?"
   :update-all		"hg pull"
   :push		"hg push")
  (:documentation "Backend for Mercurial."))

(defmethod check-existence ((type (eql :hg)))
  (probe-directory ".hg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *backends* '(:git :cvs :svn :hg)
  "The availible backends.")

(defun draw-message (p)
  (move (- *lines* 2) 2)
  (clrtoeol)
  (addstr (puca-message p)))

(defmethod message ((p puca) format-string &rest format-args)
  "Display a message in the message area."
  (setf (puca-message p) (apply #'format nil format-string format-args))
  (draw-message p)
  (refresh))

(defun draw-goo (i)
  "Draw the goo object, with the appropriate color."
  (with-slots (goo top) *puca*
    (let ((g (elt goo i)))
      (let* ((attr
	      (case (aref (goo-modified g) 0)
		(#\M (color-attr +color-red+	 +color-black+)) ; modified
		(#\? (color-attr +color-white+	 +color-black+)) ; unknown
		(#\C (color-attr +color-magenta+ +color-black+)) ; conflicts
		(t   (color-attr +color-green+	 +color-black+))))) ; updated or other
	(move (+ (- i top) 3) 4)
	;; (clrtoeol)
	(attron attr)
	(addstr (format nil "~a ~a ~30a"
			(if (goo-selected g) "x" " ")
			(goo-modified g)
			(goo-filename g)))
	(attroff attr)
	(when (goo-extra-lines g)
;; Ugly inline error display:
;;      (if (= i cur)
;; 	  (loop :with j = 0
;; 	     :for line :in (goo-extra-lines g)
;; 	     :do
;; 	     (mvaddstr (+ (- i top) 3 j) 20 line)
;; 	     (incf j))
	  (mvaddstr (+ (- i top) 3) 30 " ****** "))))))

(defun draw-line (i)
  "Draw line I, with the appropriate color."
  (draw-goo i))

(defun get-list ()
  "Get the list of files/objects from the backend and parse them."
  (message *puca* "Listing...")
  (with-slots (goo top errors maxima (point inator::point) cur extra) *puca*
    (setf goo '()
	  top 0
	  errors '())
    (let* ((i 0)
	   (cmd (backend-list-command *backend*))
	   (cmd-name (car cmd))
	   (cmd-args (cdr cmd))
	   line)
      (setf extra '())
      (with-process-output (stream cmd-name cmd-args)
	(loop :while (setf line (read-line stream nil nil))
	   :do
	   (parse-line *backend* line i)
	   (incf i)
	   (message *puca* "Listing...~d" i)))
      (setf goo (nreverse goo)
	    errors (nreverse errors))
      (setf maxima (length goo))
      (when (>= (inator-point *puca*) maxima)
	(setf (inator-point *puca*) (1- maxima)))))
  (addstr "done"))

(defun draw-screen ()
  (with-slots (maxima top bottom goo message) *puca*
    (clear)
    (border 0 0 0 0 0 0 0 0)
    (setf bottom (min (- maxima top) (- curses:*lines* 7)))
    (let* ((title (format nil "~a Muca (~a)" 
			  (backend-name *backend*) (machine-instance))))
      (move 1 (truncate (- (/ *cols* 2) (/ (length title) 2))))
      (addstr title)
      ;; top scroll indicator
      (when (> top 0)
	(mvaddstr 2 2 "^^^^^^^^^^^^^^^^^^^^^^^"))
      (when (> (length goo) 0)
	(loop :for i :from top :below (+ top bottom)
	   :do (draw-goo i)))
      ;; bottom scroll indicator
      (when (< bottom (- maxima top))
	(mvaddstr (+ bottom 4) 2 "vvvvvvvvvvvvvvvvvvvvvvv")))
    (when message
      (draw-message *puca*)
      (setf message nil))))

(defun do-literal-command (format-str format-args
			   &key (relist t) (do-pause t) confirm)
  "Do the command resulting from applying FORMAT-ARGS to FORMAT-STRING.
If RELIST is true (the default), regenerate the file list. If DO-PAUSE is true,
pause after the command's output. If CONFIRM is true, ask the user for
confirmation first."
  (clear)
  (refresh)
  (endwin)
  (when (and confirm (not (yes-or-no-p "Are you sure? ")))
    (return-from do-literal-command (values)))
  (let* ((command (apply #'format nil format-str format-args)))
    (lish:! command))
  (when do-pause
    (write-string "[Press Return]")
    (terpri)
    (read-line))
  (initscr)
  (when relist
    (get-list))
  (draw-screen))

(defun do-command (command format-args
		   &rest keys &key (relist t) (do-pause t) confirm)
  "Do a command resulting from the backend function COMMAND, applying
FORMAT-ARGS. If RELIST is true (the default), regenerate the file list.
If CONFIRM is true, ask the user for confirmation first."
  (declare (ignorable relist do-pause confirm))
  (apply #'do-literal-command (apply command (list *backend*))
	 (list format-args) keys)
  (values))

(defun selected-files ()
  "Return the selected files or the current line, if there's no selections."
  (with-slots (maxima goo (point inator::point)) *puca*
    (let* ((files
	    (loop :for i :from 0 :below maxima
	       :when (goo-selected (elt goo i))
	       :collect (goo-filename (elt goo i)))))
      (or files (and goo (list (goo-filename (elt goo point))))))))

(defun select-all ()
  (with-slots (maxima goo) *puca*
    (loop :for i :from 0 :below maxima
       :do (setf (goo-selected (elt goo i)) t))))

(defun select-none ()
  (with-slots (maxima goo) *puca*
    (loop :for i :from 0 :below maxima
       :do (setf (goo-selected (elt goo i)) nil))))

#|
(defun fake-draw-screen ()
  "For debugging."
;  (init-curses)
  (draw-screen)
  (get-char)
  (endwin))
|#

(defun debug-msg (fmt &rest args)
  "For debugging."
  (move (- *lines* 2) 2)
;  (clrtoeol)
  (addstr (apply #'format nil fmt args))
  (refresh)
  (get-char))

(defun info-window (title text-lines)
  (fui:display-text title text-lines)
  (clear)
  (refresh)
  (draw-screen)
  (refresh))

(defun input-window (title text-lines)
  (prog1
      (display-text
       title text-lines
       :input-func #'(lambda (w)
		       (cffi:with-foreign-pointer-as-string (str 40)
			 (curses:echo)
			 (wgetnstr w str 40)
			 (curses:noecho))))
    (clear)
    (refresh)
    (draw-screen)
    (refresh)))

(defun puca-yes-or-no-p (&optional format &rest arguments)
  (equalp "Yes" (input-window
		 "Yes or No?"
		 (list 
		  (if format
		      (apply #'format nil format arguments)
		      "Yes or No?")))))

(defun delete-files ()
  (when (puca-yes-or-no-p
	 "Are you sure you want to delete these files from disk?~%~{~a ~}"
	 (selected-files))
    (loop :for file :in (selected-files) :do
       (delete-file file))
    (get-list)))

(defparameter *extended-commands*
  '(("delete" delete-files))
  "An alist of extended commands, key is the command name, value is a symbol
for the command-function).")

(defvar *complete-extended-command*
  (completion:list-completion-function (mapcar #'car *extended-commands*))
  "Completion function for extended commands.")

(defun extended-command (p)
  "Extended command"
  (declare (ignore p))
  (move (- *lines* 2) 2)
  (refresh)
  (reset-shell-mode)
  (let ((command (tiny-rl:tiny-rl
		  :prompt ": "
		  :completion-func *complete-extended-command*
		  :context :puca))
	func)
    (reset-prog-mode)
    (setf func (cadr (assoc command *extended-commands* :test #'equalp)))
    (when (and func (fboundp func))
      (funcall func)))
  (draw-screen)
  (values))

(defun show-errors (p)
  "Show all messages / errors"
  (with-slots (errors) p
    (info-window "All Errors"
		 (or errors
		     '("There are no errors." "So this is" "BLANK")))))

(defun show-extra (p)
  "Show messages / errors"
  (with-slots (goo) p
    (let ((ext (and goo (goo-extra-lines (elt goo (inator-point *puca*))))))
      (info-window "Errors"
		   (or ext
		       '("There are no errors." "So this is" "BLANK"))))))

(defun pick-backend (&optional type)
  ;; Try find what was asked for.
  (when type
    (let ((be (find type *backends*)))
      (when be
	(return-from pick-backend
	  (make-instance (intern (symbol-name be) :puca))))))
  ;; Try to figure it out.
  (let ((result
	 (loop :for backend :in *backends* :do
	    (dbug "Trying backend ~s~%" backend)
	    (when (check-existence backend)
	      (dbug "Picked ~s~%" backend)
	      (return (make-instance (intern (symbol-name backend) :puca)))))))
    (if (not result)
	(make-instance 'cvs)
	result)))

(defun add-command (p)
  "Add file"
  (declare (ignore p))
  (do-command #'backend-add (selected-files)))

(defun reset-command (p)
  "Revert file (undo an add)"
  (declare (ignore p))
  (do-command #'backend-reset (selected-files) :confirm t))

(defun diff-command (p)
  "Diff"
  (declare (ignore p))
  (do-command #'backend-diff (selected-files)
	      :relist nil :do-pause nil))

(defun diff-repo-command (p)
  "Diff against commited (-r HEAD)"
  (declare (ignore p))
  (do-command #'backend-diff-repo (selected-files)
	      :relist nil :do-pause nil))

(defun commit-command (p)
  "Commit selected"
  (declare (ignore p))
  (do-command #'backend-commit (selected-files)))

(defun update-command (p)
  "Update selected"
  (declare (ignore p))
  (do-command #'backend-update (selected-files)))

(defun update-all-command (p)
  "Update all"
  (declare (ignore p))
  (do-command #'backend-update-all nil))

(defun push-command (p)
  "Push"
  (declare (ignore p))
  (do-command #'backend-push nil :relist nil))

(defun add-ignore-command (p)
  "Ignore"
  (declare (ignore p))
  (loop :for f :in (selected-files)
     :do (add-ignore *backend* f)))

(defun view-file (p)
  "View file"
  (declare (ignore p))
  (pager:pager (selected-files))
  (draw-screen))

(defmethod previous ((p puca))
  "Previous line"
  (with-slots ((point inator::point) top) p
    (decf point)
    (when (< point 0)
      (setf point 0))
    (when (< point top)
      (decf point)
      (draw-screen))))

(defmethod next ((p puca))
  "Next line"
  (with-slots ((point inator::point) maxima top bottom) p
    (incf point)
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (and (> point (- bottom 1)) (> bottom 1))
      (incf top)
      (draw-screen))))

(defmethod next-page ((p puca))
  "Next page"
  (with-slots ((point inator::point) maxima top bottom) p
    (setf point (+ point 1 (- curses:*lines* 7)))
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (> point bottom)
      (setf top (max 0 (- point (- curses:*lines* 7))))
      (draw-screen))))

(defmethod previous-page ((p puca))
  "Previous page"
  (with-slots ((point inator::point) top) p
    (setf point (- point 1 (- curses:*lines* 7)))
    (when (< point 0)
      (setf point 0))
    (when (< point top)
      (setf top point)
      (draw-screen))))

(defmethod move-to-bottom ((p puca))
  "Bottom"
  (with-slots ((point inator::point) maxima top bottom) *puca*
    (setf point (1- maxima))
    (when (> maxima bottom)
      (setf top (max 0 (- maxima (- curses:*lines* 7))))
      (draw-screen))))

(defmethod move-to-top ((p puca))
  "Top"
  (with-slots ((point inator::point) top) p
    (setf point 0)
    (when (> top 0)
      (setf top 0)
      (draw-screen))))

(defun set-mark (p)
  "Set Mark"
  (with-slots ((point inator::point) mark) p
    (setf mark point)
    (message p "Set mark.")))

(defun toggle-region (p)
  "Toggle region"
  (with-slots ((point inator::point) mark) p
    (if mark
      (let ((start (min mark point))
	    (end (max mark point)))
	(loop :for i :from start :to end :do
	   (setf (goo-selected (elt (puca-goo p) i))
		 (not (goo-selected (elt (puca-goo p) i))))
	   (draw-line i)))
      (message p "No mark set."))))

(defun toggle-line (p)
  "Toggle line"
  (with-slots ((point inator::point) goo) p
    (when goo
      (setf (goo-selected (elt goo point))
	    (not (goo-selected (elt goo point))))
      (draw-line point))))

(defun select-all-command (p)
  "Select all"
  (declare (ignore p))
  (select-all)
  (draw-screen))

(defun select-none-command (p)
  "Select none"
  (declare (ignore p))
  (select-none)
  (draw-screen))

(defun relist (p)
  "Re-list"
  (declare (ignore p))
  (clear)
  (refresh)
  (get-list)
  (draw-screen)
  (refresh))

(defmethod redraw ((p puca))
  "Re-draw"
  (clear)
  (refresh)
  (draw-screen)
  (refresh))

(defkeymap *puca-keymap*
  `((#\q		. quit)
    (#\Q		. quit)
    (#\?		. help)
    (#\h		. help)
    (#\a		. add-command)
    (#\r		. reset-command)
    (#\d		. diff-command)
    (#\D		. diff-repo-command)
    (#\c		. commit-command)
    (#\u        	. update-command)
    (#\U        	. update-all-command)
    (#\P        	. push-command)
    (#\i        	. add-ignore-command)
    (#\v        	. view-file)
    (:UP        	. previous)
    (,(code-char 16)	. previous)
    (,(ctrl #\p)       	. previous)
    (:DOWN      	. next)
    (,(code-char 14)    . next)
    (,(ctrl #\n)	. next)
    (:NPAGE		. next-page)
    (,(ctrl #\V)	. next-page)
    (,(ctrl #\F)	. next-page)
    (:PPAGE		. previous-page)
    (,(ctrl #\B)	. previous-page)
    (#\>		. move-to-bottom)
    (#\<		. move-to-top)
    (,(meta-char #\>)	. move-to-bottom)
    (,(meta-char #\<)	. move-to-top)
    (,(ctrl #\@)	. set-mark)
    (,(code-char 0)	. set-mark)
    (#\X		. toggle-region)
    (#\space		. toggle-line)
    (#\x		. toggle-line)
    (#\return		. toggle-line)
    (#\s		. select-all-command)
    (#\S		. select-none-command)
    (#\g		. relist)
    (#\e		. show-extra)
    (#\E		. show-errors)
    (#\:		. extended-command)
    ;;(,(ctrl #\L)	. redraw)
    (,(code-char 12)	. redraw)
    (,(meta-char #\=)	. describe-key-briefly)
    (#\escape		. *puca-escape-keymap*)))

(defparameter *puca-escape-keymap* (build-escape-map *puca-keymap*))

(defun describe-key-briefly (p)
  "Prompt for a key and say what function it invokes."
  (message p "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *puca-keymap*)))
    (if action
	(message p "~a is bound to ~a" (nice-char key) action)
	(message p "~a is not defined" (nice-char key)))))

(defmethod update-display ((p puca))
  (with-slots ((point inator::point) top) p
    (draw-screen)
    (move (+ (- point top) 3) 2)))

(defmethod start-inator ((p puca))
  (call-next-method)
  (draw-screen)
  (get-list)
  (draw-screen)
  (when (puca-errors p)
    (message p "**MESSAGES**")))

(defun puca (&key backend-type)
  (let ((*puca* (make-instance
		 'puca :keymap (list *puca-keymap* *default-inator-keymap*)))
	(*backend* (pick-backend backend-type)))
    (event-loop *puca*)))

(defun make-standalone (&optional (name "puca"))
  "FUFKFUFUFUFUFF"
  #+sbcl (sb-ext:save-lisp-and-die name :executable t
				   :toplevel #'puca)
  #+clisp (saveinitmem name :executable t :quiet t :norc t
		       :init-function #'puca:puca)
  #-(or sbcl clisp) (declare (ignore name))
  #-(or sbcl clisp) (missing-implementation 'make-standalone)
  )

(lish:defcommand puca
  (("cvs" boolean :short-arg #\c :help "True to use CVS.")
   ("svn" boolean :short-arg #\s :help "True to use SVN.")
   ("git" boolean :short-arg #\g :help "True to use GIT.")
   ("hg"  boolean :short-arg #\m :help "True to use Mercurial."))
  "Putative Muca interface to your version control software.
Arguments are: -c for CVS, -s for SVN, -g GIT, -m Mercurial."
  (puca :backend-type (cond (cvs :cvs) (svn :svn) (git :git) (hg :hg))))

; Joe would probably like the name but not the software.

;; EOF
