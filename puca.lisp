;;
;; puca.lisp - Putative Muca (A very simple(istic) interface to CVS/git/svn)
;;
;; (a.k.a Muca-ing for fun)
;; This was translated from puca.pl, written in Perl, which is my excuse for
;; why it's poorly organized.
;;
;; TODO:
;;  - puca options help?
;;  - way to provide command options?
;;  - way to invoke unbound subcommand?
;;  - improve git mode
;;    - show things to pull? (e.g. changes on remote)
;;    - stash editing mode
;;    - Consider making a branch editing mode
;;  - Consider configuration / options editing
;;     like for "git config ..." or whatever the equivalent is in other systems

(defpackage :puca
  (:documentation
   "Putative Muca (A very simple(istic) interface to CVS/git/svn).")
  (:use :cl :dlib :dlib-misc :opsys :keymap :char-util :rl :completion :inator
	:terminal :terminal-inator :fui :options :fatchar :table :table-print
	:collections
	#+use-re :re #-use-re :ppcre)
  (:export
   ;; Main entry point
   #:puca
   #:!puca
   #:make-standalone
   ))
(in-package :puca)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defclass puca-item ()
  ()
  (:documentation "A generic item."))

(defclass goo (puca-item)
  ((selected
    :initarg :selected :accessor goo-selected :initform nil :type boolean
    :documentation "True if this item is selected.")
   (modified
    :initarg :modified :accessor goo-modified :initform nil
    :documentation "@@@@ i dunno")
   (filename
    :initarg :filename :accessor goo-filename :initform nil
    :documentation "Name of the file?")
   (extra-lines
    :initarg :extra-lines :accessor goo-extra-lines :initform nil
    :documentation "Extra lines output from the backend command."))
  (:documentation "A file/object under version control."))

(defun make-goo (&rest initargs)
  (apply #'make-instance 'goo initargs))

(defvar *puca* nil
  "The current puca instance.")

(defclass puca-app (terminal-inator options-mixin)
  ((backend
    :initarg :backend :accessor puca-backend :initform nil
    :documentation "The revision control system backend that we are using.")
   (goo
    :initarg :goo :accessor puca-goo :initform nil
    :documentation "A sequence of puca-item entries.")
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
    :documentation "A message to show.")
   (first-line
    :initarg :first-line :accessor puca-first-line :initform nil 
    :documentation "The first line of the objects.")
   (debug
    :initarg :debug :accessor puca-debug :initform nil :type boolean
    :documentation "True to turn on debugging."))
  (:default-initargs
   :point 0)
  (:documentation "A version control frontend app."))

(defclass puca (puca-app)
  ()
  (:documentation
   "The main view which operates on file and things in local repository."))

(defparameter *puca-prototype* nil
  "Prototype PUCA options.")

(defoption puca show-all-tracked option :value nil
	   :documentation "Show all tracked files.")

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
   (diff-history
    :initarg :diff-history :accessor backend-diff-history
    :documentation
    "Command to compare a change and the previous in the history.")
   (diff-history-head
    :initarg :diff-history-head :accessor backend-diff-history-head
    :documentation
    "Command to compare a change in history to the current version.")
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
   (history
    :initarg :history :accessor backend-history
    :documentation "Get the history for a specific file.")
   (history-all
    :initarg :history-all :accessor backend-history-all
    :documentation "Get the history for all changes.")
   (ignore-file
    :initarg :ignore-file :accessor backend-ignore-file  :type string
    :documentation "File which contains a list of files to ignore."))
  (:documentation "A generic version control back end."))

;; Things a backend may want / need to implement.

(defgeneric check-existence (type)
  (:documentation
   "Return true if we guess we are in a directory under this type."))

(defgeneric parse-line (backend line i)
  (:documentation "Take a line and add stuff to goo and/or *errors*."))

(defgeneric add-ignore (backend file)
  (:documentation "Add FILE to the list of ignored files."))

(defgeneric banner (backend)
  (:documentation "Print something at the top of the screen."))

(defgeneric get-status-list (backend)
  (:documentation "Return a list of files and their status."))

(defgeneric get-history (backend &optional files)
  (:documentation "Return a list of history entries for FILE. If FILE is nil,
return history for the whole repository."))

;; Generic implementations of some possibly backend specific methods.

(defmethod parse-line ((backend backend) line i)
  "Parse a status line LINE for a typical RCS. I is the line number."
  (with-slots (goo errors extra) *puca*
    (let (match words tag file)
      (multiple-value-setq (match words)
	(scan-to-strings "\\s*(\\S+)\\s+(.*)$" line))
      (when (and match words)
	(setf tag (elt words 0)
	      file (elt words 1)))
      ;; (debug-msg "~s ~s (~s) (~s)" line words tag file)
      (cond
	;; If the first word is more than 1 char long, save it as extra
	((> (length tag) 2)
	 (push line extra)
	 (push (format nil "~d: ~a" i line) errors))
	;; skip blank lines
	((or (not match) (not words)))
	(t
	 (push (make-goo :modified (subseq tag 0 1) :filename file) goo)
	 ;; If we've accumulated extra lines add them to this line.
	 (when extra
	   (setf (goo-extra-lines (car goo)) (nreverse extra))
	   (setf extra nil)))))))

(defmethod get-status-list ((backend backend))
  "This is for backends which just have a fixed list command."
  (let* ((i 0)
	 (cmd (backend-list-command (puca-backend *puca*)))
	 (cmd-name (car cmd))
	 (cmd-args (cdr cmd))
	 line)
    (with-process-output (stream cmd-name cmd-args)
      (loop :while (setf line (read-line stream nil nil))
	 :do
	 (incf i)
	 (message *puca* "Listing...~d" i)
	 ;; (tt-finish-output)
	 :collect line))))

(defmethod add-ignore ((backend backend) file)
  "Add FILE to the list of ignored files."
  (when (not (backend-ignore-file backend))
    (info-window
     "Problem"
     (list (format nil "I don't know how to ignore with ~a."
		   (backend-name backend))))
    (return-from add-ignore nil))
  (when (puca-yes-or-no-p "Are you sure you want ignore ~s ?" file)
    (with-open-file (stream (backend-ignore-file backend)
			    :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (write-line file stream)))
  (get-list *puca*)
  (draw-screen *puca*))

(defmethod banner ((backend backend))
  "Print something useful at the top of the screen."
  (tt-format "~a~%" (nos:current-directory)))

(defun check-dir-and-command (dir command)
  (let ((result
	 ;; On git .git can sometimes be a regular file. (for submodules)
	 ;;(probe-directory dir)
	 (probe-file dir)
	 ))
    (when (and result (not (command-pathname command)))
      (cerror "Proceed anyway."
	      "Looks like a ~a directory, but ~a isn't installed?"
	      dir command))
    result))

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
  (check-dir-and-command "CVS" "cvs"))

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
  ((saved-branch
    :initarg :saved-branch :accessor git-saved-branch :initform nil
    :documentation "Saved branch description.")
   (saved-remotes
    :initarg :saved-remotes :accessor git-saved-remotes :initform nil
    :documentation "Saved list of remotes."))
  (:default-initargs
   :name	      "git"
   :list-command      '("git" "status" "--porcelain")
   :add		      "git --no-pager add ~{~a ~}"
   :reset	      "git --no-pager reset ~{~a ~}"
   :diff	      "git diff --color ~{~a ~} | pager"
   :diff-repo	      "git diff --color --staged | pager"
   :diff-history      "git diff --color ~a ~a -- ~{~a ~} | pager"
   :diff-history-head "git diff --color ..~a -- ~{~a ~} | pager"
   :commit	      "git --no-pager commit ~{~a ~}"
   :update	      "git --no-pager pull ~{~a ~}"
   :update-all	      "git --no-pager pull"
   :push	      "git --no-pager push"
   :history           '("git" "--no-pager" "log"
                        ;;"--format=(\"%h\" \"%ae\" %ct \"%s\")" "--")
                        ;; "--format=%h%x00%ae%x00%ct%x00%s" "--")
                        "--pretty=format:%h%x00%ae%x00%ct%x00%B%x1a" "--")
   :history-all	      '("git" "--no-pager" "log"
			;;"--format=(\"%h\" \"%ae\" %ct \"%s\")")
			"--format=%h%x00%ae%x00%ct%x00%s")
   :ignore-file	      ".gitignore")
  (:documentation "Backend for git."))

(defmethod check-existence ((type (eql :git)))
  (and (check-dir-and-command ".git" "git")
       (equal "true" (shell-line "git" "rev-parse" "--is-inside-work-tree"))))

(defun get-branch (git)
  (or (git-saved-branch git)
      (setf (git-saved-branch git)
	    (subseq (first (lish:!_ "git status -s -b --porcelain")) 3))))

(defun get-remotes (git)
  (or (git-saved-remotes git)
      (setf (git-saved-remotes git)
	    (lish:!_ "git remote -v"))))

(defmethod banner ((backend git))
  "Print something useful at the top of the screen."
  (let ((line (terminal-get-cursor-position *terminal*))
	(col 5)
	(branch (get-branch backend)))
    (labels ((do-line (fmt &rest args)
	       (let ((str (apply #'format nil fmt args)))
		 (tt-move-to (incf line) col)
		 (tt-write-string (subseq str 0 (min (length str)
						     (- (tt-width) col 1)))))))
      (do-line "Repo:    ~a" (nos:current-directory))
      (do-line "Branch:  ~a" branch)
      (do-line "Remotes: ")
      (loop :with s
	 :for r :in (get-remotes backend)
	 :do
	 (setf s (split "\\s" r))
	 (do-line "~a ~a ~a" (elt s 0) (elt s 2) (elt s 1)))
      (tt-move-to (incf line) col))))

(defmethod get-status-list ((backend git))
  (with-slots (backend) *puca*
    (let* ((cmd (backend-list-command backend))
	   (cmd-name (car cmd))
	   (cmd-args (cdr cmd))
	   (i 0)
	   line result)
      ;; Invalidate the cache of banner info.
      (setf (git-saved-branch backend) nil
	    (git-saved-remotes backend) nil
	    result
	    (with-process-output (stream cmd-name cmd-args)
	      (loop :while (setf line (read-line stream nil nil))
		 :do
		 (incf i)
		 (message *puca* "Listing...~d" i)
		 ;; (tt-finish-output)
		 :collect line)))
      ;;(debug-msg "~a from git status" i)
      (when (puca-show-all-tracked *puca*)
	(setf result
	      (append result
		      (with-process-output (stream "git" '("ls-files"))
			(loop :while (setf line (read-line stream nil nil))
			   :do
			   (incf i)
			   (message *puca* "Listing...~d" i)
			   ;; (tt-finish-output)
			   :collect (s+ " _ " line)))))
	;;(tt-move-to 0 0) (tt-home) (tt-erase-below)
	;;(tt-format nil "~w" result)
	;;(debug-msg "~a from git ls-files" i)
	)
      result)))

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
  (check-dir-and-command ".svn" "svn"))

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
  (check-dir-and-command ".hg" "hg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *backends* '(:git :cvs :svn :hg)
  "The availible backends.")

(defun output-message (message)
  (tt-move-to (- (tt-height) 2) 2)
  (tt-erase-to-eol)
  (tt-write-string message))

(defun format-message (fmt &rest args)
  (output-message (apply #'format nil fmt args)))

(defun draw-message (p)
  (output-message (puca-message p)))

(defmethod message ((p puca-app) format-string &rest format-args)
  "Display a message in the message area."
  (setf (puca-message p) (apply #'format nil format-string format-args))
  (draw-message p)
  ;; (refresh)
  )

(defun goo-color (status-char)
  "Set the color based on the status character."
  (case status-char
    (#\M :red)	    ; modified
    (#\? :white)    ; unknown
    (#\C :magenta)  ; conflicts
    (t   :green)))  ; updated or other

(defmethod draw-goo ((puca puca) i)
  "Draw the goo object, with the appropriate color."
  (with-slots (goo top first-line) *puca*
    (let ((g (elt goo i)))
      (let* ((color (goo-color (aref (goo-modified g) 0))))
	(tt-move-to (+ (- i top) first-line) 4)
	;; (clrtoeol)
	(tt-color color :default)
	(tt-format "~a ~a ~30a"
		   (if (goo-selected g) "x" " ")
		   (goo-modified g)
		   (goo-filename g)))
	(tt-color :default :default)
	(when (goo-extra-lines g)
;; Ugly inline error display:
;;      (if (= i cur)
;; 	  (loop :with j = 0
;; 	     :for line :in (goo-extra-lines g)
;; 	     :do
;; 	     (mvaddstr (+ (- i top) 3 j) 20 line)
;; 	     (incf j))
	  (tt-move-to (+ (- i top) 3) 30)
	  (tt-write-string " ****** ")))))

(defun draw-line (i)
  "Draw line I, with the appropriate color."
  (draw-goo *puca* i))

(defmethod get-list ((puca puca))
  "Get the list of files/objects from the backend and parse them."
  (message *puca* "Listing...")
  (with-slots (goo top errors maxima (point inator::point) cur extra) *puca*
    (setf goo '()
	  errors '()
	  extra '())
    (loop
       :for line :in (get-status-list (puca-backend *puca*))
       :and i = 0 :then (1+ i)
       :do
       (parse-line (puca-backend *puca*) line i))
    (setf goo (nreverse goo)
	  errors (nreverse errors))
    (setf maxima (length goo))
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (>= top point)
      (setf top (max 0 (- point 10)))))
  (message *puca* "Listing...done"))

(defgeneric draw-inner-screen (puca)
  (:documentation "Draw the inner part of the screen."))

(defmethod draw-inner-screen ((puca puca))
  (with-slots (top bottom goo) puca
    (when (> (length goo) 0)
      (loop :for i :from top :below (+ top bottom)
	 :do (draw-goo puca i)))))

(defmethod draw-screen ((puca puca-app))
  (with-slots (maxima top bottom goo message backend first-line) puca
    (tt-home)
    (tt-erase-below)
    (draw-box 0 0 (tt-width) (tt-height))
    (let* ((title (format nil "~a Muca (~a)" 
			  (backend-name backend)
			  (machine-instance)))
	   y x)
      (declare (ignorable x))
      (tt-move-to 1 (truncate (- (/ (tt-width) 2) (/ (length title) 2))))
      (tt-write-string title)
      (tt-move-to 2 2) ;; Start of the banner area
      (banner backend)
      (multiple-value-setq (y x) (terminal-get-cursor-position *terminal*))
      ;; End of the banner and start of the first line of objects
      (setf first-line (1+ y)
	    bottom (min (- maxima top) (- (tt-height) first-line 3)))
      ;; top scroll indicator
      (when (> top 0)
	(tt-write-string-at (1- first-line) 2 "^^^^^^^^^^^^^^^^^^^^^^^"))
      ;; (when (> (length goo) 0)
      ;; 	(loop :for i :from top :below (+ top bottom)
      ;; 	   :do (draw-goo puca i)))
      (draw-inner-screen puca)
      ;; bottom scroll indicator
      (when (< bottom (- maxima top))
	(tt-write-string-at (+ first-line bottom) 2 "vvvvvvvvvvvvvvvvvvvvvvv"))
    (when message
      (draw-message *puca*)
      (setf message nil)))))
  
(defun do-literal-command (format-str format-args
			   &key (relist t) (do-pause t) confirm)
  "Do the command resulting from applying FORMAT-ARGS to FORMAT-STRING.
If RELIST is true (the default), regenerate the file list. If DO-PAUSE is true,
pause after the command's output. If CONFIRM is true, ask the user for
confirmation first."
  (let* ((command (apply #'format nil format-str format-args)))
    (handler-case
      (progn
	(tt-clear)
	(tt-finish-output)
	(terminal-end *terminal*)
	(when (and confirm (not (yes-or-no-p "Are you sure? ")))
	  (return-from do-literal-command (values)))
	;; (debug-msg "command ~s" command)
	(lish:! command)
	(when do-pause
	  (write-string "[Press Return]")
	  (terpri)
	  (read-line))
	(terminal-start *terminal*)
	(message *puca* "*terminal* = ~s" *terminal*)
	(when relist
	  (get-list *puca*))
	(tt-clear)
	(draw-screen *puca*))
      (error (c)
	(terminal-start *terminal*)
	(when relist
	  (get-list *puca*))
	(tt-clear)
	(draw-screen *puca*)
	(info-window
	 "Error"
	 (mapcar #'span-to-fat-string
		 `((:red (:underline ,(type-of c)))
		   ("The command \"" (:cyan ,command) "\" got an error:")
		   (:red ,(format nil "~a" c)))))))))

(defun do-command (command format-args
		   &rest keys &key (relist t) (do-pause t) confirm)
  "Do a command resulting from the backend function COMMAND, applying
FORMAT-ARGS. If RELIST is true (the default), regenerate the file list.
If CONFIRM is true, ask the user for confirmation first."
  (declare (ignorable relist do-pause confirm))
  (apply #'do-literal-command (apply command (list (puca-backend *puca*)))
	 format-args keys)
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
  (tt-get-char)
  (endwin))
|#

(defun debug-msg (fmt &rest args)
  "For debugging."
  (tt-move-to (- (tt-height) 2) 2)
;  (clrtoeol)
  (apply #'terminal-format *terminal* fmt args)
  (tt-get-char))

(defun info-window (title text-lines &key (justify t))
  ;; @@@ Is all this clearing and refreshing necessary?
  (fui:display-text title text-lines :justify justify)
  (tt-clear)
  ;;(refresh)
  (draw-screen *puca*)
  (tt-finish-output))

(defun input-window (title text-lines)
  (prog1
      (display-text title text-lines
		    :input-func #'(lambda (w)
				    (declare (ignore w))
				    (rl:rl)))
    (tt-clear)
    (draw-screen *puca*)
    (tt-finish-output)))

(defun puca-yes-or-no-p (&optional format &rest arguments)
  (equalp "Yes" (input-window
		 "Yes or No?"
		 (list 
		  (if format
		      (apply #'format nil format arguments)
		      "Yes or No?")))))

(defun delete-files ()
  (when (puca-yes-or-no-p
	 "Are you sure you want to delete these files from storage?~%~{~a ~}"
	 (selected-files))
    (loop :for file :in (selected-files) :do
       (delete-file file))
    (get-list *puca*)))

(defparameter *extended-commands*
  '(("delete" delete-files))
  "An alist of extended commands, key is the command name, value is a symbol
for the command-function).")

(defvar *complete-extended-command*
  (completion:list-completion-function (mapcar #'car *extended-commands*))
  "Completion function for extended commands.")

(defun extended-command (p)
  "Extended command"
  (tt-move-to (- (tt-height) 2) 2)
  (tt-finish-output)
  (let ((command (rl:rl
		  :prompt ": "
		  :completion-func *complete-extended-command*
		  :context :puca))
	func)
    (setf func (cadr (assoc command *extended-commands* :test #'equalp)))
    (when (and func (fboundp func))
      (funcall func)))
  (draw-screen p)
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
    ;; (if (not result)
    ;; 	(make-instance 'cvs)
    ;; 	result)))
    result))

(defun add-command (p)
  "Add file"
  (declare (ignore p))
  (do-command #'backend-add (list (selected-files))))

(defun reset-command (p)
  "Revert file (undo an add)"
  (declare (ignore p))
  (do-command #'backend-reset (list (selected-files)) :confirm t))

(defun diff-command (p)
  "Diff"
  (declare (ignore p))
  (do-command #'backend-diff (list (selected-files))
	      :relist nil :do-pause nil))

(defun diff-repo-command (p)
  "Diff against commited (-r HEAD)"
  (declare (ignore p))
  (do-command #'backend-diff-repo (list (selected-files))
	      :relist nil :do-pause nil))

(defun commit-command (p)
  "Commit selected"
  (declare (ignore p))
  (do-command #'backend-commit (list (selected-files))))

(defun update-command (p)
  "Update selected"
  (declare (ignore p))
  (do-command #'backend-update (list (selected-files))))

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
  (loop :for f :in (selected-files)
     :do (add-ignore (puca-backend p) f)))

(defun view-file (p)
  "View file"
  (declare (ignore p))
  ;; (pager:pager (selected-files))
  ;;(view:view-things (selected-files))
  ;;(draw-screen p)
  (do-literal-command "view ~{\"~a\" ~}" (list (selected-files))
		      :do-pause nil))

(defmethod previous ((p puca-app))
  "Previous line"
  (with-slots ((point inator::point) top) p
    (decf point)
    (when (< point 0)
      (setf point 0))
    (when (< point top)
      (decf top)
      ;;(draw-screen p)
      )))

(defmethod next ((p puca-app))
  "Next line"
  (with-slots ((point inator::point) maxima top bottom) p
    (incf point)
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (and (> (- point top) (- bottom 1)) (> bottom 1))
      (incf top)
      ;;(draw-screen p)
      )))

(defmethod next-page ((p puca-app))
  "Next page"
  (with-slots ((point inator::point) maxima top bottom) p
    (setf point (+ point 1 bottom))
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (>= point (+ top bottom))
      (setf top (max 0 (- point (1- bottom))))
      ;;(draw-screen p)
      )))

(defmethod previous-page ((p puca-app))
  "Previous page"
  (with-slots ((point inator::point) top) p
    (setf point (- point 1 (- (tt-height) 7)))
    (when (< point 0)
      (setf point 0))
    (when (< point top)
      (setf top point)
      ;;(draw-screen p)
      )))

(defmethod move-to-bottom ((p puca-app))
  "Bottom"
  (with-slots ((point inator::point) maxima top bottom) *puca*
    (setf point (1- maxima))
    (when (> point (+ top bottom))
      (setf top (max 0 (- maxima bottom)))
      ;;(draw-screen p)
      )))

(defmethod move-to-top ((p puca-app))
  "Top"
  (with-slots ((point inator::point) top) p
    (setf point 0)
    (when (> top 0)
      (setf top 0)
      ;;(draw-screen p)
      )))

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
  ;; (draw-screen p)
  )

(defun select-none-command (p)
  "Select none"
  (declare (ignore p))
  (select-none)
  ;; (draw-screen p)
  )

(defun relist (p)
  "Re-list"
  ;; (tt-clear)
  ;;(refresh)
  (get-list p)
  ;; (draw-screen p)
  ;(refresh)
  )

;; (defmethod redraw ((p puca))
;;   "Re-draw"
;;   (tt-clear)
;;   (tt-finish-output)
;;   (draw-screen p)
;;   ;(refresh)
;;   )

(defun toggle-debug (p)
  (setf (puca-debug p) (not (puca-debug p)))
  (debug-msg "Debugging turned on."))

(defparameter *option-setting*
  #((#\a show-all-tracked)))

(defun set-option-command (p)
  (format-message "Set option: ")
  (let* ((c (tt-get-char))
	 (tog (find c *option-setting* :key #'car))
	 (name (string (second tog)))
	 #| (options (options p)) |#)
    (if tog
	(progn
	  (set-option p name (not (get-option p name)))
	  (message p "~a is ~a" name (get-option p name)))
	(progn
	  (message p "Option not found: ~a" c))))
  (get-list *puca*))

(defmethod search-command ((p puca-app))
  (with-slots (goo (point inator::point) top bottom) p
    (tt-move-to (- (tt-height) 2) 3)
    (tt-finish-output)
    (flet ((find-it (str g)
	     (position str g :key #'goo-filename
		       :test (lambda (a b)
			       (search a b :test #'char-equal))))
	   (move-to (i)
	     (setf point i)
	     (cond
	       ((< point top)
		(setf top point))
	       ((and (> (- point top) (- bottom 1)) (> bottom 1))
		(setf top point)))))
      (let* ((string (rl:rl :prompt " Search: " :context :puca-search))
	     (item (find-it string (subseq goo point))))
	(if item
	    (move-to (+ point item))
	    ;; Try again from the beginning.
	    (if (setf item (find-it string goo))
		(move-to item)
		(message p "Not found.")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history mode

(defclass history (puca-item)
  ((hash
    :initarg :hash :accessor history-hash :initform nil
    :documentation "Hash for commit.")
   (email
    :initarg :email :accessor history-email :initform nil
    :documentation "Email adddress that created the item.")
   (date
    :initarg :date :accessor history-date :initform nil :type integer
    :documentation "Universal time the item was created.")
   (message
    :initarg :message :accessor history-message :initform nil
    :type (or null string)
    :documentation "Commit message."))
  (:documentation "An history editor item."))

(defun make-history (&rest initargs)
  (apply #'make-instance 'history initargs))

(defclass puca-history (puca-app)
  ((files
    :initarg :files :accessor puca-history-files :initform nil
    :documentation "Files to show history for or NIL for all files.")
   (table
    :initarg :table :accessor puca-history-table :initform nil
    :documentation "The items in a table for displaying."))
  (:documentation "History editor."))

(defclass history-table (mem-table)
  ((output-column
    :initarg :output-column :accessor output-column
    :initform 0 :type fixnum
    :documentation "The column we're at in the current row being output.")
   (output-line
    :initarg :output-line :accessor output-line :initform 0 :type fixnum
    :documentation "The line we're at in the inner screen."))
  (:documentation "A table for history."))

(defmethod get-history ((backend git) &optional files)
  (let ((hh (loop :for r
	       :in (if files
		       (split-sequence
			(code-char #x1a) ;; ^Z
			(apply #'lish:!-=
			       `(,@(backend-history backend) ,@files))
			:omit-empty t)
		       ;; @@@ we probably have to do !-= and iterate through
		       ;; all the files, so we can get multiple line comments.
		       (apply #'lish:!_=
			      `(,@(backend-history-all backend))))
	       ;;:collect (safe-read-from-string r))))
	       :collect (split-sequence (code-char 0) r :omit-empty t))))
    (coerce
     (mapcar (_ (make-history
		 :hash (trim (first _))
		 ;; :email (first (cdr _))
		 ;; :date
		 ;; #+unix (uos:unix-to-universal-time
		 ;; 	 (parse-integer (second (cdr _))))
		 ;; ;; @@@ Need to see what git on windows does?
		 ;; #+windows (parse-integer (second (cdr _)))
		 ;; :message (third (cdr _)))) hh)
		 :email (second _)
		 :date
		 #+unix (uos:unix-to-universal-time
		 	 (parse-integer (third _)))
		 ;; @@@ Need to see what git on windows does?
		 #+windows (parse-integer (third _))
		 :message (fourth _))) hh)
     'vector)))

(defmethod get-list ((puca puca-history))
  "Get the history list from the backend and parse them."
  (with-slots (goo maxima (point inator::point) top table) puca
    (setf goo (get-history (puca-backend puca) (puca-history-files puca))
	  maxima (length goo)
	  table (make-table-from goo :type 'history-table))
    (when (>= point maxima)
      (setf point (1- maxima)))
    (when (>= top point)
      (setf top (max 0 (- point 10))))))

(defun diff-history-command (p)
  "Compare this change against the previous version."
  (with-slots ((point inator::point) goo files maxima) p
    (cond
      ((>= point (1- maxima))
       (info-window "Hey"
		    '("You're at the first change."
		      "Use 'D' to compare against the HEAD version.")))
      (t
       (do-command #'backend-diff-history
	 (list (history-hash (elt goo (1+ point)))
	       (history-hash (elt goo point))
	       (mapcar #'prin1-to-string files))
	 :relist nil :do-pause nil)))))

(defun diff-history-head-command (p)
  "Compare this change against the current version."
  (with-slots ((point inator::point) goo files) p
    (do-command #'backend-diff-history-head
      (list (history-hash (elt goo point))
	    (mapcar #'prin1-to-string files))
      :relist nil :do-pause nil)))

(defun view-history-file (p)
  "View file"
  ;; (pager:pager (selected-files))
  ;;(view:view-things (selected-files))
  ;;(draw-screen p)
  (do-literal-command "view ~{~a ~}"
    (list (mapcar #'prin1-to-string (puca-history-files p))) :do-pause nil))

(defun inspect-history (p)
  (with-slots ((point inator::point) goo) p
    (let ((item (elt goo point)))
      (dbugf :puca "message = ~s~%" (quote-format (history-message item)))
      (info-window
       (s+ "Revision " (history-hash (elt goo point)))
       (list (format nil "Files: ~a" (puca-history-files p))
	     (format nil "Hash:  ~a" (history-hash item))
	     (format nil "Email: ~a" (history-email item))
	     (format nil "Date:  ~a" (dlib-misc:date-string
				      ;;:format :relative
				      :time (history-date item)))
	     "Message:" (quote-format (history-message item)))
       :justify nil))))

;; (defun inspect-history (p)
;;   (with-slots ((point inator::point) goo) p
;;     (let ((item (elt goo point)))
;;       (fui:display-text
;;        (s+ "Revision " (history-hash (elt goo point)))
;;        `(,(span-to-fat-string `((:green "Hash: ") (:cyan ,(history-hash item))))
;; 	  ,(span-to-fat-string `((:green "Email: ") (:cyan ,(history-email item))))
;; 	  ,(span-to-fat-string
;; 	    `((:green "Date: ") (:cyan ,(dlib-misc:date-string
;; 				       :time (history-date item)))))
;; 	  ,(span-to-fat-string `((:green "Message:")))
;; 	  ,(span-to-fat-string `((:cyan ,(history-message item)))))))))

(defkeymap *puca-history-keymap*
  `((#\q		. quit)
    (#\Q		. quit)
    (#\?		. help)
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
    ;;(,(ctrl #\L)	. redraw)
    (,(code-char 12)	. redraw)
    (,(meta-char #\=)	. describe-key-briefly)
    (,(ctrl #\t)	. toggle-debug)
    (#\g		. relist)
    (#\v	        . view-history-file)
    (#\i	        . inspect-history)
    (#\d	        . diff-history-command)
    (#\return	        . diff-history-command)
    (#\D	        . diff-history-head-command)
    ))

(defun history-all-command (p)
  "Show all history."
  (history-command p t))

(defun history-command (p &optional all)
  "Show history."
  (let ((files (selected-files)))
    ;;(debug-msg "selected-files ~s" files)
    (with-inator (*puca* 'puca-history
			 :files (if all nil files)
			 :keymap (list *puca-history-keymap*
				       *default-inator-keymap*)
			 :backend (puca-backend p)
			 :debug (puca-debug p))
      (event-loop *puca*))
    (inator:redraw p) ;; @@@ workaround: we should fix the real bug
    (draw-screen p)))

;;; custom table renderer

(defclass history-table-renderer (table-renderer)
  ()
  (:documentation "A table renderer to show the history."))

(defmethod table-output-header ((renderer history-table-renderer)
				table &key width sizes)
  (declare (ignore renderer width sizes))
  (setf (output-column table) 0
	(output-line table) 0))

(defmethod table-output-start-row ((renderer history-table-renderer) table)
  (declare (ignore renderer))
  (setf (output-column table) 0))

(defmethod table-output-column-separator ((renderer history-table-renderer)
					  table &key width)
  (declare (ignore renderer width))
  (incf (output-column table))
  (tt-write-char #\space))

(defmethod table-output-end-row ((renderer history-table-renderer) table n)
  (declare (ignore renderer n))
  ;; (when (< (output-column table) *max-width*)
  ;;   (tt-write-char #\newline))
  (setf (output-column table) 0)
  (incf (output-line table)))

(defmethod table-output-row-separator ((renderer history-table-renderer)
				       table n &key width sizes)
  (declare (ignore renderer table n width sizes)))

(defmethod table-output-column-title ((renderer history-table-renderer)
				      table title width justification column)
  (declare (ignore renderer))
  (when (< (output-column table) *max-width*)
    (when (not (zerop column))
      (tt-write-char #\space)
      (incf (output-column table))))

  (when (/= 0 column)
    (let* ((clipped-width (min (- *max-width* (output-column table))
			       width))
	   (len (min (length title) clipped-width)))
      (tt-underline t)
      (tt-format (if (eq justification :right) "~v@a" "~va")
		 ;;width
		 clipped-width
		 (subseq title 0 len))
      (tt-underline nil)
      (incf (output-column table) clipped-width)))

  ;; (when (and (= column (1- (olength (table-columns table))))
  ;; 	     (< (output-column table) *max-width*))
  ;;   (tt-write-char #\newline))
  )

(defmethod table-output-cell-display-width ((renderer history-table-renderer)
					    table cell column)
  (declare (ignore renderer table))
  (case column
    (0 0)
    (1 5)
    (2 (display-length (dlib-misc:date-string :format :relative
					      :time cell)))
    (3 (display-length cell))))

(defmethod table-output-cell ((renderer history-table-renderer)
			      table cell width justification row column)
  "Output a table cell."
  (declare (ignore renderer))
  (with-slots (top bottom) *puca*
    (when (and (>= row top) (< row (+ top bottom)))
      (if (= column 0)
	  (tt-move-to (+ (puca-first-line *puca*)
			 (- (output-line table) top))
		      3)
	  (let* ((*print-pretty* nil)
		 (clipped-width (min (- *max-width* (output-column table) 1)
				     width))
		 (string
		  (case column
		    (2 (dlib-misc:date-string :format :relative :time cell))
		    (otherwise
		     (let* ((s (princ-to-string cell))
			    (nl (position-if
				 (_ (find _ '(#\newline #\return))) s)))
		       (if nl
			   (subseq s 0 nl)
			   s)))))
		 (len (max 0
			   (min (length string)
				(- *max-width* (output-column table) 2)
				width))))
	    (tt-color (case column
			(1 :green)
			(2 :white)
			(3 :cyan))
		      :default)
	    (tt-format (if (eq justification :right) "~v@a" "~va")
		       ;; width
		       clipped-width
		       ;;(subseq string 0 (min width (length string)))
		       (subseq string 0 len))
	    (incf (output-column table) len)
	    (tt-color :default :default))))))

;; (defmethod draw-goo ((puca puca-history) i)
;;   "Draw the goo object, with the appropriate color."
;;   (with-slots (goo top first-line) puca
;;     (let ((g (elt goo i)))
;;       (tt-move-to (+ (- i top) first-line) 4)
;;       (tt-write-string
;;        (span-to-fat-string
;; 	`((:green ,(s+ (subseq (history-email g) 0 3) "..")) #\space
;; 	  (:white ,(dlib-misc:date-string :format :relative
;; 					  :time (history-date g))) #\space
;; 	  (:cyan ,(history-message g))))))))

(defmethod draw-inner-screen ((puca puca-history))
  (with-slots (table first-line bottom top maxima files) puca
    (when table
      (tt-move-to first-line 3)
      (tt-format "History for: ~{~s~}" files)
      (tt-move-to (+ first-line 2) 3)
      (incf first-line 3)
      (setf bottom (min (- maxima top) (- (tt-height) first-line 3)))
      (output-table table
		     (make-instance 'history-table-renderer)
		     *terminal*
		     :max-width (- (tt-width) 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defkeymap *puca-keymap*
  `((#\q		. quit)
    (#\Q		. quit)
    (#\?		. help)
    (#\h		. history-command)
    (#\H		. history-all-command)
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
    (#\-		. set-option-command)
    ;;(,(ctrl #\L)	. redraw)
    (,(code-char 12)	. redraw)
    (,(meta-char #\=)	. describe-key-briefly)
    (,(ctrl #\t)	. toggle-debug)
    (#\w                . what-command)
    (#\escape		. *puca-escape-keymap*)))

(defparameter *puca-escape-keymap* (build-escape-map *puca-keymap*))

;; (defun describe-key-briefly (p)
;;   "Prompt for a key and say what function it invokes."
;;   (message p "Press a key: ")
;;   (let* ((key (read-key-sequence p))
;; 	 (action (key-sequence-binding key *puca-keymap*)))
;;     (if action
;; 	(message p "~a is bound to ~a" (nice-char key) action)
;; 	(message p "~a is not defined" (nice-char key)))))

(defun what-command (p)
  "Try to see what backend command a key invokes."
  (message p "Press a key: ")
  (let* ((key (read-key-sequence p))
	 (action (key-sequence-binding key *puca-keymap*))
	 (command (when (and (symbolp action)
			     (ends-with "-COMMAND" (string action)))
		    (let ((func (symbolify
				 (s+ "BACKEND-"
				     (remove-suffix (string action) "-COMMAND"))
				 :package :puca)))
		      (when (fboundp func)
			(funcall func (puca-backend p)))))))
    (if action
	(if command
	    (message p "~a is bound to ~a which does ~s" (nice-char key) action
		     command)
	    (message p "~a is bound to ~a which doesn't seem to have a ~
                        backend command." (nice-char key) action))
	(message p "~a is not defined" (nice-char key)))))

;; (defmethod default-action ((p puca))
;;   (message p "Event not bound ~s" (inator-command p)))

(defmethod update-display ((p puca-app))
  (with-slots ((point inator::point) top first-line bottom debug) p
    (draw-screen p)
    (when debug
      (message p "point = ~s top = ~s first-line ~s bottom = ~s"
	       point top first-line bottom))
    (tt-move-to (+ (- point top) first-line) 2)))

(defmethod start-inator ((p puca-app))
  (call-next-method)
  (tt-clear)
  (draw-screen p)
  (get-list *puca*)
  (draw-screen p)
  (when (puca-errors p)
    (message p "**MESSAGES**")))

(defmethod finish-inator ((p puca-app))
  (tt-move-to (1- (tt-height)) 0)
  (tt-scroll-down 2)
  (tt-finish-output))

(defun puca (&key backend-type)
  (let ((backend (pick-backend backend-type)))
    (if backend
	;;(with-terminal (#| :crunch |# :curses)
	(with-new-terminal (:crunch)
	;;(with-terminal ()
	  (with-inator (*puca* 'puca
		        :keymap (list *puca-keymap* *default-inator-keymap*)
		        :backend backend)
	    (event-loop *puca*)))
	(error
  "The current directory is not under a source control system I know about."))))

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
