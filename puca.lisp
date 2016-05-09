;;
;; puca.lisp - Putative Muca (A very simple(istic) interface to CVS/git/svn)
;;
;; (a.k.a Muca-ing for fun)
;; This was translated from puca.pl, written in Perl, which is my excuse for
;; why it's poorly organized.
;;
;; TODO:
;;  - improve git mode
;;    - show things to pull? (ie changes on remote)
;;  - Consider making a branch editing mode
;;  - Consider making a version editing mode
;;  - Consider configuration / options editing
;;     like for "git config ..." or whatever the equivalent is in other systems

(defpackage :puca
  (:documentation "Putative Muca")
  (:use :cl :dlib :dlib-misc :opsys :keymap :char-util :curses :tiny-rl
	:completion :fui)
  (:export
   ;; Main entry point
   #:puca
   #:!puca
   #:make-standalone
   ))
(in-package :puca)

(defun is-cvs ()
  (probe-directory "CVS"))

(defun is-git ()
  (equal "true" (shell-line "git" "rev-parse" "--is-inside-work-tree")))

(defun is-svn ()
  (probe-directory ".svn"))

(defun is-hg ()
  (probe-directory ".hg"))

(defstruct goo
  "A file/object under version control."
  selected
  modified
  filename
  extra-lines)

(defstruct puca
  "An instance of a version control frontend app."
  (goo nil)				; A list of goo entries.
  (maxima 0)				; Number of items.
  (cur 0)				; Current item.
  (mark nil)				; One end of the region
  (top 0)				; Top item
  (bottom nil)				; Bottom item.
  (window nil)				; Curses WINDOW
  (screen nil)				; Curses SCREEN
  (quit-flag nil)			; set to true to quit
  (errors nil)				; Error output
  (extra nil))				; extra lines

(defvar *puca* nil
  "The current puca instance.")

(defstruct backend
  "A specific version control system. Mostly how to do things with it."
  name
  type
  check-func
  list-command
  add
  reset
  diff
  diff-repo
  commit
  update
  update-all
  push)

(defvar *backend* nil
  "The current backend.")

#|
(defclass backend ()
  ((name
    :initarg :name :accessor backend-name  :type string
    :documentation "The name of the back-end."))
  (:documentation "A generic version control back end."))
|#

(defgeneric parse-line (backend line i)
  (:documentation "Take a line and add stuff to goo and/or *errors*."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVS

(defparameter *backend-cvs*
  (make-backend :name		"CVS"
		:type		:cvs
		:check-func	#'is-cvs
		:list-command	'("cvs" "-n" "update")
		:add		"cvs add ~{~a ~}"
		:reset		"echo 'No reset in CVS'"
		:diff		"cvs diff ~{~a ~} | pager"
		:diff-repo	"cvs diff -r HEAD ~{~a ~} | pager"
		:commit		"cvs commit ~{~a ~}"
		:update		"cvs update ~{~a ~}"
		:update-all	"cvs update"
		:push		"echo 'No push in CVS'"))

(defmethod parse-line ((type (eql :cvs)) line i)
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

(defparameter *backend-git*
  (make-backend :name		"git"
		:type		:git
		:check-func	#'is-git
		:list-command	'("git" "status" "--porcelain")
		:add		"git --no-pager add ~{~a ~}"
		:reset		"git --no-pager reset ~{~a ~}"
		:diff		"git diff --color ~{~a ~} | pager"
;		:diff-repo	"git diff --cached HEAD ~{~a ~}"
		:diff-repo	"git diff --color --staged | pager"
		:commit		"git --no-pager commit ~{~a ~}"
		:update		"git --no-pager pull ~{~a ~}"
		:update-all	"git --no-pager pull"
		:push		"git --no-pager push"))

(defmethod parse-line ((type (eql :git)) line i)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVN

(defparameter *backend-svn*
  (make-backend :name		"SVN"
		:type		:svn
		:check-func	#'is-svn
		:list-command	'("svn" "status")
		:add		"svn add ~{~a ~}"
		:reset		"svn revert ~{~a ~}"
		:diff		"svn diff ~{~a ~} | pager"
		:diff-repo	"svn diff -r HEAD ~{~a ~} | pager"
		:commit		"svn commit ~{~a ~}"
		:update		"svn update ~{~a ~}"
		:update-all	"svn update"
		:push		"echo 'No push in SVN'"))

(defmethod parse-line ((type (eql :svn)) line i)
  (parse-line :git line i)) ; just use git

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mercurial (hg)

(defparameter *backend-hg*
  (make-backend :name		"hg"
		:type		:hg
		:check-func	#'is-hg
		:list-command	'("hg" "status")
		:add		"hg add ~{~a ~}"
		:reset		"hg revert ~{~a ~}"
		:diff		"hg diff ~{~a ~} | pager"
;		:diff-repo	"hg diff --cached HEAD ~{~a ~}"
		:diff-repo	"hg diff ~{~a ~} | pager"
		:commit		"hg commit ~{~a ~}"
		:update		"hg pull ~{~a ~}?"
		:update-all	"hg pull"
		:push		"hg push"))

(defmethod parse-line ((type (eql :hg)) line i)
  (parse-line :git line i)) ; just use git

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *backends* `((:git ,*backend-git*)
			   (:cvs ,*backend-cvs*)
			   (:svn ,*backend-svn*)
			   (:hg ,*backend-hg*))
  "The availible backends.")

(defun message (format-string &rest format-args)
  (move (- *lines* 2) 2)
  (clrtoeol)
  (addstr (apply #'format nil format-string format-args))
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
;; Ugly inline display:
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
  (message "Listing...")
  (with-slots (goo top errors maxima cur extra) *puca*
    (setf goo    '()
	  top    0
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
	   (parse-line (backend-type *backend*) line i)
	   (incf i)
	   (message "Listing...~d" i)))
      (setf goo (nreverse goo)
	    errors (nreverse errors))
      (setf maxima (length goo))
      (when (>= cur maxima)
	(setf cur (1- maxima)))))
  (addstr "done"))

(defun draw-screen ()
  (with-slots (maxima top bottom goo window) *puca*
;  (box *stdscr* (acs-vline) (acs-hline))
;  (box window 0 0)
    (clear)
    (border 0 0 0 0 0 0 0 0)
    (setf bottom (min (- maxima top) (- curses:*lines* 7)))
    (let* ((title (format nil "~a Muca (~a)" 
			  (backend-name *backend*) (machine-instance))))
      (move 1 (truncate (- (/ *cols* 2) (/ (length title) 2))))
      (keypad window 1)
      (addstr title)
      ;; top scroll indicator
      (when (> top 0)
	(mvaddstr 2 2 "^^^^^^^^^^^^^^^^^^^^^^^"))
      (when (> (length goo) 0)
	(loop :for i :from top :below (+ top bottom)
	   :do (draw-goo i)))
      ;; bottom scroll indicator
      (when (< bottom (- maxima top))
	(mvaddstr (+ bottom 4) 2 "vvvvvvvvvvvvvvvvvvvvvvv"))
      (refresh))))

(defun do-command (command format-args
		   &key (relist t) (do-pause t) confirm)
  (clear)
  (refresh)
  (endwin)
  (when (and confirm (not (yes-or-no-p "Are you sure? ")))
    (return-from do-command (values)))
  (let* ((format-command (apply command (list *backend*)))
	 (command (apply #'format nil format-command (list format-args))))
    (lish:! command))
  (when do-pause
    (write-string "[Press Return]")
    (terpri)
    (read-line))
  (initscr)
  (when relist
    (get-list))
  (draw-screen)
  (values))

(defun selected-files ()
  (with-slots (maxima goo cur) *puca*
    (let* ((files
	    (loop :for i :from 0 :below maxima
	       :when (goo-selected (elt goo i))
	       :collect (goo-filename (elt goo i)))))
      (or files (and goo (list (goo-filename (elt goo cur))))))))

(defun select-all ()
  (with-slots (maxima goo) *puca*
    (loop :for i :from 0 :below maxima
       :do (setf (goo-selected (elt goo i)) t))))

(defun select-none ()
  (with-slots (maxima goo) *puca*
    (loop :for i :from 0 :below maxima
       :do (setf (goo-selected (elt goo i)) nil))))

#|
(defun get-char ()
  (let ((c (getch)))
    (cond
      ((= c -1) ; Error, probably from signal or resize, simulate a ^L
       #\^L)
      ((> c #xff) (function-key c))
;      ((characterp c) (code-char c))
      (t (code-char c)))))
|#

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

(defun extended-command ()
  "Extended command"
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

(defun show-errors ()
  "Show all messages / errors"
  (with-slots (errors) *puca*
    (info-window "All Errors"
		 (or errors
		     '("There are no errors." "So this is" "BLANK")))))

(defun show-extra ()
  "Show messages / errors"
  (with-slots (goo cur) *puca*
    (let ((ext (and goo (goo-extra-lines (elt goo cur)))))
      (info-window "Errors"
		   (or ext
		       '("There are no errors." "So this is" "BLANK"))))))

(defun pick-backend (&optional type)
  ;; try find what was asked for
  (when type
    (let ((be (find type *backends* :key #'car)))
      (when be (return-from pick-backend (cadr be)))))
  ;; try to figure it out
  (let ((result
	 (loop :for (type b) :in *backends* :do
	    (dbug "Trying backend ~s~%" type)
	    (when (funcall (backend-check-func b))
	      (dbug "Picked ~s~%" type)
	      (return b)))))
    (if (not result)
	*backend-cvs*
	result)))

(defun quit ()
  "Quit"
  (setf (puca-quit-flag *puca*) t))

(defun add-command ()
  "Add file"
  (do-command #'backend-add (selected-files)))

(defun reset-command ()
  "Revert file (undo an add)"
  (do-command #'backend-reset (selected-files) :confirm t))

(defun diff-command ()
  "Diff"
  (do-command #'backend-diff (selected-files)
	      :relist nil :do-pause nil))

(defun diff-repo-command ()
  "Diff against commited (-r HEAD)"
  (do-command #'backend-diff-repo (selected-files)
	      :relist nil :do-pause nil))

(defun commit-command ()
  "Commit selected"
  (do-command #'backend-commit (selected-files)))

(defun update-command ()
  "Update selected"
  (do-command #'backend-update (selected-files)))

(defun update-all-command ()
  "Update all"
  (do-command #'backend-update-all nil))

(defun push-command ()
  "Push"
  (do-command #'backend-push nil :relist nil))

(defun view-file ()
  "View file"
  (pager:pager (selected-files))
  (draw-screen))

(defun previous-line ()
  "Previous line"
  (with-slots (cur top) *puca*
    (decf cur)
    (when (< cur 0)
      (setf cur 0))
    (when (< cur top)
      (decf top)
      (draw-screen))))

(defun next-line ()
  "Next line"
  (with-slots (cur maxima top bottom) *puca*
    (incf cur)
    (when (>= cur maxima)
      (setf cur (1- maxima)))
    (when (and (> cur (- bottom 1)) (> bottom 1))
      (incf top)
      (draw-screen))))

(defun next-page ()
  "Next page"
  (with-slots (cur maxima top bottom) *puca*
    (setf cur (+ cur 1 (- curses:*lines* 7)))
    (when (>= cur maxima)
      (setf cur (1- maxima)))
    (when (> cur bottom)
      (setf top (max 0 (- cur (- curses:*lines* 7))))
      (draw-screen))))

(defun previous-page ()
  "Previous page"
  (with-slots (cur top) *puca*
    (setf cur (- cur 1 (- curses:*lines* 7)))
    (when (< cur 0)
      (setf cur 0))
    (when (< cur top)
      (setf top cur)
      (draw-screen))))

(defun go-to-end ()
  "Bottom"
  (with-slots (cur maxima top bottom) *puca*
    (setf cur (1- maxima))
    (when (> maxima bottom)
      (setf top (max 0 (- maxima (- curses:*lines* 7))))
      (draw-screen))))

(defun go-to-beginning ()
  "Top"
  (with-slots (cur top) *puca*
    (setf cur 0)
    (when (> top 0)
      (setf top 0)
      (draw-screen))))

(defun set-mark ()
  "Set Mark"
  (with-slots (cur mark) *puca*
    (setf mark cur)
    (message "Set mark.")))

(defun toggle-region ()
  "Toggle region"
  (with-slots (cur mark) *puca*
    (if mark
      (let ((start (min mark cur))
	    (end (max mark cur)))
	(loop :for i :from start :to end :do
	   (setf (goo-selected (elt (puca-goo *puca*) i))
		 (not (goo-selected (elt (puca-goo *puca*) i))))
	   (draw-line i)))
      (message "No mark set."))))

(defun toggle-line ()
  "Toggle line"
  (with-slots (cur goo) *puca*
    (when goo
      (setf (goo-selected (elt (puca-goo *puca*) cur))
	    (not (goo-selected (elt (puca-goo *puca*) cur))))
      (draw-line cur))))

(defun select-all-command ()
  "Select all"
  (select-all)
  (draw-screen))

(defun select-none-command ()
  "Select none"
  (select-none)
  (draw-screen))

(defun relist ()
  "Re-list"
  (clear)
  (refresh)
  (get-list)
  (draw-screen)
  (refresh))

(defun redraw ()
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
    (#\v        	. view-file)
    (:UP        	. previous-line)
    (,(code-char 16)	. previous-line)
    (,(ctrl #\p)       	. previous-line)
    (:DOWN      	. next-line)
    (,(code-char 14)    . next-line)
    (,(ctrl #\n)	. next-line)
    (:NPAGE		. next-page)
    (,(ctrl #\V)	. next-page)
    (,(ctrl #\F)	. next-page)
    (:PPAGE		. previous-page)
    (,(ctrl #\B)	. previous-page)
    (#\>		. go-to-end)
    (#\<		. go-to-beginning)
    (,(meta-char #\>)	. go-to-end)
    (,(meta-char #\<)	. go-to-beginning)
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
    (,(ctrl #\L)	. redraw)
    (,(code-char 12)	. redraw)
    (,(meta-char #\=)	. describe-key-briefly)
    (#\escape		. *puca-escape-keymap*)))

(defparameter *puca-escape-keymap* (build-escape-map *puca-keymap*))

(defun describe-key-briefly ()
  "Prompt for a key and say what function it invokes."
  (message "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *puca-keymap*)))
    (if action
	(message "~a is bound to ~a" (nice-char key) action)
	(message "~a is not defined" (nice-char key)))))

(defparameter *handmade-help*
  '("q - Quit"
    "a - Add file"
    "r - Revert file (undo an add)"
    "d - Diff"
    "D - Diff -r HEAD"
    "c - Commit selected"
    "u - Update selected"
    "U - Update all"
    "P - Push"
    "^N,^P,up,down - Move around"
    "< - Top"
    "> - Bottom"
    "Space,x,Return - Select"
    "s - Select all"
    "S - Select none"
    "^@, ^space - Set Mark"
    "X - Select region"
    "g - Re-list"
    "e - Show messages / errors"
    ": - Extended command"
    "^L - Re-draw"
    "?,h - Help"))

(defun help ()
  "Help"
  (info-window "Help for Puca" (help-list *puca-keymap*)))

;; (defun help-list ()
;;   "Return a list of key binding help lines, suitable for the HELP function."
;;   ;; Make a reverse hash of functions to keys, so we can put all the bindings
;;   ;; for a function on one line.
;;   (let ((rev-hash (make-hash-table)))
;;     (flet ((add-key (k v) (push k (gethash v rev-hash))))
;;       (map-keymap #'add-key *puca-keymap*))
;;     (loop :for func :being :the :hash-keys :of rev-hash
;;        :collect
;;        (with-output-to-string (str)
;; 	 (format str "~{~a~^, ~} - ~a"
;; 		 (loop :for k :in (gethash func rev-hash)
;; 		    :collect (nice-char k :caret t))
;; 		 ;; Look up the documentation for the function.
;; 		 (cond
;; 		   ((or (functionp func)
;; 			(and (symbolp func) (fboundp func)))
;; 		    (let ((doc (documentation func 'function)))
;; 		      (or doc (string-downcase func))))
;; 		   ((keymap-p (string-downcase func)))
;; 		   (t func)))))))

(defun perform-key (key &optional (keymap *puca-keymap*))
  ;; Convert positive integer keys to characters
  (when (and (integerp key) (>= key 0))
    (setf key (code-char key)))
  (let ((binding (key-binding key keymap)))
    (cond
      ((not binding)
       (message "No binding for ~a" key))
      ((symbolp binding)
       (cond
	 ((fboundp binding)
	  (funcall binding))
	 ((keymap-p (symbol-value binding))
	  (message (princ-to-string (nice-char key)))
	  (perform-key (fui:get-char) (symbol-value binding)))
	 (t
	  (error "Unbound symbol ~s in keymap" binding))))
      ((consp binding)
       (apply (car binding) (cdr binding)))
      (t
       (error "Weird thing ~s in keymap" binding)))))

(defun puca (&key #|device term-type |# backend-type)
  (with-curses
    ;; (init-curses pu device term-type)
    (let* ((*puca* (make-puca))
	   (*backend* (pick-backend backend-type)) c)
      (with-slots
	    (goo maxima cur mark top bottom window screen quit-flag
	     errors) *puca*
	(setf window *stdscr*)
	(setf goo nil)
	(draw-screen)
	(get-list)
	(draw-screen)
	(when errors (message "**MESSAGES**"))
	(setf cur 0)
	(do () (quit-flag)
	  (move (+ (- cur top) 3) 2)
	  (setf c (fui:get-char))
	  (perform-key c)
;	  (move (- *lines* 2) 2)
;	  (mvprintw (- *lines* 2) 2 "%-*.*s" :int 40 :int 40 :string "")
;	  (mvaddstr (- *lines* 2) 2 (format nil "~a (~a)" c (type-of c))))
	  (refresh)))
      (endwin))))

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
