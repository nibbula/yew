;;
;; puca.lisp - Putative Muca (A very simple(istic) interface to CVS/git/svn)
;;
;; (a.k.a Muca-ing for fun)
;; This was translated from puca.pl, written in Perl, which is my excuse for
;; why it's poorly organized.
;;
;; TODO:
;;  - convert to using FUI.
;;  - convert to using keymaps
;;  - improve git mode
;;    - show things to pull? (ie changes on remote)
;;  - Consider making a branch editing mode
;;  - Consider making a version editing mode
;;  - Consider configuration / options editing
;;     like for "git config ..." or whatever the equivalent is in other systems

;; $Revision: 1.20 $

(defpackage :puca
  (:documentation "Putative Muca")
  (:use :cl :dlib :dlib-misc :opsys :curses :pager :tiny-rl :completion :fui
	:keymap :char-util)
  (:export
   ;; Main entry point
   #:puca
   #+lish #:!puca
   #:make-standalone
   ))
(in-package :puca)

(defun is-cvs ()
  (probe-directory "CVS"))

(defun is-git ()
  (equal "true" (shell-line "git" "rev-parse" "--is-inside-work-tree")))

(defun is-svn ()
  (probe-directory ".svn"))

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

(defstruct backend
  "A specific version control system. Mostly how to do things with it."
  name
  type
  check-func
  list-command
  list-args
  add
  reset
  diff
  diff-repo
  commit
  update
  update-all
  push)

(defgeneric parse-line (pu type line i)
  (:documentation "Take a line and add stuff to goo and/or *errors*."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CVS

(defparameter *backend-cvs*
  (make-backend :name		"CVS"
		:type		:cvs
		:check-func	#'is-cvs
		:list-command	"cvs"
		:list-args	'("-n" "update")
		:add		"cvs add ~{~a ~}"
		:reset		"echo 'No reset in CVS'"
		:diff		"cvs diff ~{~a ~} | less"
		:diff-repo	"cvs diff -r HEAD ~{~a ~} | less"
		:commit		"cvs commit ~{~a ~}"
		:update		"cvs update ~{~a ~}"
		:update-all	"cvs update"
		:push		"echo 'No push in CVS'"))

(defmethod parse-line (pu (type (eql :cvs)) line i)
  (with-slots (goo errors extra) pu
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
		:list-command	"git"
;		:list-args	'("--no-pager" "diff" "--name-status")
		:list-args	'("status" "--porcelain")
		:add		"git --no-pager add ~{~a ~}"
		:reset		"git --no-pager reset ~{~a ~}"
		:diff		"git diff ~{~a ~}"
;		:diff-repo	"git diff --cached HEAD ~{~a ~}"
		:diff-repo	"git diff --staged"
		:commit		"git --no-pager commit ~{~a ~}"
		:update		"git --no-pager pull ~{~a ~}"
		:update-all	"git --no-pager pull"
		:push		"git --no-pager push"))

(defmethod parse-line (pu (type (eql :git)) line i)
  (with-slots (goo errors extra) pu
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
		:list-command	"svn"
		:list-args	'("status")
		:add		"svn add ~{~a ~}"
		:reset		"svn revert ~{~a ~}"
		:diff		"svn diff ~{~a ~} | less"
		:diff-repo	"svn diff -r HEAD ~{~a ~} | less"
		:commit		"svn commit ~{~a ~}"
		:update		"svn update ~{~a ~}"
		:update-all	"svn update"
		:push		"echo 'No push in SVN'"))

(defmethod parse-line (pu (type (eql :svn)) line i)
  (parse-line pu :git line i)) ; just use cvs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *backend* nil
  "The current backend.")

(defparameter *backends* `((:git ,*backend-git*)
			   (:cvs ,*backend-cvs*)
			   (:svn ,*backend-svn*))
  "The availible backends.")

#|
(defun init-curses (pu device term-type)
  (with-slots (window screen) pu
    (setf window
	  (if device
	      (progn
		(let (fin fout)
		  (if (cffi:null-pointer-p (setq fin (fopen device "r")))
		      (error "Can't open curses input device"))
		  (if (cffi:null-pointer-p (setq fout (fopen device "w")))
		      (error "Can't open curses output device"))
		  (if (cffi:null-pointer-p
		       (setf screen (newterm term-type fout fin)))
		      (error "Can't initialize curses terminal"))
		  (set-term screen)))
	      (progn
		(initscr))))
  (initscr)
  (start-color)
  (clear)
  (init-pair 0 +COLOR-WHITE+    +COLOR-BLACK+)
  (init-pair 1 +COLOR-RED+      +COLOR-BLACK+)
  (init-pair 2 +COLOR-GREEN+    +COLOR-BLACK+)
  (init-pair 3 +COLOR-YELLOW+   +COLOR-BLACK+)
  (init-pair 4 +COLOR-BLUE+     +COLOR-BLACK+)
  (init-pair 5 +COLOR-MAGENTA+  +COLOR-BLACK+)
  (init-pair 6 +COLOR-CYAN+     +COLOR-BLACK+)
  (init-pair 7 +COLOR-WHITE+    +COLOR-BLACK+)
  (noecho)
  (cbreak)))
|#

(defun message (format-string &rest format-args)
  (move (- *lines* 2) 2)
  (clrtoeol)
  (addstr (apply #'format nil format-string format-args))
  (refresh))

(defun draw-goo (pu i)
  "Draw the goo object, with the appropriate color."
  (with-slots (goo top) pu
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

(defun draw-line (pu i)
  "Draw line I, with the appropriate color."
  (draw-goo pu i))

(defun get-list (pu)
  (message "Listing...")
  (with-slots (goo top errors maxima cur extra) pu
    (setf goo    '()
	  top    0
	  errors '())

    (let ((i 0) line)
      (setf extra '())
      (with-process-output (s (backend-list-command *backend*)
			      (backend-list-args *backend*))
	(loop :while (setf line (read-line s nil nil))
	   :do
	   (parse-line pu (backend-type *backend*) line i)
	   (incf i)
	   (message "Listing...~d" i)))
      (setf goo (nreverse goo)
	    errors (nreverse errors))
      (setf maxima (length goo))
      (when (>= cur maxima)
	(setf cur (1- maxima)))))
  (addstr "done"))

(defun draw-screen (pu)
  (with-slots (maxima top bottom goo window) pu
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
	   :do (draw-goo pu i)))
      ;; bottom scroll indicator
      (when (< bottom (- maxima top))
	(mvaddstr (+ bottom 4) 2 "vvvvvvvvvvvvvvvvvvvvvvv"))
      (refresh))))

(defun do-command (pu command format-args
		   &key (relist t) (do-pause t) confirm)
  (clear)
  (refresh)
  (endwin)
  (when (and confirm (not (yes-or-no-p "Are you sure? ")))
    (return-from do-command (values)))
  (let* ((format-command (apply command (list *backend*)))
	 (command (apply #'format nil format-command (list format-args))))
    (system-command command))
  (when do-pause
    (write-string "[Press Return]")
    (terpri)
    (read-line))
  (initscr)
  (when relist
    (get-list pu))
  (draw-screen pu)
  (values))

(defun selected-files (pu)
  (with-slots (maxima goo cur) pu
    (let* ((files
	    (loop :for i :from 0 :below maxima
	       :when (goo-selected (elt goo i))
	       :collect (goo-filename (elt goo i)))))
      (or files (and goo (list (goo-filename (elt goo cur))))))))

(defun select-all (pu)
  (with-slots (maxima goo) pu
    (loop :for i :from 0 :below maxima
       :do (setf (goo-selected (elt goo i)) t))))

(defun select-none (pu)
  (with-slots (maxima goo) pu
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

#|
(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

;; @@@ Something like this should probably move to FUI.
(defun puca-display-text (pu title text-lines &key input-func)
  (let* ((mid    (truncate (/ *cols* 2)))
	 (width  (min (- *cols* 6)
		      (+ 4 (loop :for l :in text-lines :maximize (length l)))))
	 (height (min (+ 4 (loop :for l :in text-lines
			      :sum
			      (1+ (count
				   #\newline
				   (justify-text l :cols (- width 2)
						 :stream nil)))))
		      (- *lines* 4)))
	 (xpos   (truncate (- mid (/ width 2))))
	 w result)
    (unwind-protect
       (progn
	 (setf w (newwin height width 3 xpos))
	 (box w 0 0)
	 (wcentered w width 0 title)
	 (loop :with i = 2 :for l :in text-lines
	    :do
	    (loop :for sub-line
	       :in (split-sequence
		    #\newline (justify-text l :cols (- width 2)
					    :stream nil))
	       :do
	       (mvwaddstr w i 2 sub-line)
	       (incf i)))
	 (wrefresh w)
	 (when input-func
	   (setf result (funcall input-func pu w))))
      (when w
	(delwin w)))
    result))
|#

(defun info-window (pu title text-lines)
#|  (puca-display-text
   pu title text-lines
   :input-func #'(lambda (pu w)
		   (declare (ignore pu w))
		   (get-char))) |#
  (fui:display-text title text-lines)
  (clear)
  (refresh)
  (draw-screen pu)
  (refresh))

(defun input-window (pu title text-lines)
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
    (draw-screen pu)
    (refresh)))

(defun puca-yes-or-no-p (pu &optional format &rest arguments)
  (equalp "Yes" (input-window
		 pu "Yes or No?"
		 (list 
		  (if format
		      (apply #'format nil format arguments)
		      "Yes or No?")))))

(defun delete-files (pu)
  (when (puca-yes-or-no-p
	 pu "Are you sure you want to delete these files from disk?~%~{~a ~}"
	 (selected-files pu))
    (loop :for file :in (selected-files pu) :do
       (delete-file file))
    (get-list pu)))

(defparameter *extended-commands*
  '(("delete" delete-files))
  "An alist of extended commands, key is the command name, value is a symbol
for the command-function).")

(defvar *complete-extended-command*
  (completion:list-completion-function (mapcar #'car *extended-commands*))
  "Completion function for extended commands.")

(defun extended-command (pu)
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
      (funcall func pu)))
  (draw-screen pu)
  (values))

(defun help (pu)
  (info-window pu "Help for Puca"
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
		  "?,h - Help")))

(defun show-errors (pu)
  (with-slots (errors) pu
    (info-window pu "All Errors"
		 (or errors
		     '("There are no errors." "So this is" "BLANK")))))

(defun show-extra (pu)
  (with-slots (goo cur) pu
    (let ((ext (and goo (goo-extra-lines (elt goo cur)))))
      (info-window pu "Errors"
		   (or ext
		       '("There are no errors." "So this is" "BLANK"))))))

(defun pick-backend (&optional type)
  ;; try find what was asked for
  (when type
    (let ((be (find type *backends* :key #'car)))
      (when be (return-from pick-backend (cadr be)))))
  ;; try to figure it out
  (let ((result
	 (loop :for (nil b) :in *backends* :do
	    (when (funcall (backend-check-func b))
	      (return b)))))
    (if (not result)
	*backend-cvs*
	result)))

(defun quit (pu)
  (setf (puca-quit-flag pu) t))

(defun add-command (pu)
  (do-command pu #'backend-add (selected-files pu)))

(defun reset-command (pu)
  (do-command pu #'backend-reset (selected-files pu) :confirm t))

(defun diff-command (pu)
  (do-command pu #'backend-diff (selected-files pu)
	      :relist nil :do-pause nil))

(defun diff-repo-command (pu)
  (do-command pu #'backend-diff-repo (selected-files pu)
	      :relist nil :do-pause nil))

(defun commit-command (pu)
  (do-command pu #'backend-commit (selected-files pu)))

(defun update-command (pu)
  (do-command pu #'backend-update (selected-files pu)))

(defun update-all-command (pu)
  (do-command pu #'backend-update-all nil))

(defun push-command (pu)
  (do-command pu #'backend-push nil :relist nil))

(defun view-file (pu)
  (pager:pager (selected-files pu))
  (draw-screen pu))

(defun previous-line (pu)
  (with-slots (cur top) pu
    (decf cur)
    (when (< cur 0)
      (setf cur 0))
    (when (< cur top)
      (decf top)
      (draw-screen pu))))

(defun next-line (pu)
  (with-slots (cur maxima top bottom) pu
    (incf cur)
    (when (>= cur maxima)
      (setf cur (1- maxima)))
    (when (and (> cur (- bottom 1)) (> bottom 1))
      (incf top)
      (draw-screen pu))))

(defun next-page (pu)
  (with-slots (cur maxima top bottom) pu
    (setf cur (+ cur 1 (- curses:*lines* 7)))
    (when (>= cur maxima)
      (setf cur (1- maxima)))
    (when (> cur bottom)
      (setf top (max 0 (- cur (- curses:*lines* 7))))
      (draw-screen pu))))

(defun previous-page (pu)
  (with-slots (cur top) pu
    (setf cur (- cur 1 (- curses:*lines* 7)))
    (when (< cur 0)
      (setf cur 0))
    (when (< cur top)
      (setf top cur)
      (draw-screen pu))))

(defun go-to-end (pu)
  (with-slots (cur maxima top bottom) pu
    (setf cur (1- maxima))
    (when (> maxima bottom)
      (setf top (max 0 (- maxima (- curses:*lines* 7))))
      (draw-screen pu))))

(defun go-to-beginning (pu)
  (with-slots (cur top) pu
    (setf cur 0)
    (when (> top 0)
      (setf top 0)
      (draw-screen pu))))

(defun set-mark (pu)
  (with-slots (cur mark) pu
    (setf mark cur)
    (message "Set mark.")))

(defun toggle-region (pu)
  (with-slots (cur mark) pu
    (if mark
      (let ((start (min mark cur))
	    (end (max mark cur)))
	(loop :for i :from start :to end :do
	   (setf (goo-selected (elt (puca-goo pu) i))
		 (not (goo-selected (elt (puca-goo pu) i))))
	   (draw-line pu i)))
      (message "No mark set."))))

(defun toggle-line (pu)
  (with-slots (cur goo) pu
    (when goo
      (setf (goo-selected (elt (puca-goo pu) cur))
	    (not (goo-selected (elt (puca-goo pu) cur))))
      (draw-line pu cur))))

(defun select-all-command (pu)
  (select-all pu)
  (draw-screen pu))

(defun select-none-command (pu)
  (select-none pu)
  (draw-screen pu))

(defun relist (pu)
  (clear)
  (refresh)
  (get-list pu)
  (draw-screen pu)
  (refresh))

(defun redraw (pu)
  (clear)
  (refresh)
  (draw-screen pu)
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
    (#\^p       	. previous-line)
    (:DOWN      	. next-line)
    (,(code-char 14)    . next-line)
    (#\^n		. next-line)
    (:NPAGE		. next-page)
    (#\^V		. next-page)
    (#\^F		. next-page)
    (:PPAGE		. previous-page)
    (#\^B		. previous-page)
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
    (#\^L		. redraw)
    (,(code-char 12)	. redraw)
    (,(meta-char #\=)	. describe-key-briefly)
    (#\escape		. *puca-escape-keymap*)))

(defparameter *puca-escape-keymap* (build-escape-map *puca-keymap*))

(defun describe-key-briefly (pu)
  "Prompt for a key and say what function it invokes."
  (declare (ignore pu))
  (message "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *puca-keymap*)))
    (if action
	(message "~a is bound to ~a" (nice-char key) action)
	(message "~a is not defined" (nice-char key)))))

(defun perform-key (pu key &optional (keymap *puca-keymap*))
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
	  (funcall binding pu))
	 ((keymap-p (symbol-value binding))
	  (message (princ-to-string (nice-char key)))
	  (perform-key pu (fui:get-char) (symbol-value binding)))
	 (t
	  (error "Unbound symbol ~s in keymap" binding))))
      ((consp binding)
       (apply (car binding) pu (cdr binding)))
      (t
       (error "Weird thing ~s in keymap" binding)))))

(defun puca (&key #|device term-type |# backend-type)
  (with-curses
    ;; (init-curses pu device term-type)
    (let* ((pu (make-puca))
	   (*backend* (pick-backend backend-type)) c)
      (with-slots
	    (goo maxima cur mark top bottom window screen quit-flag errors) pu
	(setf window *stdscr*)
	(setf goo nil)
	(draw-screen pu)
	(get-list pu)
	(draw-screen pu)
	(when errors (message "**MESSAGES**"))
	(setf cur 0)
	(do () (quit-flag)
	  (move (+ (- cur top) 3) 2)
	  (setf c (fui:get-char))
	  (perform-key pu c)
;	  (move (- *lines* 2) 2)
;	  (mvprintw (- *lines* 2) 2 "%-*.*s" :int 40 :int 40 :string "")
;	  (mvaddstr (- *lines* 2) 2 (format nil "~a (~a)" c (type-of c))))
	  (refresh)))
      (endwin))))

(defun make-standalone (&optional (name "puca"))
  "FUFKFUFUFUFUFF"
  #+sbcl (sb-ext:save-lisp-and-die name :executable t
				   :toplevel #'puca:puca)
  #+clisp (saveinitmem name :executable t :quiet t :norc t
		       :init-function #'puca:puca)
  #-(or sbcl clisp) (declare (ignore name))
  #-(or sbcl clisp) (missing-implementation 'make-standalone)
  )

#+lish
(lish:defcommand puca
  (("cvs" boolean :short-arg #\c)
   ("svn" boolean :short-arg #\s)
   ("git" boolean :short-arg #\g))
  "Putative Muca interface to your version control software.
Arguments are: -c for CVS, -s for SVN, -g GIT."
  (puca :backend-type (cond (cvs :cvs) (svn :svn) (git :git))))

; Joe would probably like the name but not the software.

;; EOF
