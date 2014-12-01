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
;;  - Consider configuration / options editing
;;     like for "git config ..." or whatever the equivalent is in other systems

;; $Revision: 1.20 $

(defpackage :puca
  (:documentation "Putative Muca")
  (:use :cl :dlib :dlib-misc :opsys :curses :pager)
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

(defstruct puca-app
  "An instance of a version control frontend app."
  (goo nil)				; A list of goo entries.
  (maxima 0)				; Number of items.
  (cur 0)				; Current item.
  (top 0)				; Top item
  (bottom nil)				; Bottome item.
  (window nil)				; Curses WINDOW
  (screen nil)				; Curses SCREEN
  (errors nil)				; Error output
  (extra nil))				; extra lines

(defstruct backend
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
      (dbug "~s~%" words)
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

(defvar *backend* nil
  "The current backend.")

(defparameter *backends* `((:cvs ,*backend-cvs*)
			   (:git ,*backend-git*)
			   (:svn ,*backend-svn*))
  "The availible backends.")

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

(defun message (format-string &rest format-args)
  (clrtoeol)
  (mvaddstr (- *lines* 2) 2 (apply #'format nil format-string format-args))
  (refresh))

(defun draw-goo (pu i)
  "Draw the goo object, with the appropriate color."
  (with-slots (goo top) pu
    (let ((g (elt goo i)))
      (let* ((attr
	      (case (aref (goo-modified g) 0)
		(#\M 1)			; red     - modified
		(#\? 0)			; white   - unknown
		(#\C 5)			; magenta - conflicts
		(t 2))))		; green   - updated or other
	(move (+ (- i top) 3) 4)
					;    (clrtoeol)
	(attron (color-pair attr))
	(addstr (format nil "~a ~a ~30a"
			(if (goo-selected g) "x" " ")
			(goo-modified g)
			(goo-filename g)))
	(attroff (color-pair attr))
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

(defun get-char ()
  (let ((c (getch)))
    (cond
      ((= c -1) ; Error, probably from signal or resize, simulate a ^L
       #\^L)
      ((> c #xff) (function-key c))
;      ((characterp c) (code-char c))
      (t (code-char c)))))

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

(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

(defun display-text (pu title text-lines)
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
	 (w 	 (newwin height width 3 xpos)))
    (box w 0 0)
    (wcentered w width 0 title)
    (loop :with i = 2 :for l :in text-lines
       :do
       (loop :for sub-line :in (split-sequence
				#\newline (justify-text l :cols (- width 2)
							:stream nil))
	  :do
	  (mvwaddstr w i 2 sub-line)
	  (incf i)))
    (wrefresh w)
    (get-char)
    (delwin w))
  (clear)
  (refresh)
  (draw-screen pu)
  (refresh))

(defun help (pu)
  (display-text pu "Help for Puca"
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
		  "g - Re-list"
		  "e - Show messages / errors"
		  "^L - Re-draw"
		  "?,h - Help")))

(defun show-errors (pu)
  (with-slots (errors) pu
    (display-text pu "All Errors"
		  (or errors
		      '("There are no errors." "So this is" "BLANK")))))

(defun show-extra (pu)
  (with-slots (goo cur) pu
    (let ((ext (and goo (goo-extra-lines (elt goo cur)))))
      (display-text pu "Errors"
		    (or ext
			'("There are no errors." "So this is" "BLANK"))))))

(defun pick-backend (&optional type)
  ;; try find what was asked for
  (when type
    (let ((be (find type *backends* :key #'car)))
      (when be (return-from pick-backend (cadr be)))))
  ;; try to figure it out
  (let ((result
	 (loop :for (tag b) :in *backends* :do
	    (when (funcall (backend-check-func b))
	      (return b)))))
    (if (not result)
	*backend-cvs*
	result)))

(defun puca (&key device term-type backend-type)
  (let ((pu (make-puca-app)))
    (with-slots (goo maxima cur top bottom window screen errors) pu
      (init-curses pu device term-type)
      (let ((*backend* (pick-backend backend-type)) c)
	(setf goo nil)
	(draw-screen pu)
	(get-list pu)
	(draw-screen pu)
	(when errors (message "**MESSAGES**"))
	(setf cur 0)
	(do ((quit-flag nil)) (quit-flag)
	  (move (+ (- cur top) 3) 2)
	  (setf c (get-char))
	  (case c
	    ((#\q #\Q)
	     (setf quit-flag t))
	    ((#\? #\h)
	     (help pu))
	    (#\a (do-command pu #'backend-add (selected-files pu)))
	    (#\r (do-command pu #'backend-reset (selected-files pu)
			     :confirm t))
	    (#\d (do-command pu #'backend-diff (selected-files pu)
			     :relist nil :do-pause nil))
	    (#\D (do-command pu #'backend-diff-repo (selected-files pu)
			     :relist nil :do-pause nil))
	    (#\c (do-command pu #'backend-commit (selected-files pu)))
	    (#\u (do-command pu #'backend-update (selected-files pu)))
	    (#\U (do-command pu #'backend-update-all nil))
	    (#\P (do-command pu #'backend-push nil :relist nil))
	    (#\v
	     (pager:pager (selected-files pu))
	     (draw-screen pu))
	    ((:UP (code-char 16) #\^p)
	     (decf cur)
	     (when (< cur 0)
	       (setf cur 0))
	     (when (< cur top)
	       (decf top)
	       (draw-screen pu)))
	    ((:DOWN (code-char 14) #\^n)
	     (incf cur)
	     (when (>= cur maxima)
	       (setf cur (1- maxima)))
	     (when (and (> cur (- bottom 1)) (> bottom 1))
	       (incf top)
	       (draw-screen pu)))
	    ((:NPAGE #\^V #\^F)
	     (setf cur (+ cur 1 (- curses:*lines* 7)))
	     (when (>= cur maxima)
	       (setf cur (1- maxima)))
	     (when (> cur bottom)
	       (setf top (max 0 (- cur (- curses:*lines* 7))))
	       (draw-screen pu)))
	    ((:PPAGE #\^B)
	     (setf cur (- cur 1 (- curses:*lines* 7)))
	     (when (< cur 0)
	       (setf cur 0))
	     (when (< cur top)
	       (setf top cur)
	       (draw-screen pu)))
	    (#\>
	     (setf cur (1- maxima))
	     (when (> maxima bottom)
	       (setf top (max 0 (- maxima (- curses:*lines* 7))))
	       (draw-screen pu)))
	    (#\<
	     (setf cur 0)
	     (when (> top 0)
	       (setf top 0)
	       (draw-screen pu)))
	    ((#\space #\x #\return #\nul)
	     (when goo
	       (setf (goo-selected (elt goo cur))
		     (not (goo-selected (elt goo cur))))
	       (draw-line pu cur)))
	    (#\s			; select all
	     (select-all pu)
	     (draw-screen pu))
	    (#\S			; select none
	     (select-none pu)
	     (draw-screen pu))
	    (#\g			; re-list
	     (clear)
	     (refresh)
	     (get-list pu)
	     (draw-screen pu)
	     (refresh))
	    (#\e
	     (show-extra pu))
	    (#\E
	     (show-errors pu))
	    ((#\^L (code-char 12))	; re-draw
	     (clear)
	     (refresh)
	     (draw-screen pu)
	     (refresh)))
;	  (move (- *lines* 2) 2)
	  (mvprintw (- *lines* 2) 2 "%-*.*s" :int 40 :int 40 :string "")
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
