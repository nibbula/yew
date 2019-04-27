;;
;; pick-hist.lisp - Pick a history item using pick-list.
;;

(defpackage :pick-hist
  (:documentation
   "Pick a history item using pick-list. Meant as an edtior command.")
  (:use :cl :dlib :collections :terminal :inator :rl :pick-list)
  ;; We use a bunch of internal RL stuff.
  (:import-from :rl
		#:use-first-context
		#:with-context
		#:buf
		#:history-context
		#:history-head
		#:get-history
		#:buffer-delete
		#:buffer-insert
		#:point
		)
  (:export
   #:pick-hist
   ))
(in-package :pick-hist)

(defun pick-hist (editor)
  "Pick a line from your history, with a popup list picker. This is meant to
be invoked from inside an editor, most likely RL, which is given as EDITOR,
and most likely invoked from a key binding."
  (use-first-context (editor)
    (with-context ()
      (with-slots ((buf buf)
		   (history-context history-context)) editor
	(let ((cur-y (terminal-get-cursor-position *terminal*))
	      hist pick height start)
	  (omapn (_ (push _ hist)) (history-head
				    (get-history history-context)))
	  (setf start (+ 2 cur-y)
		height (+ 2 (length hist)))
	  (when (> start (truncate (tt-height) 2))
	    (setf height (truncate (tt-height) 2))
	    (tt-move-to (1- (tt-height)) 0)
	    (tt-scroll-down (- (+ start height) (tt-height)))
	    (setf start (1+ height))
	    (tt-finish-output))
	  (when (setf pick (pick-list
			    hist :popup t :typing-searches t
			    :y start :height height
			    :before-hook #'(lambda (p)
					     (move-to-bottom p))))
	    (buffer-delete editor 0 (length buf) point)
	    (buffer-insert editor 0 pick point)
	    (setf point (length buf)))
	  (tt-move-to (- start 2) 0))))))

;; EOF
