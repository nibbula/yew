;;;
;;; pick-hist.lisp - Pick a history item using pick-list.
;;;

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
   #:pick-hist-value
   ))
(in-package :pick-hist)

(defun %pick-history (editor &key generate-function result-function)
  "The real history picker which calls GENERATE-FUNCTION on the history entry to
get what to pick, and calls RESULT-FUNCTION with the picked index to get what to
insert into the buffer. If GENERATE-FUNCTION isn't given, it defaults to
‘history-entry-line’. RESULT-FUNCTION defaults to returning the picked element
of the generated list."
  (use-first-context (editor)
    (with-context ()
      (with-slots ((buf buf)
		   (history-context history-context)) editor
	(let ((cur-y (terminal-get-cursor-position *terminal*))
	      hist pick height start)

	  ;; Set default functions.
	  (when (not generate-function)
	    (setf generate-function #'history-entry-line))

	  (when (not result-function)
	    (setf result-function (_ (oelt hist _))))

	  ;; Generate the list.
	  (omapn (_ (let ((result (funcall generate-function _)))
		      (when result (push result hist))))
		 (history-head (get-history history-context)))
	  ;; (setf hist (nreverse hist))

	  ;; Scroll up half the screen if we have no room.
	  (setf start (+ 2 cur-y)
		height (+ 2 (length hist)))
	  (when (> start (truncate (tt-height) 2))
	    (setf height (truncate (tt-height) 2))
	    (tt-move-to (1- (tt-height)) 0)
	    (tt-scroll-down (- (+ start height) (tt-height)))
	    (setf start (1+ height))
	    (tt-finish-output))

	  ;; Pick it and insert it.
	  (when (setf pick (pick-list
			    hist :popup t :typing-searches t
			    :by-index t
			    :y start :height height
			    :before-hook #'(lambda (p)
					     (move-to-bottom p))))
	    (buffer-delete editor 0 (length buf) point)
	    (buffer-insert editor 0 (funcall result-function pick) point)
	    (setf point (length buf)))

	  ;; Go back to where we were.
	  (tt-move-to (- start 2) 0))))))

(defun pick-hist (editor)
  "Pick a line from your history, with a popup list picker. This is meant to
be invoked from inside an editor, most likely RL, which is given as EDITOR,
and most likely invoked from a key binding."
  (%pick-history editor))

(defun pick-hist-value (editor)
  "Pick a previous result from your history, with a popup list picker. This is
meant to be invoked from inside an editor, most likely RL, which is given as
EDITOR, and most likely invoked from a key binding."
  (let ((i 0) index-list)
    (%pick-history editor
		   :generate-function
		   (_
		    (let ((extra (history-entry-extra _))
			  result)
		      (when (and extra (listp extra))
			(let ((vals (getf (history-entry-extra _) :values)))
			  (setf result
				(if (= (length vals) 1)
				    (car vals)
				    vals))))
		      (incf i)
		      (when result
			(push i index-list)
			(prin1-to-string result))))
		   :result-function
		   (_ (s+ "(!v "
			  (oelt (setf index-list (nreverse index-list)) _)
			  ")")))
    (message editor "i ~s length index-list ~s" i (length index-list))))

;; EOF
