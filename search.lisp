;;;
;;; search.lisp - Search commands
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

(defsingle isearch-backward (e)
  "Incremental search backward."
  (isearch e :backward))

(defsingle-method search-command ((e line-editor))
  (isearch-backward e))

(defsingle isearch-forward (e)
  "Incremental search forward."
  (isearch e :forward))

(defun display-search (e str start end prompt)
  "Display the current line with the search string highlighted."
  (with-slots ((contexts inator::contexts)
	       buf history-context temporary-message) e
    (setf temporary-message
	  (s+ prompt str))
      (if (and start (history-current history-context))
	  (save-excursion (e)
            ;; (setf point start
	    ;; 	  mark end		; for the highlightify
	    ;; 	  buf (highlightify
	    ;; 	       e (make-fatchar-string (history-current history-context))
	    ;; 	       :style
	    ;; 	       (or (theme-value *theme* '(:program :search-match :style))
	    ;; 		   (theme-value *theme* '(:rl :search-match :style))
	    ;; 		   '(:underline)))
	    ;; 	  mark nil)		; for the redraw-display
	    (setf contexts (make-contexts)
		  (inator-point (aref contexts 0)) start
		  (inator-mark (aref contexts 0)) end
		  buf (highlightify
		       e (make-fatchar-string (history-current history-context))
		       :style
		       (or (theme-value *theme* '(:program :search-match :style))
			   (theme-value *theme* '(:rl :search-match :style))
			   '(:underline))))
	    (redraw-display e))
	  (redraw-display e))))

(defun search-start-forward (history-context)
  (or (history-current-get history-context)
      (history-head (get-history history-context))))

(defun search-start-backward (history-context)
  (or (history-current-get history-context)
      (history-tail (get-history history-context))))

(defun backward-start-pos (str pos)
  (min (length str)
       (or pos (length str))))

(defun forward-start-pos (str pos)
  (cond
    ((not pos)
     0)
    ((< pos (1- (length str)))
     (1+ pos))
    (t (length str))))

(defun test-for-string (string)
  "Return the test function we should use for STRING. This 'smart' case folding,
where using any upper case triggers exact matching, and all lower case means,
be case insensitive."
  (if (some #'upper-case-p string) #'char= #'char-equal))

(defun search-history (e str direction start-from search-pos)
  (with-slots (history-context) e
    (let ((hist (get-history history-context))
	  (first-time t))
	;; (dbug "yoyo history-context ~w ~w~%" histroy-context hist)
      (if (eq direction :backward)
	  (progn
	    ;; (dbug "starting-at ~w~%" start-from)
	    (dl-list-do-element
	     start-from
	     #'(lambda (x)
		 (when (dl-content x)
		   ;; (dbug "(search ~w ~w :end2 ~w) search-pos = ~w~%"
		   ;; 	 str (dl-content x)
		   ;; 	 (backward-start-pos (dl-content x) search-pos)
		   ;; 	 search-pos)
		   (let (pos)
		     (if first-time
			 (setf pos (search str (history-line x)
					   :from-end t
					   :end2 (backward-start-pos
						  (history-line x) search-pos)
					   :test (test-for-string str)
					   )
			       first-time nil)
			 (setf pos (search str (history-line x) :from-end t
					   :test (test-for-string str))))
		     (when pos
		       ;; (dbug "found pos = ~w in ~w (~w) x=~a~%"
		       ;; 	     pos (dl-content x) str x)
		       (setf (history-cur hist) x)
		       (return-from search-history pos)))))))
	  (dl-list-do-backward-element
	   start-from
	   #'(lambda (x)
	       (when (dl-content x)
		 (let (pos)
		   (if first-time
		       (setf pos (search str (history-line x)
					 :start2 (forward-start-pos
						  (history-line x) search-pos)
					 :test (test-for-string str))
			     first-time nil)
		       (setf pos (search str (history-line x)
					 :test (test-for-string str))))
		   (when pos
		     (setf (history-cur hist) x)
		     (return-from search-history pos)))))))))
  nil)

(defparameter *isearch-prompt* "~:[~;failed ~]~:[>~;<~] i-search: ")

(defun isearch (e &optional (direction :backward))
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (with-slots ((contexts inator::contexts)
	       buf command history-context temporary-message last-search) e
    (with-slots ((point inator::point)) (aref contexts 0)
      (let ((quit-now nil)
	    (start-point point)
	    (start-hist (history-current-get history-context))
	    (search-string (make-stretchy-string *initial-line-size*))
	    (start-from (or (history-current-get history-context)
			    (history-head (get-history history-context))))
	    (pos point) end c #| added |# failed)
	(labels ((redisp ()
		   (display-search e search-string point end
				   (format nil *isearch-prompt*
					   failed (eq direction :backward))))
		 (resync ()
		   (setf point 0)
		   (buffer-delete e 0 (length buf) point)
		   (buffer-insert e 0 (or (history-current
					   (history-context e)) "")
				  point)
		   (setf point (min (or pos (length buf)) (length buf))
			 temporary-message nil
			 last-search search-string)
		   (dbugf :rl "CHOW ~s ~s ~s ~a~%"
			  pos (length buf) point contexts)
		   (redraw-display e)
		   ))
	  (loop :while (not quit-now)
	     :do
	     ;; (when (debugging e)
	     ;;   (debug-message e "pos = ~a start-from = ~a" pos start-from))
	     (redisp)
	     (tt-finish-output)
	     (setf c (get-a-char e))
	     ;; (setf added nil)
	     (cond
	       ((eql c (ctrl #\G))
		(setf point start-point)
		(setf (history-current history-context) start-hist)
		(use-hist e)
		(setf quit-now t))
	       ((eql c (ctrl #\S))
		(when (and (zerop (length search-string)) last-search)
		  (stretchy-append search-string last-search))
		(setf direction :forward
		      start-from (search-start-forward history-context)))
	       ((eql c (ctrl #\R))
		(when (and (zerop (length search-string)) last-search)
		  (stretchy-append search-string last-search))
		(setf direction :backward
		      start-from (search-start-backward history-context)))
	       ((eql c (ctrl #\L))
		(redisp))
	       ((or (eql c (ctrl #\h)) (eql c #\backspace) (eql c #\rubout))
		(stretchy-truncate search-string
				   (max 0 (1- (length search-string)))))
	       ((or (and (characterp c)
			 (or (control-char-p c) (meta-char-p (char-code c))))
		    (not (characterp c)))
		(resync)
		(return-from isearch c))
	       (t
		(stretchy-append search-string c)
		;; (setf added t)
		))
	     (if (setf pos (search-history
			    e search-string direction start-from pos))
		 (progn
		   (setf end (+ pos (length search-string))
			 point pos
			 failed nil))
		 (progn
		   (setf failed t)
		   ;; (when added
		   ;;   (stretchy-truncate search-string
		   ;; 		      (max 0 (1- (length search-string))))
		   ;;   (setf pos old-pos))
		   ;; (beep e "Not found")
		   )))
	  (resync))))))

;; End
