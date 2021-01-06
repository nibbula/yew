;;;
;;; complete.lisp
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defvar *completion-list-technique* :under
  "Technique for dispalying completion lists.
:UNDER - Display the the completions under the command line.
:OVER  - Display the completion list like normal output scrolled up over the
command line.")

; XXX This should depend on the size of the screen.
(defvar *completion-really-limit* 100
  "How much is too much.")

(defun set-completion-count (e n)
  "Set the completion count to N."
  (setf *completion-count* n
	(last-completion-not-unique-count e) n))

;; Most of the work is done by print-columns, from dlib-misc.
(defun print-completions-over (e comp-result)
  (with-slots (completion-func buf saved-point prompt-string prompt-func) e
    ;; @@@ I'm not even sure if this works anymore.
    ;; (let ((saved-point point))
    ;;   (end-of-line e)
    ;;   (setf point saved-point))
    (tt-write-char #\newline)
    ;; downcased list 1 per line
    #| (tt-format "~{~a~%~}" comp-list) |#
    (let ((len (completion-result-count comp-result)))
      (when (> len *completion-really-limit*)
	(tt-format "Really list ~a things? (y or n) " len)
	(let ((chr (get-a-char e)))
	  (tt-write-char #\newline)
	  (when (not (equalp #\y chr))
	    (return-from print-completions-over))))
      ;; (tt-write-string
      ;;  (with-output-to-string (str)
      ;; 	 (print-columns (completion-result-completion comp-result)
      ;; 			:smush t
      ;; 			:columns (terminal-window-columns
      ;; 				  (line-editor-terminal e))
      ;; 			:stream str))))
      (princ
       (with-output-to-fat-string (str)
	 (print-columns (completion-result-completion comp-result)
			:smush t
			:columns (terminal-window-columns
				  (line-editor-terminal e))
			:stream str))
       *terminal*))))

;; @@@ Couldn't we just call editor-calculate-line-endings now??
(defun figure-line-endings (e content)
  "Call calculate-line-endings with proper processing of the string CONTENT."
  (editor-calculate-line-endings e :buffer (fat-string-to-string content)
				 :start-column 0))

(defun figure-content-rows (e content)
  "Take a string and return how many rows it takes up when output to the
terminal."
  ;; (apply #'+ (map 'list
  ;; 		  (_ (length
  ;; 		      (editor-calculate-line-endings e
  ;; 		       :buffer (fatchar-string-to-string
  ;; 				(process-ansi-colors
  ;; 				 (make-fatchar-string _)))
  ;; 		       :start-column 0)))
  ;; 		  content)))
  (length (figure-line-endings e content)))

(defvar *completion-short-divisor* 1
  "Divisor of your screen height for short completion.")

#|
(defun print-completions-under (e comp-result)
  (let* ((comp-list (completion-result-completion comp-result))
	 (term (line-editor-terminal e))
	 (rows (terminal-window-rows term))
	 (cols (terminal-window-columns term))
	 (short-limit (truncate rows *completion-short-divisor*))
	 content-rows content-cols column-size
	 output-string
	 real-content-rows
	 rows-output
	 back-adjust
	 (x (screen-col e))
	 (y (screen-relative-row e))
	 end-x
	 row-limit
	 snip-lines
	 line-endings
	 (prefix (or (completion-result-prefix comp-result) ""))
	 (prefix-height (or (and prefix (figure-content-rows e prefix)) 0)))
    (declare (ignorable end-x column-size content-cols))
    (multiple-value-setq (content-rows content-cols column-size)
      (print-columns-sizer comp-list :columns cols))

    ;; Move over the rest of the input line.
    ;;(move-over e (- (length (buf e)) (point e)))
    (tt-write-char #\newline)
    (tt-erase-below)
    (setf back-adjust (- (screen-relative-row e) y))
    (setf (did-under-complete e) t)

    (setf row-limit
	  (if (and (< (last-completion-not-unique-count e) 2)
		   (< short-limit rows))
	      short-limit
	      (- (- rows 1) ;; minus the "more" line
		 (1+ (prompt-height e))
		 (- (screen-relative-row e) (start-row e)) ;; input line height
		 prefix-height
		 ))
	  output-string
	  (with-output-to-fat-string (str)
	    (setf content-rows
		  (print-columns comp-list :columns cols
				 :smush t :row-limit row-limit
				 :format-char "/fatchar-io:print-string/"
				 :stream str)))
	  line-endings (figure-line-endings e output-string)
	  real-content-rows (length line-endings)
	  rows-output (min real-content-rows row-limit)
	  snip-lines (max 0 (- real-content-rows row-limit)))
    (tt-write-string prefix)
    ;; Trim output-string to row-limit really for real lines.
    (when (plusp snip-lines)
      (setf output-string
	    (osubseq output-string 0 (car (nth snip-lines line-endings)))))
    (tt-write-string output-string)
    (if (plusp (- content-rows rows-output))
	(tt-format "[~d more lines]" (- content-rows row-limit))
	(when (plusp snip-lines)
	  (tt-format "~%[~d more lines]" snip-lines)))
    (incf back-adjust (- (1+ real-content-rows) snip-lines))
    ;; Go back to where we should be?
    (tt-up back-adjust)
    (tt-beginning-of-line)
    (tt-move-to-col x)
    (setf (screen-relative-row e) y
	  (screen-col e) x)))
|#

(defun print-completions-under (e comp-result)
  (let* ((comp-list (completion-result-completion comp-result))
	 (term (line-editor-terminal e))
	 #| (rows (terminal-window-rows term)) |#
	 (cols (terminal-window-columns term))
	 ;; (top (message-top e))
	 #| (short-limit (truncate rows *completion-short-divisor*)) |#
	 content-rows content-cols column-size
	 output-string
	 ;; real-content-rows
	 ;; rows-output
	 ;; back-adjust
	 ;; (x (screen-col e))
	 ;; (y (screen-relative-row e))
	 end-x
	 #| row-limit |#
	 ;; snip-lines
	 ;; line-endings
	 (prefix (or (completion-result-prefix comp-result) ""))
	 ;; (prefix-height (or (and prefix (figure-content-rows e prefix)) 0))
	 )
    (declare (ignorable end-x column-size content-cols))
    (multiple-value-setq (content-rows content-cols column-size)
      (print-columns-sizer comp-list :columns cols))

    ;; Move over the rest of the input line.
    ;;(move-over e (- (length (buf e)) (point e)))
    ;; (tt-write-char #\newline)
    ;; (tt-erase-below)
    ;; (setf back-adjust (- (screen-relative-row e) y))
    (setf (did-under-complete e) t)

    (setf #| row-limit
	  (if (and (< (last-completion-not-unique-count e) 2)
		   (< short-limit rows))
	      short-limit
	      #|
	      (- (- rows 1) ;; minus the "more" line
		 (1+ (prompt-height e))
		 (- (screen-relative-row e) (start-row e)) ;; input line height
		 prefix-height) |#
	      (max-message-lines e)
	      ) |#
	  output-string
	  (with-output-to-fat-string (str)
	    (setf content-rows
		  (print-columns comp-list :columns cols
				 :smush t ;; :row-limit row-limit
				 :format-char "/fatchar-io:print-string/"
				 :stream str)))
	  ;; line-endings (figure-line-endings e output-string)
	  ;; real-content-rows (length line-endings)
	  ;; rows-output (min (- real-content-rows top) row-limit)
	  ;; snip-lines (max 0 (- (- real-content-rows top) row-limit))
	  )
    ;;z (tt-write-string prefix)
    ;; Trim output-string to row-limit really for real lines.
    ;; (dbugf :rlp "prefix ~s line-endings = ~s~%" prefix line-endings)
    #|
    (let (start end)
      (when (and (plusp top) line-endings)
	(setf start (car (nth top line-endings))))
      (when (and (plusp snip-lines) line-endings)
	(setf end  (car (nth snip-lines line-endings))))
      (when (or start end)
	(setf output-string
	      (osubseq output-string (or start 0)
		       (or end (length output-string))))))
    |#

    ;; (tt-write-string output-string)
    ;; (if (plusp (- content-rows rows-output))
    ;; 	(tt-format "[~d more lines]" (- content-rows row-limit))
    ;; 	(when (plusp snip-lines)
    ;; 	  (tt-format "~%[~d more lines]" snip-lines)))
    ;; (incf back-adjust (- (1+ real-content-rows) snip-lines))
    ;; Go back to where we should be?
    ;; (tt-up back-adjust)
    ;; (tt-beginning-of-line)
    ;; (tt-move-to-col x)
    ;; (setf (screen-relative-row e) y
    ;; 	  (screen-col e) x)
    (message e "~a~a"
	     prefix
	     output-string
	     ;; (cond
	     ;;   ((plusp (- content-rows rows-output))
	     ;; 	(format nil "[~d more lines]" (- content-rows row-limit)))
	     ;;   ((plusp snip-lines)
	     ;; 	(format nil "~%[~d more lines]" snip-lines))
	     ;;   (t ""))
	     )
    (setf (keep-message e) t)
    ))

(defun show-completions (e &key func string)
  (with-slots (completion-func buf) e
    (setf func (or func completion-func))
    (if (not func)
      (beep e "No completion installed.")
      (progn
	(let* ((result
		(funcall func (or string (fatchar-string-to-string buf))
			 (first-point e) t))
	       comp-count)
	  (if (not (typep result 'completion-result))
	      (progn
		;; It's really annoying to get error here.
		(beep e "Completion function returned NIL!"))
	      (progn
		(setf comp-count (completion-result-count result))
		(dbugf 'completion "result2 = ~a ~s~%" (type-of result) result)
		(when (and comp-count (> comp-count 0))
		  (setf (did-complete e) t)
		  (set-completion-count
		   e (1+ (last-completion-not-unique-count e)))
		  (if (eq *completion-list-technique* :under)
		      (print-completions-under e result)
		      (print-completions-over e result))))))))))

#|
(defun last-input-was-completion (e)
  "Return true if the last input invoked a completion function."
  (with-slots (last-input completion-func) e
    (let ((func (key-sequence-binding last-input *normal-keymap*)))
      (log-message e "func = ~s" func)
      ;; @@@ This is not really ideal. Perhaps we should specify that completion
      ;; functions should set another flag, or that last last-completion-unique
      ;; also specifies that last-input-was-completion?
      (or (eql func completion-func)
	  (eql func #'complete)
	  (eql func 'complete)
	  (eql func #'complete-filename-command)
	  (eql func 'complete-filename-command)))))
|#

(defun complete (e &key function (start-from 0) no-beep)
  "Call the completion function and display the results, among other things.
Return the completion result, or NIL if there wasn't one."
  (with-slots (completion-func buf) e
    (setf function (or function completion-func))
    (when (not function)
      (when (not no-beep)
	(beep e "No completion active."))
      (return-from complete nil))
    (use-first-context (e)
      (assert (and (numberp start-from) (<= start-from (first-point e))))
      (let* ((saved-point (first-point e))
	     (str (fatchar-string-to-string buf))
	     (result
	      (funcall function (if (plusp start-from)
				    (subseq str start-from (first-point e))
				    str)
		       (first-point e) nil))
	     (comp (completion-result-completion result))
	     (replace-pos (completion-result-insert-position result))
	     (unique (completion-result-unique result)))
	(dbugf 'completion "result = ~a ~s~%" (type-of result) result)
	(when (and (not (zerop (last-completion-not-unique-count e)))
		   (last-command-was-completion e))
	  (log-message e "show mo")
	  (show-completions e :func function))
	(setf (did-complete e) t)
	(if (not unique)
	    (set-completion-count e (1+ (last-completion-not-unique-count e)))
	    (set-completion-count e 0))
	;; (format t "comp = ~s replace-pos = ~s~%" comp replace-pos)
	;; If the completion succeeded we need a replace-pos!
	(assert (or (not comp) (numberp replace-pos)))
	(if comp
	    (progn
	      ;; delete the different part
	      (delete-region e (+ start-from replace-pos) saved-point)
	      (setf (first-point e) (+ start-from replace-pos))
	      (insert e comp)
	      (incf (first-point e) (length comp)))
	    (progn
	      (setf (first-point e) saved-point) ; Go back to where we were
	      (when (not no-beep)
		(beep e "No completions")))) ; Ring the bell
	result))))

;; EOF
