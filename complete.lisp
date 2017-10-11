;;
;; complete.lisp
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

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
(defun print-completions-over (e comp-list)
  (with-slots (completion-func buf point saved-point prompt prompt-func) e
    ;; downcased list 1 per line
    (let ((saved-point point))
      (end-of-line e)
      (setf point saved-point))
    (tt-write-char #\newline)
    #| (tt-format "~{~a~%~}" comp-list) |#
    (let ((len (length comp-list)))
      (when (> len *completion-really-limit*)
	(tt-format "Really list ~a things? (y or n) " len)
	(let ((chr (get-a-char e)))
	  (tt-write-char #\newline)
	  (when (not (equalp #\y chr))
	    (return-from print-completions-over))))
      (tt-write-string
       (with-output-to-string (str)
	 (print-columns comp-list :smush t
			:columns (terminal-window-columns
				  (line-editor-terminal e))
			:stream str))))
    (setf (screen-col e) 0)
    (do-prompt e prompt prompt-func)
    (display-buf e)
    (when (< point (length buf))
      (move-over e (- (- (length buf) point))))))

(defun figure-line-endings (e content)
  "Call calculate-line-endings with proper processing of the string CONTENT."
  (calculate-line-endings e :buffer (fat-string-to-string
				     (process-ansi-colors
				      (make-fat-string content)))
			  :start 0))

(defun figure-content-rows (e content)
  "Take a sequence of strings and return how many rows it takes up when output
to the terminal." ;; @@ maybe I mean a string, not a sequence of strings???
  ;; (apply #'+ (map 'list
  ;; 		  (_ (length
  ;; 		      (calculate-line-endings e
  ;; 		       :buffer (fat-string-to-string
  ;; 				(process-ansi-colors
  ;; 				 (make-fat-string _)))
  ;; 		       :start 0)))
  ;; 		  content)))
  (length (figure-line-endings e content)))

(defvar *completion-short-divisor* 3
  "Divisor of your screen height for short completion.")

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
	 rows-scrolled
	 back-adjust
	 (x (screen-col e))
	 (y (screen-row e))
	 end-x end-y
	 row-limit
	 snip-lines
	 line-endings
	 (prefix (or (completion-result-prefix comp-result) ""))
	 (prefix-height (or (and prefix (figure-content-rows e prefix)) 0)))
    (declare (ignorable end-x column-size content-cols))
    ;;(multiple-value-setq (y x) (terminal-get-cursor-position term))
    (multiple-value-setq (content-rows content-cols column-size)
      (print-columns-sizer comp-list :columns cols))

    ;; Move over the rest of the input line.
    (move-over e (- (length (buf e)) (point e)))
    (tt-write-char #\newline)
    (tt-erase-below) (tt-finish-output)
    (setf (did-under-complete e) t)

    (setf row-limit
	  (if (and (< (last-completion-not-unique-count e) 2)
		   (< short-limit rows))
	      short-limit
	      (- (- rows 2)			  ;; minus the "more" line
		 (prompt-height e)
		 (- (screen-row e) (start-row e)) ;; height of the input line
		 prefix-height
		 ))
	  output-string
	  (with-output-to-string (str)
	    (setf content-rows
		  (print-columns comp-list :columns cols
				 :smush t :row-limit row-limit :stream str)))
	  line-endings (figure-line-endings e output-string)
	  real-content-rows (length line-endings)
	  rows-output (min real-content-rows row-limit)
	  snip-lines (max 0 (- real-content-rows row-limit)))
    (tt-write-string prefix)
    ;; Trim output-string to row-limit really for real lines.
    (when (plusp snip-lines)
      (setf output-string
	    (subseq output-string 0 (car (nth snip-lines line-endings)))))
    (tt-write-string output-string)
    ;; (tt-format "content-rows ~a rows-output ~s " content-rows rows-output)
    (if (plusp (- content-rows rows-output))
	(tt-format "[~d more lines]" (- content-rows row-limit))
	(when (plusp snip-lines)
	  (tt-format "~%[~d more lines]" snip-lines)))
    (multiple-value-setq (end-y end-x) (terminal-get-cursor-position term))
    ;;(setf rows-scrolled (max 0 (- (+ y (1+ rows-output)) (1- rows)))
    ;;(setf rows-scrolled (max 0 (- (+ y (1+ real-content-rows)) (1- rows)))
    (setf rows-scrolled (max 0 (- (+ y (- (1+ real-content-rows) snip-lines))
				  (1- rows)))
     	  back-adjust (+ (- end-y y) rows-scrolled))
    (log-message e "row-limit = ~s real-content-rows = ~s"
		 row-limit real-content-rows)
    (log-message e "rows-scrolled = ~s prompt-height = ~s" rows-scrolled
		 (prompt-height e))
    (tt-up back-adjust)
    (tt-beginning-of-line)
    (tt-move-to-col x)
    (setf (screen-row e) (- end-y back-adjust)
	  (screen-col e) x)))

(defun show-completions (e)
  (with-slots (completion-func buf point) e
    (if (not completion-func)
      (beep e "No completion installed.")
      (progn
	(let* ((result
		(funcall completion-func (fat-string-to-string buf) point t))
	       ;;(comp-list (completion-result-completion result))
	       comp-count)
	  (check-type result completion-result)
	  (setf comp-count (completion-result-count result))
	  (dbugf 'completion "result2 = ~a ~s~%" (type-of result) result)
	  (when (and comp-count (> comp-count 0))
	    (setf (did-complete e) t)
	    (set-completion-count e (1+ (last-completion-not-unique-count e)))
	    (if (eq *completion-list-technique* :under)
		;; (print-completions-under e comp-list)
		;; (print-completions-over e comp-list))))))))
		(print-completions-under e result)
		(print-completions-over e result))))))))

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

(defun complete (e &optional comp-func)
  "Call the completion function and display the results, among other things."
  (with-slots (completion-func point buf last-input) e
    (setf comp-func (or comp-func completion-func))
    (when (not comp-func)
      (beep e "No completion active.")
      (return-from complete))
    (let* ((saved-point point)
	   (result
	    (funcall comp-func (fat-string-to-string buf) point nil))
	   (comp (completion-result-completion result))
	   (replace-pos (completion-result-insert-position result))
	   (unique (completion-result-unique result)))
      (dbugf 'completion "result = ~a ~s~%" (type-of result) result)
      (when (and (not (zerop (last-completion-not-unique-count e)))
		 (last-command-was-completion e))
	(log-message e "show mo")
	(show-completions e))
      (setf (did-complete e) t)
      (if (not unique)
	  (set-completion-count e (1+ (last-completion-not-unique-count e)))
	  (set-completion-count e 0))
      ;; (format t "comp = ~s replace-pos = ~s~%" comp replace-pos)
      ;; If the completion succeeded we need a replace-pos!
      (assert (or (not comp) (numberp replace-pos)))
      (if comp
	  (let* ((same (- saved-point replace-pos))) ; same part
	    ;; f o o b a r
	    ;;       ^    ^
	    ;;       |    |___ saved-point
	    ;;       |
	    ;;       +-- replace-pos
	    ;;
	    ;; f o o b a r n a c l e

	    ;; back up
	    (move-over e (- same))

	    ;; delete the different part
	    (delete-region e replace-pos saved-point)
	    (setf point replace-pos)
	    (insert-string e comp)

	    ;; write out the new part
	    (editor-write-string e comp)
	    (incf point (length comp))
	    (update-for-insert e))
	  (progn
	    (setf point saved-point)		   ; go back to where we were
	    (beep e "No completions"))))))	   ; ring the bell

(defun complete-filename-command (e)
  "Filename completion. This useful for when you want to explicitly complete a
filename instead of whatever the default completion is. Convenient for a key
binding."
  (complete e #'completion::complete-filename))

;; EOF
