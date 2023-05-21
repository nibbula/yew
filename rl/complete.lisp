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

;; XXX This should depend on the size of the screen.
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
    (let ((len (completion-result-count comp-result)))
      (when (> len *completion-really-limit*)
	(tt-format "Really list ~a things? (y or n) " len)
	(let ((chr (get-a-char e)))
	  (tt-write-char #\newline)
	  (when (not (equalp #\y chr))
	    (return-from print-completions-over))))
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
  (length (figure-line-endings e content)))

(defvar *completion-short-divisor* 1
  "Divisor of your screen height for short completion.")

(defun print-completions-under (e comp-result)
  (let* ((comp-list (completion-result-completion comp-result))
	 (term (line-editor-terminal e))
	 (cols (terminal-window-columns term))
	 content-rows content-cols column-size
	 output-string
	 end-x
	 (prefix (or (completion-result-prefix comp-result) "")))
    (declare (ignorable end-x column-size content-cols content-rows))
    (multiple-value-setq (content-rows content-cols column-size)
      (print-columns-sizer comp-list :columns cols))

    (setf (did-under-complete e) t)

    (setf output-string
	  (with-output-to-fat-string (str)
	    (setf content-rows
		  (print-columns comp-list :columns cols
				 :smush t ;; :row-limit row-limit
				 :format-char "/fatchar-io:print-string/"
				 :stream str))))
    (message e "~a~a" prefix output-string)
    (setf (keep-message e) t)))

(defun show-completions (e &key func string start-from)
  (with-slots (completion-func buf) e
    (setf func (or func completion-func))
    (if (not func)
      (beep e "No completion installed.")
      (progn
	(let* ((str (or string (fatchar-string-to-string buf)))
	       (result
		(funcall func (if (and start-from (plusp start-from))
				  (subseq str start-from (first-point e))
				  str)
			 (first-point e) t))
	       comp-count)
	  (if (not (typep result 'completion-result))
	      (progn
		;; It's really annoying to get error here.
		(beep e "Completion function returned NIL!"))
	      (progn
		(setf comp-count (completion-result-count result))
		(dbugf :completion "result2 = ~a ~s~%" (type-of result) result)
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
	     (comp)
	     (replace-pos)
	     (unique))
	(when (or (null result) (not (completion-result-p result)))
	  (message e "Completion function ~a returned a bogus result: ~s ~s."
		   function (type-of result) result)
	  (return-from complete nil))
	(setf comp (completion-result-completion result)
	      replace-pos (completion-result-insert-position result)
	      unique (completion-result-unique result))

	(when (and (not (zerop (last-completion-not-unique-count e)))
		   (last-command-was-completion e))
	  (log-message e "show mo")
	  (show-completions e :func function :start-from start-from))
	(setf (did-complete e) t)
	(if (not unique)
	    (set-completion-count e (1+ (last-completion-not-unique-count e)))
	    (set-completion-count e 0))
	;; If the completion succeeded we need a replace-pos!
	(assert (or (not comp) (numberp replace-pos)))
	(if comp
	    (progn
	      ;; delete the different part
	      (delete-region e (+ start-from replace-pos) saved-point)
	      (setf (first-point e) (+ start-from replace-pos))
	      (setf comp (make-fat-string :string comp))
	      (insert e comp)
	      (incf (first-point e) (olength comp)))
	    (progn
	      (setf (first-point e) saved-point) ; Go back to where we were
	      (when (not no-beep)
		(beep e "No completions")))) ; Ring the bell
	result))))

;; End
