;;
;; completion.lisp - Pull endings out of somewhere.
;;

;; TODO:
;;  - limit to window
;;  - scrolling?
;;  - finish dictionary completion
;;  - more languageishnessification

(defpackage :completion
  (:documentation
   ;; @@@ Improve this docstring
"Completion functions are called with line of context and a position where
completion was requested. The are asked to return one completion, or all
completions. When asked for one completion, they return a completion and a
position where to insert it. Text should be replaced from the starting and
result position. This allows completion functions to look at whatever they
want to, and replace whatever they want to, even prior to the starting
point. When asked for all completions, they return a sequence of strings and
a count which is the length of the sequence.")
  (:use :cl :dlib :opsys :glob :char-util :dlib-misc :reader-ext
	:syntax :syntax-lisp :terminal :terminal-ansi
	#+use-regex :regex
	#-use-regex :cl-ppcre
	:theme :fatchar :style)
  (:export
   ;; generic
   #:complete-print
   #:complete-print-long
   #:*completion-count*
   #:make-completion-result
   #:completion-result-p
   #:completion-result
   #:completion-result-completion
   #:completion-result-insert-position
   #:completion-result-unique
   #:completion-result-count
   #:completion-result-prefix
   ;; list
   #:list-completion-function 
   #:string-completion-list
   #:complete-list
   #:*completion-list*
   ;; iter
   #:iterator-completion-function
   #:iterator-completion-list
   #:iterator-completion
   #:complete-iterator
   ;; symbol
   #:complete-symbol
   #:function-help
   ;; filenames
   #:complete-filename
   #:complete-directory
   ))
(in-package :completion)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defstruct completion-result
  "The result of completion functions."
  completion
  (insert-position 0)
  unique
  (count 0)		; just so we don't have to recount long lists
  prefix)		; Part to output before a list.

(defgeneric complete-print (object stream)
  (:documentation "Print the object for showing in a completion list.")
  (:method ((object t) stream) (princ object stream)))

(defgeneric complete-print-long (object stream)
  (:documentation "Print a long, perhaps more descriptive, version of the object
for showing in a completion list.")
  (:method ((object t) stream) (princ object stream)))

(defvar *completion-count* 0
  "Count of how many completions in a row were done. This must be maintained
by callers of this package and should usually be dynamically bound.")

;; @@@ Duplication from RL
(defvar *lisp-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\" #\' #\\ #\# #\, #\` #\|
    #| #\. |# ;; <-- This depends on the context?
    )
  "Characters that are not considered to be part of a word in lisp.")
;; removed #\/ since it's common in package names

;; @@@ Perhaps this should be merged with the one in RL?
(defun scan-over-str (str pos direction &key func not-in)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIRECTION is :forward or :backward. Moves over characters in STR starting
at POS. Returns the new position after moving."
  (when (and (not func) not-in)
    (setf func #'(lambda (c) (not (position c not-in)))))
  (if (eql direction :backward)
      ;; backward
      (loop :while (and (> pos 0)
			(funcall func (aref str (1- pos))))
	 :do (decf pos))
      ;; forward
      (let ((len (length str)))
	(loop :while (and (< pos len)
			  ;; Why was this 1+ here???
			  ;; (funcall func (aref str (1+ pos))))
			  (funcall func (aref str pos)))
	   :do (incf pos))))
  pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List completion
;;
;; There's two ways to use list completion:
;;  1. Compose a completion function with your list and then use that.
;;  2. Bind *completion-list* before calling.
;;
;; This is mostly useful for small things. For big things like a filessytem or
;; database, that you don't want to make a copy of, use iterator completion.

(defun stringify (obj)
  "Return a string representation of OBJ."
  (typecase obj
    (string obj)
    (t (princ-to-string obj))))

(defun string-list (list)
  "Return a new list of strings from a list of random objects. If there are
strings in the list already, use them. If everything in the list is a string
already, just return the list."
  (if (every #'stringp list)
      list
      (loop :for i :in list :collect (stringify i))))

(defun list-completion-function (list)
  "Return a completion function for a list."
  ;; First convert the list contents into strings.
  (let ((new-list (string-list list)))
    (lambda (context pos all)
      (complete-list context pos all new-list))))

(defun string-completion-list (word list)
  "Return all the words from LIST that have the prefix WORD. Second value is
the count of matches."
  (let ((i 0) pos)
    (make-completion-result
     :completion
     (loop :for w :in list
	:if (or (not (setq pos (mismatch (stringify w) word)))
		(>= pos (length word)))
	  :collect (prog1 w (incf i))
	:end)
     :count i)))

(defun string-completion (word list)
  "Return the longest possible unambiguous completion of WORD from LIST. Second
value is true if it's unique."
  (let ((match nil) (unique t) (match-len 0) pos)
    (loop :with w
       :for obj :in list :do
       (setf w (stringify obj))
       (if (setq pos (mismatch w (or match word)))
	   (when (and (/= 0 pos) (>= pos (length word)))
	     (if (not match)
		 (progn
		   (setf match w)
		   (setf match-len (length w)))
		 (progn
		   ;; shorter match, but no shorter the prefix
		   (if (> match-len pos)
		       (setf match w
			     match-len pos)
		       (setf unique nil)))))
	   (progn
	     (when match
	       (setf unique nil))
	     (setf match w
		   match-len (length w)))))
    (make-completion-result
     :completion (subseq match 0 match-len)
     :unique unique)))

(defvar *completion-list* nil
  "List to use for completion.")
		   
(defun complete-list (context pos all &optional (list *completion-list*))
  "Completion function for lists. The entire context is matched."
;  (let* ((word-start (scan-over-str context pos :backward
;				    :not-in *lisp-non-word-chars*))
  (let* ((word-start 0)
	 (word (subseq context word-start pos)))
    (if all
	(string-completion-list word list)
	(let ((result (string-completion word list)))
	  (setf (completion-result-insert-position result) word-start)
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterator completion
;;
;; This provides a generic way of doing completion dynamically, without having
;; to have a premade list or make a copy the completion data. This is good
;; when the data set is big or not known beforehand, but can be cycled through
;; relatively quickly.
;;
;; For these purposes an iterator is a function that when called with a true
;; argument returns the first item in a sequence, then when subsequently called
;; with a false or not providied arguemnt, returns the subsequent items in a
;; sequence. Upon reaching the end it returns false.
;;
;; Note that this means the sequence can't contain NIL.

(defun stringish (obj)
  "Return a string representation of OBJ, except if it's NIL."
  (typecase obj
    (string obj)
    (null obj)
    (t (princ-to-string obj))))

(defun iterator-completion-function (iterator)
  "Return a completion function for an iterator."
  ;; First convert the list contents into strings.
  (lambda (context pos all)
    (complete-iterator context pos all iterator)))

(defun iterator-completion-list (word iterator)
  "Return all the words from ITERATOR that have the prefix WORD. Second value is
the count of matches."
  (let ((i 0) pos)
    (make-completion-result
     :completion
     (loop :with w = (stringish (funcall iterator t))
	:if (or (not (setq pos (mismatch w word)))
		(>= pos (length word)))
	  :collect (prog1 w (incf i))
	:end
	:while (setq w (stringish (funcall iterator))))
     :count i)))

(defun iterator-completion (word iterator)
  "Return the longest possible unambiguous completion of WORD from the
ITERATOR. Second value is true if it's unique."
  (let ((match nil) (unique t) (match-len 0) pos)
    (loop :with w = (stringish (funcall iterator t))
       :do
       (if (setq pos (mismatch w (or match word)))
	   (when (and (/= 0 pos) (>= pos (length word)))
	     (if (not match)
		 (progn
		   (setf match w)
		   (setf match-len (length w)))
		 (progn
		   ;; shorter match, but no shorter the prefix
		   (if (> match-len pos)
		       (setf match w
			     match-len pos)
		       (setf unique nil)))))
	   (progn
	     (when match
	       (setf unique nil))
	     (setf match w
		   match-len (length w))))
       :while (stringish (setq w (funcall iterator))))
    (make-completion-result
     :completion (subseq match 0 match-len)
     :unique unique)))

(defun complete-iterator (context pos all iterator)
  "Completion function for iterators. The entire context is matched."
;  (let* ((word-start (scan-over-str context pos :backward
;				    :not-in *lisp-non-word-chars*))
  (let* ((word-start 0)
	 (word (subseq context word-start pos)))
    (if all
	(iterator-completion-list word iterator)
	(let ((result (iterator-completion word iterator)))
	  (setf (completion-result-insert-position result) word-start)
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol completion

;; @@@ I really want these hacks to go away in favor of real things in
;; syntax-lisp.
(defun find-back-pack (context pos)
  "Check for colons and a package name just prior to the point POS in CONTEXT ~
and return the package and whether we should look for only external symbols."
  (when (and (> pos 0) (eql (aref context (1- pos)) #\:))
    (decf pos)
    (let ((external t))
      (when (and (> pos 0) (eql (aref context (1- pos)) #\:))
	(decf pos)
	(setf external nil))
      (let ((p-p pos))
	(setf pos (scan-over-str context pos
				 :backward :not-in *lisp-non-word-chars*))
	(values
	 (find-package (string-upcase (subseq context pos p-p)))
	 external)))))

(defun find-forward-pack (context pos)
  (let ((end (scan-over-str context pos :forward
			    :not-in *lisp-non-word-chars*)))
    (if (and end (<= end (length context)) (char= (char context end) #\:))
	(find-package (string-upcase (subseq context pos end)))
	nil)))

#|
(defun try-symbol-help (context pos)
  "If POS is right before a function in CONTEXT, return a string that shows the
arguments for that function, otherwise return NIL."
  (let* ((trimmed (rtrim (subseq context 0 pos)))
	 (word-start (scan-over-str trimmed (min pos (length trimmed))
				    :backward
				    :not-in *lisp-non-word-chars*))
	 (pack (find-back-pack trimmed (length trimmed)))
	 (sym (and word-start
		   (symbolify (subseq trimmed word-start)
			      (or pack *package*)))))
    (if (fboundp sym)
	(format nil "~((~a~{ ~a~})~)"
		sym (lambda-list (symbol-function sym)))
	nil)))
|#

(defparameter *lambda-list-keywords*
  #(&allow-other-keys &aux &key &optional &rest &body &environment &whole))

;;
;; “the necessary flexibility is provided for individual programs
;;   to achieve an arbitrary degree of aesthetic control.”
;;
;; Of course this whole thing should be in the syntax colorization module.

(defun function-help-show-function (symbol expr-number)
  (let (past-key past-rest #|standout|# (i 0) result)
    (declare (special i))
    (push #\( result)
    (push (list :magenta (format nil "~(~a~)" symbol)) result)
    (labels ((output-atom (a)
	       (typecase a
		 ((or null keyword number string boolean array)
		  (push (list :white (format nil "~(~s~)" a)) result))
		 (t
		  (push (format nil "~(~a~)" a) result))))
	     (print-list-interior (list)
	       (let ((i 0))
		 (declare (special i))
		 (loop :for item :on list :do
		    (print-it (car item))
		    (when (and (consp item) (cdr item) (atom (cdr item)))
		      (push " . " result)
		      (output-atom (cdr item))))))
	     (print-it (s)
	       (when (not (zerop i))
		 (push #\space result))
	       (cond
		 ((atom s)
		  (cond
		    ((position s *lambda-list-keywords*)
		     (push `(:yellow ,(format nil "~(~a~)" s)) result)
		     (when (equal s '&key)
		       (setf past-key t))
		     (when (equal s '&rest)
		       (setf past-rest t)))
		    (t
		     (if (and expr-number
			      (= i expr-number) (not (or past-key past-rest)))

			 (push `(:standout ,(format nil "~(~a~)" s)) result)
			 (push (format nil "~(~a~)" s) result)))))
		 ;; @@@ Despite the above quote, pretty is very ugly here.
		 ((and s (listp s) (cdr s) (not (atom (cdr s))))
		  (push #\( result)
		  (print-list-interior s)
		  (push #\) result))
		 ((consp s) ;; a cons but not a list
		  (push #\( result)
		  (output-atom (car s))
		  (push " " result)
		  (output-atom (cdr s))
		  (push #\) result)))
	       (incf i)))
      (push #\space result)
      ;;(print-it (dlib:lambda-list symbol))
      (print-list-interior (dlib:lambda-list symbol))
      (push #\) result)
      (nreverse result))))

(defun function-help-show-doc (symbol cols)
  (let (result)
    (labels ((maybe-doc (obj type)
	       (without-warning (documentation obj type)))
	     (print-doc (sym pkg doc-type &optional fake-type)
	       (when (maybe-doc sym doc-type)
		 ;; (when did-one
		 (push #\newline result)
		 (if (and (eq doc-type 'function) (maybe-doc sym doc-type))
		     (progn
		       (when (and pkg (not (eq pkg (find-package :cl))))
			 (push `(:green ,(format nil "~a " (package-name pkg))
					,(format nil "~:(~a~):~%"
						 (or fake-type doc-type)))
			       result))
		       ;;(tt-format "~a~%" (function-help sym 0))
		       )
		     (progn
		       (push `(:green
			       ,(format nil "~:(~a~): " (or fake-type doc-type))
			       ,@(when pkg
				   (list (format nil "~a:" (package-name pkg))))
			       ,(format nil "~a~%" sym))
			     result)))
		 (if (eq doc-type :command)
		     (progn
		       (push `(:white
			       ,(format nil "~a" (maybe-doc sym doc-type)))
			     result))
		     (progn
		       (push (fatchar-io:with-output-to-fat-string (stream)
			       (format-comment-text
				(make-instance 'lisp-token
					       :object (maybe-doc sym doc-type))
				;;*standard-output*
				;; *terminal*
				stream
				:columns cols))
			     ;;(output-text (maybe-doc sym doc-type) cols)
			     ;;(grout-princ #\newline)
			     result)))
		 (nreverse result)
		 )))
      (print-doc symbol nil 'function))))

(defun function-help (symbol expr-number &key width
					   (show-doc (> *completion-count* 2)))
  "Return a string with help for the function designated by SYMBOL."
  (let* ((cols (or width
		   (or (and *terminal*
			    (progn
			      (terminal-get-size *terminal*)
			      (terminal-window-columns *terminal*)))
		       80)))
	 (*print-right-margin* cols))
    ;;(dbug "~s ~s ~s~%" *terminal* cols (terminal-window-columns *terminal*))
    (span-to-fat-string
     (cons
      (function-help-show-function symbol expr-number)
      (when show-doc ; (> *completion-count* 2)
	(function-help-show-doc symbol cols))))))

#|
(defun function-help-show-function (symbol expr-number)
  (let (past-key past-rest did-standout (i 0))
    (declare (special i))
    (tt-write-char #\()
    (tt-color :magenta :default)
    (tt-format "~(~a~)" symbol)
    (tt-color :default :default)
    (labels ((output-atom (a)
	       (typecase a
		 ((or null keyword number string boolean array)
		  (tt-color :white :default)
		  (tt-format "~(~s~)" a)
		  (tt-color :default :default))
		 (t
		  (tt-format "~(~a~)" a))))
	     (print-list-interior (list)
	       (let ((i 0))
		 (declare (special i))
		 (loop :for item :on list :do
		    (print-it (car item))
		    (when (and (consp item) (cdr item) (atom (cdr item)))
		      (tt-write-string " . ")
		      (output-atom (cdr item))))))
	     (print-it (s)
	       (when (not (zerop i))
		 (tt-write-char #\space))
	       (cond
		 ((atom s)
		  (cond
		    ((position s *lambda-list-keywords*)
		     (tt-color :yellow :default)
		     (tt-format "~(~a~)" s)
		     (tt-color :default :default)
		     (when (equal s '&key)
		       (setf past-key t))
		     (when (equal s '&rest)
		       (setf past-rest t)))
		    (t
		     (if (and expr-number
			      (= i expr-number) (not (or past-key past-rest)))
			 (progn
			   (tt-standout t)
			   (setf did-standout t))
			 (setf did-standout nil))
		     (tt-format "~(~a~)" s))))
		 ;; @@@ Despite the above quote, pretty is very ugly here.
		 ((and s (listp s) (cdr s) (not (atom (cdr s))))
		  (tt-write-char #\()
		  (print-list-interior s)
		  (tt-write-char #\)))
		 ((consp s) ;; a cons but not a list
		  (tt-write-char #\()
		  (output-atom (car s))
		  (tt-write-string " ")
		  (output-atom (cdr s))
		  (tt-write-char #\))))
	       (incf i)
	       (when did-standout
		 (tt-standout nil))))
      (tt-write-char #\space)
      ;;(print-it (dlib:lambda-list symbol))
      (print-list-interior (dlib:lambda-list symbol))
      (tt-write-char #\)))))

(defun function-help-show-doc (symbol cols)
  (labels ((maybe-doc (obj type)
	     (without-warning (documentation obj type)))
	   (print-doc (sym pkg doc-type &optional fake-type)
	     (when (maybe-doc sym doc-type)
	       ;; (when did-one
	       (tt-write-char #\newline)
	       (if (and (eq doc-type 'function) (maybe-doc sym doc-type))
		   (progn
		     (when (and pkg (not (eq pkg (find-package :cl))))
		       (tt-color :green :default)
		       (tt-format "~a " (package-name pkg))
		       (tt-format "~:(~a~):~%" (or fake-type doc-type))
		       (tt-color :default :default))
		     ;;(tt-format "~a~%" (function-help sym 0))
		     )
		   (progn
		     (tt-color :green :default)
		     (tt-format "~:(~a~): " (or fake-type doc-type))
		     (when pkg
		       (tt-format "~a:" (package-name pkg)))
		     (tt-format "~a~%" sym)
		     (tt-color :default :default)))
	       (if (eq doc-type :command)
		   (progn
		     (tt-color :white :default)
		     (tt-format "~a" (maybe-doc sym doc-type))
		     (tt-color :default :default))
		   (progn
		     (format-comment-text
		      (make-instance 'lisp-token
				     :object (maybe-doc sym doc-type))
		      ;;*standard-output*
		      *terminal*
		      :columns cols)
		     ;;(output-text (maybe-doc sym doc-type) cols)
		     ;;(grout-princ #\newline)
		     )))))
    (print-doc symbol nil 'function)))

(defun function-help (symbol expr-number &key width)
  "Return a string with help for the function designated by SYMBOL."
  (let* ((cols (or width
		   (or (and *terminal*
			    (progn
			      (terminal-get-size *terminal*)
			      (terminal-window-columns *terminal*)))
		       80)))
	 (*print-right-margin* cols))
    ;;(dbug "~s ~s ~s~%" *terminal* cols (terminal-window-columns *terminal*))
    (make-fat-string
     :string
     ;; @@@ This seems very wasteful. We should probably make function-help-*
     ;; generate spans, so we don't have to do as much useless work.
     (process-ansi-colors
      (make-fatchar-string
       (with-terminal-output-to-string (:ansi-stream)
	 (function-help-show-function symbol expr-number)
	 (when (> *completion-count* 2)
	   (function-help-show-doc symbol cols))))))))
|#

(defun function-keyword-completion (sym context pos word-start all)
  (dbug "function-keyword-completion ~s ~s~%" sym (subseq context pos))
  (let* ((args (and (fboundp sym) (dlib:lambda-list sym)))
	 (key-pos (position '&key args))
	 (word (subseq context word-start))
	 (case-in (string-character-case word))
	 (stringser (if (eql case-in :lower) #'string-downcase #'string))
	 keywords)
    (when (not (and key-pos args))
      (return-from function-keyword-completion
	(if all (symbol-completion-list word) (symbol-completion word))))
    (setf keywords
	  (loop :for s :in (subseq args (1+ key-pos))
		:while (not (position s *lambda-list-keywords*))
		:if (symbolp s)
		  :collect (funcall stringser s)
		:else :if (consp s)
		  :if (consp (car s))
		    :collect (funcall stringser (caar s))
		  :else
		    :collect (funcall stringser (car s))))
    (if (not all)
	(let ((result (complete-list word (length word) all keywords)))
	  (setf (completion-result-insert-position result)
		(+ word-start pos))
	  result)
	(complete-list word (length word) all keywords))))

(defun in-expr (string position)
  "Given STRING and a POSITION in it, return the Lisp expression number it
is in, and the start and end position of the expression in STRING."
  (let ((i 0) (begin 0) (end 0) (pos 0) sym (len (length string)))
    (loop :do
       (when (< pos len)
	 (setf begin pos))
       (let ((*read-eval* nil))
         (setf (values sym pos)
               ;; (with-package :lish-junk
               ;;   (ignore-errors
	       ;; 	   (read-from-string string nil nil :start pos)))))
	       (ignore-errors
		 (clean-read-from-string string nil nil nil :start pos))))
       (when sym
	 (setf end (if (= pos len) pos (1- pos)))
	 (incf i))
       :while (and sym (<= pos position)))
    (values i begin (if (< end begin) begin end))))

#|
(defun tt (s)
  "test in-expr"
  (loop :for i :from 0 :below (length s) :do
     (multiple-value-bind (n st e)
	 (in-expr s i)
       (format t "~s ~2d ~2d ~2d ~2d ~s~%~va|~%"
	       s i n st e (subseq s st e) (1+ i) #\space))))
|#

(defun lisp-token-char-p (c)
  (not (position c *lisp-non-word-chars*)))

(defun custom-resolver (name)
  (cond
    ((string= name "lispy") #'lisp-token-char-p)
    ((string= name "true") (constantly t))
    (t (error "Can't resolve ~S." name))))

(defun possible-package-symbol (str1 str2)
  (if str2
      (symbolify str2
		 :package (or (find-package (string-upcase str1))
			      *package*)
		 :no-new t)
      (symbolify str1 :package *package* :no-new t)))

;; This is, of course, wrong. We need a real reader and walker.
(defun symbol-whose-args-we-are-in (context pos)
  "Return the symbol in CONTEXT whose argument list POS is in. Second value is
the index in CONTEXT of the start of the symbol."
  (let* ((ppos (matching-paren-position context :position pos))
	 (start (or (and ppos (1+ ppos)) 0))
	 (end (min (1+ (scan-over-str
			context start :forward
			:func #'(lambda (c)
				  (or (not (position c *lisp-non-word-chars*))
				      (eql c #\:)))))
		   (length context)))
	 (word (subseq context start end))
	 (*property-resolver* #'custom-resolver))
    ;; This is a total stop-gap measure.
    (multiple-value-bind (match regs)
	(scan-to-strings
	 (parse-string "([\\p{lispy}]+)(:[:]*([\\p{lispy}]+)){0,1}")
	 word :sharedp t)
      (values
       (when match
	 (possible-package-symbol (elt regs 0) (elt regs 2)))
       start))))

(defun try-symbol-help (context pos)
  "If POS is right after a function in CONTEXT, return a string that shows the
arguments for that function, otherwise return NIL."
  (multiple-value-bind (sym start) (symbol-whose-args-we-are-in context pos)
    (let ((word-start (scan-over-str context pos :backward
				     :not-in *lisp-non-word-chars*)))
      (if (and sym (fboundp sym))
	  (if (could-be-a-keyword nil word-start context)
	      (function-keyword-completion sym context pos word-start t)
	      (make-completion-result
	       :completion
	       (list (function-help sym (in-expr (subseq context start) pos)))
	       :count 1
	       :insert-position 1))
	  nil))))

(defmacro do-the-symbols ((sym pak ext) &body forms)
  "Evaluate the forms with SYM bound to each symbol in package PAK. If EXT is
true, then just look at the external symbols."
  `(if ,ext
       (do-external-symbols (,sym ,pak) ,@forms)
       (do-symbols (,sym ,pak) ,@forms)))

(defun not-inherited (symbol package)
  (multiple-value-bind (sym status)
      (find-symbol (string symbol) (or package *package*))
    (declare (ignore sym))
    (case status
      ((:internal :external) t)
      (t nil))))

;; XXX We make the bogus assumption that the symbols are in uppercase
;; already. We should really base it on what the implementation does.
;; (probably mostly for Allegro)

(defun symbol-completion (w &key package (external nil) (include-packages t))
  "Return the first completion for W in the symbols of PACKAGE, which defaults
to the current package. If EXTERNAL is true, use only the external symbols.
Return nil if none was found."
;  (message (format nil "c: ~w ~w" w package))
  (let ((case-in (string-character-case w))
	(match nil) match-len pos
	(unique t))
    (do-the-symbols (sym (or package *package*) external)
      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (when (and
	     ;; It's actually in the package if the package is set
	     (or external (not package) (not-inherited sym package))
	     (setf pos (search w (string sym) :test #'equalp))
	     (= pos 0)
	     ;; (or (boundp sym) (fboundp sym)
	     ;;     (find-package sym)
		 ;;     (ignore-errors (find-class sym)))
	     )
	(let ((s (string sym)))
	  (if (not match)
	      (progn
		(setf match s)
		(setf match-len (length s)))
	      (let ((l (mismatch match s :end1 match-len)))
		(when l
		  (setf unique nil)
		  (setf match-len l)))))))
    (when include-packages
      (loop :with s :for p :in (list-all-packages) :do
	 (setf s (package-name p))
	 (when (and (setf pos (search w s :test #'equalp))
		 (= pos 0))
	   (if (not match)
	       (progn
		 (setf match s)
		 (setf match-len (length s)))
	       (let ((l (mismatch match s :end1 match-len)))
		 (when l
		   (setf unique nil)
		   (setf match-len l)))))))
    (make-completion-result
     :completion 
     (and match
	  (let ((s (subseq match 0 match-len)))
	    (if (eql case-in :lower)
		(string-downcase s)
		s)))
     :unique unique)))

(defun symbol-completion-list (w &key package
				   (external nil) (include-packages t))
  "Print the list of completions for W in the symbols of PACKAGE, which
defaults to the current package. Return how many symbols there were."
;;  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((count 0)
	(l '()) (pkg-list '())
	(case-in (string-character-case w))
	(token (make-instance 'lisp-symbol-token))
	pos)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (do-the-symbols (s (or package *package*) external)
;;;      (princ (s+ (symbol-package s) "::" s " " #\newline))
      (when (and
	     ;; It's actually in the package if the package is set
	     (or external (not package) (not-inherited s package))
	     ;; It matches the beginning
	     (and (setf pos (search w (string s) :test #'equalp))
		  (= pos 0)))
	(push s l)
	(incf count)))
    (when (and include-packages (not package))
      (loop :with s :for p :in (list-all-packages) :do
	 (setf s (package-name p))
	 (when (and
		;; It's actually in the package if the package is set
;;;		(or (not package) (eql (symbol-package s) package))
		;; It matches the beginning
		(and (setf pos (search w s :test #'equalp))
		     (= pos 0)))
	   (push s pkg-list)
	   (incf count))))
    (make-completion-result
     :completion
     ;;(sort (append (mapcar (_ (stylize-symbol _ case-in)) l)
     (sort (append (mapcar (_ (setf (token-object token) _)
			      (stylize-token token :case case-in))
			   l)
		   pkg-list)
	   #'fat-string-lessp)
     :count count)))

(defun could-be-a-keyword (pack word-start context)
  "True if the thing right before the cursor could be a keyword."
  (and (not pack)
       word-start (> word-start 1) (>= (length context) word-start)
       (and (eql (aref context (1- word-start)) #\:)
	    (eql (aref context (- word-start 2)) #\space))))

(defun complete-symbol (context pos all)
  "Completion function for symbols."
  (let* ((word-start (scan-over-str context pos :backward
				    :not-in *lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil)
	 result)
    (multiple-value-setq (pack external) (find-back-pack context word-start))
    ;; If the package is NIL, we complete on inherited symbols too.

    (dbug "pack=~s word-start=~s context=~s~%" pack word-start context)
    (if (could-be-a-keyword pack word-start context)
	(progn
	  (dbug "Could be a keyword ~s~%" (subseq context word-start))
	  (if all
	      (or (try-symbol-help context pos)
		  (symbol-completion-list
		   word :package pack :external external))
	      (let* ((sym (symbol-whose-args-we-are-in context pos))
		     (result (function-keyword-completion sym context pos
							 word-start nil)))
		(dbug "snoopy ~a~%" pos)
		(setf (completion-result-insert-position result)
		      word-start)
		result)))
	(if all
	    (if (and (= (length word) 0)
		     (setf result (try-symbol-help context pos)))
		;; (make-completion-result
		;;  :completion (list result)
		;;  :insert-position 1)
		result
		(symbol-completion-list word :package pack :external external))
	    (progn
	      (setf result (symbol-completion word :package pack
					      :external external)
		    (completion-result-insert-position result) word-start)
	      result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename completion

; (defun directory-files ()
;   (directory (make-pathname :name :wild :type :wild))

; (defun directory-part (path)
;   "Return the directory part of the given namestring."
;   (let ((pos (position *dir-sep* path :from-end t)))
;     (if pos
; 	(subseq path 0 pos)
; 	nil)))

; (defun filename-quotify-like (name like)
;   (if 

;; (defmacro safe-read-directory (&rest args)
;;   "Call read-directory without errors."
;;   `(ignore-errors (read-directory ,@args)))

(defun safe-read-directory (&rest args)
  "Call read-directory without errors."
  (ignore-errors (apply #'read-directory args)))

#|
(defun filename-completion-OLD (word)
  "Return the first completion for w in the current directory's files ~
 or nil if none was found. The second value is true if the first value ~
 matches a full filename."
  (let* (pos
	 (full-match t)
	 (match nil)
	 match-len
	 (w (expand-tilde word))
	 (dir-part (directory-namestring (pathname w)))
	 (dir-part-path (pathname-directory (pathname word)))
	 (file-part (file-namestring (pathname w))))
    (when (> (length file-part) 0)
      (loop :for f :in (if (> (length dir-part) 0)
			   (safe-read-directory :dir dir-part)
			   (safe-read-directory))
	 :do
	 (when (and (setf pos (search file-part f :test #'equal))
		    (= pos 0))
	   (if (not match)
	       (setf match f
		     match-len (length f))
	       (setf match-len (mismatch match f :end1 match-len)
		     full-match nil)))))
;    (format t "~&match = ~a~%" match)
    (values
     (and match
	  (let* ((match-sub (subseq match 0 match-len))
		 (p (pathname match-sub)))
	    (if (> (length dir-part) 0)
		(namestring (make-pathname :directory dir-part-path
					   :name (pathname-name p)
					   :type (pathname-type p)))
		match-sub)))
     full-match)))
|#

(defun filename-completion (word &optional extra-test)
  "Return the first completion for WORD in the current directory's files ~
 or nil if none was found. The second value is true if the first value ~
 matches a full filename."
  (let* (pos
	 match-len
	 (full-match t)
	 (match nil)
	 ;;(start         (position-if (_ (position _ "\" ")) word :from-end t))
	 ;; @@@ XXX This is quite wrong. I should fix it in lish or something.
	 ;; It's a kludge so complete-filename-command can work.
	 (start         (position-if (_ (position _ "\"")) word :from-end t))
	 ;;(w             (expand-tilde word))
	 (w             (expand-tilde
			 (subseq word (or (and start (1+ start)) 0))))
	 (dir-part      (dirname w))
	 (dir-part-path (dirname word))
	 (file-part     (basename w))
	 (hidden        (hidden-file-name-p file-part))
	 (dir-list	(if (> (length dir-part) 0)
			    (safe-read-directory :full t :dir dir-part
						 :omit-hidden (not hidden))
			    (safe-read-directory :full t
						 :omit-hidden (not hidden)))))
    (when (> (length file-part) 0)
      (loop :for f :in dir-list
	 :do
	 (when (and (setf pos (search file-part (dir-entry-name f)
				      :test #'equal))
		    (= pos 0)
		    (or (not extra-test) (funcall extra-test f dir-part)))
	   (if (not match)
	       (setf match (dir-entry-name f)
		     match-len (length (dir-entry-name f)))
	       (setf match-len (or (mismatch match (dir-entry-name f)
					     :end1 match-len)
				   match-len)
		     full-match nil))
	   ;;(dbug "~%match-len = ~a entry = ~s" match-len (dir-entry-name f))
	   )))
    ;;(dbug "~&match = ~a match-sub = ~a~%" match (subseq match 0 match-len))
    (make-completion-result
     :completion
     (and match
	  (let* ((match-sub (subseq match 0 match-len)))
	    ;; If it had a directory part, put it back on.
	    (if (> (length dir-part) 0)
		(if (and (= (length dir-part) 1)
			 (char= (char dir-part 0) nos:*directory-separator*))
		    ;; If it's the root directory, just put a separator.
		    (s+ nos:*directory-separator* match-sub)
		    (path-append dir-part-path match-sub))
		match-sub)))
     :unique full-match)))

(defun filename-completion-list (w &optional extra-test)
  "Return the list of completions for W and how many there were."
  (let* (pos
	 (count 0)
	 (result-list '())
	 (word (expand-tilde w))
;	 (dir-part (directory-namestring (pathname word)))
;	 (file-part (file-namestring (pathname word)))
	 (dir-part (or (and word (path-directory-name word)) ""))
	 (file-part (and word (path-file-name word)))
	 (hidden (hidden-file-name-p file-part))
	 (dir-list (if (> (length dir-part) 0)
		       (safe-read-directory
			:full t :dir dir-part :append-type t
			:omit-hidden (not hidden))
		       (safe-read-directory
			:full t :append-type t :omit-hidden (not hidden)))))
    ;; (dbugf :completion "dir-part = ~a~%file-part = ~a~%dir-list ~s~%"
    ;; 	   dir-part file-part dir-list)
    (locally
	#+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	(setq dir-list (sort dir-list #'string> :key #'dir-entry-name)))
    (loop :for file :in dir-list
       :do
       (when (and (setf pos (search file-part (dir-entry-name file)
				    :test #'equalp))
		  (= pos 0)
		  (or (not extra-test)
		      (funcall extra-test file dir-part)))
	 (push (styled-file-name file) result-list)
	 (incf count)))
    ;;(setq result-list (sort result-list #'string-lessp))
    (make-completion-result
     :completion result-list :count count)))

;; We have to assume the entire context is a filename. Shells that want to
;; do completion in a command line, will have to arrange for this to be so.
(defun complete-filename (context pos all &key parsed-exp)
  "Completion function for file names."
  (declare (ignore parsed-exp pos))
  ;; (format t "jinko (~a) ~a~%" context pos)
  ;; Let's ignore anything after pos for the sake of completion.
  (if all
      (filename-completion-list context)
      (let ((result (filename-completion context)))
	(setf (completion-result-insert-position result) 0)
	result)))

(defun is-actually-directory (f &optional (dir ""))
  ;; @@@ perhaps we need to do a loop to resolve multiple links?
  (or (eq (dir-entry-type f) :directory)
      (and (eq (dir-entry-type f) :link)
	   (eq :directory
	       (let ((path (path-append dir (dir-entry-name f))))
		 (and (file-exists path)
		      (file-info-type
		       (get-file-info path))))))))

;; We have to assume the entire context is a filename. Things that want to
;; do completion in a command line, will have to arrange for this to be so.
(defun complete-directory (context pos all &key parsed-exp)
  "Completion function for file names."
  (declare (ignore parsed-exp pos))
  ;; Let's ignore anything after pos for the sake of completion.
;  (format t "jinko (~a) ~a~%" context pos)
  (if all
      (filename-completion-list context #'is-actually-directory)
      (let ((result (filename-completion context #'is-actually-directory)))
	(setf (completion-result-insert-position result) 0)
	result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary completion
;;
;; This is completion for a big list of words, presumably in a natural
;; language. We read a file of one word per line into a trie.

;; @@@ Of course this should use some generic Trie data structure from
;; somewhere, IF THERE WAS ONE.

(defparameter *default-word-file* "/usr/share/dict/words")

;; foo
;; fooble
;; foozle
;; fimbar
;; fill
;; no
;; noodle
;; nog
;; zibble
;;
;; (f (o (o)))
;; (f (o (o () (b (l (e ()))))))

;; ((f (o (o ())) (b (l (e ()))))
;;  (n (o ()) (d (l (e ()))) (g ()))
;;  (z (i (b (b (l (e ())))))))

(defun make-completion-dictionary (file)
  (let ((dict '(_ )))
    (with-open-file (stm file)
      (loop :with line :and node
	 :while (setf line (read-line stm nil nil)) :do
	 (setf node dict)
	 (loop :with n
	    :for c :across line :do
	    (if (setf n (find (list c) node :key #'car))
		(setf node n)
		(progn
		  (setf n (list c))
		  (pushnew n node :key #'car)
		  (setf node n)))
	    (format t "~s~%" dict) (finish-output))
	 (pushnew '() node)))
    dict))

#|
(defun add-word (w d)
  (let ((node d) n)
    (loop for c across w do
	 (if (setf n (find-if
		      (lambda (x)
			(and x (consp x) (eql (car x) c)))
		      node))
	     (setf node n)
	     (progn (push c node))))
    d))

(defun dump-dict (dict)
  (let ((str (make-stretchy-string 25)))
    (labels ((do-node (node)
	       (loop :for n :in node
		  :if (not node)
		  :then (write-line str)
		  (do-node
  (loop :with node

     :for w :in dict :do
     (setf node w)
     (loop :while (> (length node) 0)
	:do
	(stretchy-append str (car node))
	(
|#

(defun dictionary-completion-list (w dict)
  (declare (ignore w dict))
  (make-completion-result)) ;; @@@

(defun dictionary-completion (w dict)
  (declare (ignore w dict))
  (make-completion-result)) ;; @@@

(defun complete-dictionary-word (context pos all &optional dict)
  "Completion function for dictionary words."
  (declare (ignore pos))
  ;; Let's ignore anything after pos for the sake of completion.
  (if all
      (dictionary-completion-list context dict)
      (let ((result (dictionary-completion context dict)))
	(setf (completion-result-insert-position result) 0)
	result)))

(defun dictionary-completion-function (file)
  "Return a completion function for a dictionary."
  (let ((dict (make-completion-dictionary file)))
    (lambda (context pos all)
      (complete-dictionary-word context pos all dict))))

;; EOF
