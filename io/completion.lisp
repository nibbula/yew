;;;
;;; completion.lisp - Pull endings out of somewhere.
;;;

(defpackage :completion
  (:documentation
"The completion package provides a generic interface to functions that find
an expansion or the the set of possible expansaion for a string. It is
typically used to find and insert endings for a prefix string that the user
has input, but it could provide other kinds of expansions.

Completion functions are called with line of context and a position where
completion was requested. The are asked to return one completion, or all
completions. When asked for one completion, they return a completion and a
position where to insert it. Text should be replaced from the starting and
result position. This allows completion functions to look at whatever they
want to, and replace whatever they want to, even prior to the starting
point. When asked for all completions, they return a sequence of strings and
a count which is the length of the sequence. Completion functions return a
completion-result structure.

A small set of completion functions are provided for :
  Symbols:                  ‘complete-symbol’
  Files and directories:    ‘complete-filename’ and ‘complete-directory’
  Character names:          ‘complete-char-name’

Facilities are also provided for making your own completion functions.

List completion:

For completing from lists, you can make new completion functions from a
list with:
  ‘list-completion-function’

Or you can bind ‘*completion-list*’ and use ‘complete-list’.

This is mostly useful for small things. For big things like a filessytem or
database, that you don't want to make a copy of, use iterator completion.

Iterator completion:

You can make a compltion function from a iterator function with:
  ‘iterator-completion-function’

For completion purposes an iterator is a function that when called with a true
argument returns the first item in a sequence, then when subsequently called
with a false or not providied arguemnt, returns the subsequent items in a
sequence. Upon reaching the end it returns false.

Note that this means the sequence can't contain NIL.

Dictionary completion:
  [not documented yet]
")
  (:use :cl :dlib :opsys :glob :collections :char-util :dlib-misc
        :string-expand :reader-ext :syntax :syntax-lisp :terminal
	#+use-regex :regex
	#-use-regex :cl-ppcre
	:theme :fatchar :style)
  (:export
   ;; generic
   #:complete-print
   #:complete-print-long
   #:*completion-count*
   #:*highlight-differences*
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
   #:string-completion
   #:ostring-completion
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
   #:*filename-completion-value-funcs*
   #:*filename-completion-value-allowed*
   ;; dictionary
   #:*dictionary*
   #:*default-word-file*
   #:make-completion-dictionary
   #:dictionary-completion-list
   #:dictionary-completion
   #:complete-dictionary-word
   #:dictionary-completion-function
   ;; characters
   #:complete-char-name
   ))
(in-package :completion)

;; TODO:
;;  - finish dictionary completion
;;  - more languageishnessification

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
;; 		   (compilation-speed 0)))

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

(defvar *highlight-differences* t
  "True to use the theme's program.completion.difference.style on the first
different character of completion lists.")

;; @@@ Duplication from RL
(defvar *lisp-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\" #\' #\\ #\# #\, #\` #\|
    #| #\. |# ;; <-- This depends on the context?
    )
  "Characters that are not considered to be part of a word in lisp.")
;; removed #\/ since it's common in package names

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

(defun highlight-difference (pos string)
  "Highlight the difference at position POS in STRING."
  (if (and *highlight-differences*
	   (plusp pos)			; Don't bother if it's the first char.
	   (< pos (olength string)))
      (let* ((str (etypecase string
		    (string (string-to-fat-string string))
		    (fat-string string)))
	     (style (theme-value *theme*
				 '(:program :completion :difference :style))))
	(when style
	  (setf (oelt str pos)
		(styled-char (append (char-style (oelt str pos)) style)
			     (oelt str pos))))
	str)
      string))

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
  "Return a completion-result for all the words from ‘list’ that have the
prefix ‘word’. The result contains a count of matches."
  (let ((i 0) pos (len (olength word)) sw)
    (make-completion-result
     :completion
     (loop :for w :in list
       :do (setf sw (stringify w))
       :when (or (not (setq pos (mismatch sw word)))
		 (>= pos len))
         :collect (prog1 (highlight-difference len sw) (incf i)))
     :count i)))

;; @@@ Consdier ditching this in favor of the "generic" ostring-completion.
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

(defun ostring-completion (word string-sequence)
  "Return the longest possible unambiguous completion of WORD from
STRING-SEQUENCE which should be something for which OMAPN iterates over strings.
Returns a completion-result."
  (let ((match nil) (unique t) (match-len 0) pos w)
    (omapn (lambda (obj)
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
	   string-sequence)
    (make-completion-result
     :completion (subseq match 0 match-len)
     :unique unique)))

(defvar *completion-list* nil
  "List to use for completion.")
		   
(defun complete-list (context pos all &optional (list *completion-list*))
  "Completion function for lists. The entire context is matched."
  ;; (let* ((word-start (scan-over-string context pos :backward
  ;; 				       :not-in *lisp-non-word-chars*))
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
  (let ((i 0) pos (len (olength word)))
    (make-completion-result
     :completion
     (loop :with w = (stringish (funcall iterator t))
	:if (or (not (setq pos (mismatch w word)))
		(>= pos len))
	  :collect (prog1 (highlight-difference len w) (incf i))
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
  ;; (let* ((word-start (scan-over-string context pos :backward
  ;; 				       :not-in *lisp-non-word-chars*))
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
	(setf pos (scan-over-string context pos
				    :backward :not-in *lisp-non-word-chars*))
	(values
	 (find-package (string-upcase (subseq context pos p-p)))
	 external)))))

(defun find-forward-pack (context pos)
  (let ((end (scan-over-string context pos :forward
			       :not-in *lisp-non-word-chars*)))
    (if (and end (<= end (length context)) (char= (char context end) #\:))
	(find-package (string-upcase (subseq context pos end)))
	nil)))

#|
(defun try-symbol-help (context pos)
  "If POS is right before a function in CONTEXT, return a string that shows the
arguments for that function, otherwise return NIL."
  (let* ((trimmed (rtrim (subseq context 0 pos)))
	 (word-start (scan-over-string trimmed (min pos (length trimmed))
				       :backward
				       :not-in *lisp-non-word-chars*))
	 (pack (find-back-pack trimmed (length trimmed)))
	 (sym (and word-start
		   (symbolify (subseq trimmed word-start)
			      (or pack *package*)))))
    (if (fboundp sym)
	(format nil "~((~a~{ ~a~})~)"
		sym (argument-list (symbol-function sym)))
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
			 ;;(push (format nil "~(~a~)" s) result)
			 (output-atom s)
			 ))))
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
      ;;(print-it (dlib:argument-list symbol))
      (print-list-interior (dlib:argument-list symbol))
      (push #\) result)
      (nreverse result))))

#|
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
      (print-doc symbol nil 'function)
      )))
|#

(defun function-help-show-doc (symbol cols)
  (let (result)
    (labels ((maybe-doc (obj type)
	       (without-warning (documentation obj type)))
	     (print-doc (sym doc-type)
	       (when (maybe-doc sym doc-type)
		 (push #\newline result)
		 (push (fatchar-io:with-output-to-fat-string (stream)
			 (format-comment-text
			  (make-instance 'lisp-token
					 :object (maybe-doc sym doc-type))
			  stream
			  :columns cols))
		       result))
	       (nreverse result)))
      (print-doc symbol 'function))))

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
      ;;(print-it (dlib:argument-list symbol))
      (print-list-interior (dlib:argument-list symbol))
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
  (let* ((args (and (fboundp sym) (dlib:argument-list sym)))
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
	 (end (min (1+ (scan-over-string
			context start :forward
			:function
			#'(lambda (c)
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
    (let ((word-start (scan-over-string context pos :backward
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

;; It might be nice if it also could do expansion like:
;; m-v-b -> multiple-value-bind
;; But that is indeed another interface, perhaps akin to shell expansions.

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
	(token (make-instance 'lisp-symbol-token :object nil))
	(empty (zerop (length w)))
	pos)
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (do-the-symbols (s (or package *package*) external)
      (when (and
	     ;; It's actually in the package if the package is set
	     (or external (not package) (not-inherited s package))
	     #+ecl (not (eq 'si:unbound s)) ;; XXX This is some bullshit.
	     (or
	      ;; There's no word
	      empty
	      ;; It matches the beginning
	      (and (setf pos (search w (string s) :test #'equalp))
		   (= pos 0))))
	(push s l)
	(incf count)))
    (when (and include-packages (not package))
      (loop :with s :for p :in (list-all-packages) :do
	 (setf s (package-name p))
	 (when (and
		;; It's actually in the package if the package is set
		;; (or (not package) (eql (symbol-package s) package))
		;; It matches the beginning
		(or
		 empty
		 (and (setf pos (search w s :test #'equalp))
		      (= pos 0))))
	   (push s pkg-list)
	   (incf count))))
    (make-completion-result
     :completion
     ;;(sort (append (mapcar (_ (stylize-symbol _ case-in)) l)
     (sort (append (mapcar (_
			    (setf (token-object token) _)
			    (highlight-difference
			     (length w)
			     (stylize-token token :case case-in)))
			   l)
		   pkg-list)
	   #'fat-string-lessp)
     :count count)))

(defun symbol-prefix-is (prefix pack word-start context)
  "Return true if the preceding two chars are PREFIX."
  (and (not pack)
       word-start (> word-start 1) (>= (length context) word-start)
       (and (eql (aref context (1- word-start)) (char prefix 1))
	    (eql (aref context (- word-start 2)) (char prefix 0)))))

(defun could-be-a-keyword (pack word-start context)
  "True if the thing right before the cursor could be a keyword."
  (symbol-prefix-is " :" pack word-start context))

(defun could-be-a-char (pack word-start context)
  "True if the thing right before the cursor could be a character."
  (symbol-prefix-is "#\\" pack word-start context))

(defun complete-symbol (context pos all)
  "Completion function for symbols."
  (let* ((word-start (scan-over-string context pos :backward
				       :not-in *lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil)
	 result)
    (multiple-value-setq (pack external) (find-back-pack context word-start))
    ;; If the package is NIL, we complete on inherited symbols too.

    (dbug "pack=~s word-start=~s context=~s~%" pack word-start context)
    (cond
      ((could-be-a-keyword pack word-start context)
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
      ((could-be-a-char pack word-start context)
       (dbug "Could be a character ~s~%" (subseq context word-start))
       (if all
	   (if (zerop (length word))	; @@@ bug in print-columns-smush ?
	       (make-completion-result)
	       (complete-char-name word all))
	   (let ((case-in (string-character-case word)))
	     (setf result (complete-char-name word all)
		   (completion-result-insert-position result) word-start)
	     (when (and result
			(completion-result-completion result)
			(eql case-in :lower))
	       (setf (completion-result-completion result)
		     (string-downcase (completion-result-completion result))))
	     result)))
      (t
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
	     result))))))

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

(defvar *filename-completion-value-funcs* `(,#'nos:env)
  "List of value functions to be provided to ‘expand-variables’.")

(defvar *filename-completion-value-allowed* nil
  "A boolean function of a single character or a string specifiying what
characters are allowed in the variable expansion.")

(defun expand-things (s)
  "Do string expansion on ‘s’."
  (expand-tilde (expand-variables s
		  :value-funcs *filename-completion-value-funcs*
		  :allowed *filename-completion-value-allowed*)))

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
	 (w             (expand-things
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
	  (let* ((match-sub (subseq match 0 match-len))
		 (result-path
		  ;; If it had a directory part, put it back on.
		  (if (> (length dir-part) 0)
		      (if (and (= (length dir-part) 1)
			       (char= (char dir-part 0)
				      nos:*directory-separator*))
			  ;; If it's the root directory, just put a separator.
			  (s+ nos:*directory-separator* match-sub)
			  (path-append dir-part-path match-sub))
		      match-sub)))
	    (if (and full-match (directory-p (expand-things result-path)))
		(s+ result-path nos:*directory-separator*)
		result-path)))
     :unique full-match)))

(defun filename-completion-list (w &optional extra-test)
  "Return the list of completions for W and how many there were."
  (let* (pos
	 (count 0)
	 (result-list '())
	 (word (expand-things w))
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
	 (push (highlight-difference
		(length file-part) (styled-file-name file)) result-list)
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

(defparameter *default-word-file* "/usr/share/dict/words")
(defvar *dictionary* nil
  "The current default dictionary.")

(defun make-completion-dictionary (file)
  (setf *dictionary* (prefix-tree:make-trie (file-lines file))))

(defun dictionary-completion-list (word &optional (dict *dictionary*))
  (let ((endings (prefix-tree:endings word dict)))
    (make-completion-result
     :completion (mapcar (_ (s+ word _)) endings)
     :count (length endings))))

(defun dictionary-completion (word &optional (dict *dictionary*))
  (let ((word-list
	  (mapcar (_ (s+ word _)) (prefix-tree:endings word dict))))
    (when (prefix-tree:lookup word dict)
      (setf word-list (cons word word-list)))
    (string-completion word word-list)))

(defun complete-dictionary-word (context pos all &optional (dict *dictionary*))
  "Completion function for dictionary words."
  (declare (ignore pos))
  (when (not *dictionary*)
    (make-completion-dictionary *default-word-file*))
  ;; Let's ignore anything after pos for the sake of completion.
  (if all
      (dictionary-completion-list context dict)
      (let ((result (dictionary-completion context dict)))
	(setf (completion-result-insert-position result) 0)
	result)))

(defun dictionary-completion-function (&optional (file *default-word-file*))
  "Return a completion function for the dictionary in ‘file’, which should be
a list of words, one per line. ‘file’ defaults to ‘*default-word-file*’."
  (let ((dict (make-completion-dictionary file)))
    (lambda (context pos all)
      (complete-dictionary-word context pos all dict))))

;; @@@ This is completely wateful. We should use dictionary completion as soon
;; as we finish it.

(defvar *char-names* nil
  "A terrible way to hog memory.")

(defun char-names ()
  (or *char-names*
      (setf *char-names*
	    (prefix-tree:make-trie
	     (with-spin ()
	       (loop :for i :from 0 :below char-code-limit
	         :collect (char-name (code-char i))
		 :do (when (zerop (mod i 1000)) (spin))))))))

(defun complete-char-name (str all)
  ;; (complete-string-sequence
  ;;  str all (mapcar #'(lambda (x) (string (car x))) (nos:environment))))
  ;; (complete-list (string-upcase str) (length str) all (char-names))
  (complete-dictionary-word (string-upcase str) (length str) all (char-names))
  )

;; EOF
