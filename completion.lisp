;;
;; completion.lisp - Pull endings out of somewhere.
;;

;; TODO:
;;   - finish dictionary completion
;;   - more languageishnessification

(defpackage :completion
  (:documentation
;; @@@: This description needs work. It confuses me and I wrote it. Also it's
;; not a good overall description of the package, rather it's an incomplete
;; description of the specifics of calling completion functions.
"Completion functions are called with line of context and a position where
completion was requested. The are asked to return one completion, or all
completions. When asked for one completion, they return a completion and a
position where to insert it. Text should be replaced from the starting and
result position. This allows completion functions to look at whatever they
want to, and replace whatever they want to, even prior to the starting
point. When asked for all completions, they return a sequence of strings and
a count which is the length of the sequence.")
  (:use :cl :dlib :opsys :glob :dlib-misc :syntax-lisp
        :terminal :terminal-ansi :cl-ppcre)
  (:export
   ;; generic
   #:complete-print
   #:complete-print-long
   #:*completion-count*
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

;; @@@ Duplication from tiny-rl
(defvar *lisp-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\" #\' #\\ #\# #\, #\` #\| #\.)
  "Characters that are not considered to be part of a word in lisp.")
;; removed #\/ since it's common in package names

;; @@@ Perhaps this should be merged with the one in tiny-rl?
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
			  (funcall func (aref str (1+ pos))))
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
    (values
     (loop :for w :in list
	:if (or (not (setq pos (mismatch w word)))
		(>= pos (length word)))
	  :collect (prog1 w (incf i))
	:end)
     i)))

(defun string-completion (word list)
  "Return the longest possible unambiguous completion of WORD from LIST. Second
value is true if it's unique."
  (let ((match nil) (unique t) (match-len 0) pos)
    (loop :for w :in list :do
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
    (values (subseq match 0 match-len) unique)))

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
	(multiple-value-bind (completion unique) (string-completion word list)
	  (values completion word-start unique)))))

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
    (values
     (loop :with w = (stringish (funcall iterator t))
	:if (or (not (setq pos (mismatch w word)))
		(>= pos (length word)))
	  :collect (prog1 w (incf i))
	:end
	:while (setq w (stringish (funcall iterator))))
     i)))

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
    (values (subseq match 0 match-len) unique)))

(defun complete-iterator (context pos all iterator)
  "Completion function for iterators. The entire context is matched."
;  (let* ((word-start (scan-over-str context pos :backward
;				    :not-in *lisp-non-word-chars*))
  (let* ((word-start 0)
	 (word (subseq context word-start pos)))
    (if all
	(iterator-completion-list word iterator)
	(multiple-value-bind (completion unique)
	    (iterator-completion word iterator)
	  (values completion word-start unique)))))

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

(defun function-help-show-function (str symbol expr-number)
  (let (past-key past-rest did-standout)
    (write-char #\( str)
    (tt-color :magenta :default)
    (write symbol :stream str :case :downcase :escape nil)
    (tt-color :default :default)
    (loop
       :for s :in (dlib:lambda-list symbol) :and i = 1 :then (1+ i)
       :do
       (write-char #\space str)
       (if (position s *lambda-list-keywords*)
	   (progn
	     (tt-color :yellow :default)
	     (write s :stream str :case :downcase :escape nil :pretty nil)
	     (tt-color :default :default)
	     (when (equal s '&key)
	       (setf past-key t))
	     (when (equal s '&rest)
	       (setf past-rest t)))
	   ;; @@@ Despite the above quote, pretty is very ugly here.
	   (progn
	     (if (and (= i expr-number) (not (or past-key past-rest)))
		 (progn
		   (tt-standout t)
		   (setf did-standout t))
		 (setf did-standout nil))
	     (typecase s
	       (cons
		(write-char #\( str)
		(loop :with first = t
		   :for ss :in s :do
		   (if first
		       (setf first nil)
		       (write-char #\space str))
		   (typecase ss
		     ((or null keyword number string boolean array)
		      (tt-color :white :default)
		      (write ss :stream str :case :downcase :escape nil
			     :pretty nil :readably t)
		      (tt-color :default :default))
		     (t
		      (write ss :stream str :case :downcase :escape nil
			     :pretty nil :readably nil))))
		(write-char #\) str))
	       (t
		(write s :stream str :case :downcase :escape nil :pretty nil
		       :readably nil)))
	     (when did-standout
	       (tt-standout nil)))))
    (write-char #\) str)))

(defun output-text (s stream cols)
  (let (par new-paragraph)
    (labels ((print-it ()
	       (when new-paragraph
		 (princ (s+ #+nil "•" #\newline) stream))
	       (tt-color :white :default)
	       ;; (dbug "cols = ~a~%" cols)
	       (write-string
		(justify-text (join (nreverse par) #\space)
			      :stream nil
			      :cols cols)
		stream)
	       (tt-color :default :default)
	       (princ (s+ #+nil "‼" #\newline) stream)
	       (setf new-paragraph t)))
      (loop :for l :in (split-sequence #\newline s)
	 :do
	 (cond
	   ((zerop (length l))
	    (when par
	      (print-it)
	      (setf par nil))
	    ;;(grout-princ (s+ #+nil "•" #\newline))
	    )
	   ((alpha-char-p (char l 0))
	    (push l par))
	   (t
	    (when par
	      (print-it)
	      ;;(setf new-paragraph nil)
	      ;;(grout-princ (s+ "˚" #\newline))
	      (setf par nil))
	    (tt-color :white :default)
	    (write-string l stream)
	    (tt-color :default :default)
	    (princ (s+ #+nil "¶" #\newline) stream))))
      (when par
	(print-it)))))

(defun function-help-show-doc (stream symbol cols)
  (labels ((maybe-doc (obj type)
	     (without-warning (documentation obj type)))
	   (print-doc (sym pkg doc-type &optional fake-type)
	     (when (maybe-doc sym doc-type)
	       ;; (when did-one
	       (princ #\newline stream)
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
		     (output-text (maybe-doc sym doc-type) stream cols)
		     ;;(grout-princ #\newline)
		     )))))
    (print-doc symbol nil 'function)))

(defun function-help (symbol expr-number)
  "Return a string with help for the function designated by SYMBOL."
  (let ((cols (or (and *terminal*
		       (progn
			 (terminal-get-size *terminal*)
			 (terminal-window-columns *terminal*))) 80)))
    ;;(dbug "~s ~s ~s~%" *terminal* cols (terminal-window-columns *terminal*))
    (with-output-to-string (str)
      (let ((*terminal* (make-instance 'terminal-ansi-stream
				       :output-stream str)))
	(function-help-show-function str symbol expr-number)
	(when (> *completion-count* 2)
	  (function-help-show-doc str symbol cols))))))

(defun function-keyword-completion (sym context pos word-start all)
  (dbug "function-keyword-completion ~s ~s~%" sym (subseq context pos))
  (let* ((args (dlib:lambda-list sym))
	 (key-pos (position '&key args))
	 (word (subseq context word-start))
	 (case-in (character-case word))
	 (stringser (if (eql case-in :lower) #'string-downcase #'string))
	 keywords)
    (when (not key-pos)
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
	(multiple-value-bind (comp pos uniq)
	    (complete-list word (length word) all keywords)
	  (values comp (+ word-start pos) uniq))
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
	       (clean-read-from-string string nil nil nil :start pos)))
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
	 (ppcre:*property-resolver* #'custom-resolver))
    ;; This is a total stop-gap measure.
    (multiple-value-bind (match regs)
	(ppcre:scan-to-strings
	 (ppcre:parse-string "([\\p{lispy}]+)(:[:]*([\\p{lispy}]+)){0,1}")
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
	      (values
;;	       (list (function-help sym (in-expr (subseq context start) pos)))
	       (function-help sym (in-expr (subseq context start) pos))
	       1))
	  nil))))

(defmacro do-the-symbols ((sym pak ext) &body forms)
  "Evaluate the forms with SYM bound to each symbol in package PAK. If EXT is
true, then just look at the external symbols."
  `(if ,ext
       (do-external-symbols (,sym ,pak) ,@forms)
       (do-symbols (,sym ,pak) ,@forms)))

(defun character-case (s)
  (declare (type string s))
  "Return :upper :lower :mixed or :none as a description of the set of~
 character cases in the given string."
  (let ((state :none))
    (loop :for c :across s
       :do
       (if (both-case-p c)
	   (if (upper-case-p c)
	       (case state
		 (:lower (return-from character-case :mixed))
		 (:none  (setf state :upper)))
	       (case state
		 (:upper (return-from character-case :mixed))
		 (:none  (setf state :lower))))))
    state))

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
  (let ((case-in (character-case w))
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
    (values (and match
		 (let ((s (subseq match 0 match-len)))
		   (if (eql case-in :lower)
		       (string-downcase s)
		       s)))
	    unique)))

(defun symbol-completion-list (w &key package
				   (external nil) (include-packages t))
  "Print the list of completions for W in the symbols of PACKAGE, which
defaults to the current package. Return how many symbols there were."
;;  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((count 0)
	(l '())
	(case-in (character-case w))
	pos)
    (do-the-symbols (s (or package *package*) external)
      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
;;;      (princ (s+ (symbol-package s) "::" s " " #\newline))
      (when (and
	     ;; It's actually in the package if the package is set
	     (or external (not package) (not-inherited s package))
	     ;; It matches the beginning
	     (and (setf pos (search w (string s) :test #'equalp))
		  (= pos 0)))
	(pushnew (if (fboundp s) (s+ s " (F)") (string s)) l :test #'equal)
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
	   (push s l)
	   (incf count))))
    (setq l (sort l #'string-lessp))
    (when (eql case-in :lower)
      (setf l (mapcar #'string-downcase l)))
    (values l count)))

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
	      (if (setf result (multiple-value-list
				(try-symbol-help context pos)))
		  (values-list result)
		  (symbol-completion-list
		   word :package pack :external external))
	      (let ((sym (symbol-whose-args-we-are-in context pos)))
		(dbug "snoopy ~a~%" pos)
		(multiple-value-bind (completion unique)
		    (function-keyword-completion sym context pos
						 word-start nil)
		  (values completion word-start unique)))))
	(if all
	    (if (and (= (length word) 0)
		     (setf result (try-symbol-help context pos)))
		(values (list result) 1)
		(symbol-completion-list word :package pack :external external))
	    (multiple-value-bind (completion unique)
		(symbol-completion word :package pack :external external)
	      (values completion word-start unique))))))

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

(defmacro safe-read-directory (&rest args)
  "Call read-directory without errors."
  `(ignore-errors (read-directory ,@args)))

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
  "Return the first completion for w in the current directory's files ~
 or nil if none was found. The second value is true if the first value ~
 matches a full filename."
  (let* (pos
	 match-len
	 (full-match t)
	 (match nil)
	 (w             (expand-tilde word))
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
    (values
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
     full-match)))

(defun filename-completion-list (w &optional extra-test)
  "Return the list of completions for W and how many there were."
  (let* (pos
	 (count 0)
	 (result-list '())
	 (word (expand-tilde w))
;	 (dir-part (directory-namestring (pathname word)))
;	 (file-part (file-namestring (pathname word)))
	 (dir-part (path-directory-name word))
	 (file-part (path-file-name word))
	 (hidden (hidden-file-name-p file-part))
	 (dir-list (if (> (length dir-part) 0)
		       (safe-read-directory
			:full t :dir dir-part :append-type t
			:omit-hidden (not hidden))
		       (safe-read-directory
			:full t :append-type t :omit-hidden (not hidden)))))
;    (format t "dir-part = ~a~%file-part = ~a~%" dir-part file-part)
    (loop :for file :in dir-list
       :do
;      (format t "~f~%" f)
       (when (and (setf pos (search file-part (dir-entry-name file)
				    :test #'equalp))
		  (= pos 0)
		  (or (not extra-test)
		      (funcall extra-test file dir-part)))
	 (push (dir-entry-name file) result-list)
	 (incf count)))
    ;;(setq result-list (sort result-list #'string-lessp))
    (locally
	#+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	(setq result-list (sort result-list #'string<)))
    (values result-list count)))

;; We have to assume the entire context is a filename. Shells that want to
;; do completion in a command line, will have to arrange for this to be so.
(defun complete-filename (context pos all &key parsed-exp)
  "Completion function for file names."
  (declare (ignore parsed-exp pos))
  ;; (format t "jinko (~a) ~a~%" context pos)
  ;; Let's ignore anything after pos for the sake of completion.
  (if all
      (filename-completion-list context)
      (multiple-value-bind (result full-match) (filename-completion context)
	(declare (ignore full-match))
	(values result 0))))

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
      (multiple-value-bind (result full-match)
	  (filename-completion context #'is-actually-directory)
	(declare (ignore full-match))
	(values result 0))))

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
  )

(defun dictionary-completion (w dict)
  (declare (ignore w dict))
  )

(defun complete-dictionary-word (context pos all &optional dict)
  "Completion function for dictionary words."
  (declare (ignore pos))
  ;; Let's ignore anything after pos for the sake of completion.
  (if all
      (dictionary-completion-list context dict)
      (multiple-value-bind (result full-match)
	  (dictionary-completion context dict)
	(declare (ignore full-match))
	(values result 0))))

(defun dictionary-completion-function (file)
  "Return a completion function for a dictionary."
  (let ((dict (make-completion-dictionary file)))
    (lambda (context pos all)
      (complete-dictionary-word context pos all dict))))

;; EOF
