;;
;; completion.lisp - Complete your words. Fill the gap. Type less.
;;

;; $Revision: 1.2 $

;; TODO:
;;   - filename-completion
;;     - twiddle
;;     - files with spaces

(defpackage :completion
  (:documentation "Complete your words. Fill the gap. Type less.

Completion functions are called with line of context and a position
where completion was requested. The are asked to return one completion,
or all completions. When asked for one completion, they return a
completion and a position where to insert it. Text should be replaced
from the starting and result position. This allows completion functions
to look at whatever they want to, and replace whatever they want to,
even prior to the starting point. When asked for all completions, they return a
 sequence of strings and a count which is the length of the sequence.")
  (:use :cl :opsys)
  (:export
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
   #:complete-filename
   ))
(in-package :completion)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; @@@ Duplication from tiny-rl
(defvar *lisp-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\" #\' #\\ #\# #\, #\` #\| #\.)
  "Characters that are not considered to be part of a word in lisp.")
;; removed #\/ since it's common in package names

;; @@@ Perhaps this should be merged with the one in tiny-rl?
(defun scan-over-str (str pos dir &key func not-in)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIR is :forward or :backward. Moves over characters in STR starting at POS.
Returns the new position after moving."
  (if (and (not func) not-in)
      (setf func #'(lambda (c) (not (position c not-in)))))
  (if (eql dir :backward)
      ;; backward
      (loop while (and (> pos 0)
		       (funcall func (aref str (1- pos))))
	do (decf pos))
      ;; forward
      (let ((len (length str)))
	(loop while (and (< pos len)
			 (funcall func (aref str (1+ pos))))
	  do (incf pos))))
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
  "Return a new list strings from a list of random objects. If there are strings
in the list already, use them. If everything in the list is a string already,
just return the list."
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
  "Return the longest possible unambiguous completion of WORD from the ITERATOR. Second value is true if it's unique."
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

(defun complete-symbol (context pos all)
  "Completion function for symbols."
  (let* ((word-start (scan-over-str context pos :backward
				    :not-in *lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil))
    (multiple-value-setq (pack external) (find-back-pack context word-start))
    (if all
	(symbol-completion-list word :package pack :external external)
	(multiple-value-bind (completion unique)
	    (symbol-completion word :package pack :external external)
	  (values completion word-start unique)))))

(defmacro do-the-symbols (sym pak ext &body forms)
  "Evaluate the forms with SYM bound to each symbol in package PAK. If EXT is true, then just look at the external symbols."
  `(if ,ext
    (do-external-symbols (,sym ,pak) ,@forms)
    (do-symbols (,sym ,pak) ,@forms)))

(defun character-case (s)
  (declare (type string s))
  "Return :upper :lower :mixed or :none as a description of the set of~
 character cases in the given string."
  (let ((state :none))
    (loop for c across s
	  do
	  (if (both-case-p c)
	      (if (upper-case-p c)
		  (case state
		    (:lower (return-from character-case :mixed))
		    (:none  (setf state :upper)))
		  (case state
		    (:upper (return-from character-case :mixed))
		    (:none  (setf state :lower))))))
    state))

;; XXX We make the bogus assumption that the symbols are in uppercase
;; already. We should really base it on what the implementation does.
;; (probably mostly for Allegro)

(defun symbol-completion (w &key (package *package*) (external nil)
			      (include-packages t))
  "Return the first completion for W in the symbols of PACKAGE, which defaults to the current package. If EXTERNAL is true, use only the external symbols. Return nil if none was found."
;  (message (format nil "c: ~w ~w" w package))
  (let ((case-in (character-case w))
	(match nil) match-len pos
	(unique t))
    (do-the-symbols sym (or package *package*) external
      (when (and (setf pos (search w (string sym) :test #'equalp))
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

(defun symbol-completion-list (w &key (package *package*)
				   (external nil) (include-packages t))
  "Print the list of completions for W in the symbols of PACKAGE, which defaults to the current package. Return how many symblos there were."
  (let ((count 0)
	(l '())
	(case-in (character-case w))
	pos)
    (do-the-symbols s (or package *package*) external
      (when (and
	     ;; It's actually in the package if the package is set
	     (or (not package) (eql (symbol-package s) package))
	     ;; It matches the beginning
	     (and (setf pos (search w (string s) :test #'equalp))
		  (= pos 0)))
	(push (string s) l)
	(incf count)))
    (when (and include-packages (not package))
      (loop :with s :for p :in (list-all-packages) :do
	 (setf s (package-name p))
	 (when (and
		;; It's actually in the package if the package is set
;		(or (not package) (eql (symbol-package s) package))
		;; It matches the beginning
		(and (setf pos (search w s :test #'equalp))
		     (= pos 0)))
	   (push s l)
	   (incf count))))
    (setq l (sort l #'string-lessp))
    (when (eql case-in :lower)
      (setf l (mapcar #'string-downcase l)))
    (values l count)))

(defun find-back-pack (context pos)
  "Check for colons and a package name just prior to the point POS in CONTEXT ~
and return the package and whether we should look for only external symbols."
;  (setf pos (scan-over-str context pos :backward :func
;			   #'(lambda (c) (position c *lisp-non-word-chars*))))
  (when (and (> pos 0)
	     (eql (aref context (1- pos)) #\:))
      (decf pos)
      (let ((external t))
	(when (and (> pos 0)
		   (eql (aref context (1- pos)) #\:))
	  (decf pos)
	  (setf external nil))
	(let ((p-p pos))
	  (setf pos (scan-over-str context pos
				   :backward :not-in *lisp-non-word-chars*))
	  (values
	   (find-package (string-upcase (subseq context pos p-p)))
	   external)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename completion

;; We have to assume the entire context is a filename. Shells that want to
;; do completion in a command line, will have to arrange for this to be so.
(defun complete-filename (context pos all &key parsed-exp)
  "Completion function for file names."
  (declare (ignore parsed-exp pos))
  ;; Let's ignore anything after pos for the sake of completion.
;  (format t "jinko (~a) ~a~%" context pos)
  (if all
      (filename-completion-list context)
      (multiple-value-bind (result full-match) (filename-completion context)
	(declare (ignore full-match))
	(values result 0))))

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

(defun filename-completion (w)
  "Return the first completion for w in the current directory's files ~
 or nil if none was found. The second value is true if the first value ~
 matches a full filename."
  (let (pos (full-match t)
	(match nil)
	match-len
	(dir-part (directory-namestring (pathname w)))
	(dir-part-path (pathname-directory (pathname w)))
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

(defun filename-completion-list (w)
  "Print the list of completions for w. Return how many there were."
  (let* (pos
	 (count 0)
	 (l '())
	 (dir-part (directory-namestring (pathname w)))
	 (file-part (file-namestring (pathname w)))
	 (dir-list (if (> (length dir-part) 0)
		       (safe-read-directory :dir dir-part :append-type t)
		       (safe-read-directory :append-type t))))
;    (format t "dir-part = ~a~%file-part = ~a~%" dir-part file-part)
    (loop :for f :in dir-list
       :do
;      (format t "~f~%" f)
       (when (and (setf pos (search file-part f :test #'equalp))
		  (= pos 0))
	 (push f l)
	 (incf count)))
    (setq l (sort l #'string-lessp))
    (values l count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary completion
;;
;; This is completion for a big list of words, presumably in a natural
;; language. We read a file of one word per line into a trie.

(defparameter *default-word-file* "/usr/share/dict/words")

(defun make-completion-dictionary (file)
  (let ((dict '()))
    (with-open-file (stm file)
      (loop :with line
	 :while (setf line (read-line stm nil nil)) :do
	 (loop :with node = dict :and n
	    :for c :across line :do
	    (if (setf n (find c node :key #'car))
		(setf node n)
		(progn
		  (setf n (list c))
		  (pushnew n node)
		  (setf node n))))))))

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
