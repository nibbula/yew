;;
;; grid.lisp - Character grid for terminals.
;;

(defpackage :terminal-grid
  (:documentation
   "Character grid for terminals. This isn't a terminal type, just things for
other terminals to use.")
  (:use :cl :char-util :ochar :fatchar :fatchar-io)
  (:export
   #:grid-char
   #:grid-char-c
   #:grid-char-fg
   #:grid-char-bg
   #:grid-char-line
   #:grid-char-attrs
   #:make-grid-char
   #:copy-grid-char
   #:grid-string
   #:make-grid-string
   #:blank-char
   #:grid-char-same-effects
   #:grid-char=
   #:set-grid-char
   #:grapheme-to-grid-char
   #:set-fat-char
   #:grid-char-character-length
   #:grid-string-character-length
   #:grid-to-fat-string
   #:fat-string-to-grid-string
   #:unset-grid-char
   #:any-char-c
   #:any-char-fg
   #:any-char-bg
   #:any-char-line
   #:any-char-attrs
   ))
(in-package :terminal-grid)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

;; As you may know, many of the world's lovely scripts, do not fit perfectly
;; into a character grid, so neither will all unicode characters. This will only
;; really work for those scripts that do. But we do at least try to make it a
;; grid of unicode graphemes, instead of just characters. That means there can
;; be multiple characters per screen cell, or sort of hidden empty cells that
;; are under double wide characters.
;;
;; So, grid-char is like a fatchar with coalesced graphemes.
;; @@@ Someday do the experiment to make them a class and see what happens.

(defstruct grid-char
  "A grapheme with attributes."
  (c nil :type (or null character string))
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(deftype grid-string (&optional n) `(vector grid-char ,(or n '*)))
(defun make-grid-string (n) (make-array n :element-type 'grid-char))

(defparameter *blank-char* (make-grid-char :c #\space))
(defun blank-char ()
  (copy-grid-char *blank-char*))

(defmethod display-length ((c grid-char))
  (cond
    ((not (zerop (grid-char-line c)))
     1)				    ; assume line drawing can happen in 1 cell
    ;; ((char= #\nul (grid-char-c c))
    ;;  0)		; since an unset fatchar is #\nul
    (t (display-length (grid-char-c c)))))

;; This probably gives a warning since it's already done in crunch.
;; (defmethod display-length ((c null))
;;   0) ;; @@@ so bogus

(defun gs-display-length (gs)
  (loop :for c :across gs :sum (display-length c)))

(defgeneric grid-char-same-effects (a b)
  (:documentation
   "Return true if the two fatchars have the same colors and attributes.")
  (:method ((a grid-char) (b grid-char))
    (and (equal (grid-char-fg a) (grid-char-fg b))
	 (equal (grid-char-bg a) (grid-char-bg b))
	 (not (set-exclusive-or (grid-char-attrs a) (grid-char-attrs b)
				:test #'eq))))
  (:method ((a grid-char) (b fatchar))
    (and (equal (grid-char-fg a) (fatchar-fg b))
	 (equal (grid-char-bg a) (fatchar-bg b))
	 (not (set-exclusive-or (grid-char-attrs a) (fatchar-attrs b)
				:test #'eq)))))

(defgeneric grid-char= (a b)
  (:documentation "True if everything about a grid-char is the equivalent.")
  (:method ((a grid-char) (b grid-char))
    (and (equal (grid-char-c a) (grid-char-c b))
	 (grid-char-same-effects a b)
	 (= (grid-char-line a) (grid-char-line b))))
  (:method ((a grid-char) (b fatchar))
    (and (characterp (grid-char-c a))
	 (char= (grid-char-c a) (fatchar-c b))
	 (grid-char-same-effects a b)
	 (= (grid-char-line a) (fatchar-line b)))))

(defgeneric set-grid-char (char value)
  (:documentation "Set the grid-char CHAR to VALUE.")
  (:method ((char grid-char) (value grid-char))
    (setf (grid-char-c char)     (grid-char-c value)
	  (grid-char-fg char)    (grid-char-fg value)
	  (grid-char-bg char)    (grid-char-bg value)
	  (grid-char-attrs char) (grid-char-attrs value)
	  (grid-char-line char)  (grid-char-line value)))
  (:method ((char grid-char) (value fatchar))
    (setf (grid-char-c char)     (fatchar-c value)
	  (grid-char-fg char)    (fatchar-fg value)
	  (grid-char-bg char)    (fatchar-bg value)
	  (grid-char-attrs char) (fatchar-attrs value)
	  (grid-char-line char)  (fatchar-line value))))

;;; @@@ crunch may have to change to this rendition
(defun grapheme-to-grid-char (grapheme &key rendition)
  "Make a GRID-CHAR from GRAPHEME, which can be any of fat-string,
fatchar-string, string, fatchar, or character. Note that in the case of a fat
strings, only the attributes of the first character are preserved.
RENDITION is a fatchar to take effects from."
  (typecase grapheme
    (fat-string
     (grapheme-to-grid-char (fat-string-string grapheme)
			    :rendition rendition))
    (fatchar-string
     ;; Take the attributes from the first character only.
     (if (not (zerop (length grapheme)))
	 (make-grid-char :fg    (fatchar-fg    (elt grapheme 0))
			 :bg    (fatchar-bg    (elt grapheme 0))
			 :attrs (fatchar-attrs (elt grapheme 0))
			 :line  (fatchar-line  (elt grapheme 0))
			 :c (if (= 1 (length grapheme))
				(fatchar-c (elt grapheme 0))
				(coerce (loop :for c :across grapheme
					   :collect (fatchar-c c))
					'string)))
	 (make-grid-char :c nil)))
    (string
     (if rendition ;; @@@ the non tty case probably isn't used, also below
	 (make-grid-char :c (case (length grapheme)
			      (0 nil)
			      (1 (char grapheme 0))
			      (t grapheme))
			 :fg (fatchar-fg rendition)
			 :bg (fatchar-bg rendition)
			 :attrs (fatchar-attrs rendition))
	 (make-grid-char :c (case (length grapheme)
			      (0 nil)
			      (1 (char grapheme 0))
			      (t grapheme)))))
    (fatchar
     (let ((result (make-grid-char)))
       (set-grid-char result grapheme)
       result))
    (character
     (if rendition ;; @@@
	 (make-grid-char :c grapheme
			 :fg (fatchar-fg rendition)
			 :bg (fatchar-bg rendition)
			 :attrs (fatchar-attrs rendition))
	 (make-grid-char :c grapheme)))))

;; @@@ should probably rename to set-fat-char-from-grid-char or something
(defun set-fat-char (fc gc)
  "Set the fatchar CHAR to a grid-char VALUE."
  (let* ((c (grid-char-c gc))
	 (cc (etypecase c
	       (character c)
	       (string (if (>= (length c) 1) (char c 0) nil))
	       (null (code-char 0))))) ;; @@@ o'really?
    ;; (assert (characterp cc))
    (setf (fatchar-c fc)     cc
	  (fatchar-fg fc)    (grid-char-fg gc)
	  (fatchar-bg fc)    (grid-char-bg gc)
	  (fatchar-attrs fc) (grid-char-attrs gc)
	  (fatchar-line fc)  (grid-char-line gc))
    fc))

(defun grid-char-character-length (c)
  "Return the length of a grid-char in characters."
  (if c
      (etypecase (grid-char-c c)
	(null 0)
	(character 1)
	(string (length (grid-char-c c))))
      0))

(defun grid-string-character-length (s)
  "Return the length of a grid-char string in characters."
  (etypecase s
    (null 0)
    (grid-char (grid-char-character-length s))
    (vector
     (loop :for g :across s :sum
	(grid-char-character-length g)))))

#|
;; @@@ no-nulls seems broken at least w/regard to *-line
(defun grid-to-fat-string (s &key (start 0) end no-nulls)
  "Return a fat-string equivalent to S. S can be a grid-string or a grid-char."
  ;; (with-output-to-fat-string (str)
  ;;   (let ((fc (make-fatchar)))
  ;;     (flet ((print-it (c)
  ;; 	       (set-fat-char fc c)
  ;; 	       (if (characterp (grid-char-c c))
  ;; 		   (progn
  ;; 		     (setf (fatchar-c fc) (grid-char-c c))
  ;; 		     (princ fc str))
  ;; 		   (loop :for j :from 0 :below (length (grid-char-c c))
  ;; 		      :do
  ;; 		      (setf (fatchar-c fc) (aref (grid-char-c c) j))
  ;; 		      (princ fc str)))))
  ;; 	(if (grid-char-p s)
  ;; 	    (print-it s)
  ;; 	    (loop :for i :from start :below (or end (length s))
  ;; 	       :do (print-it (aref s i))))))))
  (let* ((len (grid-string-character-length s))
	 (result (make-array len
			     :element-type 'fatchar
			     :initial-element (make-fatchar)))
	 (j 0))
z    (flet ((add-grid-char (char)
	     "Add CHAR to the result."
	     (etypecase (grid-char-c char)
	       (null
		;; Ignore it unless it has other data.
		;; @@@ It this reasonable?
		(when (and (or (grid-char-fg    char)
			       (grid-char-bg    char)
			       (grid-char-attrs char)
			       (not (zerop (grid-char-line char))))
			   (not no-nulls))
		  (setf (aref result j)
			(make-fatchar :fg    (grid-char-fg    char)
				      :bg    (grid-char-bg    char)
				      :attrs (grid-char-attrs char)
				      :line  (grid-char-line  char)))
		  (incf j)))
	       (character
		(when (or (not no-nulls)
			  (char/= (grid-char-c char) #.(code-char 0)))
		  (setf (aref result j)
			(make-fatchar :c     (grid-char-c char)
				      :fg    (grid-char-fg    char)
				      :bg    (grid-char-bg    char)
				      :attrs (grid-char-attrs char)
				      :line  (grid-char-line  char)))
		  (incf j)))
	       (string
		(loop :for c :across (grid-char-c char)
		   :do
		     (when (or (not no-nulls)
			       (char/= (grid-char-c char) #.(code-char 0)))
		       (setf (aref result j)
			     (make-fatchar :c c
					   :fg    (grid-char-fg    char)
					   :bg    (grid-char-bg    char)
					   :attrs (grid-char-attrs char)
					   :line  (grid-char-line  char)))
		       (incf j)))))))
    (etypecase s
      (null result)
      (grid-char (add-grid-char s))
      (vector
       (loop
	  :for i :from start :below (or end (length s))
	  :do
	  (add-grid-char (aref s i)))))
    (make-fat-string :string result))))
|#

(defun grid-to-fat-string (s &key (start 0) end null-as keep-nulls)
  "Return a fat-string equivalent to S. S can be a grid-string or a grid-char."
  (let* ((len (length s)) ;; (grid-string-character-length s))
	 (result (make-array len
			     :element-type 'fatchar
			     :initial-element (make-fatchar)))
	 (j 0)
	 output-start output-end)
    (flet ((add-grid-char (i char)
	     "Add CHAR to the result."
	     (etypecase (grid-char-c char)
	       (null
		;; Ignore it unless it has other data.
		;; @@@ It this reasonable?
		(when (or (grid-char-fg    char)
			  (grid-char-bg    char)
			  (grid-char-attrs char)
			  (not (zerop (grid-char-line char)))
			  output-start)
		  (when (not output-start)
		    (setf output-start i))
		  (setf (aref result j)
			(make-fatchar :fg    (grid-char-fg    char)
				      :bg    (grid-char-bg    char)
				      :attrs (grid-char-attrs char)
				      :line  (grid-char-line  char)
				      :c null-as))
		  (when (not output-end)
		    (setf output-end j))
		  (incf j)))
	       (character
		(when (or output-start
			  ;; (char/= (grid-char-c char) #.(code-char 0)))
			  (or (char/= (grid-char-c char) #.(code-char 0))
			      keep-nulls))
		  (when (not output-start)
		    (setf output-start i))
		  (setf (aref result j)
			(make-fatchar :c     (or (grid-char-c char) null-as)
				      :fg    (grid-char-fg    char)
				      :bg    (grid-char-bg    char)
				      :attrs (grid-char-attrs char)
				      :line  (grid-char-line  char)))
		  (when (not output-end)
		    (setf output-end
			  (if (grid-char-c char) nil j)))
		  (incf j)))
	       (string
		(loop :for c :across (grid-char-c char)
		   :do
		   (when (or output-start
			     (char/= (grid-char-c char) #.(code-char 0))
			     keep-nulls)
		     (when (not output-start)
		       (setf output-start i))
		     (setf (aref result j)
			   (make-fatchar :c     (or c null-as)
					 :fg    (grid-char-fg    char)
					 :bg    (grid-char-bg    char)
					 :attrs (grid-char-attrs char)
					 :line  (grid-char-line  char)))
		     (when (not output-end)
		       (setf output-end
			     (if c nil j)))
		     (incf j)))))))
      (etypecase s
	(null result)
	(grid-char (add-grid-char 0 s))
	(vector
	 (loop
	    :for i :from start :below (or end (length s))
	    :do
	    (add-grid-char i (aref s i)))))
      (if (not output-start)
	  (values (make-fat-string :string (vector)) nil)
	  (values (make-fat-string :string (if keep-nulls
					       result
					       (subseq result 0 output-end)))
		  output-start)))))

;; @@@ This is probably only for debugging
(defun fat-string-to-grid-string (fs)
  "Make a grid-char string from a fat-string."
  (coerce
   (loop :for c :in (graphemes fs)
      :collect (grapheme-to-grid-char c))
   'vector))

;; (defmethod osimplify ((thing grid-string))
;;   (fat-string-to-string thing))

(defmethod osimplify ((thing grid-char))
  (grid-char-c thing))

(defmethod print-object ((obj grid-char) stream)
  "Print a FATCHAR to a FAT-STRING-OUTPUT-STREAM."
  ;;(format t "stream is a ~a ~a~%" (type-of stream) stream)
  (cond
    ((or *print-readably* *print-escape*)
     ;; Print as a structure:
     ;;(dbugf :crunch "NOPE ~s~%" (type-of obj))
     ;;(print-unreadable-object (obj stream :identity t :type t))
     (call-next-method obj stream)
     )
    ((typep stream 'terminal:terminal-stream)
     ;;(format t "BLURB~s~%" (type-of obj)) (finish-output)
     (let ((str (grid-to-fat-string obj)))
       (render-fat-string str)))
    ((typep stream 'fat-string-output-stream)
     ;;(dbugf :crunch "BLURB Good ~s~%" (type-of obj))
     (let ((str (grid-to-fat-string obj)))
       (write-fat-string str :stream stream)))
    (t
     ;; (dbugf :crunch "NLURB not so good ~s ~s~%"
     ;; 	    (type-of obj) (type-of stream))
     (let ((str (grid-to-fat-string obj)))
       (write (fat-string-to-string str) :stream stream))
     )))

(defun unset-grid-char (c)
  "Make a grid-char unset."
  (setf (grid-char-c     c)	nil
	(grid-char-fg    c)	nil
	(grid-char-bg    c)	nil
	(grid-char-line  c)	0
	(grid-char-attrs c)	nil))

;; Should things like this be in ochar?
;; Or should grid-char and fatchar be classes????
(defgeneric any-char-c (c)
  (:method ((c character)) c)
  (:method ((c fatchar)) (fatchar-c c))
  (:method ((c grid-char)) (grid-char-c c)))

(defgeneric any-char-fg (c)
  (:method ((c character)) nil)
  (:method ((c fatchar)) (fatchar-fg c))
  (:method ((c grid-char)) (grid-char-fg c)))

(defgeneric any-char-bg (c)
  (:method ((c character)) nil)
  (:method ((c fatchar)) (fatchar-bg c))
  (:method ((c grid-char)) (grid-char-bg c)))

(defgeneric any-char-line (c)
  (:method ((c character)) nil)
  (:method ((c fatchar)) (fatchar-line c))
  (:method ((c grid-char)) (grid-char-line c)))

(defgeneric any-char-attrs (c)
  (:method ((c character)) nil)
  (:method ((c fatchar)) (fatchar-attrs c))
  (:method ((c grid-char)) (grid-char-attrs c)))

;; EOF
