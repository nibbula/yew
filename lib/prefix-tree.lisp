;;;
;;; prefix-tree.lisp - Prefix tree data structure.
;;;

(defpackage :prefix-tree
  (:documentation
   "This provides a prefix tree data structure. Otherwise know as a trie.
This is not an space or time efficient version. But at least it's relatively
simple. It's stored as conses, characters, strings, and an :end keyword.
Summary:
  - Make a new tree from a word list:

      (make-trie word-list) => tree

  - Test if a word is in the tree:

      (lookup word tree) => boolean

  - Return a list of words in the tree:

      (word-list tree) => <a list of string>

    This also works on sub-trees.

  - Return a list of endings for a prefix:

    (endings string) => <a list of possible endings for string>
")
  (:use :cl :dlib :dlib-misc)
  (:export
   #:make-trie
   #:lookup
   #:tree-list
   #:endings
   ))
(in-package :prefix-tree)

;; @@@ This should be fixed to do radix style with string branches!
;; Single branch nodes typically waste a lot of space.

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defun trieify (list)
  "Trim one layer off a word list."
  (let ((result (sort (copy-list list) #'string>)))
    (if (cadr result) ;; aka (> (length result) 1)
	(flet ((snip-first (l)
		 (mapcar (_ (if (= 1 (length _))
				:end
				(subseq _ 1)))
			    l)))
	  (loop
	    :for thing :in (group-by-alist
			    (_ (if (stringp _) (char _ 0) _))
			    result)
	    :if (eq (car thing) :end)
	      :collect :end
	    :else
              :collect
	      (cons (first thing) (snip-first (rest thing)))))
	result)))

(defun make-trie (word-list)
  "Make a word list into a trie."
  (let ((result (trieify word-list)))
    (loop :for e :in result
	  :when (and (consp e) (caddr e))
	    :collect (cons (first e) (make-trie (rest e)))
          :else
            :collect e)))

(defun find-node (letters tree)
  "Return the part of ‘tree’ that matches the list of ‘letters’."
  (let* (sub-string
	 (branch
	   (find-if
	    (_ (typecase _
		 (character
		  (if (null (car letters))
		      (return-from find-node tree) ; nil
		      (char= _ (car letters))))
		 (symbol
		  ;; (assert (eq _ :end))
		  (when (null letters)
		    (return-from find-node tree)))
		 (string
		  (if (equal _ (or sub-string
				   (setf sub-string
					 (coerce letters 'string))))
		      (return-from find-node tree)
		      nil))
		 (t
		  (error "Unexpected ~s in tree ~s."
			 (type-of _) _))))
	    tree :key (_ (if (consp _) (car _) _)))))
    (if (null branch)
	(return-from find-node (if (null (rest letters)) tree nil))
	(find-node (rest letters) (rest branch)))))

;; @@@ We should probably change lookup to use find-node.

(defun lookup (word tree)
  "Return true if ‘word’ is in ‘tree’."
  ;; (declare
  ;;  (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((letters (coerce word 'list)))
    (labels ((find-in-tree (letters sub-tree)
	       (let* (sub-string
		      (branch
			(find-if
			 (_ (typecase _
			      (character
			       ;; If there's no more letters, we need a :end.
			       (if (null (car letters))
				   nil
				   (char= _ (car letters))))
			      (symbol
			       ;; (assert (eq _ :end))
			       ;; An :end with no more letters means it's here.
			       (when (null letters)
				 (return-from lookup t)))
			      (string
			       ;; Save our current sub-string for matching
			       ;; next time.
			       (if (equal _ (or sub-string
						(setf sub-string
						      (coerce letters 'string))))
				   ;; The sub-string matchs our remaining
				   ;; sub-string, so we're done.
				   (return-from lookup t)
				   nil))
			      (t
			       (error "Unexpected ~s in tree ~s."
				      (type-of _) _))))
			 sub-tree
			 ;; It's either a (letter sub-tree...) or string or :end
			 :key (_ (if (consp _) (car _) _)))))
		 ;; (if (or (null branch) (null (rest letters)))
		 (if (null branch)
		     (return-from lookup nil)
		     (find-in-tree (rest letters) (rest branch))))))
      (find-in-tree letters tree))))

(defun tree-list (tree)
  "Return a list of words in ‘tree’."
  (labels ((tree-words (prefix tree)
	     (loop :with result = '()
	       :for branch :in tree :do
		 (etypecase branch
		   (symbol
		    (assert (eq branch :end))
		    (push (coerce prefix 'string) result))
		   (string
		    (push (concatenate 'string (coerce prefix 'string) branch)
			  result))
		   (cons
		    (dolist (l (tree-words (append prefix (list (car branch)))
					   (cdr branch)))
		      (push l result))))
		 :finally (return (nreverse result)))))
    (tree-words '() tree)))

(defun endings (word tree)
  (tree-list (find-node (coerce word 'list) tree)))

;; head -Cq -n 20 /usr/share/dict/words | ez-trie:make-tree | cons "foo" | tb:view-tree

;; End
