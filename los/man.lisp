;;;
;;; man.lisp - Show a manual entry.
;;;

;; Note that this just uses an already existing system "man" command.
;; Otherwise we would have to implement a lot of crap, like troff.
;; This is just so we can have our own command with completion and
;; use our pager.

(defpackage :man
  (:documentation "Show a manual entry.")
  (:use :cl :dlib :dlib-misc :opsys :glob :grout :lish :pager :pick-list
	:table :collections :fatchar-io :tree-viewer :view-generic)
  (:export
   #:*man-sections*
   #:get-manual-sections
   #:arg-manual-section
   #:*man-entries*
   #:get-manual-entries
   #:arg-manual-entry
   #:!man
   ))
(in-package :man)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defvar *man-sections* nil
  "Cached manual sections. Set it to NIL again to recalculate them.")

(defparameter *prog* "/usr/bin/man"
  "The man program we use.")

;; @@@ resolve vs. lish::manpath
(defun manpath ()
  "Return the manual path."
  (or (nos:environment-variable "MANPATH")
      (and (nos:command-pathname "manpath") (!$ "manpath"))))

(defun manpath-list ()
  (let ((p (manpath)))
    (or
     ;; First try a manpath from the the environment or the manpath command.
     (and p (split-sequence #\: p))
     ;; Try to read it from /etc/man.conf
     (and (nos:file-exists "/etc/man.conf")
	  (lish::expand-braces
	   (second
	    (split-sequence
	     '(#\space #\tab)
	     (oitem 0 (grep:grep "^_default" "/etc/man.conf"
				 :collect t :quiet t))
	     :bag t))))
     ;; Fallback to a reasonable? default
     '("/usr/share/man" "/usr/local/man")
     ;; #-windows ;; Don't bother warning on windows.
     ;; (warn "Can't figure out the where the manuals are.")
     )))

(defun get-manual-sections ()
  "Approximate and flawed method for finding manual sections."
  (or *man-sections*
      (setf *man-sections*
	    (remove-duplicates
	     (loop :for d :in (manpath-list)
		;; :do (format t "  ~a " d) (finish-output)
		:append
		(mapcar (_ (remove-prefix (nos:basename _) "man"))
			(glob (s+ d "/man*/"))))
	     :test #'equal))))

;; (defclass arg-manual-section (arg-lenient-choice)
;;   ()
;;   (:default-initargs
;;    :choice-func #'get-manual-sections)
;;   (:documentation "Manual section."))
(defargtype manual-section (arg-lenient-choice)
  "Manual section."
  ()
  (:default-initargs
   :choice-func #'get-manual-sections))

(defparameter *suffixes* #(".gz" ".bz2" ".z" ".Z" ".F" ".Y" ".bz" ".xz" ".7z"))

(defun remove-compression-suffix (name)
  (or (loop :for s :across *suffixes* :do
        (when (ends-with s name)
	  (return (remove-suffix name s))))
      name))

(defun remove-man-page-suffixes (name &key keep-section-p)
  "Desperately try to get rid of man page suffixes on NAME."
  (let ((c (count #\. name)) matches)
    (case c
      (0 name)
      (1
       (if keep-section-p
	   (remove-compression-suffix name)
	   (subseq name 0 (position #\. name))))
      (t
       (let ((result name))
	 (setf result (remove-compression-suffix name))
	 (when (not keep-section-p)
	   (loop :for s :in (get-manual-sections) :do
	      (when (ends-with (s+ #\. s) result)
		(setf result (remove-suffix result (s+ #\. s)))))
	   (when (setf matches (ppcre:all-matches "\\.[1-9][^.]*$" result))
	     (setf result (subseq result 0 (car matches)))))
	 result)))))

(defun section-from-file-name (file-name)
  "Return the section name from the FILE-NAME."
  (let* ((base (remove-man-page-suffixes (nos:path-file-name file-name)
					 :keep-section-p t))
	 (matches (ppcre:all-matches "\\.([1-9][^.]*)$" base)))
    (if matches
	(subseq base (1+ (car matches)))
	(multiple-value-bind (s e ss ee)
	  (ppcre:scan "(^[^.]*)\\." base)
	  (declare (ignore s e ss))
	  (subseq base (1+ (aref ee 0)))))))

(defvar *man-entries* nil
  "Cached manual entries. Set it to NIL again to recalculate them.")

(defvar *man-entries-table* nil
  "Cached manual entries hash table. Keys are the entry name. Values are the
file or list of files.")

(defparameter *man-tree-init* '("Manuals")
  "Initial value for the manual browsing tree.")

(defvar *man-tree* (copy-list *man-tree-init*)
  "Tree of sections and entries.")

(defun get-manual-entries (&key rehash)
  "Approximate and flawed method for finding manual entries."
  (when rehash
    (setf *man-sections* nil
	  *man-entries* nil
	  *man-tree* (copy-list *man-tree-init*)))
  (or *man-entries*
      (progn
	;; (format t "Getting manual entries...~%") (finish-output)
	(with-spin ()
	  (setf *man-entries-table* (make-hash-table :test #'equal)
	        *man-entries*
		(loop :for d :in (manpath-list)
		   ;; :do (format t "  ~s~%" d) (finish-output)
		   :append
		   (loop :with result :and section
		      :for s :in (get-manual-sections)
		      :do
		      ;;(format t "   ~s ~%" s) (finish-output)
		      (spin)
		      (setf section (list s))
		      (pushnew section (cdr *man-tree*)
			       :key #'car :test #'equal)
		      (setf section (find s (cdr *man-tree*)
					  :key #'car :test #'equal))
		      (setf result
			    (loop :with key :and val
			       :for ff :in
			       (glob (s+ d *directory-separator* "man"
					 s *directory-separator* "*"))
			       ;;:do (format t "  --> ~s~%" ff)
			       :do
			       (setf key (remove-man-page-suffixes
					  (basename ff))
				     val (gethash key *man-entries-table*))
			       (pushnew key (cdr section) :test #'equal)
			       ;; (setf (cdr (cdr sec
			       (if val
				   (if (listp val)
				       (push ff
					     (gethash key *man-entries-table*))
				       (setf (gethash key *man-entries-table*)
					     (list val ff)))
				   (setf (gethash key *man-entries-table*) ff))
			       :collect key))
		      :append result)
		   ;; :do (terpri)
		   )))
	(setf (cdr *man-tree*)
	      (sort-muffled (cdr *man-tree*) #'string-lessp :key #'car))
	(loop :for section :in (cdr *man-tree*)
	      :do (setf (cdr section) (sort-muffled (cdr section)
						    #'string-lessp)))
	;;(format t "  Sorting...~%") (finish-output)
	(setf *man-entries* (sort-muffled *man-entries* #'string-lessp))
	;;(format t "  De-duping...~%") (finish-output)
	(setf *man-entries* (remove-duplicates *man-entries* :test #'equal))
	*man-entries*)))

(defun man-description (entry section)
  "Return a one line description for ENTRY in SECTION."
  (ppcre:regex-replace "^[^-]*-[ ]*"
		       #-netbsd
		       (!$= *prog* "-s" section "-f" entry)
		       #+netbsd
		       (find (s+ entry "(" section ")")
			     (!_= "whatis" entry) :test #'begins-with)
		       ""))

(defun ask-which (entry table-entry)
  "Ask the user which one ENTRY they want from the ones in TABLE-ENTRY.
Return a list of (<entry> <section>), or just ENTRY if something goes wrong."
  (let* ((sections (mapcar #'section-from-file-name table-entry))
	 (tab
	  (make-table-from
	   (loop :for sect :in sections
	      :collect
	      (list
	       entry
	       sect
	       (man-description entry sect)))
	   :column-names '("Name" "Section" "Description")))
	 (lines (osplit
		 #\newline
		 (fatchar:process-ansi-colors
		  (terminal:with-terminal-output-to-string ()
		    (table-print:print-table
		     tab
		     :stream terminal:*terminal*
		     :renderer
		     (make-instance 'terminal-table:terminal-table-renderer)
		     #| :print-titles nil |#
		     )))
		 :omit-empty t))
	 (item-no
	  (pick-list (cdr lines)
		     :message
		     (fs+ "There is more than one entry for " entry "." #\newline
			  "Which one do you want?" #\newline #\newline
			  "  " (first lines) #\newline)
		     :by-index t
		     ;; :popup t
		     )))
    (if item-no
	;; (s+ entry "." (elt sections item-no))
	(list entry (elt sections item-no))
	entry)))

(defargtype manual-entry (arg-lenient-choice)
  "Manual entry."
  ()
  (:default-initargs
   ;; :test #'arg-choice-compare
   :test #'string=
   :choice-func #'get-manual-entries))

(defun section-p (string)
  "Return true if the ‘string’ matches a manual section."
  (find-if (_ (begins-with _ string)) man::*man-sections*))

(defclass manual-node (object-node)
  ((section :initarg :section :accessor manual-node-section :initform nil)))

(defmethod view ((node manual-node))
  (dbugf :foo "chowza ~s ~s~%" node (type-of node))
  (!man :entry (node-object node) :section (manual-node-section node)))

(defun view-manuals ()
  (view-tree (convert-tree *man-tree* :type 'manual-node)))

;; @@@ This needs work
(defcommand man
   ((apropos   string :short-arg #\k :help "Search page titles for a string.")
    (section   manual-section :short-arg #\s :help "Show page from section.")
    (all       boolean :short-arg #\a :help "Show all matching pages.")
    (rehash    boolean :short-arg #\r :help "Re-calculate caches.")
    (use-pager boolean :short-arg #\p :default t
     :help "True to view the output in the pager.")
    (path string :short-arg #\M
     :help "Colon separated path to search for pages.")
    ;; (entry manual-entry :optional t :help "Name of manual entry.")
    (entry manual-entry :rest t :help "Name of manual entry."))
  "Display a manual page for a command. With -k search for a matching page."
  :accepts '(arg-manual-entry)

  (when (and *input* (not entry))
    (setf entry (if (listp *input*) *input* (list *input*))))

  ;; Try to pull out a section from the entries.
  (when (> (length entry) 1)
    (cond
      ((or (section-p (first entry))
	   (integerp (first entry)))
       (setf section (first entry))
       (pop entry))
      ((or (section-p (second entry))
	   (integerp (second entry)))
       (setf section (second entry)
	     entry (first entry)))))

  (when rehash
    (setf *man-entries* (get-manual-entries :rehash t)))

  (when (and (listp entry) (= 1 (length entry)))
    (setf entry (first entry)))

  (when (not (or entry apropos))
    ;; (format t "But what manual page do you want?~%")
    (when (not rehash)
      (view-manuals))
    (return-from !man (values)))

  (let* ((base (s+ *prog* " " #-(or netbsd openbsd) "-P cat "))
	 (cmd (make-array (list (length base))
			  :element-type 'character
			  :adjustable t
			  :fill-pointer t))
	 (cols (princ-to-string (with-grout () (grout-width))))
	 (table-entry (when (and entry (not apropos))
			(gethash entry *man-entries-table*)))
	 stream)
    #-(or linux freebsd openbsd) (declare (ignorable cols))

    (when (and table-entry (listp table-entry) (> (length table-entry) 1)
	       (not section))
      (when (consp (setf entry (ask-which entry table-entry)))
	(psetf entry (first entry)
	       section (second entry))))

    (setf (fill-pointer cmd) 0)
    (with-output-to-string (str cmd)
      #+(or linux freebsd)
      ;; (format str "env MANWIDTH=~d MAN_KEEP_FORMATTING=t ~a " cols base)
      (progn
	(setf (nos:environment-variable "MANWIDTH") cols
	      (nos:environment-variable "MAN_KEEP_FORMATTING") "t"
	      (nos:environment-variable "MANPAGER") "cat")
	(princ base str))
      #-(or linux freebsd) (princ base str)
      ;;#+linux (princ "-Tutf8 " str) ; force fancy text
      (when apropos (format str "-k ~a " apropos))
      (when all     (format str "-a "))
      #+openbsd (format str "-O width=~a " cols)
      (when section (format str "~a " section))
      (when path    (format str "-M ~a " path)))
    (unwind-protect
	 (if (or (out-pipe-p) (not use-pager))
	     ;; (! cmd (or entry ""))
	     (pipe (s+ cmd (or entry "")) "cat")
	     (progn
	       (setf stream (!! cmd (or entry "")))
	       (pager:pager stream)))
      (when stream
	(close stream)))))

;; @@@ This is only here because manual-entry completion fails for the above
(defcommand crap
  ((use-pager boolean :short-arg #\p :default t
   :help "True to view the output in the pager.")
   (crap manual-entry :help "Name of the manual entry."))
  "Very crap"
  (let* (#+(or linux freebsd openbsd)
	   (cols (princ-to-string (with-grout () (grout-width))))
	 stream)
    ;;(setf (nos:environment-variable "COLUMNS") cols)
    #+(or linux freebsd) (setf (nos:environment-variable "MANWIDTH") cols)
    (unwind-protect
	 (if (or (out-pipe-p) (not use-pager))
	     ;; (! "/usr/bin/man -P cat " (or crap ""))
	     (pipe (s+ *prog* " " #+openbsd (s+ "-O width=" cols " ") (or crap "")) "cat")
	     (progn
	       (setf stream (!! *prog* #-openbsd " -P cat "
				       #+openbsd (s+ " -O width=" cols " ")
				(or crap "")))
	       (pager:pager stream)))
      (when stream
	(close stream)))))

;; EOF
