;;;
;;; view-html.lisp - View HTML as a tree.
;;;

;; This uses the excellent PLUMP library to read HTML/XML and the tree-viewer
;; to display it. Using the TREE-VIEWER generally consists of making node
;; type subclasses and display methods for them. Here we make one HTML-NODE
;; subclass of the tree-viewer's CACHED-DYNAMIC-NODE and use PLUMP methods to
;; dynamically pull out content from the parsed file. We make the PLUMP node
;; be the tree-viewer's OBJECT-NODE object.

;; TODO:
;;  - hook into pager to:
;;    - view as text
;;    - view as source text

(defpackage :view-html
  (:documentation "View HTML as a tree.")
  (:use :cl :dlib :dlib-misc :dtime :rl :inator :file-inator :tree-viewer
	:terminal :fui :char-util :keymap :rl-widget :collections
	:terminal-inator :ostring :parse-util :magic)
  (:export
   #:view-html
   #:*user-agent*
   ))
(in-package :view-html)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-windows (d-add-feature :use-drakma))

#+use-drakma
(defvar *user-agent* "Drakma"
  "User-agent for requests, defaulting not to spilling too much information.")

(defvar *html-viewer* nil
  "The current HTML viewer.")

(defun html-node-contents (node)
  "Get the contents of an HTML-NODE, which is a list of PLUMP node children."
  (when (and node
	     (typep node 'plump::node)
	     (plump::has-child-nodes node))
    ;; The
    (map 'list #'identity
	 (remove-if (lambda (x)
		      (or (and (or (plump:text-node-p x)
				   (plump:textual-node-p x))
			       (= (length (dlib:trim (plump:text x))) 0))
			  (and (not (tree-view-show-comments *viewer*))
			       (plump:comment-p x))))
		    (plump::children node)))))

(defclass html-node (cached-dynamic-node)
  ()
  (:default-initargs
   :func #'html-node-contents)
  (:documentation "A tree node for plump HTML."))

(defstruct history-node
  "Record the place and time."
  resource
  time)

(defclass location-node (object-node)
  ((parent
    :initarg :parent :accessor location-node-parent
    :documentation "Parent node in the location tree."))
  (:documentation "Node in the location tree. The object is the resource name."))

(defclass view ()
  ((switch-view-p
    :initarg :switch-view-p :accessor switch-view-p :initform nil :type boolean
    :documentation "True to switch views instead of exiting."))
  (:documentation "A view of a thing."))

(defgeneric view-loop (view)
  (:documentation "Enter the viewing event loop. Return NIL to exit."))

(defgeneric view-document (view)
  (:documentation "Initialize VIEW to view the loaded document."))

(defgeneric reset-view (view)
  (:documentation "Reset the view for viewing a different tree."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree view

(defclass tree-view (view tree-viewer)
  ((show-comments
    :initarg :show-comments :accessor tree-view-show-comments
    :initform nil :type boolean
    :documentation "True to show comments."))
  (:documentation "View the document as a tree."))

(defkeymap *view-html-ctrl-x-keymap* ()
  `((,(ctrl #\s)	. save-file)
    (,(ctrl #\f)	. load-file)))

(defkeymap *view-html-keymap* ()
  `((,(ctrl #\x)	. *view-ctrl-x-keymap*)
    (#\h		. history-command)
    (#\b		. back-command)
    (#\v		. visit-thing-command)
    (#\c		. toggle-show-comments)
    (#\t		. toggle-view)
    (#\return		. ask-url-command)
    ))

(defmethod initialize-instance
    :after ((o tree-view) &rest initargs &key &allow-other-keys)
  "Initialize a tree-view."
  (declare (ignore initargs))
  ;; Add our own keymap.
  (when (not (find *view-html-keymap* (inator-keymap o)))
    (push *view-html-keymap* (inator-keymap o))))

(defmethod view-loop ((view tree-view))
  ;; (view-tree (html-viewer-document *viewer*) :viewer view)
  (view-tree (tree-viewer::root view) :viewer view)
  (switch-view-p view))

;; (defun make-tree-from-document (location document parsed-document)
(defun make-tree-from-document (location document)
  "Make a tree-viewer tree from a parsed plump document."
  (make-instance
   'object-node
   ;; :object (or (and location (princ-to-string location))
   ;; 	       (princ-to-string document))
   :object (or (and location (princ-to-string location))
	       ;; (princ-to-string document)
	       "What ???"
	       )
   :branches
   (loop :for n :in (map 'list #'identity (plump::children document))
      :if (or (not (or (plump:text-node-p n)
		       (plump:textual-node-p n)))
	      (> (length (dlib:trim (plump:text n))) 0))
      :collect
      (make-instance 'html-node :object n))))

(defmethod view-document ((view tree-view))
  (with-slots ((root tree-viewer::root)) view
    (with-slots (location document) *html-viewer*
      (setf root (make-tree-from-document (node-object location) document))))
  (reset-view view))

(defmethod reset-view ((view tree-view))
  "Reset the viewer for viewing a different tree."
  (with-slots ((current tree-viewer::current)
	       (root    tree-viewer::root)
	       (top     tree-viewer::top)
	       (parents tree-viewer::parents)) view
    (setf parents (make-hash-table :test #'equal)
	  current root
	  top root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree displaying

(defgeneric display-thing (obj stream)
  (:documentation "Display a plump element OBJ to STREAM."))

(defmethod display-thing ((obj t) stream)
  "Display an anything."
  (format stream "~(~a~) " (type-of obj)))

(defun print-attrubutes (obj stream)
  "Display an anything."
  (loop :for k :being :the :hash-keys :of (plump:attributes obj) :using
     (hash-value v) :do
     (format stream "~a='~a' " k v)))

(defparameter *attr-tags*
  #(:meta :link :script :a :form :input :img :div :span :frame)
  "Tags to print attributes for.")

(defmethod limited-display-thing ((obj plump-dom:element) stream)
  (let ((tag (keywordify (plump:tag-name obj))))
    (format stream "~(~a~) " (plump:tag-name obj))
    (when (position tag *attr-tags*)
      (print-attrubutes obj stream))))

(defmethod display-thing ((obj plump-dom:element) stream)
  (format stream "~(~a~) " (plump:tag-name obj))
  (print-attrubutes obj stream))

(defmethod display-thing ((obj plump-dom:doctype) stream)
  (format stream "~(~a~) ~a" (type-of obj) (plump-dom:doctype obj)))

(defmethod print-object ((object html-node) stream)
  "Print an html-node to STREAM."
  (let ((obj (node-object object)))
    (if (or *print-readably* *print-escape*)
	(print-unreadable-object (object stream)
	  (format stream "html-node ~w" obj))
	(let ((*standard-output* stream))
	  (when (or (plump:text-node-p obj) (plump:textual-node-p obj))
	    (justify-text (dlib:trim (plump:text obj)) :stream stream))))))

(defmethod display-node ((node html-node) level)
  "Display an HTML node."
  (let* ((obj (node-object node))
         (str (with-output-to-string (stream)
                (cond
                  ((plump:text-node-p obj)
		   (justify-text
		    (dlib:trim (plump:text obj))
		    :stream stream
		    ;;:prefix (make-string level :initial-element #\space)
		    ))
                  ((plump:comment-p obj)
		   (format stream "Comment ")
		   (justify-text
		    (dlib:trim (plump:text obj))
		    :stream stream
		    ;;:prefix (make-string level :initial-element #\space)
		    ))
                  ((plump:textual-node-p obj)
                   ;;(format stream "~a" (dlib:trim (plump:text obj))))
                   (format stream "textual-node-p~%")
		   (justify-text
		    (dlib:trim (plump:text obj))
		    :stream stream
		    ;;:prefix (make-string level :initial-element #\space)
		    ))
                  (t
                   (display-thing obj stream))))))
    (display-object node str level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text view

(defstruct blok ;; @@@ maybe change to text-block
  x y z width height
  background
  lines)

(defstruct link
  text
  url)

(defkeymap *text-view-keymap* ()
  `((#\q		. quit)
    (#\Q		. quit)
    (#.(ctrl #\C)	. quit)
    (#\t		. toggle-view)
    (#\space		. next-page)
    (#\n		. next-link)
    (,(ctrl #\N)	. next-line)
    (:down		. next-line)
    (#\p		. previous-link)
    (,(ctrl #\P)	. previous-line)
    (:up		. previous-line)
    (#\l		. pick-link)
    ;; (#\N		. next-hierarchical-node)
    ;; (#\P		. previous-hierarchical-node)
    (,(ctrl #\F)	. forward-some)
    (,(ctrl #\B)	. backward-some)
    (,(ctrl #\V)	. next-page)
    (:npage		. next-page)
    (:ppage		. previous-page)
    (,(meta-char #\v)	. previous-page)
    ;; (#\<		. goto-first-node)
    ;; (,(meta-char #\<)	. goto-first-node)
    ;; (:home		. goto-first-node)
    ;; (#\>		. goto-bottom-node)
    ;; (,(meta-char #\>)	. goto-bottom-node)
    ;; (:end		. goto-bottom-node)
    ;; (:left		. shift-left)
    ;; (:right	     	. shift-right)
    ;; (,(ctrl #\A)	. shift-beginning)
    ;; (,(ctrl #\E)	. shift-end)
    (,(ctrl #\S)	. search-forward-command)
    (#\/		. search-forward-command)
    (,(ctrl #\R)	. search-backward-command)
    ;; Miscellaneous
    (#\m		. toggle-modeline)
    (,(ctrl #\L)	. redraw)
    (#\i		. node-info)
    (#\?		. help)
    (,(meta-char #\n)	. next-file)
    (,(meta-char #\p)	. previous-file)
    (,(meta-char #\=)	. describe-key-briefly)
    (#\escape		. *text-view-escape-keymap*)))

(defparameter *text-view-escape-keymap* (build-escape-map *text-view-keymap*))

(defclass text-view (view terminal-inator)
  ((blocks
    :initarg :blocks :accessor blocks :initform nil
    :documentation "Rendered text blocks.")
   (links
    :initarg :links :accessor links :initform nil
    :documentation "List of links in the text.")
   (title
    :initarg :title :accessor text-view-title :initform nil
    :documentation "Title of the page.")
   (styles
    :initarg :styles :accessor text-view-styles :initform nil
    :documentation "List of style strings.")
   (top
    :initarg :top :accessor text-view-top :initform 0 :type fixnum
    :documentation "First line to display.")
   (left
    :initarg :left :accessor text-view-left :initform 0 :type fixnum
    :documentation "First column to display.")
   (max-line
    :initarg :max-line :accessor text-view-max-line :initform 0 :type fixnum
    :documentation "Line number of the the bottom."))
  (:default-initargs
   :keymap (list *text-view-keymap*
		 inator:*default-inator-keymap*))
  (:documentation "View the document as a text."))

;; (defmethod initialize-instance
;;     :after ((o text-view) &rest initargs &key &allow-other-keys)
;;   "Initialize a text-view."
;;   (declare (ignore initargs))
;;   (when (not (getf initargs :keymap))
;;     (setf (inator-keymap o)
;; 	  (list *text-view-keymap*
;; 		inator:*default-inator-keymap*))))

(defun visible-p (view block)
  (with-slots (top left) view
    (and (< (blok-x block) (tt-width))
	 (>= (+ (blok-x block) (blok-width block)) left)
	 (< (blok-y block) (+ top (tt-height)))
	 (> (+ (blok-y block) (blok-height block)) top))))

(defun draw-block (view block)
  (with-slots (top left) view
    (let* ((start-x (max (- (blok-x block) left) 0))
	   (start-y (max (- (blok-y block) top) 0))
	   (end-x   (+ start-x (min (blok-width block) (tt-width))))
	   (end-y   (+ start-y (min (blok-height block) (tt-height)))))
      (dbugf :html "move ~s ~s ~s ~s~%" start-y start-x end-y end-x)
      (tt-move-to start-y start-x)
      (loop
	 :with sx = (- start-x (blok-x block))
	 :and  ex = (- end-x (blok-x block))
	 :and line
	 :for l :from 0 :below (- end-y start-y)
	 :do
	 (dbugf :html "line ~s ~s ~s~%" sx ex (oelt (blok-lines block) l))
	 (setf line (oelt (blok-lines block) l))
	 (tt-write-string (osubseq line
				   (min sx 0)
				   (min ex (olength line))))))))

(defmethod update-display ((view text-view))
  (with-slots (blocks) view
    (tt-home)
    (tt-erase-below)
    (loop :for block :in blocks
       :do
       (when (visible-p view block)
	 (draw-block view block)))))

(defmethod view-loop ((view text-view))
  (with-terminal ()
    (with-immediate ()
      (with-slots (title) view
	(let ((saved-title (tt-title)))
	  (when saved-title
	    (setf (tt-title) (or title (html-viewer-location *html-viewer*))))
	  ;; Enter the inator event loop.
	  (event-loop view)
	  (when saved-title
	    (setf (tt-title) saved-title))))))
  (switch-view-p view))

(defstruct textify-state
  (x 0)
  (y 0)
  (z 0)
  background
  style
  str
  path)

(defparameter *textify-state* nil
  "State used while rendering to text blocks.")

(defgeneric textify-node (view node)
  (:documentation "Render the NODE in VIEW."))

(defmethod textify-node ((view text-view) (node plump:node))
  "Render the NODE in VIEW."
  ;; Some kind of node we don't care about textifying.
  (dbugf :html "textify-node unknown ~s~%" (type-of node))
  )

(defun new-block (view &key string)
  (with-slots (blocks max-line) view
    (with-slots (x y z str) *textify-state*
      (when str ;; only when there's already been some output
	(finish-output str)
	(let* ((out-str (or string (get-output-stream-string str)))
	       (just (justify-text
		      (ostring-trim *ascii-whitespace* out-str)
		      :cols (tt-width) :stream nil))
	       ;; (lines (split-sequence #\newline just))
	       (lines (osplit #\newline just))
	       (height (length lines))
	       (width (loop :for l :in lines
			    :maximize (char-util:display-length l))))
	  (dbugf :html "new block ~a ~a~%" out-str just)
	  (push (make-blok :x x :y y :z z
			   :width width
			   :height height
			   :background :black ;; @@@
			   :lines lines) blocks)
	  (incf y height)
	  (setf max-line (max y max-line)))))))

(defmacro with-new-block ((view) &body body)
  `(progn
     (new-block ,view)
     (setf (textify-state-str *textify-state*) (make-string-output-stream))
     ,@body))

(defmacro with-tag ((tag) &body body)
  `(unwind-protect ;; Do we really need this?
	(progn
	  (push ,tag (textify-state-path *textify-state*))
	  ,@body)
     (pop (textify-state-path *textify-state*))))

(defmacro with-text-style ((style) &body body)
  `(unwind-protect ;; Do we really need this?
	(progn
	  (push ,style (textify-state-style *textify-state*))
	  ,@body)
     (pop (textify-state-style *textify-state*))))

(defun style-stack-to-style (x stack)
  "Make a style enclosing X out of the STACK of styles."
  (let ((result x))
    (loop :for s :in stack :do (setf result (list s result)))
    result))

(defgeneric do-tag (view node tag)
  (:documentation "Handle a element with TAG."))

(defmethod do-tag (view node tag)
  ;; Some unknown tag.
  (with-tag (tag)
    (omapn (_ (textify-node view  _)) (plump:children node))))

(defmethod do-tag (view node (tag (eql 'title)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node))))
  (with-slots (str path) *textify-state*
    ;; Only set the title when we're inside a head tag.
    (when (find 'head path)
      (setf (text-view-title view)
	    (get-output-stream-string str)))))

(defmethod do-tag (view node (tag (eql 'style)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node))))
  (with-slots (styles) view
    (with-slots (str) *textify-state*
      ;; Collect styles
      (push (get-output-stream-string str) (text-view-styles view)))))

(defmethod do-tag (view node (tag (eql 'span)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node)))))

(defmethod do-tag (view node (tag (eql 'div)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node)))))

(defmethod do-tag (view node (tag (eql 'p)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node)))))

(defmethod do-tag (view node (tag (eql 'a)))
  (with-new-block (view)
    (with-tag (tag)
      (omapn (_ (textify-node view  _)) (plump:children node)))
    (with-slots (links) view
      (with-slots (str) *textify-state*
	(let ((text (get-output-stream-string str)))
	  (push (make-link
		 :text text
		 :url (plump:attribute node "href"))
		links)
	  (new-block view :string (fatchar:span-to-fat-string
				   `(:underline ,text))))))))

(defmethod textify-node ((view text-view) (node plump:element))
  "Render the NODE in VIEW."
  (dbugf :html "textify-node element ~s tag ~s~%" (type-of node)
	 (plump:tag-name node))
  (with-slots (blocks) view
    (do-tag view node (symbolify (plump:tag-name node) :package :view-html))))

(defun add-text (text)
  (dbugf :html "add-text ~s~%" text)
  (with-slots (str) *textify-state*
    (when (not str)
      (setf str (make-string-output-stream)))
    (write-string text str)))

(defmethod textify-node ((view text-view) (node plump:text-node))
  "Textify a text NODE in VIEW."
  (add-text (plump:text node)))

(defmethod textify-node ((view text-view) (node plump:nesting-node))
  "Textify some kind of node that we don't care about the text of, just the
children."
  (omapn (_ (textify-node view _)) (plump:children node)))

(defun textify (view)
  (with-slots (document) *html-viewer*
    (let ((*textify-state* (make-textify-state)))
      (omapn (_ (textify-node view  _)) (plump:children document))
      (new-block view))))

(defmethod view-document ((view text-view))
  (with-slots (blocks links) view
    (textify view)))

(defmethod reset-view ((view text-view))
  "Reset the viewer for viewing a different tree."
  (with-slots (top) view
    (setf top 0)))

(defmethod next-page ((o text-view))
  (with-slots (top) o
    (incf top (- (tt-height) 1))
    (setf top (min top (tt-height)))))

(defmethod previous-page ((o text-view))
  (with-slots (top) o
    (decf top (- (tt-height) 1))
    (setf top (max top 0))))

(defmethod previous-line ((o text-view))
  (with-slots (top) o
    (decf top)
    (setf top (max top 0))))

(defmethod next-line ((o text-view))
  (with-slots (top max-line) o
    (incf top)
    (setf top (min max-line top))))

(defmethod pick-link ((o text-view))
  (with-slots (links) o
    (let ((link (pick-list:pick-list links :by-index t :popup t)))
      (when link
	(visit-location (link-url (oelt links link)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass html-viewer ()
  ((view
    :initarg :view :accessor html-viewer-view :initform nil
    :documentation "The current view.")
   (views
    :initarg :views :accessor html-viewer-views :initform nil
    :documentation "The list of views of the document.")
   (default-view-type
    :initarg :default-view-type :accessor default-view-type
    :initform 'tree-view
    :documentation "Default view type when opening an HTML URL.")
   (document
    :initarg :document :accessor html-viewer-document :initform nil
    :documentation "The parsed document.")
   (visited
    :initarg :visited :accessor html-viewer-visited
    :initform (make-hash-table :test #'equal) :type hash-table
    :documentation "Table of places visited.")
   (history
    :initarg :history :accessor html-viewer-history :initform nil :type list
    :documentation
    "A list of resources vistited. This records a path through the location
tree.")
   (location-tree
    :initarg :location-tree :accessor html-viewer-location-tree :initform nil
    :documentation "A navigation tree of where we've visted.")
   (location
    :initarg :location :accessor html-viewer-location :initform nil
    ;; :type location-node
    :documentation "The current location.")
   (cookies
    :initarg :cookies :accessor html-viewer-cookies :initform nil
    :documentation "The cookie jar."))
  (:documentation "A tree view of HTML/XML."))

(defmethod initialize-instance
    :after ((o html-viewer) &rest initargs &key &allow-other-keys)
  "Initialize a html-viewer."
  (declare (ignore initargs))
  (setf (slot-value o 'cookies) (make-instance 'drakma:cookie-jar)))

(defun add-to-history (uri)
  "Add a location to the linear history."
  (with-slots (visited history) *html-viewer*
    (let ((node (make-history-node :resource uri :time (get-dtime))))
      (setf (gethash uri visited) node)
      (push node history))))

(defun make-location-current (uri &key parent)
  (with-slots (location) *html-viewer*
    (setf location
	  (make-instance 'location-node :object uri :parent parent))))

(defun register-visit (uri &key parent)
  (with-slots (location) *html-viewer*
    (add-to-history uri)
    (make-location-current uri :parent (or parent location))))

(defun relative-uri (relative)
  "Return the full URI from a possibly relative URI."
  (with-slots (location) *html-viewer*
    (let* ((base (node-object location))
	   (base-uri (handler-case (puri:parse-uri base)
		       (puri:uri-parse-error (c)
			 (declare (ignore c))
			 nil)))
	   merged)
      (cond
	((and base-uri (member (puri:uri-scheme base-uri) '(:http :https)))
	 (setf merged (puri:merge-uris relative base-uri))
	 (values (puri:render-uri merged nil) merged))
	((and (not (puri:uri-scheme base-uri)) ; no scheme
	      (nos:file-exists base))
	 ;; @@@ should we even do this???? security issue?
	 (let ((new (nos:path-append (nos:path-directory-name base) relative)))
	   (and (nos:file-exists new)
		(values new nil))))))))


(defparameter *known-media-types*
  '(application audio image message multipart text video)) ; and X-<token>

(defun parse-content-type (content-type)
  (with-parsing (content-type)
    (sequence-of
     (with-sub-sequence (type)
       ;; (note ((list :type (type)))
       (note ((list :type type))
	     (one-or-more (is-not-element #\/))))
     (is-element #\/)
     (optional
      (let (tree-str)
	(sequence-of
	 (with-sub-sequence (tree)
	   (one-or-more (is-not-element #\.))
	   ;; (setf tree-str (tree)))
	   (setf tree-str tree))
	 (is-element #\.)
	 (note ((list :tree tree-str)) t))
       ))
     (with-sub-sequence (sub-type)
       ;; (note ((list :sub-type (sub-type)))
       (note ((list :sub-type sub-type))
	     (one-or-more
	      (when (and (peek) (not (element-in (peek) ".+;")))
		(next-element)))))
     (optional
      (sequence-of
       (is-element #\+)
       (with-sub-sequence (suffix)
         (note ((list :suffix suffix))
	       (one-or-more (not (is-element #\;)))))))
     (let (parameters name value)
       (note (`(:parameters ,(nreverse parameters)))
	     (zero-or-more
	      (sequence-of
	       (is-element #\;)
	       (zero-or-more (is-element #\space))
	       (with-sub-sequence (pname)
		 (one-or-more (is-not-element #\=))
		 (setf name pname))
	       (is-element #\=)
	       (with-sub-sequence (pvalue)
		 (one-or-more (is-not-element #\;))
		 (setf value pvalue))
	       (push `(:name ,name :value ,value) parameters))))))))

(defun content-type-from-string (string)
  (multiple-value-bind (success ct)
      (parse-content-type string)
    (when success
      (magic:make-content-type
       :name (second (assoc :type ct))
       :category (second (assoc :sub-type ct))
       :properties
       (mapcar (_ (cons (symbolify (getf _ :name))
			(getf _ :value))) (second (assoc :parameters ct)))))))

#|
(defun convert-content (content headers)
  ;;; @@@@ pull the charset out of a content-type and convert from the
  ;;l encoding if we can.
  ;; (cond
  ;;   ())
  (let ((ct "text/html; charset=ISO-8859-1"))
    (multiple-value-bind (s e ss ee) (ppcre:scan "charset=(.*)$" ct)
      (declare (ignore e))
      (when (and s ss ee)
	(let ((enc (keywordify (subseq ct (aref ss 0) (aref ee 0)))))
	  (format t "~s~%" enc)
	  (when (find enc (babel:list-character-encodings))
	    (format t "~s~%" enc)))))))
|#

(defun web-get (uri)
  "Get a URI using HTTP or something."
  (multiple-value-bind (content status headers actual-uri stream close-p reason)
      (drakma:http-request uri :user-agent *user-agent*
			   :decode-content t)
    (declare (ignore actual-uri stream close-p headers)) ;; @@@
    (case status
      (200
       (typecase content
	 (string content)
	 ((array (unsigned-byte 8) *)
	  ;; This probably means that flexi-streams couldn't figure out how to
	  ;; decode it, so we try ourselves.
	  ;; @@@@ @@@@@ try finish and use convert-content above
	  ;; @@@ also fix unicode:octets-to-string etc
	  ;; (let ((content-type (cdr (assoc :content-type headers)))
	  ;;   (cond
	  ;;     (equal (initial-span
	  ;; (let ((content-type
	  ;; 	 (content-type-from-string (cdr (assoc :content-type headers)))))
	  ;;   (@@@@))
	  (unicode:utf8-bytes-to-string content))
	 (t content)))			; Just try I guess.
      (t
       (format t "Status code ~s: ~s" status reason)))))

(defun coerce-to-parseable (file)
  "Make FILE into something parseable by PLUMP:PARSE."
  (block nil
    (etypecase file
      (string
       (let* ((path (pathname (nos:quote-filename file)))
	      uri)
	 (or (and path (nos:file-exists path) path)
	     (and (setf uri (handler-case
				(puri:parse-uri file)
			      (puri:uri-parse-error (c)
				(declare (ignore c))
				nil)))
		  (member (puri:uri-scheme uri) '(:http :https))
		  ;; @@@ We should probably check something like:
		  ;; (sb-unicode:confusable-p url
		  ;;   (sb-unicode:canonically-deconfuse url))
		  ;; and some site blacklist or something.
		  #+use-drakma
		  (web-get uri)))))
      ((or pathname stream)
       file)
      (null
       #| (read-filename :prompt "HTML file: ") |#
       (pathname (nos:quote-filename
		  (or (pick-list:pick-file)
		      (return nil))))))))

(defun load-document (location)
  "Load the document at LOCATION."
  ;; (with-slots ((root tree-viewer::root)) *viewer*
  ;;   (let* ((document (or (coerce-to-parseable location)
  ;; 			 (return-from load-document nil)))
  ;; 	   (parsed-document (plump:parse document))
  ;; 	   (tree (make-tree-from-document location document parsed-document)))
  ;;     (setf root tree)
  ;;     (reset-view)
  ;;     ;; (register-visit location)
  ;;     tree)))
  (with-slots (document) *html-viewer*
    (let ((parseable-document (or (coerce-to-parseable location)
				  (return-from load-document nil))))
      (setf document (plump:parse parseable-document)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visiting

(defgeneric visit-thing (thing)
  (:documentation
   "Visit a thing, most likely in a URL, in a way that replaces
the current view with it, but lets us back."))

(defmethod visit-thing ((thing t))
  (display-text
   "What is it?"
   (list (format nil "I don't know how to visit a ~a." (type-of thing)))))

(defmacro external-visit (&body body)
  `(unwind-protect
	(progn
	  (terminal-end *terminal*)
	  (handler-case
	      ,@body
	    (simple-error (c)
	      (message *viewer* "Error: ~a" c))))
     (terminal-start *terminal*)
     (tt-clear)
     (tt-finish-output)))

(defun visit-location (location)
  "Visit a location, probably a URL."
  ;; (view-html location)
  (with-slots (view) *html-viewer*
    (initialize-instance view)
    (load-document location)
    ;; (view-document *viewer*)
    (view-document view)
    (register-visit location)))

(defun preview-visit (thing-type relative location)
  (display-text
   (s+ "Visiting " thing-type)
   (list (format nil "Visiting ~s ~s -> ~a" thing-type relative location)
	 (with-output-to-string (s)
	   (describe (html-viewer-location *html-viewer*) s))
	 (format nil "~s" (node-object (html-viewer-location *html-viewer*)))
	 )))

(defmethod visit-thing ((thing plump-dom:element))
  (case (symbolify (plump:tag-name thing) :package :view-html)
    ((a link)
     (let* ((relative (gethash "href" (plump:attributes thing)))
	    (location (relative-uri relative)))
       (preview-visit "location" relative location)
       (visit-location location)))
    (img
     (let ((relative (gethash "src" (plump:attributes thing))))
       (multiple-value-bind (location real-uri) (relative-uri relative)
	 (preview-visit "image" relative location)
	 ;; (display-text
	 ;;  "Visiting image"
	 ;;  (list (format nil "Visiting image ~s -> ~a" relative location)
	 ;; 	(with-output-to-string (s)
	 ;; 	  (describe (html-viewer-location *html-viewer*) s))
	 ;; 	(s+ real-uri)
	 ;; 	(format nil "~s" (node-object (html-viewer-location *html-viewer*)))))
	 (add-to-history location)
	 (external-visit
	  (view-image:view-image
	   (if (puri:uri-p real-uri)
	       #+use-drakma (drakma:http-request real-uri
						 :user-agent *user-agent*)
	       #-use-drakma nil
	       location))))))
    (script
     (let ((relative (gethash "src" (plump:attributes thing))))
       (multiple-value-bind (location real-uri) (relative-uri relative)
	 (preview-visit "script" relative location)
	 ;; (display-text
	 ;;  "Visiting script"
	 ;;  (list (format nil "Visiting script ~s -> ~a" relative location)
	 ;; 	(with-output-to-string (s)
	 ;; 	  (describe (html-viewer-location *html-viewer*) s))
	 ;; 	(format nil "~s" (node-object (html-viewer-location *html-viewer*)))))
	 (add-to-history location)
	 (external-visit
	  (pager:pager
	   (if (puri:uri-p real-uri)
	       #+use-drakma (drakma:http-request real-uri
						 :user-agent *user-agent*
						 :want-stream t)
	       #-use-drakma nil
	       location))))))
    (otherwise
     (display-text
      "Unknown tag"
      (list (format nil "I don't know how to visit a ~s tag."
		    (plump:tag-name thing)))))))

;; (defmethod visit-thing-command ((o html-viewer))
(defmethod visit-thing-command ((o tree-view))
  (visit-thing (node-object (tree-viewer::current o))))

(defun back-command (o)
  "Load the previously viewed document."
  (declare (ignore o))
  (with-slots (history) *html-viewer*
    (when history
      (pop history) ;; current
      (when history
	(let ((location (history-node-resource (first history))))
	  (load-document location)
	  (make-location-current location)
	  (view-document *viewer*))))))

(defun history-command (o)
  (declare (ignore o))
  (with-slots (history) *html-viewer*
    ;; (display-text
    ;;  "History"
    ;;  (loop :for h :in history
    ;; 	:collect (history-node-resource h)))
    (pick-list:pick-list (loop :for h :in history
			    :when h
			    :collect (history-node-resource h))
			 :popup t)))

(defun ask-url-command (o)
  (declare (ignore o))
  (let ((url (widget-read)))
    (when (and url (not (zerop (length url))))
      (visit-location url))))

(defgeneric toggle-show-comments (viewer)
  (:documentation "Toggle showing comments."))

(defmethod toggle-show-comments ((o tree-view))
  (setf (tree-view-show-comments o) (not (tree-view-show-comments o)))
  (message o "~:[Don't s~;S~]how comments." (tree-view-show-comments o))
  (view-document o))

(defgeneric toggle-view (viewer)
  (:documentation "Toggle between tree and text view."))

(defmethod toggle-view ((o view))
  "Toggle between tree and text view."
  (with-slots (view views) *html-viewer*
    (flet ((other-type (o)
	     (ecase (type-of o)
	       (text-view 'tree-view)
	       (tree-view 'text-view))))
      (dbugf :html "other type ~s~%" (other-type o))
      (setf view (or (find-if (_ (eq (other-type o) (type-of _))) views)
		     (make-view (other-type o))))
      (view-document view)
      (dbugf :html "~s ~s~%" view views)
      (setf (switch-view-p o) t
	    (inator-quit-flag o) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-view (&optional (type (default-view-type *html-viewer*)))
  (with-slots (view views) *html-viewer*
    (setf view (make-instance type))
    (push view views)
    view))

;; Here we parse the whole file with PLUMP and make a tree-viewer tree out
;; of it.

(defun view-html (&optional file)
  (let ((*html-viewer* (or *html-viewer* (make-instance 'html-viewer))))
    (unwind-protect
	 (with-slots (document view views) *html-viewer*
	   (make-view)
	   (load-document file)
	   (make-location-current file)
	   (view-document view)
	   ;; Loop to allow view switching. We exit when view-loop returns false.
	   (loop :while (view-loop view)
	      :do (setf (inator-quit-flag view) nil
			(switch-view-p view) nil)))
      (when (streamp file)
	(close file)))))

#+lish
(lish:defcommand view-html
  ((things pathname :repeating t :help "File or URL to view as HTML."))
  "View HTML as a tree."
  (if (not things)
      (view-html *standard-input*)
      (with-file-list (file things)
	(when (= 1 (length (multiple-value-list (view-html file))))
	  (return)))))

;; EOF
