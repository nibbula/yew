;;
;; theme.lisp - Customize the style.
;;

(defpackage :theme
  (:documentation "Customize the style.")
  (:use :cl :dlib :fatchar)
  (:export
   #:theme
   #:theme-activate
   #:theme-deactivate
   #:theme-value
   #:theme-as-tree
   #:print-theme
   #:make-theme
   #:save-theme
   #:load-theme
   #:*theme*
   #:*file-type-suffixes*
   #:file-suffix-type
   #:set-theme-items
   #:default-theme
   #:set-theme-defaults-for-16-color
   #:default-theme-16-color
   ))
(in-package :theme)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; This is really just a fairly lame word based hierarchical data store.

(deftype collection ()
  `(or sequence hash-table))

(defclass theme-node ()
  ((name
    :initarg :name :accessor theme-name :type symbol
    :documentation "Name of the theme node.")
   (description
    :initarg :description :accessor theme-description
    :type (or string null) :initform nil
    :documentation "Tell me about it.")
   (children
    :initarg :children :accessor theme-children :initform nil
    :type (or collection null)
    :documentation "A sequence of sub-nodes, or NIL."))
  (:documentation "A node in the theme tree."))

(defclass theme-value-node (theme-node)
  ((value
    :initarg :value :accessor theme-node-value
    :documentation "The value of this node.")
   ;; (default-value
   ;;  :initarg :default-value :accessor theme-node-default-value  
   ;;  :documentation "The default value for this node.")
   )
  (:documentation "A theme value."))

(defclass theme (theme-node)
  ((title
    :initarg :title :accessor theme-title :type string
    :documentation "The title of the theme."))
  (:documentation "Set of customizations."))

(defmethod initialize-instance
    :after ((o theme) &rest initargs &key &allow-other-keys)
  "Initialize a theme."
  (declare (ignore initargs))
  (when (not (slot-boundp o 'name))
    (error "The theme should have a name."))
  ;; Default the title from the name.
  (when (not (slot-boundp o 'title))
    (setf (theme-title o) (format nil "~:(~a~)" (theme-name o)))))

(defgeneric theme-activate (theme &key &allow-other-keys)
  (:documentation "Make the theme active."))

(defgeneric theme-deactivate (theme &key &allow-other-keys)
  (:documentation "Make the theme inactive."))

(defgeneric theme-value (theme item)
  (:documentation "Get the value of a theme item. An ITEM is a theme node path,
which is a list of symbols which are the names of nodes in a path in the tree.
"))

(defgeneric (setf theme-value) (value theme item)
  (:documentation "Set the value of a theme item."))

(defgeneric theme-delete (theme item)
  (:documentation "Destructively delete ITEM from THEME."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((object theme-node) stream)
  "Print a theme-node to STREAM."
  (with-slots (name description children) object
    (print-unreadable-object (object stream :type t)
      (format stream "~a ~d" name (length children)))))

(defmethod print-object ((object theme-value-node) stream)
  "Print a theme-value-node to STREAM."
  (with-slots (name description children value #| default-value |#) object
    (print-unreadable-object (object stream :type t)
      ;; (format stream "~a ~d ~s [~s]" name (length children)
      ;; 	      (and (slot-boundp object 'value) value)
      ;; 	      (and (slot-boundp object 'default-value) default-value)))))
      (format stream "~a ~d ~s" name (length children)
      	      (and (slot-boundp object 'value) value)))))

#|
(defgeneric get-item-in-node (node name)
  (:documentation "Return an item named NAME from NODE."))

(defgeneric put-item-in-node (node name type)
  (:documentation "Put a item named NAME in NODE."))

(defgeneric make-node (theme item)
  (:documentation "Make ITEM exist in THEME if it doesn't, and return it."))
|#

(defgeneric get-node (theme item)
  (:documentation "Get the node for ITEM from THEME."))

;---------------------------------------------------------------------------

#|
(defmethod get-item-in-node ((node theme-node) name)
  "Return an item named NAME from NODE."
  (find name (theme-children node) :key #'theme-name))

(defmethod put-item-in-node ((node theme-node) name type)
  "Put a item named NAME in NODE."
  (let ((new (make-instance type :name name)))
    (push new (theme-children node))
    new))
|#

(defmethod get-node ((theme theme) item)
  "Get a node in THEME described by the node path ITEM."
  (let ((node theme))
    (loop :for n :in item
       :do (setf node (find n (theme-children node) :key #'theme-name))
       :while node)
    node))

(defun set-node (theme item value)
  "Set the node in THEME described by ITEM to VALUE. ITEM is a theme node path,
which is a list of symbols which are the names of nodes in a path in the tree.
If the path doesn't exist, it is created."
  (let ((cur (car item))
	existing)
    (if cur
	(progn
	  ;;(format t "~a " cur)
	  (setf existing (find cur (theme-children theme) :key #'theme-name))
	  (if existing
	      (progn
		;;(write-string ". ")
		(set-node existing (rest item) value))
	      (progn
		(if (= 1 (length item))
		    (progn
		      ;;(format t " := ~s~%" value)
		      (push (make-instance 'theme-value-node
					   :name cur
					   :value value)
			    (theme-children theme)))
		    (let ((new-node
			   (make-instance 'theme-node :name cur)))
		      ;;(write-string "+ ")
		      (push new-node (theme-children theme))
		      (set-node new-node (rest item) value))))))
	(progn
	  ;;(format t " = ~s~%" value)
	  (if (typep theme 'theme-value-node)
	      (setf (theme-node-value theme) value)
	      (error
	       "Attempt to set the value of an existing non-value node."))))))

(defmethod theme-value ((theme theme) item)
  "Return the value in THEME given by the node path ITEM,"
  (let ((node (get-node theme item)))
    (when node
      (cond
	((typep node 'theme-value-node)
	 (theme-node-value node))
	((and (eq (type-of node) 'theme-node)
	      (theme-children node))
	 ;; @@@ could return the whole sub-tree
	 )))))

(defmethod (setf theme-value) (value (theme theme) item)
  "Setter for theme node values."
  (set-node theme item value))

(defun print-theme-node-stupidly (theme-node &key prefix repeat-line stream)
  (let* ((name-string (format nil "~(~a~)" (theme-name theme-node)))
	 (new-prefix (+ (or prefix 0) (length name-string) 1)))
    (when (and repeat-line prefix (> prefix 0))
      (format t "~v,,,va" prefix #\space #\space))
    (princ name-string)
    (if (typep theme-node 'theme-value-node)
	(progn
	  (if (slot-boundp theme-node 'value)
	      (progn
		(format t ": ~w" (theme-node-value theme-node))
		;; (when (slot-boundp theme-node 'default-value)
		;; 	(format t " [~w]" (theme-node-default-value theme-node)))
		)
	      (princ ": unbound"))
	  (terpri))
	(when (theme-children theme-node)
	  (princ #\.)
	  (print-theme-node (first (theme-children theme-node))
			    :prefix new-prefix :stream stream)
	  (loop :for n :in (rest (theme-children theme-node)) :do
	     (print-theme-node n :prefix new-prefix :repeat-line t
			       :stream stream))))))

(defun print-theme-node (theme-node &key prefix repeat-line stream max-size)
  (let* ((name-string (format nil "~(~a~)" (theme-name theme-node)))
	 (new-prefix
	  (if prefix
	      (s+ prefix "." name-string)
	      name-string)))
    (if (typep theme-node 'theme-value-node)
	(progn
	  (when (and repeat-line prefix)
	    (format stream "~a" new-prefix))
	  (if (slot-boundp theme-node 'value)
	      (format stream ":~vt~w~%" max-size (theme-node-value theme-node))
	      (format stream ":~vtunbound~%" max-size))
	  (length new-prefix))
	(when (theme-children theme-node)
	  (loop :for n :in (theme-children theme-node)
	     :maximize
	     (print-theme-node n :prefix new-prefix :repeat-line t
			       :stream stream :max-size max-size))))))

(defun theme-as-tree (theme)
  (let (;(result `(,(theme-name theme) ,@(or (theme-description theme)))))
	(result `(,(theme-name theme))))
    (if (theme-children theme)
	(append result (mapcar #'theme-as-tree (theme-children theme)))
	(if (typep theme 'theme-value-node)
	    (cons (theme-name theme) (theme-node-value theme))
	    result))))

(defconstant +theme-version+ 1)

(defgeneric serialize (theme version)
  (:documentation "Keep your eyes on the prize, or some other platitude."))

(defmethod serialize (theme (version (eql 1)))
  (typecase theme
    (theme
     `(:theme ,version
	      :name ,(theme-name theme)
	      :title ,(theme-title theme)
	      :description ,(theme-description theme)
	      ,(mapcar (_ (serialize _ version)) (theme-children theme))))
    (theme-value-node
     `(:value ,(theme-node-value theme)
	      ,(theme-name theme) ,(theme-description theme)))
    (theme-node
     `(,(theme-name theme) ,(theme-description theme)
	,(mapcar (_ (serialize _ version)) (theme-children theme))))))

(defgeneric unserialize (theme version)
  (:documentation "Keep your eyes on the prize, or some other platitude."))

(defmethod unserialize (obj (version (eql 1)))
  (let ((o (pop obj)))
    (cond
      ((eq o :theme)
       (let (name title description children)
	 (when (/= (pop obj) version)
	   (error "Incorrect version number."))
	 (when (< (length obj) 6)
	   (error "Theme list too short."))
	 (when (not (eql (pop obj) :name))
	   (error "Theme missing name tag."))
	 (setf name (pop obj))
	 (when (not (eql (pop obj) :title))
	   (error "Theme missing title tag."))
	 (setf title (pop obj))
	 (when (not (eql (pop obj) :description))
	   (error "Theme missing description tag."))
	 (setf description (pop obj))
	 (when (not (listp (car obj)))
	   (error "Theme missing children list."))
	 (setf children
	       (mapcar (_ (unserialize _ version)) (pop obj)))
	 ;;(format t "children = ~s~%" children)
	 (make-instance 'theme :name name :title title :description description
			:children children)))
      ((eq o :value) ; leaf
       (when (< (length obj) 3)
	 (error "Theme value node too short."))
       (let (value name description)
	 (setf value (pop obj)
	       name (pop obj)
	       description (pop obj))
	 (make-instance 'theme-value-node
			:name name :description description :value value)))
      ((symbolp o) ; branch
       (when (< (length obj) 2)
	 (error "Theme branch node too short."))
       (let (name description children)
	 ;;(format t "branch ~s~%" o)
	 (setf name o
	       description (pop obj)
	       children (mapcar (_ (unserialize _ version)) (pop obj)))
	 (make-instance 'theme-node
			:name name :description description
			:children children))))))

(defun print-theme (theme &key as-tree readably (stream *standard-output*))
  (cond
    (readably
     (print (serialize theme +theme-version+) stream))
    (as-tree
     (print (theme-as-tree theme) stream))
    (t
     (format stream "Theme:  ~s ~a~%" (theme-name theme) (theme-title theme))
     ;; Fake print-table so we don't have to depend on it.
     (let ((max-size
	    (print-theme-node theme :stream nil)))
       (loop :for n :in (theme-children theme) :do
	  (print-theme-node n :stream stream :max-size max-size)))))
  (values))

(defun make-theme (name &rest keys &key title description)
  "Make a new theme named NAME. If TITLE isn't given it's set from name."
  (declare (ignorable title description))
  (apply #'make-instance 'theme :name name keys))

(defun write-theme (theme stream)
  (write (serialize theme +theme-version+) :stream stream))

(defun read-theme (stream)
  (let ((s (read stream)))
    (when (not (consp s))
      (error "Theme is not a CONS."))
    (when (not (eql (car s) :theme))
      (error "Theme doesn't have a :theme tag."))
    (when (let ((n (second s)))
	    (or (not (numberp n)) (> n +theme-version+)))
      (error "Theme version is not recognized."))
    (unserialize s +theme-version+)))

(defun save-theme (theme file)
  "Save THEME to FILE."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (write (serialize theme +theme-version+) :stream out)))

(defun load-theme (file)
  "Load a theme from FILE and return it."
  (with-open-file (in file :direction :input)
    (unserialize (safe-read in) +theme-version+)))

;; (defun import-dircolors (file)
;;   (with-open-file-or-stream (in file)
;;     ))

;; Fake:
(defun test-theme ()
  (let ((tt (make-theme 'cool
			:title "( ͡° ͜ʖ ͡°)"
			:description "Spooty booty foo.")))
    (setf (theme:theme-value tt '(file dir fg)) :blue)
    (setf (theme:theme-value tt '(file dir bg)) :black)
    (setf (theme:theme-value tt '(file regular fg)) :white)
    (setf (theme:theme-value tt '(file regular bg)) :black)
    tt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme-ish things that maybe should be elsewhere?

(defvar *theme* nil
  "The current theme.")

;; (defun print-themed (thing-type thing)
;;   (let ((style (theme-value *theme* thing)))
;;     (when style
;;       (with-style (style)
;; 	(print thing)))))

;; @@@ This should probably come from a system database.
(defparameter *file-type-suffixes*
  '((:archive (".tar" ".tgz" ".tbz"".tbz2" ".arc" ".arj" ".taz" ".lha" ".lz4"
	       ".lzh" ".lzma" ".tlz" ".txz" ".tzo" ".t7z" ".zip"
	       ".deb" ".rpm" ".jar" ".war" ".ear" ".sar" ".rar" ".alz" ".ace"
	       ".zoo" ".cpio" ".7z" ".rz" ".cab"))
    (:compressed (".z" ".Z" ".dz" ".gz" ".lrz" ".lz" ".lzo" ".xz" ".bz2" ".bz"
		  ".tz"))
    (:image (".jpg" ".jpeg" ".gif" ".bmp" ".pbm" ".pgm" ".ppm" ".tga" ".xbm"
	     ".xpm" ".tif" ".tiff" ".png" ".svg" ".svgz" ".mng" ".pcx"
	     ".xwd" ".yuv" ".cgm"".emf"
	     ".gl" ".dl"))
    (:video (".mov" ".mpg" ".mpeg" ".m2v" ".mkv" ".webm" ".ogm" ".mp4" ".m4v"
	     ".mp4v" ".vob" ".qt" ".nuv" ".wmv" ".asf" ".rm" ".rmvb" ".flc"
	     ".avi" ".fli"".flv"
	     ".ogv" ".ogx"))
    (:audio (".aac" ".au" ".flac" ".m4a" ".mid" ".midi" ".mka" ".mp3" ".mpc"
	     ".ogg" ".ra" ".wav" ".oga" ".opus" ".spx"".xspf"))))

(defparameter *file-suffix-table* nil)

(defun file-suffix-type (file)
  "Return the file suffix type for the given file name."
  (when (not *file-suffix-table*)
    (setf *file-suffix-table* (make-hash-table :test #'equal))
    (loop :for type :in theme:*file-type-suffixes* :do
       (loop :for suffix :in (second type) :do
	  (setf (gethash suffix *file-suffix-table*) (first type)))))
  (let* ((dot (position #\. file :from-end t))
	 (suffix (and dot (subseq file dot))))
    (when suffix
      (gethash suffix *file-suffix-table*))))

(defparameter *dircolor-map*
  #(("rs" . nil) 			; reset
    ("di" . (:file :type :directory             :style))
    ("mh" . (:file :type :link                  :style))
    ("ln" . (:file :type :symbolic-link         :style))
    ("pi" . (:file :type :pipe                  :style))
    ("so" . (:file :type :socket                :style))
    ("bd" . (:file :type :block-device          :style))
    ("cd" . (:file :type :character-device      :style))
    ;;("or" . (:file :type :orphan                :style)) ;; broken symlink 
    ;;("mi" . (:file :type :missing               :style)) ;; the missing file
    ("su" . (:file :type :setuid                :style))
    ("sg" . (:file :type :setgid                :style))
    ;;("ca" . (:file :type :capability            :style)) ;; file with capability
    ("st" . (:file :type :sticky                :style))
    ("tw" . (:file :type :sticky-other-writable :style))
    ("ow" . (:file :type :other-writable        :style))
    ("ex" . (:file :type :executable            :style))))

(defun find-theme-node-for (name)
  (let (result)
    (or (and (setf result (find name theme::*dircolor-map*
				:key #'car :test #'equal))
	     (cdr result))
	(and (setf result (file-suffix-type name))
	     `(:file :suffix ,result)))))
  
(defun dircolors-to-theme (&optional (dircolors (nos:env "LS_COLORS")))
  "Convert from a dircolors format string, to a plist of theme-item value,
which can be used to set the appropriate theme values, for example with
set-theme-items."
  (let (result name value tt)
    (loop :for e in (split-sequence #\: dircolors) :do
	 (setf name (split-sequence #\= e)
	       value (second name)
	       name (first name))
       :if (setf tt (find-theme-node-for name))
       :do
	 (let ((i (assoc tt result :test #'equalp))
	       (val (delete-if #'stringp
			       (flatten
				(fatchar-string-to-span
				 (process-ansi-colors
				  (make-fatchar-string
				   (format nil "~c[~amx" #\escape value))))))))
	   (if i (rplacd i val)
	       (setf result (acons tt val result)))))
    (alexandria:alist-plist result)))

(defun set-theme-items (theme item-value-list)
  "Set item in theme from item-value-list which is a plist like list of
alternating (item value ...)."
  (loop
     :for i = item-value-list :then (cdr i)
     :while i
     :do
     (setf (theme-value theme (car i)) (cadr i)
	   i (cdr i))))

(defun default-theme ()
  (let ((tt (make-theme
	     'default
	     :title "Default theme."
	     :description (format nil "~
A default theme that isn't too unexpected and works for a dark background.
Something like the default setting of typical GNU tools."))))
    (set-theme-items
     tt
     ;; File type
     '((:file :type :directory             :style) (:bold :fg-blue)
       (:file :type :link                  :style) (:bold :fg-cyan)
       (:file :type :symbolic-link         :style) (:bold :fg-cyan)
       (:file :type :pipe                  :style) (:fg-black :fg-yellow)
       (:file :type :socket                :style) (:bold :fg-magenta)
       (:file :type :block-device          :style) (:bg-black :fg-yellow :bold)
       (:file :type :character-device      :style) (:bg-black :fg-yellow :bold)
       (:file :type :setuid                :style) (:fg-white :bg-red)
       (:file :type :setgid                :style) (:fg-black :bg-yellow)
       (:file :type :sticky                :style) (:fg-white :bg-blue)
       (:file :type :sticky-other-writable :style) (:fg-black :bg-green)
       (:file :type :other-writable        :style) (:fg-blue :bg-green)
       (:file :type :executable            :style) (:bold)
       ;; File name suffixes
       (:file :suffix :archive             :style) (:bold :fg-red)
       (:file :suffix :compressed          :style) (:bold :fg-cyan)
       (:file :suffix :image               :style) (:bold :fg-magenta)
       (:file :suffix :video               :style) (:bold :fg-magenta)
       (:file :suffix :audio               :style) (:fg-cyan)
       ;; Syntax
       (:syntax :comment :line :semicolon  :style) (:fg-cyan)
       (:syntax :comment :block            :style) (:fg-cyan)
       (:syntax :comment :block            :style) (:fg-cyan)
       (:syntax :constant :character       :style) (:fg-white)
       (:syntax :constant :language        :style) (:fg-white)
       (:syntax :constant :other           :style) (:fg-white)
       (:syntax :entity :name :function    :style) (:fg-red)
       (:syntax :entity :name :macro       :style) (:fg-red)
       (:syntax :entity :name :type        :style) (:fg-blue)
       (:syntax :entity :keyword :operator :style) (:fg-magenta)
       (:syntax :entity :keyword :control  :style) (:fg-magenta)
       (:syntax :support :function         :style) (:fg-magenta)
       (:syntax :support :variable         :style) (:fg-green)
       (:syntax :support :function         :style) (:fg-magenta)
       (:syntax :variable :other           :style) (:fg-green)
       ;; Commands
       (:command :not-found	    :style) (:red)
       (:command :found		    :style) (:bold :blue)
       (:command :directory	    :style) (:fg :color #(:rgb .0 .5 .9))
       (:command :system-command    :style) (:fg :color #(:rgb .4 .5 .9))
       (:command :external-command  :style) (:fg :color #(:rgb .0 .5 .9))
       (:command :builtin-command   :style) (:fg :color #(:rgb .9 .4 .7))
       (:command :shell-command	    :style) (:fg :color #(:rgb .7 .5 .9))
       (:command :loadable-system   :style) (:fg :color #(:rgb .9 .6 .0))
       (:command :command	    :style) (:bold :blue)
       (:command :alias		    :style) (:fg :color #(:rgb .8 .7 .9))
       (:command :function	    :style) (:magenta)
       (:command-arg :existing-path :style) (:underline)
       ;; Programs
       (:program :modeline             :style)     (:standout)
       (:program :search-match         :style)     (:underline :red)
       (:program :empty-line-indicator :style)     (:normal)
       (:program :empty-line-indicator :character) #\~
       (:program :selection            :style)     (:standout)
       (:program :label		       :style)     (:cyan)
       (:program :data		       :style)     (:green)
       ;; Line editor
       ;; (:rl      :search-match         :style)     (:underline :red)
       ;; (:rl      :selection            :style)     (:standout)
       ))
    tt))

(defun set-theme-defaults-for-16-color (theme)
  (set-theme-items theme
    `((:file :type :executable     :style) (:green :bold)
      (:command :directory	   :style) (:blue)
      (:command :system-command    :style) (:cyan)
      (:command :external-command  :style) (:cyan :bold)
      (:command :builtin-command   :style) (:magenta)
      (:command :shell-command     :style) (:magenta)
      (:command :loadable-system   :style) (:magenta)
      (:command :alias		   :style) (:magenta :bold))))

(defun default-theme-16-color ()
  "Default theme for 16-color"
  (let ((theme (default-theme)))
    (set-theme-defaults-for-16-color theme)))

;; @@@ We should probably make a default monochrome theme.

;; EOF
