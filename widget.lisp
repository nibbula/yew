;;;
;;; widget.lisp - Line editor as a widget.
;;;

(defpackage :rl-widget
  (:documentation "An editor widget for terminals.")
  (:use :cl :dlib :terminal :collections :ochar :char-util :fatchar 
	:completion :keymap :inator :terminal-inator :ostring :rl)
  (:import-from :inator
		#:point #:mark #:clipboard #:quit-flag #:command #:last-command)
  (:export
   #:widget
   #:widget-bbox
   #:widget-rendition
   #:widget-box-p
   #:widget-read
   #:make-widget))

(in-package :rl-widget)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

(defclass bbox ()
  ((x :initarg :x :accessor bbox-x :type fixnum :initform 0
      :documentation "Horizontal position.")
   (y :initarg :y :accessor bbox-y :type fixnum :initform 0
      :documentation "Vertical position.")
   (width :initarg :width :accessor bbox-width :type fixnum :initform 0
	  :documentation "Horizontal extent.")
   (height :initarg :height :accessor bbox-height :type fixnum :initform 0
	   :documentation "Horizontal extent."))
  (:documentation "Bounding box in a character."))

(defclass widget (line-editor)
  ((bbox
    :initarg :bbox :accessor widget-bbox :initform nil :type bbox
    :documentation "The bounding box.")
   (rendition
    :initarg :rendition :accessor widget-rendition
    :initform (make-fatchar) :type fatchar
    :documentation "The default character rendition.")
   (box-p
    :initarg :box-p :accessor widget-box-p :initform nil :type boolean
    :documentation "True to draw a box around the widget."))
  (:documentation "The line editor as a widget."))

(defparameter *widget-keys*
  `((,(ctrl #\P)	. previous-line)
    (,(ctrl #\N)	. next-line)
    (,(ctrl #\D)	. delete-char)
    (,(meta-char #\<)	. beginning-of-buffer)
    (,(meta-char #\>)	. end-of-buffer)
    ((#\escape #\<)	. beginning-of-buffer)
    ((#\escape #\>)	. end-of-buffer)
    )
  "Keys for text widgets.")

(defmethod initialize-instance
    :after ((o widget) &rest initargs &key &allow-other-keys)
  "Initialize a widget."
  (declare (ignore initargs))

  ;; Set up a bounding box
  (when (or (not (slot-boundp o 'bbox))
	    (not (slot-value o 'bbox)))
    (setf (slot-value o 'bbox)
	  (multiple-value-bind (row col)
	      (terminal-get-cursor-position *terminal*)
	    (make-instance 'bbox
			   :x col :y row
			   :width (tt-width) :height (tt-height)))))

  ;; Add customized keys for widgets.
  (when (and (slot-boundp o 'rl::local-keymap)
	     (slot-value o 'rl::local-keymap))
    ;; (add-keymap *widget-keymap* (slot-value o 'rl::local-keymap))
    (loop :for (key-seq . binding) :in *widget-keys* :do
       (set-key key-seq binding (slot-value o 'rl::local-keymap)))))

;; @@@ copied from fui
(defun erase-area (x y width height &key string rendition)
  (let ((str (cond
	       ((and string (not rendition))
		string ;; Just use the string.
		;; (fill string #\space :end width))
		)
	       ((and string rendition)
		(error "FUK THIS")
		;; (ofill string rendition :end width)
		)
	       ((not rendition)
		(make-string width :initial-element #\space))))
	(char (or (and rendition (let ((c (copy-fatchar rendition)))
				   (setf (fatchar-c c) #\space)
				   c))
		  #\space)))
    (loop :for iy :from y :below (+ y height) :do
       (if str
	   (tt-write-string-at iy x str)
	   (loop :for ix :from x :below (+ x width) :do
	      (tt-write-char-at iy ix char))))))

(defun erase-bbox (bbox &key string rendition)
  (erase-area (bbox-x bbox) (bbox-y bbox)
	      (bbox-width bbox) (bbox-height bbox)
	      :string string
	      :rendition rendition
	      ))

(defun apply-rendition (rendition string)
  "Apply the effects from the fatchar RENDITION to STRING. If string is a normal
string, make a new fat-string with the attributes set. If STRING is already a
fat-string, set unset effects to be from RENDITION."
  (let (result)
    (etypecase string
      (string
       ;; Make a new fat-string with the rendition.
       (setf result
	     (make-fat-string :length (length string)
			      :initial-element rendition))
       (loop :for i :from 0 :below (length string)
	  :do (setf (aref result i) (aref string i)))
       result)
      (fat-string
       (omapn (_ (when (not (fatchar-fg _))
		   (setf (fatchar-fg _) (fatchar-fg rendition)))
		 (when (not (fatchar-bg _))
		   (setf (fatchar-bg _) (fatchar-bg rendition)))
		 (when (not (fatchar-attrs _))
		   (setf (fatchar-attrs _)
			 (copy-seq (fatchar-attrs rendition)))))
	      string)
       string))))

;; This is to get different defaults for the view. We should probably have
;; just made a view object.
(defmethod calculate-line-endings ((editor widget)
				   &key
				     (buffer (rl::buf editor))
				     (start-column
				      (bbox-x (widget-bbox editor)))
				     (end-column
				      (bbox-width (widget-bbox editor)))
				     spots column-spots)
  (rl::%calculate-line-endings
   buffer start-column end-column spots column-spots))

;; Now with much less ploof!
(defun widget-redraw (e &key erase)
  (declare (ignore erase)) ; @@@
  (with-slots ((contexts inator::contexts)
	       (buf-str rl::buf-str)
	       (buf rl::buf)
	       (start-row rl::start-row)
	       (start-col rl::start-col)		     ;; unnecessary?
	       (last-line rl::last-line)                     ;; unnecessary?
	       (temporary-message rl::temporary-message)     ;; unnecessary?
	       (region-active rl::region-active)
	       (max-message-lines rl::max-message-lines)     ;; unnecessary?
	       rendition bbox) e
    (dbugf :rl "----------------~%")
    ;; Make sure buf-str uses buf.
    (when (not (eq (fat-string-string buf-str) buf))
      (setf (fat-string-string buf-str) buf))
    (let* (;; Shorter names for the box
	   (x      (bbox-x bbox))
	   (y      (bbox-y bbox))
	   (width  (bbox-width bbox))
	   (height (bbox-height bbox))

	   ;; @@@ settings that are likely leftover junk! :
	   #|
	   (buf-lines   (length endings))
	   (msg-lines     (if temporary-message
			      (+
			       ;; (if (ends-with-newline-p temporary-message)
			       ;; 	   1 1)
			       1
			       (length msg-endings))
			      0))
	   (total-lines   (+ buf-lines msg-lines))
	   ;; (line-last-col (cddr (assoc line-end spots)))
	   new-last-line
	   ;; (start-row y)
	   ;; (old-col x)
	   ;; (max-message-lines (- height buf-lines 2))
	   |#

	   ;; Necessary & good settings:
	   
	   ;; Make the regions and/or cursors be highlighted.
	   (str (if (or (and (rl::regions-p e) region-active)
			(> (length contexts) 1))
		    (make-fat-string :string (rl::highlightify e buf))
		    buf-str))

	   ;; Calculate the line endings which we loop through
	   (line-end    (max 0 (1- (olength buf-str))))
	   (first-point (inator-point (aref contexts 0)))
	   (spots (list `(,first-point . ())
			`(,line-end . ())))
	   (endings     (calculate-line-endings e :start-column 0
						:end-column width
						:spots spots))
	   ;; Get the cursor position from the results
	   (spot          (assoc first-point spots))
	   (point-line    (cadr spot))
	   (point-col     (cddr spot))

	   (row y)	   ;; The row we're writing
	   (pos 0)	   ;; Buffer index of the start of the line
	   (end-pos)	   ;; Buffer index of the end of current line
	   ;;(end-col)	   ;; Column of the end of current line
	   (last-end-pos)  ;; Buffer index of the end of previous line
	   (last-end-col)  ;; Column of the end of the previous line
	   (end-width)	   ;; Width of the blank space after the line
	   )
      ;; Move to the top left of the box
      (tt-move-to y x)

      ;; Erase all the contents
      (erase-bbox bbox :rendition rendition)

      ;; @@@ unnecessary junk??
      ;; (setf new-last-line total-lines
      ;; 	    #| prompt-height prompt-lines |#
      ;; 	    start-col (bbox-x bbox) #|prompt-last-col |#)

      ;; Write the line
      ;;(if (or (and (regions-p e) region-active) (> (length contexts) 1))

      (when rendition
	(apply-rendition rendition str))

      (if endings
	  (progn
	    (loop
	       :for e :in (nreverse endings)
	       :while (< row (+ y height))
	       :do
	       ;; (setf end-pos (min (1- (olength str)) (car e)))
	       (setf end-pos (1+ (car e)))
	       (tt-write-string-at row x str
				   :start pos
				   :end end-pos)
	       ;; (format *trace-output* "row ~s ~s ~s~%"
	       ;; 	   row pos end-pos)
	       (incf row)
	       (setf pos (+ end-pos 1) ;; + 1 to avoid newline
		     last-end-col (if (consp (cdr e)) (cddr e) (cdr e))
		     last-end-pos end-pos))

	    ;; Write the last piece
	    (when (and (< last-end-pos (1- (olength str)))
		       (< row (+ y height)))
	      (setf last-end-pos (min (+ pos width) (olength str))
		    last-end-col (- last-end-pos pos))
	      (tt-write-string-at row x str
				  :start pos
				  :end last-end-pos))
	    )
	  ;; No endings, so less than one full line.
	  (tt-write-string-at row x str))

	  (setf end-width (- width (or last-end-col
				       (- (olength str) pos))))
	  (when (< row (+ y height))
	    (erase-area (+ x (- width end-width)) row end-width 1
			:rendition rendition))

	  #| temporary message should go somewhere else
	  (when temporary-message
	  (tt-write-char #\newline)
	  (tt-write-string temporary-message))
	  (tt-erase-to-eol)
	  |#

	  ;; Move the cursor to the point.
	  (tt-move-to (+ y point-line)
		      (+ x point-col)))))

(defmethod update-display ((e widget))
  (with-slots (bbox box-p) e
    (when box-p
      (fui:draw-box (1- (bbox-x bbox)) (1- (bbox-y bbox))
		    (+ (bbox-width bbox) 2) (+ (bbox-height bbox) 2)))
    (widget-redraw e)))

(defsingle-method redraw ((e widget))
  "Clear the screen and redraw the prompt and the input line."
  (with-slots ((keep-region-active rl::keep-region-active)
	       bbox box-p) e
    ;; (setf (rl::screen-col e) 0 (rl::screen-relative-row e) 0)
    (if box-p
	(erase-area (1- (bbox-x bbox)) (1- (bbox-y bbox))
		    (1+ (bbox-width bbox)) (1+ (bbox-height bbox)))
	(erase-bbox bbox))
    (tt-finish-output)
    (update-display e)
    (tt-finish-output)
    (setf keep-region-active t)))

(defun widget-read (&rest initargs &key (x 0) (y 0) (width 33) (height 1) box-p
				     &allow-other-keys)
  ;; (declare (ignore initargs))
  (let* ((args (append (copy-seq initargs)
		       `(:bbox ,(make-instance 'bbox
			         :x x :y y :width width :height height)
			       :box-p ,box-p)))
	(w (apply #'make-instance 'widget args)))
    (rl:rl :editor w)))

(defun make-widget (&key (x 0) (y 0) (width 33) (height 1) rendition box-p)
  (make-instance 'widget
    :bbox (make-instance 'bbox :x x :y y :width width :height height)
    :rendition (or rendition (make-fatchar))
    :box-p box-p))

;; It's an inator, so to use it as widget in another inator, or even something
;; else, you can just call process-event with it and an event.

;; End
