;;
;; undo.lisp - Undo for RL
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; undo
;;
;; Emacs style undo is weird in that it records the undoing as undoable, but
;; only puts it there when you type a non-undo command after a series of undos.
;;
;; Standard undo/redo in is easier to understand, but kind of stupid, since
;; when you do a modifying action after redoing, it loses your redo info.
;;
;; We're gonna do emacs style here, but see neox for the new tree style.

(defclass undo-item ()
  ((position :initarg :position :accessor undo-item-position)
   (data     :initarg :data     :accessor undo-item-data)
   (point    :initarg :point    :accessor undo-item-point))
  (:default-initargs
   :position nil
   :data nil
   :point nil)
  (:documentation "Record an undoable action."))

(defmethod print-object ((obj undo-item) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s ~s" (undo-item-position obj) (undo-item-data obj))))

(defclass deletion (undo-item) ())
(defclass insertion (undo-item) ())
(defclass boundary (undo-item) ())

(defun undo-item-length (item)
  "Return the length of an undo item in buffer characters."
  (with-slots (data) item
    (etypecase data
      (character 1)
      (fatchar 1)
      (string (length data))
      (sequence (length data)))))

(defgeneric undo-one-item (e item)
  (:documentation "Undo an undo item.")
  (:method (e (item boundary)) (declare (ignore e)) #| do nothing |# )
  (:method (e (item deletion))
    (with-slots (point) e
      (buffer-insert e (undo-item-position item) (undo-item-data item))
      (setf point (undo-item-position item))))
  (:method (e (item insertion))
    (with-slots (point buf) e
      (let* ((item-len (undo-item-length item)))
	(buffer-delete
	 e (undo-item-position item) (+ (undo-item-position item) item-len))
	(setf point (undo-item-position item))))))

(defun record-undo (e type &optional position data point)
  (let ((hist (car (undo-history e))))
    (cond
      ((and (eql type 'boundary) (typep hist 'boundary))
       #| Don't record multiple consecutive boundaries |#)
;       ((and (eql type 'insertion) (typep hist 'insertion))
;        (cond
; 	 ;; convert two consecutive adjacent char insertions to a string
; 	 ((and (characterp (undo-item-data hist))
; 	       (characterp data)
; 	       (= position (1+ (undo-item-position hist))))
; 	  (let ((str (make-string 2)))
; 	    (setf (aref str 0) (undo-item-data hist)
; 		  (aref str 1) data)
; 	    (push (make-instance
; 		   type :position (undo-item-position hist) :data str)
; 		  (undo-history e))))
; 	 ;; add a adjacent character insertion onto a string
; 	 ((and (characterp data)
; 	       (stringp (undo-item-data hist))
; 	       (= position (+ (undo-item-position hist)
; 			      (length (undo-item-data hist)))))
; 	  (push (make-instance
; 		 type :position (undo-item-position hist)
; 		 :data (concatenate 'string (undo-item-data hist)
; 				    (string data)))
; 		(undo-history e)))))
      (t
       (push (make-instance type :position position :data data :point point)
	     (undo-history e))))))

(defun undo-one (e)
  "Undo one item from the undo history. Return true if we should undo more."
  (let (item)
    (if (equal (inator-last-command e) 'undo-command)
	;;(ctrl #\O)) ; @@@ bogus ^O until keymaps, etc
      (progn
	(if (undo-current e)
	  (progn
	    (setf item (car (undo-current e)))
	    (undo-one-item e item)
	    (setf (undo-current e) (cdr (undo-current e))))
	  (beep e "No more undo information.")))
      (progn
	(if (undo-history e)
	  (progn
	    (setf (undo-current e) (cdr (undo-history e))
		  item (car (undo-history e)))
	    (undo-one-item e item))
	  (beep e "No undo history."))))
;    (message-pause e "Undid ~s" item)
    ;;(and item (not (typep item 'boundary)))))
    item))

(defparameter *nice-undo-char-count* 20)
(defparameter *nice-undo-big-threshold* 4)

(defun undo-heuristic (e)
  "Undo until some aesthetically pleasing point."
  (with-slots (non-word-chars undo-recent-count) e
    (when (not (equal (inator-last-command e) 'undo-command))
      (setf undo-recent-count 0))
    (let (item last (count 0) non-word-this non-word-last)
      (labels ((is-char (item)
		 (and item (undo-item-data item)
		      (= (length (undo-item-data item)) 1)
		      (ocharacterp (oelt (undo-item-data item) 0))))
	       (is-non-word (item)
		 (and (is-char item)
		      (find (simplify-char (oelt (undo-item-data item) 0))
			    non-word-chars))))
	(loop
	   :do
	   (shiftf last item (undo-one e))
	   (shiftf non-word-last non-word-this (is-non-word item))
	   (incf undo-recent-count)
	   :while
	   (and (> undo-recent-count *nice-undo-big-threshold*)
		(is-char item)
		(< count *nice-undo-char-count*)
		(or (and non-word-last non-word-this)
		    (and (not non-word-this)
			 (not non-word-last))))
	   :do
	   (incf count))))))

(defun undo (e)
  "Undo until an undo boundry or all undone."
  ;; (do () ((not (undo-one e))))
  (undo-heuristic e)
  )

(defmacro without-undo ((e) &body body)
  "Execute the body with undo recording off in the given editor."
  (let ((old-undo (gensym)))
    `(let ((,old-undo (record-undo-p ,e)))
      (unwind-protect
	   (progn
	     (setf (record-undo-p ,e) nil)
	     ,@body)
	(setf (record-undo-p ,e) ,old-undo)))))

(defun undo-command (e)
  ;;(format t "~s~%" (undo-history e))
  ;;(undo e) ;; @@@ Please make undo boundries work @@@
  ;;(undo-one e)
  (undo e)
  ;; (redraw e) ;; @@@ This is overkill! (and screws up multiline prompts)
  )

;; EOF
