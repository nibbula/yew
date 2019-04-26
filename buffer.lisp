;;
;; buffer.lisp
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer interface
;;
;; The BUF slot of line-editor should only be accessed and modified through
;; these.

;; Access methods
;; @@@ Except of course these lose the effects.

(defun buffer-string (buf)
  "Return a buffer as string."
  (fatchar-string-to-string buf))

(defun buffer-char (buf i)
  "Return the character at position I in buffer BUF."
  (fatchar-c (aref buf i)))

(defun set-buffer-char (buf i c)
  "Set the character at position I in buffer BUF to C."
  (setf (fatchar-c (aref buf i)) c))

(defsetf buffer-char set-buffer-char
  "Set the character at position I in buffer BUF.")

;; Versions which don't lose the effects.
;; @@@ And seem very pointless?

(defun buffer-fat-string (buf)
  "Return a buffer as new fat-string. Maybe you should use the buf-str slot?"
  (make-fat-string :string buf))

(defun buffer-fatchar-string (buf)
  "Return a buffer or buffer subsequence as fatchar-string."
  buf)

(defun buffer-fatchar (buf i)
  "Return the fatchar at position I in buffer BUF."
  (aref buf i))

(defun set-buffer-fatchar (buf i c)
  "Set the fatchar at position I in buffer BUF to C."
  (setf (aref buf i) c))

(defsetf buffer-char set-buffer-fatchar
  "Set the fatchar at position I in buffer BUF.")

;; Modify methods

(defgeneric buffer-delete (e start end point)
  (:documentation
   "Delete characters from the buffer from the position START to END.")
  (:method ((e line-editor) start end point)
    (with-slots (buf) e
      ;; If end and start are reversed, swap them.
      (when (< end start)
	(rotatef start end))
      ;; If start equals end, it's a empty deletion, so we don't have to do
      ;; anything.
      (when (> end start)
	(dbugf :rl "buffer-delete ~s ~s fill ~s~%" start end (fill-pointer buf))
	(record-undo e 'deletion start (subseq buf start end) point)
	;; Optimization: Deleting to the end, just decrement fill pointer.
	(when (not (= end (fill-pointer buf)))
	  (setf (subseq buf start) (subseq buf end)))
	(decf (fill-pointer buf) (- end start))))))

(defgeneric buffer-insert (e pos thing point)
  (:documentation
   "Insert something into the buffer at position POS.")
  (:method ((e line-editor) pos (c character) point)
    (let ((fc (make-fatchar :c c)))
      (with-slots (buf) e
	(record-undo e 'insertion pos (make-fatchar-string (string c)) point)
	(if (= pos (length buf))
	    ;; Appending to the end
	    (progn
	      (vector-push-extend fc buf
				  (+ (array-total-size buf)
				     (truncate
				      (* (array-total-size buf) 2/3)))))
	    ;; Inserting in the middle
	    (progn
	      (when (= (length buf) (array-total-size buf))
		(setf buf (adjust-array
			   buf (+ (array-total-size buf)
				  (truncate (* (array-total-size buf) 2/3))))))
	      (incf (fill-pointer buf))
	      (setf (subseq buf (1+ pos)) (subseq buf pos))
	      (setf (aref buf pos) fc))))))
  (:method ((e line-editor) pos (s string) point)
    (with-slots (buf) e
      (let ((len (length s))
	    (fat-string (make-fatchar-string s)))
	(record-undo e 'insertion pos fat-string point)
	(when (>= (+ len (length buf)) (array-total-size buf))
	  (setf buf (adjust-array
		     buf (+ (array-total-size buf) len
			    (truncate (* (array-total-size buf) 2/3))))))
	(incf (fill-pointer buf) len)
	(setf (subseq buf (+ pos len)) (subseq buf pos))
	(setf (subseq buf pos (+ pos len)) fat-string))))
  (:method ((e line-editor) pos (s vector) point)
    ;; This is basically for a fat string which happens to be indistinguishable
    ;; from a vector.
    ;; @@@ This has the effect of losing the attributes.
    (buffer-insert e pos (fatchar-string-to-string s) point)))

;; Replace could just be a delete followed by an insert, but
;; for efficiency sake we do something special.

(defgeneric buffer-replace (e pos thing point)
  (:documentation
   "Replace THING in the buffer at position POS.")
  (:method ((e line-editor) pos (c character) point)
    (with-slots (buf) e
      ;; (record-undo e 'deletion pos (make-fatchar-string (buffer-char buf pos)))
      ;; (record-undo e 'insertion pos (make-fatchar-string c))
      (record-undo e 'replacement pos
		   (make-fatchar-string (buffer-char buf pos)) point)
      (setf (aref buf pos) (make-fatchar :c c))))
  (:method ((e line-editor) pos (s string) point)
    (with-slots (buf) e
      (let ((len (length s))
	    (fs (make-fatchar-string s)))
	(when (> (+ pos len) (length buf))
	  (error "Replacement doesn't fit in the buffer."))
	(when (> len 0)
	  ;; (record-undo e 'deletion pos (subseq buf pos (+ pos len)))
	  ;; (record-undo e 'insertion pos fs)
	  (record-undo e 'replacement pos (subseq buf pos (+ pos len)) point)
	  (setf (subseq buf pos (+ pos len)) fs)))))
  (:method ((e line-editor) pos (fs vector) point)
    ;; ?assume this means fatchar-string?
    (with-slots (buf) e
      (let ((len (length fs)))
	(when (> (+ pos len) (length buf))
	  (error "Replacement doesn't fit in the buffer."))
	(when (> len 0)
	  ;; (record-undo e 'deletion pos (subseq buf pos (+ pos len)))
	  ;; (record-undo e 'insertion pos fs)
	  (record-undo e 'replacement pos (subseq buf pos (+ pos len)) point)
	  (setf (subseq buf pos (+ pos len)) fs))))))

;; @@@ Currently unused.
;; (defun eobp (e)
;;   "Return true if point is at (or after) the end of the buffer."
;;   (with-slots (point buf) e
;;     (>= point (length buf))))

;; EOF
