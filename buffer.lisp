;;
;; buffer.lisp
;;

(in-package :rl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer interface
;;
;; The buf slot of line-editor should only be modified through these.

(defgeneric buffer-delete (e start end)
  (:documentation
   "Delete characters from the buffer from the position START to END.")
  (:method ((e line-editor) start end)
    (with-slots (buf point) e
;      (format t "del (~s ~s)~%" start end)
      ;; If end and start are reversed, swap them.
      (when (< end start)
	(rotatef start end))
      ;; If start equals end, it's a empty deletion, so we don't have to do
      ;; anything.
      (when (> end start)
	(record-undo e 'deletion start (subseq buf start end) point)
	;; Optimization: Deleting to the end, just decrement fill pointer.
	(when (not (= end (fill-pointer buf)))
	  (setf (subseq buf start) (subseq buf end)))
	(decf (fill-pointer buf) (- end start))))))

(defgeneric buffer-insert (e pos thing)
  (:documentation
   "Insert something into the buffer at position POS.")
  (:method ((e line-editor) pos (c character))
;    (format t "ins (~s ~s)~%" pos c)
    (with-slots (buf) e
      (record-undo e 'insertion pos (string c))
      (if (= pos (length buf))
	  ;; Appending to the end
	  (progn
	    (vector-push-extend c buf
				(+ (array-total-size buf)
				   (truncate (* (array-total-size buf) 2/3)))))
	  ;; Inserting in the middle
	  (progn
	    (when (= (length buf) (array-total-size buf))
	      (setf buf (adjust-array
			 buf (+ (array-total-size buf)
				(truncate (* (array-total-size buf) 2/3))))))
	    (incf (fill-pointer buf))
	    (setf (subseq buf (1+ pos)) (subseq buf pos))
	    (setf (aref buf pos) c)))))
  (:method ((e line-editor) pos (s string))
;    (format t "ins (~s ~s)~%" pos s)
    (with-slots (buf) e
      (let ((len (length s)))
	(record-undo e 'insertion pos s)
	(when (>= (+ len (length buf)) (array-total-size buf))
	  (setf buf (adjust-array
		     buf (+ (array-total-size buf) len 
			    (truncate (* (array-total-size buf) 2/3))))))
	(incf (fill-pointer buf) len)
	(setf (subseq buf (+ pos len)) (subseq buf pos))
	(setf (subseq buf pos (+ pos len)) s)))))

;; Replace could just be a delete followed by an insert, but
;; for efficiency sake we do something special.

(defgeneric buffer-replace (e pos thing)
  (:documentation
   "Insert something into the buffer at position POS.")
  (:method ((e line-editor) pos (c character))
;    (format t "replace (~s ~s)~%" pos c)
    (with-slots (buf point) e
      ;; @@@ zorp maybe change to (make-fat-string (string ...
      (record-undo e 'deletion pos (string (aref buf pos)) point) 
      (record-undo e 'insertion pos (string c))
      (setf (aref buf pos) c)))
  (:method ((e line-editor) pos (s string))
;    (format t "replace (~s ~s)~%" pos s)
    (with-slots (buf point) e
      (let ((len (length s)))
	(when (> (+ pos len) (length buf))
	  (error "Replacement doesn't fit in the buffer."))
	(when (> len 0)
	  (record-undo e 'deletion pos (subseq buf pos (+ pos len)) point)
	  (record-undo e 'insertion pos s)
	  (setf (subseq buf pos (+ pos len)) s))))))

;; @@@ Currently unused.
;; (defun eobp (e)
;;   "Return true if point is at (or after) the end of the buffer."
;;   (with-slots (point buf) e
;;     (>= point (length buf))))

;; EOF
