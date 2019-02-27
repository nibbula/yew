;;
;; color-hash.lisp - List file color hashes.
;;

(defpackage :color-hash
  (:documentation "List file color hashes.")
  (:use :cl :dlib :opsys :collections :table :fatchar :find :md5 :color-stripe
	:table-viewer)
  (:export
   #:color-hash
   #:!color-hash
   ))
(in-package :color-hash)

(defun format-hash (cell width)
  (declare (ignore width)) ; @@@
  (with-output-to-string (str)
    ;; (when (vectorp cell)
    (loop :for n :across cell :do
	 (format str "~2,'0x" n))
    ))

(defun color-hash (path)
  (let (files)
    (cond
      ((and (or (stringp path) (pathnamep path)) (probe-directory path))
       (setf files (find:find-files :type #\r :max-depth 0 :dir path)))
      ((and (or (stringp path) (pathnamep path)) (probe-file path))
       (setf files (list path)))
      ((mappable-p path)
       (setf files path))
      (t
       (error "I don't know what to do with a ~a." (type-of path))))
    (setf files (remove-if #'directory-p files))
    (table:make-table-from
     (omap (_ (let ((checksum (md5:md5sum-file (pathname (quote-filename _)))))
		(list (fatchar:make-fat-string
		       :string (fatchar:process-ansi-colors
				(fatchar:make-fatchar-string
				 (color-stripe:color-stripe-from-hash
				  checksum))))
		      checksum _)))
	   files)
     :columns
     `((:name "Colors")
       (:name "Checksum" :format ,#'format-hash)
       (:name "Thing")))))

#+lish
(lish:defcommand color-hash
  ((files pathname :default (current-directory) :repeating t
    :help "Paths to hash."))
  "List color hashes."
  :accepts :sequence
  (let (result1 result2)
    (when lish:*input*
      (setf result1 (color-hash lish:*input*)))
    (when files
      (setf result2 (color-hash files)))
    (setf lish:*output* (cond
			  ((and result1 result2)
			   (oconcatenate result1 result2))
			  (result1 result1)
			  (result2 result2)))
    (when (not (lish:accepts 'table))
      (table-viewer:!print-table :table lish:*output*))))

;; EOF
