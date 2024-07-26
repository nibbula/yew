;;;
;;; dgray.lisp - Thinnest portability wrapper around Gray streams.
;;;

;; Because I don't like having another bogus stream class hierarchy.
;; I'm also not fond of possibly extra method calls. Of course it's pointless
;; because if other shit uses trival-gray-streams then the the bogus stuff is
;; back, and this is yet another useless package. In a world where CL wasn't
;; dead, and all this shit would have been fixed long long ago.

 #-(or sbcl clisp ecl openmcl allegro cmu clasp)
(error "We don't know how to use gray streams on ~s." (lisp-implementation-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or cmu genera) (require :gray-streams)
  #+allegro (unless (fboundp 'excl:stream-write-string)
	      (require "streamc.fasl"))
  #+(or ecl clasp) (gray::redefine-cl-functions))

(macrolet
    ((stupid ()
       (let ((symbols
	       `(;; types
		 #:fundamental-stream
		 #:fundamental-input-stream #:fundamental-output-stream
		 #:fundamental-character-stream #:fundamental-binary-stream
		 #:fundamental-character-input-stream
		 #:fundamental-character-output-stream
		 #:fundamental-binary-input-stream
		 #:fundamental-binary-output-stream
		 ;; functions
		 #:stream-read-char
		 #:stream-unread-char #:stream-read-char-no-hang
		 #:stream-peek-char #:stream-listen #:stream-read-line
		 #:stream-clear-input #:stream-write-char #:stream-line-column
		 #:stream-start-line-p #:stream-write-string #:stream-terpri
		 #:stream-fresh-line #:stream-finish-output
		 #:stream-force-output
		 #:stream-clear-output #:stream-advance-to-column
		 #:stream-read-byte #:stream-write-byte
		 #-(or ccl clisp) #:stream-file-position
		 ))
	     (package
	       #+sbcl :sb-gray
	       #+allegro :excl
	       #+cmu :ext
	       #+(or clisp ecl mocl clasp) :gray
	       #+openmcl :ccl
	       #+lispworks :stream
	       #+(or abcl genera) :gray-streams
	       #+mezzano :mezzano.gray))
	 ;; We can't use CL because of stupid shit like clashing with ‘close’.
	 `(defpackage :dgray
	    (:import-from ,package
	     ,@symbols)
	    (:export ,@symbols)
	    (:documentation
	     "Thinnest portability wrapper around Gray streams.")))))
  (stupid))


(defpackage :dgray-defaults
  (:use :cl :dgray)
  (:documentation "Make some missing default Gray stream methods."))

(in-package :dgray-defaults)

#+(or ccl clisp)
(progn
  (defgeneric stream-file-position (stream &optional position)))

(defmethod stream-file-position (stream #-cmu &optional #-cmu position)
  (declare (ignore position))
  nil)

;; End
