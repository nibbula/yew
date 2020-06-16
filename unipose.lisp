;;
;; unipose.lisp - Compose unicode characters
;;

(defpackage :unipose
  (:documentation "Compose unicode characters.")
  (:use :cl :dlib)
  (:export
   #:unipose
   #:set-unipose
   #:save-unipose-for-emacs
   ))
(in-package :unipose)

;; I know this is a strange madness that most people won't want.
;; - It should really be made customizable.
;; - It should load *unipose* from $HOME/.unipose or something.
;; - It should have an "add your own" function.
;; - It should have a "save it" function.
;; - It should have a set which is RFC 1345
;; - It should handle arbitrary length / levels

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((<= char-code-limit #xff)
     (warn "Unipose isn't going to work for 8 bit characters.")
     (d-add-feature :t-8))
    ((<= char-code-limit #x10000)
     (warn "Some unipose sequences won't work with 16 bit characters.")
     (d-add-feature :t-16))
    ((< char-code-limit #x110000)
     (warn "You might be missing some characters.")
     (d-add-feature :t-shrunk))))

;; trouble makers:
;; #x1F18E 🆎 #x1F191 🆑 #x1F194 🆔 #x1F21A 🈚 #x1F196 🆖 #x1F197 🆗 #x1F19A 🆚

;; Slow scrollin' pardner.
(defparameter *unipose*
  #-t-8
  '((#\1 ((#\2 #\½) (#\4 #\¼) (#\^ #\¹)))
    (#\2 ((#\^ #\²)))
    (#\3 ((#\^ #\³) (#\4 #\¾)))
    (#\8 ((#\8 #\∞)))
    (#\A ((#\^ #\Â) (#\' #\Á) (#\` #\À) (#\" #\Ä) (#\E #\Æ) (#\o #\Å) (#\~ #\Ã)
	  #-t-16 (#\B #\🆎)
	  ))
    (#\a ((#\^ #\â) (#\' #\á) (#\` #\à) (#\" #\ä) (#\e #\æ) (#\o #\å) (#\~ #\ã)
	  (#\_ #\ª) (#\p #\)))
    (#\B ((#\B #\ß)))
    (#\c (((#\0 #\O) #\©) ((#\/ #\|) #\¢) (#\, #\ç) (#\* #\☪) (#\C #\￠)))
    (#\C (((#\0 #\O) #\©) ((#\/ #\|) #\￠) (#\, #\Ç) (#\* #\☪)
	  #-t-16 (#\L #\🆑)))
    (#\D ((#\D #\∆) (#\- #\Ð)))
    (#\d ((#\g #\˚)))
    (#\E ((#\^ #\Ê) (#\' #\É) (#\` #\È) (#\" #\Ë) ((#\~ #\- #\=) #\€)))
    (#\f ((#\~ #\ƒ) (#\f #\ﬀ) (#\i #\ﬁ) (#\l #\ﬂ) (#\t #\ﬅ))) ; what about ﬃ ﬄ
    (#\e ((#\^ #\ê) (#\' #\é) (#\` #\è) (#\" #\ë) ((#\~ #\- #\=) #\€)))
    (#\I ((#\^ #\Î) (#\' #\Í) (#\` #\Ì) (#\" #\Ï) #-t-16 (#\D #\🆔)))
    (#\i ((#\^ #\î) (#\' #\í) (#\` #\ì) (#\" #\ï) (#\j #\ĳ)))
    (#\L ((#\L #\Λ) (#\- #\£)))
    (#\l ((#\l #\λ)))
    (#\M #-t-16 ((#\U #\🈚)))
    (#\m ((#\u #\µ)))
    (#\N ((#\~ #\Ñ) #-t-16 (#\G #\🆖)))
    (#\n ((#\~ #\ñ)))
    (#\O ((#\^ #\Ô) (#\' #\Ó) (#\` #\Ò) (#\" #\Ö) (#\E #\Œ) (#\~ #\Õ) (#\O #\Ω)
	  #-t-16 (#\K #\🆗)))
    (#\o ((#\^ #\ô) (#\' #\ó) (#\` #\ò) (#\" #\ö) (#\e #\œ) (#\~ #\õ) (#\_ #\º)))
    (#\P ((#\H #\Φ) (#\I #\Π) (#\P #\¶)))
    (#\p ((#\h #\φ) (#\i #\π)))
    (#\r (((#\0 #\O #\o) #\®)))
    (#\S ((#\S #\∑)))
    (#\s ((#\e #\§) (#\r #\√) (#\s #\ß) (#\t #\ﬆ)))
    (#\T (((#\M #\m) #\™) (#\T #\Þ)))
    (#\t (((#\M #\m) #\™) (#\T #\þ)))
    (#\U ((#\^ #\Û) (#\' #\Ú) (#\` #\Ù) (#\" #\Ü)))
    (#\u ((#\^ #\û) (#\' #\ú) (#\` #\ù) (#\" #\ü)))
    (#\V #-t-16 ((#\S #\🆚)))
    (#\x ((#\x #\×) (#\* #\※)))
    (#\Y ((#\- #\¥) (#\' #\Ý)))
    (#\y ((#\- #\¥) (#\' #\ý)))
    (#\^ (;; captial circumflex
	 (#\A #\Â) (#\C #\Ĉ) (#\E #\Ê) (#\G #\Ĝ) (#\H #\Ĥ) (#\I #\Î) (#\J #\Ĵ)
	 (#\O #\Ô) (#\S #\Ŝ) (#\U #\Û) (#\W #\Ŵ) (#\Y #\Ŷ)
	 ;; lower circumflex
	 (#\a #\â) (#\c #\ĉ) (#\e #\ê) (#\g #\ĝ) (#\h #\ĥ) (#\i #\î) (#\j #\ĵ)
	 (#\o #\ô) (#\s #\ŝ) (#\u #\û) (#\w #\ŵ) (#\y #\ŷ)))
    (#\' (;; capital acute
	 (#\A #\Á) (#\E #\É) (#\I #\Í) (#\O #\Ó) (#\U #\Ú) (#\W #\Ẃ) (#\Y #\Ý)
	 ;; lower acute
	 (#\a #\á) (#\e #\é) (#\i #\í) (#\o #\ó) (#\u #\ú) (#\w #\ẃ) (#\y #\ý)
	 ;; misc acute
	 (#\' #\´) (#\< #\‘) (#\> #\’) (#\_ #\́)))
    (#\` (;; capital grave
         (#\A #\À) (#\E #\È) (#\I #\Ì) (#\O #\Ò) (#\U #\Ù) (#\W #\Ẁ) (#\Y #\Ỳ)
	 ;; lower greve
	 (#\a #\à) (#\e #\è) (#\i #\ì) (#\o #\ò) (#\u #\ù) (#\w #\ẁ) (#\y #\ỳ)
	 ;; misc grave
	 (#\_ #\̀)))
    (#\" (;; capital diaeresis
	  (#\A #\Ä) (#\E #\Ë) (#\I #\Ï) (#\O #\Ö) (#\U #\Ü) (#\W #\Ẅ) (#\Y #\Ÿ)
	  ;; lower diaeresis
	  (#\a #\ä) (#\e #\ë) (#\i #\ï) (#\o #\ö) (#\u #\ü) (#\w #\ẅ) (#\y #\ÿ)
	  (#\v #\„) (#\< #\“) (#\> #\”) (#\s #\ß)
	  ;; misc diaeresis
	  (#\_ #\̈)))
    (#\~ (;; upper twiddles
	 (#\A #\Ã) (#\C #\Ç) (#\D #\Ð) (#\G #\Ğ) (#\N #\Ñ) (#\T #\Þ) (#\U #\Ŭ)
	 ;; lower twiddles
	 (#\a #\ã) (#\c #\ç) (#\d #\ð) (#\g #\ğ) (#\n #\ñ) (#\t #\þ) (#\u #\ŭ)
	 ;; misc twiddles
	 (#\e #\€) (#\p #\¶) (#\s #\§) (#\u #\µ) (#\x #\¤) (#\? #\¿) (#\! #\¡)
	 (#\$ #\£) (#\. #\·) (#\< #\«) (#\> #\»)))
    (#\! ((#\! #\‼) (#\v #\¡) (#\? #\⁉) (#\/ #\❢)))
    (#\? ((#\v #\¿) (#\? #\⁇) (#\! #\⁈) ((#\| #\/) #\‽)))
    (#\/ ((#\O #\Ø) (#\o #\ø) (#\c #\¢) (#\= #\≠)))
    (#\| ((#\| #\¦)))
    (#\+ ((#\- #\±) (#\+ #\†)))
    (#\- ((#\+ #\±) (#\: #\÷)))
    (#\= ((#\/ #\≠) (#\/ #\≠) (#\~ #\≈)))
    (#\_ (;; combining characters
	  (#\^ #\̂) (#\` #\̀) (#\' #\́) (#\~ #\̃) (#\- #\̄) (#\_ #\̲)
	  (#\O #\⃝) (#\[ #\⃞) (#\< #\⃟)
	  ;; misc underscore
	  (#\^ #\¯)))
    (#\< ((#\= #\≤) (#\< #\«)))
    (#\> ((#\= #\≥) (#\> #\»)))
    (#\: ((#\- #\÷)))
    (#\. ((#\o #\•) (#\. #\·) (#\- #\⋅) (#\^ #\˚)))
    (#\$ ((#\$ #\¤)))
    (#\% ((#\% #\‰))))
  "Unicode compose character lists.")

;; Since this is probably just for me, does it really matter? 
(defun save-unipose-for-emacs ()
  (with-open-file (stream (glob:expand-tilde "~/src/el/unipose-data.el")
			  :direction :output
			  :if-exists :supersede)
    (format stream ";;;~%;;; unipose-data.el~%;;;~%~@
		    ;;; Automatically generated from UNIPOSE at ~a~%~%"
	    (dlib-misc::date-string))
    (print-unipose-for-emacs :stream stream)
    (format stream "~%;; EOF~%")))

(defun print-unipose-for-emacs (&key (tree *unipose*)
				  (stream *standard-output*) (depth 0) (n 0))
  "Print the unipose list for loading into emacs."
  (if (= depth 0)
      (progn
	(format stream "(setq *unipose*~%  '(")
	(loop
	   :for branch :in tree
	   :for n = 0 :then (+ n 1)
	   :do
	   (when (> n 0)
	     (write-string "   " stream))
	   (incf depth)
	   (print-unipose-for-emacs :tree branch :stream stream
				    :depth depth :n n)
	   (terpri stream))
	(format stream "   ))~%"))
      (progn
	(incf depth)
	(typecase tree
	  (null
	   (write-char #\) stream))
	  (list
	   (incf depth)
	   (when (and (> n 0) (> depth 1))
	     (write-char #\space stream))
	   (write-char #\( stream)
	   (loop
	      :for l :in tree
	      :for n = 0 :then (+ n 1)
	      :do
	      (print-unipose-for-emacs :tree l :stream stream
				       :depth depth :n n))
	   (write-char #\) stream))
	  (character
	   (when (> n 0)
	     (write-char #\space stream))
	   (write-char #\? stream)
	   (when (char= tree #\")
	     (write-char #\\ stream))
	   (write-char tree stream))
	  (t
	   (error "Unknown object type in unipose data: ~s ~s~%"
		  (type-of tree) tree))))))

(defun unipose (first-char second-char)
  "Return the character composed from FIRST-CHAR and SECOND-CHAR, or NIL if
there is none defined."
  (let ((level2 (cadr (assoc first-char *unipose*))))
    (or
     (dolist (level3 level2)
       (if (and (listp (car level3)) (position second-char (car level3)))
	   (return (cadr level3))
	   (when (eq (car level3) second-char)
	     (return (cadr level3)))))
     nil)))

;; @@@ This needs testing!! Don't use it yet!
(defun set-unipose (first-char second-char result)
  "Add a character composition to unipose."
  (let ((level2 (cadr (assoc first-char *unipose*))))
    (if (null level2)
	;; If there' not top level first-char, add it
	(setf *unipose* (acons first-char
			       (list (list second-char result)) *unipose*))
	;; Look at sub levels
	(dolist (level3 level2)
	  (if (and (listp (car level3)) (position second-char (car level3)))
	      ;; If multiple second-chars map to the same result
	      (when (not (eq (cadr level3) result))
		;; If the result is not the same, make a new pair
		(setf level3
		      (cons level3 (list (list second-char result)))))
	      ;; If it's a one to one mapping, add
	      (when (eq (car level3) second-char)
		(return (setf (cadr level3) result))))))
    nil))

;; Clean up
#+t-8      (d-remove-feature :t-8)
#+t-16     (d-remove-feature :t-16)
#+t-shrunk (d-remove-feature :t-shrunk)

;; EOF
