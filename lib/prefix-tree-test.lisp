;;;
;;; prefix-tree-test.lisp - Test for the prefix-tree package.
;;;

(defpackage :prefix-tree-test
  (:documentation "Test for the prefix-tree package.")
  (:use :cl :test :dlib :dlib-misc :prefix-tree)
  (:export
   #:run
   ))
(in-package :prefix-tree-test)

(defparameter *tree* nil)
(defparameter *table* nil)
(defparameter *word-list* nil)
(defparameter *big-file* "/usr/share/dict/words")

(defparameter *test-words*
  '("fulfils" "traced" "meddling" "mocking" "longings" "flagitiousness" "tell"
 "overflow" "characters" "continency" "enters" "heights" "intimately"
 "materials" "baby" "exposed" "compacting" "buy" "observed" "need" "effort"
 "closing" "acquire" "err" "forsooth" "became" "pleasure" "harmonized"
 "longings" "toiling" "wounded" "flashes" "impressions" "temporal" "advantage"
 "midnight" "worthy" "meditations" "hell" "goeth" "leaveth" "excused"
 "primitive" "qualified" "resembling" "hesitating" "bottom" "employed"
 "slipped" "evils" "omnipotency" "severing" "staggered" "unwilling" "ago"
 "presideth" "forethink" "broken" "nill" "thorns" "storm" "invited" "sex"
 "stormed" "consider" "plunged" "stolen" "favourites" "prove" "healthful"
 "bore" "fettered" "faintly" "exclusions" "petty" "commendeth" "along" "highly"
 "figures" "sacraments" "prevails" "rising" "tides" "duck" "forgetfulness"
 "organization" "ages" "north" "counted" "got" "seldomness" "thanks"
 "incorruption" "habit" "sparrow" "slay" "auditor" "heaps" "wearisome" "haunt"
 "harmony" "lengthened" "effected" "uncertain" "breathing" "pilgrimage"
 "searches" "willeth" "exhibit" "harmonious" "rough" "language" "grew"
 "excelling" "battle" "rood" "only" "wanderer" "my" "examining" "dangerous"
 "prayed" "reconciliation" "govemed" "example" "cause" "error" "substances"
 "supporting" "clearness" "instantly" "observed" "ministry" "contritions"
 "despise" "gather" "slowly" "counselled" "brother" "unawares" "submitted"
 "fugitives" "whole" "exhalation" "loathing" "notes" "rushing" "birthright"
 "stomach" "renowned" "tookest" "ill" "whereupon" "marrow" "mention" "images"
 "slowly" "seldomness" "june" "virgin" "fairest" "less" "herbs" "displays"
 "known" "steered" "countenance" "sink" "extricate" "secure" "cannot" "crowned"
 "toil" "sublime" "collected" "offices" "flashing" "exist" "bereaved"
 "defilements" "appointments" "walking" "plenary" "anxiously" "endure"
 "quickened" "concerned" "continued" "owing" "willest" "estate" "affirming"
 "rhetoric" "oppresseth" "rites" "signs" "apostle" "childbearing" "prosper"
 "difficulty" "injury" "unjustly" "recalled" "minute" "vexed" "initiated"
 "vigorous" "hereat" "covetous" "treasury" "changeableness" "grantest" "bridal"
 "repentance" "metre" "everywhere" "indications" "defile" "bowing" "smelling"
 "laying" "relationship" "apply" "whirlpool" "palace" "mouthed" "drunken"
 "hearest" "point" "trials" "strife" "intercession" "disagreements" "star"
 "fixedly" "obviously" "warned" "slow" "consecrate" "resigned" "perish"
 "goaded" "organs" "celibacy" "diving" "heal" "furthermore" "confute" "feeds"
 "judgements" "consecrating" "begins" "separateth" "fluidness" "curious"
 "tempt"))

(deftests (lookup :doc "Trie creation an lookup")
  :setup (progn
	   (setf *tree* (make-trie *word-list*)
		 *table* (make-hash-table :test #'equal))
	   (loop :for word :in *word-list*
		 :do (setf (gethash word *table*) t)))
  :takedown (progn
	      (setf *tree* nil
		    *table* nil
		    *word-list* nil))
  "Test that all the words are there."
  (loop :for word :in *word-list*
	:when (not (lookup word *tree*))
	  :do
	  (return nil)
	:finally (return t))

  "Test that one shorter words aren't found."
  (loop :for word :in (mapcar (_ (subseq _ 0 (1- (length _)))) *word-list*)
	:when (and (not (gethash word *table*)) (lookup word *tree*))
	  :do (return nil)
	:finally (return t))

  "Test that first char words aren't found."
  (loop
    :for word :in (mapcar (_ (subseq _ 0 1)) *word-list*)
    :when (and (not (gethash word *table*)) (lookup word *tree*))
      :do
	 (return nil)
    :finally (return t))

  "Test that one longer aren't found."
  (loop
    :for word :in (mapcar (_ (concatenate 'string _
					  (subseq _ (1- (length _))
						  (length _))))
			  *word-list*)
    :when (and (not (gethash word *table*)) (lookup word *tree*))
      :do
	 (return nil)
    :finally (return t))

  "Test that randomized words aren't found."
  ;; Doesn't work for words shorter than 2, or words which have all letters
  ;; the same.
  (loop
    :with swizzled = (mapcar (_ 
			      (loop
				:with v
				:do
				   (setf v (randomize-vector
					    (copy-seq _) :factor 1))
				   ;; (format t "Trying ~a~%" v)
				:while (equal v _)
				:finally (return v)))
			     (remove-if
			      (lambda (w)
				(or (< (length w) 2)
				    (every (_ (char= _ (char w 0))) w)))
			      *word-list*))
    :for i = 0 :then (1+ i)
    :for word :in swizzled
    :when (and (not (gethash word *table*)) (lookup word *tree*))
      :do
	 (return nil)
    :finally (return t)))

(defun test-lookup-speed (word-list)
  (let ((tree (make-trie word-list))
	(table (make-hash-table :test #'equal)))

    (loop :for word :in word-list
      :do (setf (gethash word table) t))

    (write-line "Tree")
    (nos:fake-time ()
     (loop
       :for word :in word-list
       :when (not (lookup word tree)) :do
	 (error "failed to find ~s in ~s." word word-list)))
    (format t "~s~%" nos:*time-result*)

    (write-line "Hash")
    (nos:fake-time ()
     (loop
       :for word :in word-list
       :when (not (gethash word table)) :do
	 (error "failed to find ~s in ~s." word word-list)))
    (format t "~s~%" nos:*time-result*)

    ;; (write-line "List")
    ;; (nos:fake-time ()
    ;;  (loop
    ;;    :for word :in word-list
    ;;    :when (not (find word word-list)) :do
    ;; 	 (error "failed to find ~s in ~s." word word-list)))
    ;; (format t "~s~%" nos:*time-result*)
    ))

(deftests (small-lookup)
  :setup (setf *word-list* *test-words*)
  lookup)

(deftests (big-lookup)
  (if (probe-file *big-file*)
      (progn
	(setf *word-list* (file-lines *big-file*))
	(run-group-name 'lookup))
      (prog1 t (format t "---Skipped---~%"))))

(defparameter *endings-list* '("dharma" "dhoti's" "dhoti" "dhotis"))

(deftests (endings)
  (null (endings "dhi" (make-trie '("dharma" "dhoti" "dhotis"))))
  (equal (endings "dh" (make-trie '("dharma" "dhoti" "dhotis")))
	 '("arma" "oti" "otis"))
  (equal (endings "dho" (make-trie '("dharma" "dhoti" "dhotis")))
	 '("ti" "tis"))
  (null (endings "be" (make-trie '("foo" "bar" "baz"))))
  (equal (endings "b" (make-trie '("foo" "bar" "baz")))
	 '("ar" "az"))
  (equal (endings "fo" (make-trie '("foo" "bar" "baz")))
	 '("o"))
  (equal (endings "foo" (make-trie '("foo" "bar" "baz")))
	 nil)
  (equal (endings "bar" (make-trie '("foo" "bar" "baz")))
	 nil)
  )

(deftests (prefix-tree-all :doc "All test for prefix-tree.")
  "foo"
  small-lookup
  big-lookup
  endings
  )

(defun run ()
  (run-group-name 'prefix-tree-all :verbose t))

;; End
