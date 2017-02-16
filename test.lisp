;;;
;;; test.lisp - Test ting things
;;;

;; N.B.: This is exactly what one shouldn't do.
;; Perhaps we can agree upon a testing protocol? Then everyone could write their
;; own testing package and it wouldn't matter much? Right?

;; TODO:
;; - composability?
;; - sub-groups OR package specific groups?
;; - random testing (given argument types and ranges)
;; - heuristical fuzzing?

(defpackage :test
  (:documentation "Another crappy test framework.

Q: Hey! I just need to run some tests on my package! Like now!!
A: Okay. Here you go.

Q: How the heck do I use this thing?
A: Just say:
   (deftests group-name
     (form which is true if the test passed)
     (another such form)
     ...)

   (run-all-tests)

Q: What if a test fails? How do I know which one it was or what happened?
A: Say: (describe-test 'test-name). It will show you the code that failed.

Q: What if a bunch of tests fail?
A: Say (list-tests :failed t), to show all tests that failed.

Q: What if I need to set up pre-conditions for my tests?
A: You can use :setup and :takedown arguments to deftests.

Q: Hey, but what if I'd like to add tests dynamically whilst I'm mucking
   about in the REPL?
A: Say:
     (test-in :your-package)
   then say:
     (test (some code that returns non-NIL if it succeeds))
   When you want to run the tests, say:
     (run-tests)
   When you're done, say:
     (save-tests)
   and it will append them to a file.
   You will probably have to edit the output to make it reasonable.

Q: What if I need to capture the output from my test?
A: You're on your own.

Q: What if I need to trap exceptions from my tests?
A: You're on your own.

Q: What if I need to ...
A: I SAID, you're on your own!

Q: Why did you write yet another test framework?
A: <no comment>

Q: Are you at least going to write some documentation?
A: _This_ *is* your documentation.
")
  (:use :cl)
  (:export
   #:*verbose*
   #:test #:test-name #:test-doc #:test-func #:test-body
   #:test-group #:test-group-name #:test-group-tests #:test-group-doc
   #:test-group-setup #:test-group-takedown
   #:deftest
   #:deftests
   #:defgroup
   #:add-to-group-name
   #:run-group-name
   #:run-all-tests
   #:list-tests
   #:describe-test
   #:clear-tests
   #:test-in
   #:run-tests
   #:save-tests
   ))
(in-package :test)

(declaim (optimize (speed 1) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; If we do DEFPARAMETER this can clobber your test results. On the other
;; hand, tests from previous versions, may not be compatible. Since the normal
;; case is the tests are compatible, losing the tests is quite annoying. So
;; lets just say, if you make incompatible changes to this code, you had
;; better remember to to CLEAR-TESTS. Clobbers are bad, mmmmkay?
(defvar *tests* '()
  "List of test and/or test groups.")

(defparameter *verbose* t
  "T to print lines for every test. NIL to just show failures.")

(defvar *group* nil
  "The default test group to define tests in with the TEST macro.")

(defstruct test
  "Tests have:
- a NAME which is a symbol, perhaps a keyword if you want to run the tests
  from a different package than they're defined in.
- a function FUNC, which returns a truth value indication if the test succeeded
  or failed.
- BODY saves the forms of the test, so you don't have to refer back to the
  source.
- DOC says something about the test, or not."
   name
   doc
   func
   body
   result)

(defstruct test-group
  "Test groups are:
- a NAME which is a symbol, perhaps a keyword if you want to run the tests
  from a different package than they're defined in.
- a list of TESTS, or TEST-GROUPs
- a documentation string DOC
- a SETUP function, which is called before the group is run
- a TAKEDOWN function, which is called after the group is run"
  name
  tests
  doc
  setup
  takedown)

(defun find-test-in-group (name group)
  (loop :for x :in (test-group-tests group) :do
     (when (equalp (test-name x) name)
       (return x))))

(defun find-test (name)
  "Find a test with the given NAME."
  (loop :with result
     :for tt :in *tests* :do
     (cond
       ((test-p tt)
	(when (equalp (test-name tt) name)
	  (return tt)))
       ((test-group-p tt)
	(when (setf result (find-test-in-group name tt))
	  (return result))))))

(defun find-test-or-group (name &optional (group *tests*))
  "Find a test or a test group with the given NAME."
  (loop :with result
     :for tt :in group :do
     (cond
       ((test-p tt)
	(when (equalp (test-name tt) name)
	  (return tt)))
       ((test-group-p tt)
	(if (equal (test-group-name tt) name)
	    (return tt)
	    (when (setf result (find-test-or-group name (test-group-tests tt)))
	      (return result)))))))

(defun report-start (test)
  (when *verbose*
    (format t "Test ~a: " test)
    (finish-output)))

(defun report-done (result)
  (if *verbose*
      (if result
	  (format t "OK~%")
	  (format t "FAILED~%"))
      (if result
	  (format t ".")
	  (format t "F~_")))
  (finish-output)
  (if result t nil))

(defmacro deftest (name &body body)
  "Define an individual test, not in a group. BODY is a form returning a truth
value indicating if the test failed or not."
  (let ((func-name (intern (format nil "TEST-~:@(~a~)" name)))
	(new-body body) doc)
    (when (stringp (car body))
      (setf doc (car body)
	    new-body (cdr body)))
    `(progn
       (defun ,func-name ()
	   (report-start ',name)
	   (setf (test-result (find-test ',func-name))
		 (report-done ,@new-body)))
       (push (make-test :name ',name
			:func #',func-name
			:doc ,doc
			:body ',body) *tests*))))

(defun find-group (group-name &optional (group-list *tests*))
  "Return the test group of the given GROUP-NAME."
  (loop :for tt :in group-list :do
     (when (and (test-group-p tt) (eq group-name (test-group-name tt)))
       (return tt))))

;; (defun add-to-group (group test)
;;   "Add the TEST to the given GROUP."
;;   (push test group))

(defun clear-group (group-name)
  (let ((g (find-group group-name)))
    (when g
      (setf *tests* (delete g *tests*)))))

(defun add-to-group-name (group-name test)
  "Add the given TEST to the group named GROUP-NAME. Adds the group if it
doesn't already exist."
  (let ((group (find-group group-name)))
    (if group
	(push test (test-group-tests group))
	(push (make-test-group :name group-name :tests (list test))
	      *tests*))))

;; We used to just add the tests to the group, but then we would get multiple
;; versions of all the tests in the group when we reloaded the file. So instead
;; just clear the group and redefine all the tests.

(defmacro deftests ((group-name &key setup takedown doc) &body body)
  "Define a group of tests with the given GROUP-NAME. The BODY is any number
of forms which return a truth value indicating if the test failed. Each form
can optionally be preceded by a string, which is it's documentation. If a form
is a symbol, it is take as the name of a test group to run, allowing test group
nesting. The form can be keyword which is a substitute for one of the keyword
arguements."
;  (let* ((group (find-group group-name))
;         (n (length group)))
  (clear-group group-name)
  (let* ((n 0) (fdoc nil)
	(bodies
	 (loop :with func-name :and spot = body :and b
	    :while spot :do
	    (setf b (car spot))
	    (setf func-name
		  (intern (format nil "TEST-~:@(~a~)-~d" group-name n)))
	    :if (stringp b) :do
	      (setf fdoc b)
	    :else :if (symbolp b) :do
	      (case b
	        (:setup
		 (setf setup (cadr spot)) (setf spot (cdr spot)))
		(:takedown
		 (setf takedown (cadr spot)) (setf spot (cdr spot)))
		(:doc
		 (setf doc (cadr spot)) (setf spot (cdr spot)))
		(otherwise
		 (add-to-group-name group-name b)))
	    :else
	      :collect
	    `(progn
	       (defun ,func-name ()
		 (report-start ',func-name)
		 (setf (test-result (find-test ',func-name))
		       (report-done ,b)))
	       (add-to-group-name ',group-name
				  (make-test :name ',func-name
					     :func #',func-name
					     :doc ',fdoc
					     :body ',b)))
	      :and :do (setf fdoc nil) (incf n)
	    :end
	    :do (setf spot (cdr spot)))))
    `(progn ,@bodies
	    (let ((g (find-group ',group-name)))
	      (setf (test-group-tests g) (nreverse (test-group-tests g))
		    (test-group-setup g) ',setup
		    (test-group-takedown g) ',takedown
		    (test-group-doc g) ',doc))
;	    (setf (cadr (assoc ',group-name *tests*))
;		  (nreverse (cadr (assoc ',group-name *tests*))))
	    (values))))

(defun maybe-run-code (code)
  (cond
    ((null code) #| do nothing |#)
    ((or (functionp code) (and (symbolp code) (fboundp code)))
     (format t "Calling fixture ~a~%" code)
     (funcall code))
    ((listp code)
     (format t "Evaluating fixture ~a~%" code)
     (eval code))
    (t
     (error "Can't run fixture code ~a" code))))

;; This is so we can skip running referenced groups.
(defvar *running-all* nil
  "True if we are running all tests.")

(defun run-group (group)
  "Run the GROUP of tests. Return a pair of (pass-count fail-cout)."
  (let ((pass 0) (fail 0))
    (unwind-protect
       (progn
	 (maybe-run-code (test-group-setup group))
	 (loop :for x :in (test-group-tests group) :do
	    (cond
	      ((test-p x)
	       (if (funcall (test-func x))
		   (incf pass)
		   (incf fail)))
	      ((test-group-p x)
	       (let* ((pf (run-group x)))
		 (incf pass (first pf))
		 (incf fail (second pf))))
	      ((and (symbolp x) (not *running-all*) (find-group x))
	       (let* ((pf (run-group-name x)))
		 (incf pass (first pf))
		 (incf fail (second pf)))))))
      (maybe-run-code (test-group-takedown group)))
    (list pass fail)))

(defun run-group-name (group-name &key verbose)
  "Run the named group of tests. Return a pair of (pass-count fail-cout)."
  (let* ((pf (run-group (find-group group-name)))
	 (pass (first pf)) (fail (second pf)))
    (when verbose
      (format t "~&Passed: ~d~%Failed: ~d~%" pass fail)
      (if (zerop fail)
	  (format t "ALL Tests PASSED~%")
	  (format t "FAIL!")))
    pf))

(defun run-all-tests ()
  "Run all the tests. Let's you know what happend."
  (let ((pass 0) (fail 0) (*running-all* t))
    (loop :for x :in *tests*
       :do
       (cond
	 ((test-p x)
	  (if (funcall (test-func x))
	      (incf pass)
	      (incf fail)))
	 ((test-group-p x)
	  (let ((pf (run-group x)))
	    (incf pass (first pf))
	    (incf fail (second pf))))))
    (format t "~&Passed: ~d~%Failed: ~d~%" pass fail)
    (if (zerop fail)
	(format t "ALL Tests PASSED~%")
	(format t "FAIL!"))))

(defun list-tests (&key long (bodies t) failed)
  "List all the tests. If BODIES show the code for each. If LONG, show the
documentation."
  (labels ((print-test (test indent)
	     (cond
	       ((test-group-p test)
		(progn
		  (format t "~v,,,va~a:~%"  (- indent 2) #\space #\space
			  (test-group-name test))
		  (loop :for tst :in (test-group-tests test) :do
		     (print-test tst (+ indent 2)))))
	       ((symbolp test)
		(when failed (return-from print-test nil))
		(format t "~v,,,vaGroup ~a~%" indent #\space #\space test))
	       (t
		(when (and failed (test-result test))
		  (return-from print-test nil))
		(format t "~v,,,va~a" indent #\space #\space
			(test-name test))
		(when long
		  (format t " ~a" (test-doc test)))
		(when bodies
		  (format t " ~s" (test-body test)))
		(terpri)))))
    (loop :for x :in *tests* :do
       (print-test x 2))))

(defun describe-test (name)
  "Describe the test named NAME."
  (cond
    ((symbolp name)
     (let ((test (find-test-or-group name)))
       (if test
	   (describe-test test)
	   (format t "Can't find test name ~a.~%" name))))
    ((test-group-p name)
     (format t "Test group ~a:~%Doc: ~a~%"
	     (test-group-name name) (test-group-doc name))
     (when (test-group-setup name)
       (format t "Setup code: ")
       (pprint (test-group-setup name)) (terpri))
     (when (test-group-takedown name)
       (format t "Takedown code: ")
       (pprint (test-group-takedown name)) (terpri))
     (when (test-group-tests name)
       (format t "Tests (~d):~%" (length (test-group-tests name)))
       (loop :for tt :in (test-group-tests name) :do
	  (describe-test tt))))
    ((test-p name)
     (format t "~a:~@[~%~a~]~@[~%~s~]~%~%"
	     (test-name name) (test-doc name) (test-body name)))
    (t
     (error "Don't know how to describe a test of type ~a."
	    (type-of name)))))

(defun clear-tests ()
  "Forget about ALL the tests."
  (setf *tests* '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things for interactive test definition

(defun test-in (group)
  "Set the current test group to GROUP."
  (setf *group*
	(etypecase group
	  ((or symbol keyword)
	   group)
	  (string
	   (intern group))
	  ((or character number pathname)
	   (intern (princ-to-string group))))))

(defmacro test (&body test)
  "Define a test in the current test group. The test is an expression that
returns non-NIL to succeed."
  `(deftests (*group*) ,@test))

(defun run-tests (&optional (group *group*))
  "Run tests in the current test group."
  (run-group-name group))

;; We can't use the one in dlib-misc, because dependencies.
(defun date-string ()
  (multiple-value-bind (seconds minutes hours date month year day
				daylight-p zone) (get-universal-time)
    (declare (ignore day daylight-p zone))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    year month date hours minutes seconds)))

(defun save-header (group filename stream)
  "Write the header for the test group GROUP and a new file named FILENAME
to STREAM."
  (let ((name (test-group-name group)))
    (format stream ";;;~%;;; ~a - Tests for ~a~%;;;~%~%
(defpackage :~a-test
  (:documentation \"Tests for ~a\")
  (:use :cl :test :~a)
  (:export
   ))~%" filename name name name name)))

(defun save-tests (&optional (group *group*))
  "Write tests in GROUP to a file."
  (let* ((filename (format nil "~(~a~)-test.lisp"
			   (or (symbolp *group*) (package-name *package*))))
	 (file-exists (probe-file filename)))
    (format t "~a tests to ~a..."
	    (if file-exists "Appending" "Writing") filename)
    (finish-output)
    (with-open-file (stream filename :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
      (when (not file-exists)
	(save-header group filename stream))
      (format stream ";;; Tests for ~a generated on ~a~a~%"
	      *group* (date-string)
	      (if (find-package :nos)
		  (concatenate 'string " by "
			       (funcall (intern "USER-NAME" :nos)))
		  ""))
      (when (test-group-doc group)
	(format stream ";;; ~a:~%;;; ~a~%" (test-group-name group)
		(test-group-doc group)))
      (format stream "(deftests (~a)~%" (test-group-name group))
      (loop :for x :in (test-group-tests group) :do
	 (cond
	   ((test-p x)
	    (format stream "  ~s~%" (test-body x)))
	   ((test-group-p x)
	    (error "I don't know how to handle saving sub-groups."))))
      (format stream ")~%"))
    (format t "Tests saved to ~a.~%" filename)
    (format t "NOTE: You will probably have to edit them.~%")
    (finish-output)))

;; EOF
