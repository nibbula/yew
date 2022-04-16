;;
;; ms/events.lisp - Windos interface to events and polling
;;

(in-package :opsys-ms)

(defcfun ("WaitForMultipleObjectsEx" %wait-for-multiple-objects-ex)
    DWORD
  (count	      DWORD) 		  ; in
  (handles	      (:pointer HANDLE))  ; in
  (wait-all	      BOOL)		  ; in
  (milliseconds	      DWORD) 		  ; in
  (alertable	      BOOL))  		  ; in

(defcfun ("WaitForSingleObject" %wait-for-single-object)
    DWORD
  (handle HANDLE)
  (milliseconds DWORD))

(defcfun ("WaitForMultipleObjects" %wait-for-multiple-objects)
    DWORD
  (count DWORD)
  (handles (:pointer HANDLE))
  (wait-all BOOL)
  (milliseconds DWORD))

(defcfun ("MsgWaitForMultipleObjects" %msg-wait-for-multiple-objects)
    DWORD
  (count DWORD)
  (handles (:pointer HANDLE))
  (wait-all BOOL)
  (milliseconds DWORD)
  (wake-mask DWORD))

(defcfun ("MsgWaitForMultipleObjectsEx" %msg-wait-for-multiple-objects-ex)
    DWORD
  (count	  DWORD)		; in
  (handles	  (:pointer HANDLE))	; in
  (milliseconds   DWORD)		; in
  (wake-mask	  DWORD)		; in
  (flags	  DWORD))		; in

(defconstant +QS-ALLEVENTS+	 #x04BF
  "An input, WM_TIMER, WM_PAINT, WM_HOTKEY, or posted message is in the queue.")
(defconstant +QS-ALLINPUT+       #x04FF "Any message is in the queue.")
(defconstant +QS-ALLPOSTMESSAGE+ #x0100 "A posted message is in the queue.")
(defconstant +QS-HOTKEY+         #x0080 "A WM_HOTKEY message is in the queue.")
(defconstant +QS-INPUT+          #x0407 "An input message is in the queue.")
(defconstant +QS-KEY+		 #x0001 "A key message is queued.")
(defconstant +QS-MOUSE+          #x0006
  "A mouse move or button message is queued.")
(defconstant +QS-MOUSEBUTTON+    #x0004 "A mouse button message is queued.")
(defconstant +QS-MOUSEMOVE+      #x0002 "A mouse move message is queued.")
(defconstant +QS-PAINT+          #x0020 "A WM_PAINT message is in the queue.")
(defconstant +QS-POSTMESSAGE+    #x0008 "A posted message is in the queue.")
(defconstant +QS-RAWINPUT+       #x0400 "A raw input message is in the queue.")
(defconstant +QS-SENDMESSAGE+    #x0040
  "A message sent by another thread or application is in the queue.")
(defconstant +QS-TIMER+          #x0010 "A WM_TIMER message is in the queue.")

(defconstant +MWMO-INPUTAVAILABLE+ #x0004
  "Return if input exists for the queue.")
(defconstant +MWMO-ALERTABLE+ #x0002
  "Return if an APC has been queued to the thread.")
(defconstant +MWMO-WAITALL+ #x0001
  "Return when all handles are signaled and an input event has been received.")

(defun listen-for (seconds handle)
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available. If handle isn't provided it tries to use STD-INPUT-HANDLE."
  (let ((milliseconds
	 (if (zerop seconds) 0 +INFINITE+)
	 ;; (ceiling (* seconds 1000))
	  )
	(result 0)
	(count 2))
    (with-timer (timer)
      (with-foreign-objects ((handles 'HANDLE count)
			     (due-time 'LARGE_INTEGER))
	(setf
	 (mem-aref due-time 'LARGE_INTEGER) (- (seconds-to-100ns seconds))
	 (mem-aref handles 'HANDLE 0) handle
	 (mem-aref handles 'HANDLE 1) timer)
	(%set-waitable-timer timer
			     due-time
			     0 (null-pointer) (null-pointer)
			     +FALSE+)
	(setf result (%wait-for-multiple-objects
		      count handles +FALSE+ milliseconds))
	;; (%msg-wait-for-multiple-objects
	;;  count handles
	;;  +FALSE+
	;;  milliseconds
	;;  (logior +QS-KEY+ +QS-TIMER+))
	(cond
	  ;; None of this really matters.
	  ;; ((and (>= result +WAIT-OBJECT-0+)
	  ;; 	(<= result (+ +WAIT-OBJECT-0+ (1- count))))
	  ;;  (dbugf :ms "listen-for wait-object = ~s~%"
	  ;; 	   (- result +WAIT-OBJECT-0+)))
	  ;; ((and (>= result +WAIT-ABANDONED-0+)
	  ;; 	(<= result (+ +WAIT-ABANDONED-0+ (1- count))))
	  ;;  (dbugf :ms "listen-for abandoned = ~s~%"
	  ;; 	   (- result +WAIT-ABANDONED-0+)))
	  ;; ((= result +WAIT-IO-COMPLETION+)
	  ;;  (dbugf :ms "listen-for IO completion = #x~x~%" result))
	  ;; ((= result +WAIT-TIMEOUT+)
	  ;;  (dbugf :ms "listen-for IO timeout = #x~x~%" result))
	  ((= result +WAIT-FAILED+)
	   (error 'windows-error :error-code (get-last-error)
		  :format-control "listen-for:")))))))

(defun %create-event-set (set)
  (declare (ignore set)))

(defun %destroy-event-set (set)
  (declare (ignore set)))

(defun %add-event (event set)
  "Add the event to the SET."
  (declare (ignore event set)))

(defun %delete-event (event set)
  "Delete the event from the SET."
  (declare (ignore event set)))

(defun %clear-triggers (set)
  "Clear the triggers for the event SET."
  (declare (ignore set)))

(defun await-events (&key (event-types t) (event-set *event-set*) timeout
		       (leave-triggers nil))
  "Wait for events of the given EVENT-TYPES, or T for any event. Return if we
don't get an event before TIMEOUT. TIMEOUT can be NIL wait potentially forever,
or T to return immediately, otherwise it's a OS-TIME, or a number of seconds.
The default for TIMEOUT is NIL. If LEAVE-TRIGGERS it T, it will not clear
triggers in the EVENT-SET, that were set before being invoked.
Retruns NIL if the timeout was up before getting any events, otherwise return
the count of event triggered."
  (declare (ignore event-types event-set timeout leave-triggers)))

(defun pick-events (event-types &key (event-set *event-set*) remove timeout)
  "Return any pending events of the types given in EVENT-TYPES. If REMOVE is
true, remove the events from the EVENT-SET. Return NIL if there aren't any
events before TIMEOUT. TIMEOUT can be NIL wait potentially forever, or T to
return immediately, otherwise it's a OS-TIME. The default for TIMEOUT is NIL."
  (declare (ignore event-types event-set remove timeout))
  )

(defun map-events (function &key (event-set *event-set*) (event-types t))
  "Call FUNCTION for each event in EVENT-SET, that is pending or triggered.
EVENT-TYPES restricts the events mapped to those types."
  (declare (ignore function event-set event-types)))

(defun events-pending-p (&key (event-types t) (event-set *event-set*))
  "Return true if there are any events pending in the EVENT-SET. Restrict the
events considered to those in EVENT-TYPES, if it's not T."
  (declare (ignore event-types event-set)))

;; End
