;;;
;;; view-video.lisp - Video viewer.
;;;

;; @@@ This is a totaly fake wrapper around your video player.
;; Someday this might be a wrapper around libav.
;; It would be amazing if someday this isn't a wrapper around any non-Lisp code.

(defpackage :view-video
  (:documentation "Video viewer. This is just a wrapper for now.")
  (:use :cl :lish)
  (:export
   #:*video-player-command*
   #:play-it
   #:view-video
   #:view-videos
   #:!view-video
   ))
(in-package :view-video)

(defvar *video-player-command*
  ;; `("ffplay" "-loglevel" "-8")
  `("mpv" "--really-quiet")
  "Command to play a video. A lish command line as a list with separate
arguments, as accepted by the != function. Must accept the filename as the last
argument.")

(defgeneric play-it (file)
  (:documentation "Play the video ‘file’.")
  (:method (file)
    (apply #'!= (append *video-player-command* (list file)))))

(defun view-video (media-designator &key file-list own-window)
  "View a video described by MEDIA-DESIGNATOR.
FILE-LIST    A list of files.
OWN-WINDOW   If true use a separate window."
  (declare (ignore own-window))
  (cond
    (file-list
     (loop :for f :in file-list :do
	(play-it f)))
    (media-designator
     (etypecase media-designator
       ((or string pathname)
	(play-it media-designator))
       (t
	(error "I don't know how to play a ~s~%" media-designator))))))

(defun view-videos (files &rest args &key own-window)
  (declare (ignorable own-window))
  (if (not files)
      (apply #'view-video *standard-input* args)
      (apply #'view-video (first files) :file-list files args)))

#+lish
(lish:defcommand view-video
  ((files pathname :repeating t :help "Files to view.")
   (own-window boolean :short-arg #\o
    :help "True to use it's own window if the backend supports it.")
   )
  "View a video."
  :accepts '(:sequence :stream #| video:video |#)
  (view-videos (or files
		   (and lish:*input* (if (not (listp lish:*input*))
					 (list lish:*input*)
					 lish:*input*))
		   (list *standard-input*))
	       :own-window own-window))

;; End
