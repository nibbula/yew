;;;
;;; view-audio.lisp - Audio viewer.
;;;

;; @@@ This is a totaly fake wrapper around something, probably ffplay.
;; Someday this maybe could be a wrapper around libav.
;; Are there any CL decoders even?

(defpackage :view-audio
  (:documentation "Audio viewer. This is just a wrapper for now.")
  (:use :cl :dlib :lish)
  (:export
   #:view-audio
   #:view-audios
   #:!view-audio
   ))
(in-package :view-audio)

(defstruct player
  command
  args
  description)

(defparameter *audio-players*
  (mapcar (_ (make-player
	      :command (first _) :args (second _) :description (third _)))
    '(("nvlc"   ()                 "Ncurses Video LAN Client")
      ("mpv"    ()                 "Mplayer based media player")
      ("ffplay" ("-loglevel" "-8") "Play things with ffmpeg"))))

(defun play-it (file)
  (loop :for p :in *audio-players* :do
    (when (nos:command-pathname (player-command p))
      (apply #'!= `(,(player-command p) ,(player-args p) ,file))
      (return-from play-it nil)))
  (error "I'm very sorry, but I can't find a player. Check *audio-players*."))

(defun view-audio (media-designator &key file-list own-window)
  "View a audio described by MEDIA-DESIGNATOR.
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

(defun view-audios (files &rest args &key own-window)
  (declare (ignorable own-window))
  (if (not files)
      (apply #'view-audio *standard-input* args)
      (apply #'view-audio (first files) :file-list files args)))

#+lish
(lish:defcommand view-audio
  ((files pathname :repeating t :help "Files to view.")
   (own-window boolean :short-arg #\o
    :help "True to use it's own window if the backend supports it."))
  "View / listen to an audio file."
  :accepts '(:sequence :stream #| audio:audio |#)
  (view-audios (or files
		   (and lish:*input* (if (not (listp lish:*input*))
					 (list lish:*input*)
					 lish:*input*))
		   (list *standard-input*))
	       :own-window own-window))

;; End
