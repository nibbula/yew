;;
;; view-generic.lisp - View a thing
;;

(defpackage :view-generic
  (:documentation
   "The generic view function, mostly by itself so as to minimize dependencies.
This is currently mostly for use by the 'view' shell command.")
  (:use :cl)
  (:export
   #:view
   ))
(in-package :view-generic)

;; Problems with this idea:
;; What about where to view it? For now it is implicit in the environment.
;; Also there may be more than one way to view it. We could consider verbs
;; like, view-on or view-with, but that seems clumsy. Also, viewing implies
;; not modifiing, but what if the viewer is an editor that allows modification?
;; An edit verb? CLIM has present and accept. How do these handle where and how?

(defgeneric view (thing)
  (:documentation "Look at something."))

;; EOF
