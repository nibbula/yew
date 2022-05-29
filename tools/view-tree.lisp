;;;
;;; view-tree.lisp - Command to view a tree.
;;;

(defpackage :view-tree
  (:documentation "Command to view a tree.")
  (:use :cl :lish)
  (:export
   #:!view-tree
   ))
(in-package :view-tree)

#+lish
(lish:defcommand view-tree
  ((root object :short-arg #\r :default "<root>"
    :help "A root node to use when ‘tree’ is a list.")
   (tree object :optional t :default 'lish:*input*
    :help "Object to view as a tree. Should be a list or a tree of
           tree-viewer:node."))
  "View a tree with the tree viewer."
  (typecase tree
    (list
     (tree-viewer:view-tree (cons root tree)))
    (t
     (tree-viewer:view-tree tree))))
    

;; End
