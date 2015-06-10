;;
;; syntax.lisp - Generic language syntax
;;

;; $Revision: 1.1 $

(defpackage :syntax
  (:documentation "Generic language syntax")
  (:use :cl :dlib)
  (:export
   #:*syntaxes*
   #:syntax
   #:register-syntax
   #:guess-syntax
   #:guess-language
   #:token
   #:read-token
   #:parse-syntax
   #:print-syntax
   ))
(in-package :syntax)

(defvar *syntaxes* nil
  "List of registered syntax modules.")

(defclass syntax ()
  ((name :initarg :name :accessor syntax-name
    :documentation "Name of the syntax or language.")
   (description :initarg :description :accessor syntax-description
    :documentation "Description of the specific syntax.")
   (language-type :initarg :language-type :accessor syntax-language-type
    :documentation "Category of language.")
   (file-types :initarg :file-types :accessor syntax-file-types
    :documentation "List of likely file name extensions for this syntax.")
   (mime-type :initarg :mime-type :accessor syntax-mime-type
    :documentation "The MIME type of this language, if any."))
  (:documentation "A syntax type."))

(defun register-syntax (class)
  (pushnew class *syntaxes*))

(defmethod initialize-instance
    :after ((o syntax) &rest initargs &key &allow-other-keys)
  "Initalize a syntax."
  (declare (ignore initargs))
  (register-syntax (class-of o)))

(defgeneric guess-syntax (syntax stream)
  (:documentation
   "Return true if STREAM is very likely to be composed in the language defined
by SYNTAX."))

(defun guess-language (stream)
  (loop :for l :in *syntaxes*
     :if (guess-syntax l stream)
     :return :it))

#|

On one hand we would like to have standard types, so that we can make various
switchable themes which will cover all the types and be useful for all
languages. On the other hand, languages differ so much that it's probably
pointless and limiting to specify a required set of token types. We might also
like to have categories of types, so that we can apply styles to groups of
types.

For now, we'll just have names, and leave it up to style makers to specify what
they want. We can make something to enumerate all the currently known token
types so that you can cover what you want when writing a style.

(deftype token-type ()
  '(member
    :text.comment
    :constant.string
    :constant.number
    :constant.literal
    :name.built-in
    :name.keyword
    :name.operator
    :name.function
    :name.variable
    :name.label
    :name.namespace
    ))
|#

(defclass token ()
  ((object
    :initarg :object :accessor token-object
    :documentation "An object representing the token.")
   (type
    :initarg :type :accessor token-type
    :documentation "The token type. This is not a Lisp type.")
   (data-type
    :initarg :data-type :accessor token-data-type
    :documentation "The token data type.")
   (location
    :initarg :location :accessor token-location
    :documentation "Where the token was read from.")
   (time
    :initarg :time :accessor token-time
    :documentation "When it was read. A universal time.")
   (original
    :initarg :original :accessor token-original
    :documentation "The original string."))
  (:documentation "A syntactic unit of a language."))

(defgeneric read-token (syntax stream)
  (:documentation "Return a token from the stream"))

;; EOF
