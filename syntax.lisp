;;
;; syntax.lisp - Generic language syntax
;;

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
   #:token-object #:token-type #:token-data-type #:token-location
   #:token-time #:token-original
   #:read-token
   #:parse-syntax
   #:print-syntax
   #:format-comment-text
   #:stylize-token
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
  "Guess the language used in a stream."
  (loop :for l :in *syntaxes*
     :if (guess-syntax l stream)
     :return :it))

#|

On one hand, we would like to have standard types, so that we can make various
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

But, since it seems a widely accepted consensus (at the time), we should try
to follow the guidlines used by TextMate (and subsequently used by Linguist,
GitHub, Sublime Text, Atom, etc.) See:
http://manual.macromates.com/en/language_grammars#naming_conventions

comment
  line
    double-slash
    double-dash
    number-sign
    percentage
    <whatever-comment-character>
  block
  documentation

constant
  numeric
  character
    escape
  language
  other

entity
  name
    function
    type
    tag
    section
  other
    inherited-class
    attribute-name

invalid
  illegal
  deprecated

keyword
  control
  operator
  other

markup
  underline
    link
  bold
  heading
  italic
  list
    numbered
    unnumbered
  quote
  raw
  other

meta

storage
  type
  modifier

string
  quoted
    single
    double
    triple
    other
  unquoted
  interpolated
  regexp
  other

support
  function
  class
  type
  constant
  variable
  other

variable
  parameter
  language
  other

In general we should remember that editing/highlighting grammars are almost
always different than, or at least a superset of, parsing grammars. Editing
grammars have to represent comments, sections of invalid syntax, and
sometimes whitespace. Ideally editing grammars should be able to be included in
each other, and in the most expansive case, have any section of a file be
designated as any available grammar. 

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

(defgeneric stylize-token (token &key &allow-other-keys)
  (:documentation
   "Return a string or fat-string of the token stylized in the current theme."))

;; @@@ maybe this should just be part of stylize-token
(defgeneric format-comment-text (token stream &key columns)
  (:documentation
   "Format the comment text for reading outside the context of a program.
Usually this means as documentation. This interprets any special markup or
conventions for the language. Output should be specialized on the stream if
necessary."))

;; EOF
