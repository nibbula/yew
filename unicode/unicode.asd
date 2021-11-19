;;;								-*- Lisp -*-
;;; unicode.asd - System definition for unicode
;;;

(defsystem unicode
    :name               "unicode"
    :description        "More crapware from your favorite dummy."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "W͢͢͝h͡o͢͡ ̸͢k̵͟n̴͘ǫw̸̛s͘ ̀́w͘͢ḩ̵a҉̡͢t ̧̕h́o̵r͏̵rors̡ ̶͡͠lį̶e͟͟ ̶͝in͢ ͏t̕h̷̡͟e ͟͟d̛a͜r̕͡k̢̨ ͡h̴e͏a̷̢̡rt́͏ ̴̷͠ò̵̶f̸ U̧͘ní̛͜c͢͏o̷͏d̸͢e̡͝?͞"
    :depends-on (:dlib)
    :components
    ((:file "package")
     (:file "encoding" :depends-on ("package"))
     (:file "utf8" :depends-on ("package" "encoding"))
     (:file "utf8b" :depends-on ("package" "encoding"))
     ;; (:file "char-width" :depends-on (""))
     ))
