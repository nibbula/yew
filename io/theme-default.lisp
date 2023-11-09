;;;
;;; theme-default.lisp - Defaults for theme.
;;;

(defpackage :theme-default
  (:documentation "Defaults for theme.")
  (:use :cl :theme :fatchar)
  (:export
   #:default-theme
   #:set-theme-defaults-for-16-color
   #:default-theme-16-color
   #:set-theme-defaults-for-monochrome-dark
   #:set-theme-defaults-for-monochrome-light
   #:default-theme-monochrome-dark
   #:default-theme-monochrome-light
   ))
(in-package :theme-default)

(defun default-theme ()
  (let ((tt (make-theme
	     'default
	     :title "Default theme."
	     :description (format nil "~
A default theme that isn't too unexpected and works for a dark background.
Something like the default setting of typical GNU tools."))))
    (set-theme-items
     tt
     ;; File type
     `((:file :type :directory             :style) (:bold :fg-blue)
       (:file :type :link                  :style) (:bold :fg-cyan)
       (:file :type :symbolic-link         :style) (:bold :fg-cyan)
       (:file :type :broken-link           :style) (:bold :fg-red)
       (:file :type :pipe                  :style) (:fg-black :fg-yellow)
       (:file :type :socket                :style) (:bold :fg-magenta)
       (:file :type :block-device          :style) (:bg-black :fg-yellow :bold)
       (:file :type :character-device      :style) (:bg-black :fg-yellow :bold)
       (:file :type :setuid                :style) (:fg-white :bg-red)
       (:file :type :setgid                :style) (:fg-black :bg-yellow)
       (:file :type :sticky                :style) (:fg-white :bg-blue)
       (:file :type :sticky-other-writable :style) (:fg-black :bg-green)
       (:file :type :other-writable        :style) (:fg-blue :bg-green)
       (:file :type :group-writable        :style) (:fg :color
						    #(:rgb8 #xff #x7f #x24))
       (:file :type :executable            :style) (:bold)
       ;; File name suffixes
       (:file :suffix :archive             :style) (:bold :fg-red)
       (:file :suffix :compressed          :style) (:bold :fg-cyan)
       (:file :suffix :image               :style) (:bold :fg-magenta)
       (:file :suffix :video               :style) (:bold :fg-magenta)
       (:file :suffix :audio               :style) (:fg-cyan)
       (:file :suffix :ignorable           :style) (:fg :color #(:rgb .5 .5 .5))
       (:file :suffix :notable             :style) (:underline :fg-yellow)
       ;; Syntax
       (:syntax :comment :line :semicolon  :style) (:fg-cyan)
       (:syntax :comment :block            :style) (:fg-cyan)
       (:syntax :constant :character       :style) (:fg-white)
       (:syntax :constant :language        :style) (:fg-white)
       (:syntax :constant :other           :style) (:fg-white)
       (:syntax :entity :name :function    :style) (:fg-red)
       ;; (:syntax :entity :name :macro       :style) (:fg-red)
       (:syntax :entity :name :macro       :style) (:fg :color #(:rgb 1. .6 .1))
       (:syntax :entity :name :type        :style) (:fg-blue)
       (:syntax :entity :keyword :operator :style) (:fg-magenta)
       (:syntax :entity :keyword :control  :style) (:fg-magenta)
       (:syntax :support :function         :style) (:fg-magenta)
       (:syntax :support :variable         :style) (:fg-green)
       (:syntax :support :function         :style) (:fg-magenta)
       (:syntax :variable :other           :style) (:fg-green)
       ;; Non-standard syntax things
       (:syntax :docstring :code :style) (:fg :color #(:rgb8 #x7f #xff #xd4))
       (:syntax :docstring :style)       (:white)
       ;; Commands
       (:command :not-found	    :style) (:red)
       (:command :found		    :style) (:bold :blue)
       (:command :directory	    :style) (:fg :color #(:rgb .0 .5 .9))
       ;;(:command :system-command    :style) (:fg :color #(:rgb .4 .5 .9))
       (:command :system-command    :style) (:fg :color #(:rgb .0 .8 .9))
       (:command :external-command  :style) (:fg :color #(:rgb .0 .5 .9))
       (:command :builtin-command   :style) (:fg :color #(:rgb .9 .4 .7))
       (:command :shell-command	    :style) (:fg :color #(:rgb .7 .5 .9))
       (:command :loadable-system   :style) (:fg :color #(:rgb .9 .6 .0))
       (:command :command	    :style) (:bold :blue)
       (:command :alias		    :style) (:fg :color #(:rgb .8 .7 .9))
       (:command :function	    :style) (:magenta)
       (:command-arg :existing-path :style) (:underline)
       ;; Programs
       (:program :modeline             :style)     (:standout)
       (:program :search-match         :style)     (:underline :red)
       (:program :completion :difference :style)   (:underline :bold)
       (:program :empty-line-indicator :style)     (:normal)
       (:program :empty-line-indicator :character) #\~
       (:program :selection            :style)     (:standout)
       (:program :label		       :style)     (:cyan)
       (:program :data		       :style)     (:green)
       (:program :meter		       :character) #.(code-char #x2592)
       (:program :input                :style)     (:bg-blue :fg-cyan)
       (:program :suggestion           :style)
       (:fg :color #(:rgb8 #x50 #x50 #x50))
       (:program :table :current-cell  :style)     (:fg-yellow :inverse)
       (:program :table :current-row   :style)     (:inverse)
       (:program :tree :open-indicator)            ,(ß '(:magenta "▾"))
       (:program :tree :closed-indicator)          ,(ß '(:cyan "▸"))
       (:program :editor :paren-highlight :style)  (:standout)
       (:program :org-mode :colors)
         (:white :red :green :yellow :blue :magenta :cyan)
       (:program :org-mode :default :style)        ()
       ))
    tt))

(defun set-theme-defaults-for-16-color (theme)
  (set-theme-items theme
    `((:file :type :executable      :style) (:green :bold)
      (:command :directory	    :style) (:blue)
      (:command :system-command     :style) (:cyan)
      (:command :external-command   :style) (:cyan :bold)
      (:command :builtin-command    :style) (:magenta)
      (:command :shell-command      :style) (:magenta)
      (:command :loadable-system    :style) (:magenta)
      (:command :alias		    :style) (:magenta :bold)
      (:program :suggestion         :style) (:fg :black :bold)
      (:syntax :entity :name :macro :style) (:fg-yellow))))

(defun default-theme-16-color ()
  "Default theme for 16-color"
  (let ((theme (default-theme)))
    (set-theme-defaults-for-16-color theme)
    theme))

(defun set-theme-defaults-for-monochrome-dark (theme)
  (set-theme-items theme
    `((:file :type :directory             :style) ()
      (:file :type :link                  :style) ()
      (:file :type :symbolic-link         :style) ()
      (:file :type :broken-link           :style) ()
      (:file :type :pipe                  :style) ()
      (:file :type :socket                :style) ()
      (:file :type :block-device          :style) ()
      (:file :type :character-device      :style) ()
      (:file :type :setuid                :style) ()
      (:file :type :setgid                :style) ()
      (:file :type :sticky                :style) ()
      (:file :type :sticky-other-writable :style) ()
      (:file :type :other-writable        :style) ()
      (:file :type :group-writable        :style) ()
      (:file :type :executable            :style) ()
      ;; File name suffixes
      (:file :suffix :archive             :style) ()
      (:file :suffix :compressed          :style) ()
      (:file :suffix :image               :style) ()
      (:file :suffix :video               :style) ()
      (:file :suffix :audio               :style) ()
      ;; Syntax
      (:syntax :comment :line :semicolon  :style) ()
      (:syntax :comment :block            :style) ()
      (:syntax :comment :block            :style) ()
      (:syntax :constant :character       :style) ()
      (:syntax :constant :language        :style) ()
      (:syntax :constant :other           :style) ()
      (:syntax :entity :name :function    :style) ()
      (:syntax :entity :name :macro       :style) ()
      (:syntax :entity :name :type        :style) ()
      (:syntax :entity :keyword :operator :style) ()
      (:syntax :entity :keyword :control  :style) ()
      (:syntax :support :function         :style) ()
      (:syntax :support :variable         :style) ()
      (:syntax :support :function         :style) ()
      (:syntax :variable :other           :style) ()
      ;; Commands
      (:command :not-found	    :style) ()
      (:command :found		    :style) ()
      (:command :directory	    :style) ()
      (:command :system-command     :style) ()
      (:command :external-command   :style) ()
      (:command :builtin-command    :style) ()
      (:command :shell-command	    :style) ()
      (:command :loadable-system    :style) ()
      (:command :command	    :style) ()
      (:command :alias		    :style) ()
      (:command :function	    :style) ()
      (:command-arg :existing-path :style) (:underline)
      ;; Programs
      (:program :modeline                 :style)     (:standout)
      (:program :search-match             :style)     (:underline)
      (:program :empty-line-indicator     :style)     (:normal)
      (:program :empty-line-indicator     :character) #\~
      (:program :selection                :style)     (:standout)
      (:program :label		          :style)     ()
      (:program :data		          :style)     ()
      (:program :meter		          :character) #.(code-char #x2592)
      (:program :input                    :style)     ()
      (:program :suggestion               :style)     ()
      (:program :table :current-cell      :style)     (:inverse)
      (:program :tree :open-indicator)    "-"
      (:program :tree :closed-indicator)  "+"
      (:program :org-mode :colors)        (:default)
      ;; (:program :return-value-indicator   :string)    "⇒"
      ;; (:program :return-value-indicator   :style)     (:cyan)
      ;; (:program :multiple-value-indicator :string)    ";"
      )))

(defun set-theme-defaults-for-monochrome-light (theme)
  (set-theme-defaults-for-monochrome-dark theme))

(defun default-theme-monochrome-dark ()
  "Default theme for monochrome with a dark background."
  (let ((theme (default-theme)))
    (set-theme-defaults-for-monochrome-dark theme)
    theme))

(defun default-theme-monochrome-light ()
  "Default theme for monochrome with a light background."
  (let ((theme (default-theme)))
    (set-theme-defaults-for-monochrome-dark theme)
    theme))

;; @@@ We should probably make a default monochrome theme.
;; And default light backgroud themes.

(when (not *theme*)
  (setf *theme* (default-theme)))

;; End
