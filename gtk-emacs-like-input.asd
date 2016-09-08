;;;; gtk-emacs-like-input.asd

(asdf:defsystem #:gtk-emacs-like-input
  :description "Describe gtk-emacs-like-input here"
  :author "Stacksmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :depends-on (#:cl-cffi-gtk)
  :serial t
  :components ((:file "package")
	       (:file "conditions")
	       (:file "util")
	       (:file "keysyms")
	       (:file "keystroke")
	       (:file "keymap")
               (:file "gtk-emacs-like-input")))

