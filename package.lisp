;;;; package.lisp

(defpackage #:gtk-emacs-like-input
  (:nicknames :eli)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp :cl)
  (:export :eli-error 
	   :bind
	   :binding-of
	   :binding-name
	   :binding-print
	   :binding-locate
	   :make-bar
	   :set-interactive
	   :*keymap-top*
	   :on-key-press)
  )



