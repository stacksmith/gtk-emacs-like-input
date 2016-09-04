;;;; package.lisp

(defpackage #:gtk-emacs-like-input
  (:nicknames :eli)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp :cl)
  (:export :eli-bind
	   :make-eli-bar
	   :eli-def
	   :eli-def-inter
	   :*keymap-top*)
  )



