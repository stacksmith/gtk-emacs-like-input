;;;; package.lisp

(defpackage #:gtk-emacs-like-input
  (:nicknames :eli)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp :cl)
  (:export :eli-error
	   ;; eli structure, slots, accessors
	   :eli-bar :eli-left :eli-middle :eli-entry :eli-right :eli-keymap-top
	   :eli-keymap-instant :eli-buffer :eli-key :eli-interactive :eli-window
	    ;; gtk-emacs-like-input
	   :make-eli
	   :reset
	   ;; useful bindings and callbacks
	   :inst-cancel
	   :inst-back-up
	   :on-key-press
	   ;; keysyms.lisp
	   :gtkname->gtkcode
	   :gtkcode->gtkname
	   :gtkmodifier-p          ;is this gtkcode a modifier character
	   ;; keystroke.lisp
	   :key-val             ;accessor for key's gtkkey value
	   :key-mod             ;accessor for key's modifier mask
	   :key->character
	   :write-key
	   :key->string
	   :make-key            ;from gtk on-key data
	   :kbd
	   ;; keymap.lisp
	   :bind
	   :new-keymap 
	   :keymap-bval-at
	   :keymap-keyseq-at
	   :keymap-match
	   :keymap-exact-match
	  )
  )



