;;;; package.lisp

(defpackage #:gtk-emacs-like-input
  (:nicknames :eli)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp :cl)
  (:export :eli-error
	   ;; eli structure, slots, accessors
	   :bar :left :middle :entry :right :keymap-top
	   :keymap-instant :buffer :key :interactive :window
	   :eli-bar :eli-left :eli-middle :eli-entry :eli-right :eli-keymap-top
	   :eli-keymap-instant :eli-buffer :eli-key :eli-interactive :eli-window
	    ;; gtk-emacs-like-input
	   :make-bar
	   :new-keymap
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
	   ;; keymap.lisp
	   :bind
	   :new-keymap
	   :keymap-symbol-at
	   :keymap-keystr-at
	   :keymap-match
	   :keymap-exact-match
	  )
  )



