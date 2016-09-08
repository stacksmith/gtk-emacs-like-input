### GTK-EMACS-LIKE-INPUT

## Overview

GTK-EMACS-LIKE-INPUT, eli for short, is a library that adds an emacs-like command bar to your arsenal of cl-cffi-gtk widgets.  It is emacs-like in spirit only, and has little to do with emacs, really.  But it makes it easy to bind keystrokes to commands, and lets you enjoy the familiar interface.

eli is a work in progress, and changes are being made to the interface.

## Quickstart

To add eli to your application, do something along the lines of
```
 (let* ((eli (make-eli window))) ; create eli for this top window
        (bar (eli-bar eli))      ; get the bar widget
   ... ;pack your gadgets into the window
   (gtk-box-pack-end container bar :expand nil) ; place the eli-bar gadget on the bottom 
   (g-signal-connect window "key-press-event" #'on-key-press) ; connect default key processing
   (gtk-widget-show-all window)
   (reset eli :full t)) ;perform a full reset
   ...
```
This will provide you with a command bar with minimum functionality: <BS> (backspace), <C-g> (abort) and <C-x><C-c> (quit).

To bind simple commands, 
```
    (bind (keymap-top eli)  "<C-a>abracadabra<RET>" #'fun1)
```
fun1 must be defined like this
```
(defun fun1 (eli)
  (format t "Fun1"))
```
eli is a structure full of eli-related stuff that will allow you to do all kinds of magic.

Binding a symbol, creates an 'interactive' function.  Such a function takes over the keyboard handling and interacts with the user.  Interactive functions are slightly more complicated: they get passed an eli structure and a keyword indicating what stage of processing is requested: :initialize, :process and :finalize.  :process means athat a new key is sitting in (eli-key eli).  The other stages give you a chance to set up and break down whatever you need to do.

Interactive functions also return a value - nil to let gtk continue processing the key or t if the processing is complete.
Here is a working example that allows text entry right in the bar:
```
    (bind (keymap-top eli)  "<C-a>" 'fun3)  ; bind an interactive function
	...
(defun fun3 (eli stage)
  (case stage
    (:initialize (use-entry eli t)  ;open a text entry box
	t) ;done processing
    (:process
     (format t "fun3: key ~A~%" (eli-key eli) )
     nil ;let gtk work with the entry editor...
     )
    (:finalize
     (use-entry eli nil)
     t))
  )
```

run (test) in gtk-emacs-like-input.lisp to see an example of minimal usage.
...
