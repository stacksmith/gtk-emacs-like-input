;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
(defparameter *window* nil)
;;; The key to using eli is the eli structure.
;;; label    gtk-label containing static text on left;
;;; entry    gtk-entry containing editable text on right;
;;; binding  binding currently in effect
;;; key      last key invoking your handler or nil
;;; inter    nil=non-interactive, 1 means inter in progress, 2=killing inter
;;; An interactive function in installed in the inter field, and as of
;;; the next keystroke, takes over the keyboard processing.  It may need
;;; to construct or break down data....
(defun app-quit ()
  (gtk-widget-destroy *window*)
  (format t "quit done~%")
;  (gtk-main-quit)
;  (g-signal-emit *window* "delete-event")
  )

(defmacro eli-def-inter (name &body body)
  `(defun ,name (eli)
     (if (eli-key eli)
	 (progn ,@body)
	 (setf (eli-inter eli) #',name))))
(defmacro eli-def (name &body body)
  `(defun ,name (eli)
     ,@body
     t))

(defun fun1 () (format t "fun1") )
(defun fun2 () (format t "fun2") )

(defun fun3 (stage)
  (case stage
    (0 (format t "fun3: 0"))
    (1 (format t "fun3: 1"))
    (2 (format t "fun3: 2")))
)
(setf (get 'fun3 'interactive) t)


(defparameter *keymap-top* '(("top")))
(defun bind-keys ()
  (bind "C-x C-c" 'app-quit)
  (bind "C-a" 'fun1)
  (bind "C-b" 'fun2)
  (bind "C-c" 'fun3))





(let ((bar nil)      ;eli bar
      (label nil)    ;static label
      (entry nil)    ;text entry

      (binding nil)  ;current binding as eli walks keymaps
      (key nil)      ;key currently in effect
      (interactive nil) ;function pointer to installed interactive function
      )

  (defun label-append (text)
    "In the label widget, append text to the existing text."
    (gtk-label-set-text
     label
     (concatenate 'string (gtk-label-get-text label) text)))
  
  (defun reset (&key (full nil))
  "reset input state and visuals.  Return t"
    (setf binding (car *keymap-top*)
	  (gtk-entry-text entry) "")
    (gtk-label-set-text label "")
    (when full ; reset interactive stuff
      (and interactive
	   (funcall interactive 2))
	(setf interactive nil))
    t)
  
  (defun input-keystroke ()
    "process a keystroke."
    (if (eq key #x1000067) ;C-g is global reset
	(reset :full t) ; returns t! reset and uninstall interactive fun
	(if interactive
	    (funcall interactive 1) ;returns nil to process keys in gtk
	    ;; set binding to whatever we find for the key, and extract
	    ;; the bound value.
	    (let* ((new-binding (binding-locate key binding))
		   (bound-value (cdr new-binding)))
	      (format t "new-binding ~A~%" new-binding)
	      (typecase bound-value ;dispatch on bound value's type
		(null ())
		(cons		     ;it's another binding
		 (setf binding new-binding)
		 (label-append (key->string key))) ;display it in label
		(symbol
		 (format t "FUNCTION ~A~%" bound-value)
		 (if (get bound-value 'interactive) ;if interactive function,
		     (funcall (setf interactive (symbol-function bound-value)) 0)
		     (funcall (symbol-function bound-value)))) ;non-interactive...
		(t ;any other type is not allowed.
		 (error 'eli-error :where "input-keystroke" :what "binding malformed"
			:value binding)))
	      t))))
  

  (defun on-key-press (widget event)
    "Process a key from GTK; return key structure or nil for special keys"
    (declare (ignore widget))
    ;(format t "ON-KEY-PRESS")
    (let ((gtkkey (gdk-event-key-keyval event)))
      (setf key (make-key gtkkey (gdk-event-key-state event)))
      (or (modifier-p gtkkey) ;do not process modifiers, gtk will handle them
	  (input-keystroke) ;let them decide if to continue with key process
	  )))
  
  (defun make-bar ()
    (setf bar (make-instance 'gtk-box :orientation :horizontal :vexpand nil)
	  label (make-instance 'gtk-label :label "test"   :expand nil)
	  entry (make-instance 'gtk-entry :label "edit"))
    (gtk-box-pack-start  bar label :expand nil)
    (gtk-box-pack-end    bar entry)
    (reset :full t)
    bar
    ))

(defun  test (&key (stdout *standard-output*))
  
  (within-main-loop
    (setf *standard-output* stdout) ;enable output in this thread
    (setf *window* (make-instance 'gtk-window
				  :title "eli-test"
				  :type :toplevel
				  :border-width 0
				  :default-width 640
				  :default-height 480))
    
    ;; create a window with a command line on the bottom.      
    (let ((workspace (make-instance 'gtk-box :orientation :vertical))
	  (dummy (make-instance 'gtk-box ))
	  (bar (make-bar)))
      (gtk-box-pack-start workspace dummy)
      (gtk-box-pack-end workspace bar :expand nil)
      (gtk-container-add *window* workspace)
      )
    
      (bind-keys)
      (g-signal-connect *window* "key-press-event" #'on-key-press)
      (g-signal-connect *window* "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (format t "done")
			  (leave-gtk-main)))
      
      (gtk-widget-show-all *window*)))





