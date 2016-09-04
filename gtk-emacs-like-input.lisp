;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
(defparameter *window* nil)
;;; The key to using eli is the eli structure.
;;; label    gtk-label containing static text on left;
;;; entry    gtk-entry containing editable text on right;
;;; binding  binding currently in effect
;;; inter    interactive function installed or nil
;;; key      last key invoking your handler or nil
(defstruct eli label entry binding key inter)

;;; An interactive function in installed in the inter field, and as of
;;; the next keystroke, takes over the keyboard processing.  It may need
;;; to construct or break down data....
(defun app-quit (eli)
  (declare (ignore eli))
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

(defun fun1 (eli) (format t "fun1") )
(defun fun2 (eli) (format t "fun2") )

(defun fun3 (eli)
  (format t "fun3")
  (if (eli-key eli) 
      nil
      (setf (eli-inter eli) #'fun3) ;install itself as interactive
      ))

(eli-def-inter fun4
  (format t "OK")
  nil)

(defparameter *keymap-top* '(("top")))
(defun bind-keys ()
  (eli-bind "C-x C-c" #'app-quit)
  (eli-bind "C-c" #'fun4))

(defun input-reset (eli)
  "reset input state and visuals.  Return t"
  (setf (eli-binding eli) (car *keymap-top*)
	(gtk-entry-text (eli-entry eli)) "")
  (gtk-label-set-text (eli-label eli) "")
  t)

(defun input-reset-all(eli)
  "reset input state and visual.  If in interactive, send it
a notification to terminate.  Return t"
  (let ((inter (shiftf (eli-inter eli) nil)))
    (and old (funcall inter eli 2))) ;ask to terminate
  (input-reset eli))

(defun label-append (eli text)
  "In the label widget, append text to the existing text."
  (gtk-label-set-text
   (eli-label eli)
   (concatenate 'string (gtk-label-get-text (eli-label eli)) text)))


(defun input-keystroke (eli)
  ;;; Process C-g as global reset
  (if (eq key #x1000067)
      (input-reset-all eli) ; returns t! no further processing
      (progn
	(setf (eli-key eli) key)
	(if (eli-inter eli)
	    (funcall (eli-inter eli) eli 1) ;returns nil to process keys in gtk
	    (let* ((key (eli-key eli))
		   (new-binding (binding-locate key (eli-binding eli)))
		   (name (key-str key)))
	      (format t "New binding: ~A ~A~%" (cdr new-binding) (type-of (cdr new-binding)))
	      (if new-binding
		  (typecase (cdr new-binding)
		    (cons ;it's another binding
		     (label-append eli name)
		     (setf (eli-binding eli) new-binding))
		    (function ;it's a function
		     (format t "FUNCTION ~A~%" (cdr new-binding))
		     (setf (eli-key eli) nil) ;interactive functions check this to install
		     (funcall (cdr new-binding) eli 0)
		     (format t "FUNCTION DONE~%")
		     (input-reset eli))
		    (t (format t "ERROR: binding malformed")
		       (input-reset-all eli)))
		  (progn
		    (format t "binding not found")
		    (input-reset-all eli)))
	      t ;in all above cases, we eat the keystroke.
	      )))))

(let ((eli (make-eli))
      (eli-bar nil))
  
  (defun on-key-press (widget event)
    "Process a key from GTK; return key structure or nil for special keys"
    (declare (ignore widget))
;;;    (format t "ON-KEY-PRESS")
    (let ((gtkkey (gdk-event-key-keyval event)))
      (setf (eli-key eli)
	    (make-key gtkkey (gdk-event-key-state event)))
      ;(format t "...~A~%" gtkkey)
      (or (modifier-p gtkkey)	;t if skiping modifier keypresses
	  (input-keystroke eli) ;let them decide...
	  )))
  
  (defun make-eli-bar ()
    (setf eli-bar (make-instance 'gtk-box :orientation :horizontal :vexpand nil)
	  (eli-label eli) (make-instance 'gtk-label :label "test"
				       :expand nil)
	  (eli-entry eli) (make-instance 'gtk-entry :label "edit"))
    (gtk-box-pack-start  eli-bar (eli-label eli) :expand nil)
    (gtk-box-pack-end eli-bar (eli-entry eli))
    (input-reset-all eli)

    eli-bar))


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
	  (eli-bar (make-eli-bar)))
      (gtk-box-pack-start workspace dummy)
      (gtk-box-pack-end workspace eli-bar :expand nil)
      (gtk-container-add *window* workspace)
      )
      (bind-keys)

    (g-signal-connect *window* "destroy"
		      (lambda (widget)
			(declare (ignore widget))
			(format t "done")
			(leave-gtk-main)))
      
    (gtk-widget-show-all *window*)
      
      
      ))





