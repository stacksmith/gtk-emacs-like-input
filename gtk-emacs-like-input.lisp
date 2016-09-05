;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
;;; The key to using eli is the eli structure.
;;; label    gtk-label containing static text on left;
;;; entry    gtk-entry containing editable text on right;
;;; binding  binding currently in effect
;;; key      last key invoking your handler or nil
;;; inter    nil=non-interactive, 1 means inter in progress, 2=killing inter
;;; An interactive function in installed in the inter field, and as of
;;; the next keystroke, takes over the keyboard processing.  It may need
;;; to construct or break down data....


(defun set-interactive (symbol &optional (value t))
  "set symbol's interactive status to value"
  (setf (get symbol 'interactive) value))

(defparameter *keymap-top* '(("")))

(defun buffer->string (buffer)
  "convert a vector of keys into its string representation"
  (loop for key across buffer
       with result = ""
     do (setf result (concatenate (key->string key)))
     finally (return result)))

(let ((bar nil)      ;eli bar
      (label nil)    ;static label
      (entry nil)    ;text entry

      (buffer nil)   ;collect keys
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
    (format t "1~%")
    (setf buffer (make-array 32 :fill-pointer 0 :adjustable t)
	  (gtk-entry-text entry) "")
    (trace)
    (gtk-label-set-text label "")
    (when full ; reset interactive stuff
      (and interactive
	   (funcall interactive 2))
	(setf interactive nil))
    t)
  
  (defun back-up ()
    (format t "BACKING UP ~A~%" 1 )
     )
  
  (defun input-keystroke ()
    "process a keystroke."
    (case key
      (#x1000067 (reset :full t)) ; returns t! reset and uninstall interactive fun)
      (#x100FF08 (back-up) t)
      (t
       (if interactive
	   (funcall interactive 1) ;returns nil to process keys in gtk
	   (progn
	     (vector-push-extend key buffer) ;append key
	     (format t "key ~A" key)
	     (let ((indices (indices-of *keymap-top* (buffer->string buffer))))
	       (format t "~A matches~%" indices)
	       (when (= 1 (length indices))
		 (let ((bound-value (keymap-symbol-at *keymap-top* (car indices) )))
		   (if (get bound-value 'interactive) ;if interactive function,
		       (funcall (setf interactive (symbol-function bound-value)) 0)
		       (funcall (symbol-function bound-value))))))
	     
	     t)))))
  

  (defun on-key-press (widget event)
    "Process a key from GTK; return key structure or nil for special keys"
    (declare (ignore widget))
    (let ((gtkkey (gdk-event-key-keyval event)))
      (setf key (make-key gtkkey (gdk-event-key-state event)))
     ;;; (format t "ON-KEY-PRESS ~A~%" key)
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
    bar))

(defparameter *window* nil)
(defun app-quit ()
  (gtk-widget-destroy *window*)
  (format t "quit done~%")
;  (gtk-main-quit)
;  (g-signal-emit *window* "delete-event")
  )
(defun fun1 () (format t "fun1") )
(defun fun2 () (format t "fun2") )
(defun fun3 (stage)
  (case stage
    (0 (format t "fun3: 0") t)
    (1 (format t "fun3: 1") nil)
    (2 (format t "fun3: 2") t))
)
(setf (get 'fun3 'interactive) t)
(defun bind-keys ()
  (setf *keymap-top* (new-keymap))
  (bind *keymap-top* "C-x C-c" 'app-quit)
  (bind *keymap-top* "C-BS" 'backup)
  (bind *keymap-top* "C-a" 'fun1)
  (bind *keymap-top* "C-b" 'fun2)
  (bind *keymap-top* "C-c" 'fun3))
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





