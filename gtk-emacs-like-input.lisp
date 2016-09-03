;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
(defparameter *window* nil)


(defun app-quit ()
  (gtk-widget-destroy *window*)
  (format t "quit done~%")
;  (gtk-main-quit)
;  (g-signal-emit *window* "delete-event")
  )

(defun fun1 () (format t "fun1"))
(defun fun2 () (format t "fun2"))
(defun fun3 (key)
  (format t "fun3")
  (if key
      ()
      #'fun3) ;install itself as interactive
  )

(defparameter *keymap-top* '(("top")))
(bind "C-x C-c" #'app-quit)
(bind "C-c" #'fun1)


;;; For interactive commands, we must have access to the input buffer, as well
;;; as act on tab for completion and return for execution...

(defstruct eli label entry)

(let ((eli-bar nil)
      (eli-static nil)
      (eli-editor nil)

      (binding (car *keymap-top*))
      (interactive-function nil)
      )
  (defun input-reset ()
    "reset input state and visuals"
    (setf binding (car *keymap-top*))
    (gtk-label-set-text eli-static "")
    (setf  (gtk-entry-text eli-editor) ""))

  (defun input-reset-all ()
    "reset input state and visual as well as interactive state"
    (setf interactive-function nil)
    (input-reset))
  (defun input-keystroke (key)
    (if interactive-function)
    (let* ((new-binding (binding-locate key binding))
	   (name (key-str key)))
	      (format t "New binding: ~A ~A~%" key (type-of (cdr new-binding)))
	      (if new-binding
		  (typecase (cdr new-binding)
		    (cons
		     (gtk-label-set-text
		      eli-static
		      (concatenate 'string (gtk-label-get-text eli-static) name))
		     (setf binding new-binding))
		    (function
		     (format t "FUNCTION ~A~%" (cdr new-binding))
		     ;; install interative if function pointer returned
		     (setf interactive-function (funcall (cdr new-binding) nil))
		     (format t "FUNCTION DONE~%")
		     (input-reset)))
		  (progn
		    (format t "binding not found")
		    (input-reset-all))))
    )
  (defun on-key-press (widget event)
    "Process a key from GTK; return key structure or nil for special keys"
    (declare (ignore widget))
    (let* ((gtkkey (gdk-event-key-keyval event))
	   (key (make-key gtkkey (gdk-event-key-state event))))
      ;;(format t "...~A~%" gtkkey)
      (unless (modifier-p gtkkey)	;skip modifier keypresses
	;;(format t "key: ~A ~A ~A~%" gtkkey (gdk-event-key-state event)(key-str key))
	;;; Process C-g as global reset
	(if (eq key #x1000067)
	    (input-reset-all)
	    (input-keystroke key))))  
    t)

  (defun make-eli-bar ()
    (setf eli-bar (make-instance 'gtk-box :orientation :horizontal :vexpand nil)
	  eli-static (make-instance 'gtk-label :label "test"
				      :expand nil)
	  eli-editor (make-instance 'gtk-entry :label "edit"))
    (gtk-box-pack-start eli-bar eli-static :expand nil)
    (gtk-box-pack-end eli-bar eli-editor)
    (g-signal-connect eli-editor "key-press-event" 'on-key-press)
    (input-reset)
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
      

    (g-signal-connect *window* "destroy"
		      (lambda (widget)
			(declare (ignore widget))
			(format t "done")
			(leave-gtk-main)))
      
    (gtk-widget-show-all *window*)
      
      
      ))





