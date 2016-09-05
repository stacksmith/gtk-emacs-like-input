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
  (with-output-to-string (s)
    (loop for key across buffer do (write-key key s ))))

(let ((bar nil)      ;eli bar
      (left nil)    ;static label
      (middle nil)    ;
      (entry nil)
      (right nil)   ;status label

      (buffer nil)   ;collect keys
      (key nil)      ;key currently in effect
      (interactive nil) ;function pointer to installed interactive function
      )


  (defun render ()
    (let* ((keystr(buffer->string buffer) )
	   (match (keymap-match *keymap-top* keystr)))
      (gtk-label-set-text left keystr)
      (gtk-label-set-text middle "")
;      (setf (gtk-entry-text middle) "")
      (gtk-label-set-text right(format nil "~A matches" (length match))))
    
    )
  
  (defun reset (&key (full nil))
    "reset input state and visuals."
    (setf buffer (make-array 32 :fill-pointer 0 :adjustable t)
	  (gtk-entry-text entry) "")
    (trace)

    (gtk-label-set-text left "")
    (gtk-label-set-text middle "")
    (gtk-label-set-text right "")
    (when full ; reset interactive stuff
      (and interactive
	   (funcall interactive 2))
	(setf interactive nil))
    t)
  
  (defun back-up ()
    (format t "BACKING UP ~A~%" 1 )
    (unless (zerop (length buffer))
      (vector-pop buffer)))
  
  (defun input-keystroke ()
    "process a keystroke."
    (case key
      (#x1000067 (reset :full t)) ; returns t! reset and uninstall interactive fun)
      (#x100FF08 (back-up) t)
      (t
       (if interactive
	   (funcall interactive 1) ;returns nil/t to process keys in gtk
	   (progn
	     (vector-push-extend key buffer) ;append key
	     (render)
	     (format t "buffer string ~A~%" (buffer->string buffer))
	     (let ((match (keymap-match *keymap-top* (buffer->string buffer))))
	       (format t "~A matches~%" match)
	       (typecase match
		 (list (format t "~A possibilities: ~A~%" (length match) match))
		 (symbol
		  (if (get match 'interactive)
		      (progn ;install interactive and initialize it
			(funcall (setf interactive (symbol-function match)) 0))
		      (progn ;call function and reset
			(funcall (symbol-function match))
			(reset))))
		 ;;otherwise, unbound key - let the user back out
		 ))
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
      (setf bar (make-instance 'gtk-box :orientation :horizontal )
	    left (make-instance 'gtk-label :label "left"  )
	    middle (make-instance 'gtk-label :label "middle" )
	    entry (make-instance 'gtk-entry :label "entry" :hidden t )
	    right (make-instance 'gtk-label :label "right" ))
      (gtk-box-pack-start    bar left :expand nil)
      (gtk-box-pack-start    bar middle :expand t)
      (gtk-box-pack-start    bar right :expand nil)
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
  (bind *keymap-top* "C-xC-c" 'app-quit)
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





