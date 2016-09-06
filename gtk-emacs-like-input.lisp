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


(defun buffer->string (buffer)
  "convert a vector of keys into its string representation"
  (with-output-to-string (s)
    (loop for key across buffer do (write-key key s ))))

;;; inspired by let over lambda, eli creates a closure full of state information
;;; and functions.  It returns an eli bar, ready to insert into your application.
;;; It seems to work like a structure, without the annoyance of the accessor
;;; syntax.  My initial attempt was full of (eli-left eli) type of code, which
;;; now is simply left.
;;; - One issue: simply compiling a function inside eli no longer works; -- it
;;; compiles a function that will compile your function.  You have to run
;;; eli for that to happen!  It also creates a bar, but I think it's ok...
;;;

(defun eli (inst)
  (let ((keymap-top nil)
	(keymap-instant nil)
	(bar nil)      ;eli bar
	(left nil)     ;left - shows entered keys
	(middle nil)   ;
	(entry nil)
	(right nil)    ;right - status info

	(buffer nil)   ;collect keys
	(key nil)      ;key currently in effect
	(interactive nil) ;function pointer to installed interactive function
	(instance inst)
	)

    (defun keymap-top () keymap-top)
    (defun set-keymap-top (value) (setf keymap-top value))
    (defun keymap-instant () keymap-instant)
    (defun set-keymap-instant (value) (setf keymap-instant value))
    (defun instance () instance)
    
    (defun render (&key (match nil))
      (let ((keystr(buffer->string buffer) ))
	(format t "RENDER ~A~%" keystr)
	(unless match
	  (unless (zerop (length keystr))
	    (setf match (keymap-match keymap-top keystr))))
	(gtk-label-set-text left keystr)
	(gtk-label-set-text middle "")
					;      (setf (gtk-entry-text middle) "")
	(and (listp match)	;could be symbol
	     (gtk-label-set-text right(format nil "~A matches" (length match))))))
    
    (defun reset (&key (full nil))
      "reset input state and visuals."
      (format t "RESET: instance ~A~%" instance)
      (setf buffer (make-array 32 :fill-pointer 0 :adjustable t)
	    (gtk-entry-text entry) "")
      (gtk-widget-hide entry)
      
      (gtk-label-set-text left "")
      (gtk-label-set-text middle "")
      (gtk-label-set-text right "")

      (when full ; reset interactive stuff
	(and interactive
	     (funcall interactive 2))
	(setf interactive nil))
      t)
      
    (defun use-entry (on)
      (format t "USE-ENTRY~%")
      (if on
	  (progn
	    (gtk-widget-hide middle)
	    (gtk-widget-show entry)
	    (gtk-widget-grab-focus entry))
	  (progn
	    (gtk-widget-hide entry)
	    (gtk-widget-show middle))))

;;; Instant commands
    (defun cmd-cancel ()
      (format t "cmd-cancel~%")
      (reset :full t))

    (defun cmd-back-up ()
      (format t "int ~A SIZE: ~A~%" interactive (length buffer))
  
      (if interactive
	  nil ;let interactive do its own backup
	  (progn
	    (unless (zerop (length buffer))
	      (vector-pop buffer)
	      (render))
	    t;we eat the key
	      )))
    
    (defun dispatch-match (match)
      (if (get 'match 'interactive)
	  (funcall (setf interactive (symbol-function match)) 0)
	  (progn
	    (funcall (symbol-function match))
	    (reset)
	    t)))
    (defun input-keystroke ()
      "process a keystroke."
          (format t "~A input keystroke~%" instance)
      (let ((match (keymap-exact-match keymap-instant (key->string key))))
	(unless (if match
		    (progn
		      (format t "MATCH: ~A~%" match)
		      (funcall (symbol-function match)))
		    ) ;instant processing nil? continue
	  (if interactive
	      (funcall interactive 1) ;returns nil/t to process keys in gtk
	      (progn
		(vector-push-extend key buffer) ;append key
		(let ((match (keymap-match keymap-top (buffer->string buffer))))
		  (render :match match)
		  (unless (listp match) (dispatch-match match)))
		t)))))
    
    (defun on-key-press (widget event)
      "Process a key from GTK; return key structure or nil for special keys"
      (declare (ignore widget))

      (let ((gtkkey (gdk-event-key-keyval event)))
	(setf key (make-key gtkkey (gdk-event-key-state event)))
      (format t "~A ON-KEY-PRESS ~A~%" instance key)
	(or (modifier-p gtkkey) ;do not process modifiers, gtk will handle them
	    (input-keystroke) ;let them decide if to continue with key process
	    )))
    
    (defun make-bar ()
      
      (setf bar (make-instance 'gtk-box :orientation :horizontal )
	    left (make-instance 'gtk-label :label "left"  )
	    middle (make-instance 'gtk-label :label "middle" )
	    entry (make-instance 'gtk-entry :label "entry"  )
	    right (make-instance 'gtk-label :label "right" ))
      
      (gtk-box-pack-start    bar left :expand nil)
      (gtk-box-pack-start    bar middle :expand t)
      (gtk-box-pack-start    bar entry :expand t)
      (gtk-box-pack-start    bar right :expand nil)
      (format t "BAR: ~A ~A ~A ~A ~A~%" bar left middle entry right)
      bar))
  ;;; finally, return the closed-over bar!
  (make-bar))




(defun test (inst)
  (let ((window nil))
    (defun fun1 () (format t "fun1 instancd ~A~%" (instance))
	   (reset))
    (defun fun2 () (format t "fun2")
	   (reset :full t))
    (defun fun3 (stage)
      (case stage
	(0 (format t "fun3: 0 instance ~A~%" (instance))
	   (use-entry t) 1)
	(1 (format t "fun3: 1 instance ~A~%" (instance) ) nil)
	(2 (format t "fun3: 2  instance ~A~%" (instance))
	   (use-entry nil) t))
      )
    
    (defun bind-keys ()
      (set-keymap-top (new-keymap))
      (bind (keymap-top) "C-xC-c" 'app-quit)
      (bind (keymap-top) "C-a" 'fun1)
      (bind (keymap-top) "C-b" 'fun2)
      (bind (keymap-top) "C-c" 'fun3)

      (set-keymap-instant (new-keymap))
      (bind (keymap-instant) "C-g" 'cmd-cancel)
      (bind (keymap-instant) "BS" 'cmd-back-up)
      (setf (get 'fun3 'interactive) t)
      )

    (defun app-quit ()
      (gtk-widget-destroy window)
      (format t "quit done~%")
					;  (gtk-main-quit)
					;  (g-signal-emit *window* "delete-event")
      )
    (defun run (&key (stdout *standard-output*))
      (let ((gtk::*main-thread* nil))
	(within-main-loop
	  (setf *standard-output* stdout) ;enable output in this thread
	  (setf window (make-instance 'gtk-application-window
				      :title "eli-test"
				      :type :toplevel
				      :border-width 0
				      :default-width 640
				      :default-height 480))
	  
	  ;; create a window with a command line on the bottom.      
	  (let ((workspace (make-instance 'gtk-box :orientation :vertical))
		(dummy (make-instance 'gtk-box ))
		(bar (eli inst)))
	    (gtk-box-pack-start workspace dummy)
	    (gtk-box-pack-end workspace bar :expand nil)
	    (gtk-container-add window workspace)
	    )
	  
	  (bind-keys)
	  (g-signal-connect window "key-press-event" #'on-key-press)
	  (g-signal-connect window "destroy"
			    (lambda (widget)
			      (declare (ignore widget))
			      (format t "done")
			      (leave-gtk-main)))
	  
	  (gtk-widget-show-all window)
	  (reset :full t)
	  )))
    (run)
    ))


(defun damn ()
  (let ((window nil)
	(input nil))
    (defun ttt (&key (stdout *standard-output*))
      (within-main-loop
	(setf *standard-output* stdout) ;enable output in this thread
	
	;; create a window with a command line on the bottom.      
	(setf window (make-instance 'gtk-application-window
				    :title "eli-test"
				    :type :toplevel
				    :border-width 0
				    :default-width 640
				    :default-height 480)
	      input (make-instance 'gtk-entry ))
	(gtk-container-add window input)
	
	(g-signal-connect window "key-press-event"
			  (lambda (widget event)
			    (declare (ignore widget))
			    (let ((gtkkey (gdk-event-key-keyval event)))
			      (if (= gtkkey #xff0d)
				  (gtk-widget-hide input))
			      (format t "ON-KEY-PRESS ~A~%" gtkkey)))

			  )
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignore widget))
			    (format t "done")
			    (leave-gtk-main)))
	(gtk-widget-show-all window)
	))
    (ttt)))
