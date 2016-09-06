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
(defstruct eli keymap-top keymap-instant
	   bar left middle entry right
	   buffer key interactive instance)

(defparameter eli-map nil)

(defun render (eli &key (match nil))
  (let ((keystr(buffer->string (eli-buffer eli)) ))
    (format t "RENDER ~A~%" keystr)
    (unless match
      (unless (zerop (length keystr))
	(setf match (keymap-match (eli-keymap-top eli) keystr))))
	(gtk-label-set-text (eli-left eli)  keystr)
	(gtk-label-set-text (eli-middle eli) "")
					;      (setf (gtk-entry-text middle) "")
	(and (listp match)	;could be symbol
	     (gtk-label-set-text (eli-right eli)
				 (format nil "~A matches" (length match))))))

(defun reset (eli &key (full nil))
  "reset input state and visuals."
  (format t "RESET: instance ~A~%" (eli-instance eli))
  (setf (eli-buffer eli) (make-array 32 :fill-pointer 0 :adjustable t)
	(gtk-entry-text (eli-entry eli)) "")
  (gtk-widget-hide (eli-entry eli))
  
  (gtk-label-set-text (eli-left eli) "")
  (gtk-label-set-text (eli-middle eli) "")
  (gtk-label-set-text (eli-right eli) "")
  
  (when full ; reset interactive stuff
    (and (eli-interactive eli)
	 (funcall (eli-interactive eli) eli 2))
    (setf (eli-interactive eli) nil))
  t)

 (defun use-entry (eli on)
   (format t "USE-ENTRY~%")
   (if on
       (progn
	 (gtk-widget-hide (eli-middle eli))
	 (gtk-widget-show (eli-entry eli))
	 (gtk-widget-grab-focus (eli-entry eli)))
       (progn
	 (gtk-widget-hide (eli-entry eli))
	 (gtk-widget-show (eli-middle eli)))))

    
(defun cmd-cancel (eli)
  (format t "cmd-cancel~%")
  (reset eli :full t
	 ))

(defun cmd-back-up (eli)
  (format t "int ~A SIZE: ~A~%" (eli-interactive eli) (length (eli-buffer eli)))
  
  (if (eli-interactive eli)
      nil ;let interactive do its own backup
      (progn
	(unless (zerop (length (eli-buffer eli)))
	  (vector-pop (eli-buffer eli))
	  (render eli))
	t;we eat the key
	)))
    
(defun dispatch-match (eli match)
  (if (get match 'interactive)
      (funcall (setf (eli-interactive eli) (symbol-function match)) eli 0)
      (progn
	(funcall (symbol-function match) eli)
	(reset eli)
	t)))

(defun input-keystroke (eli)
  "process a keystroke."
  (format t "input keystroke inst ~A~%" (eli-instance eli))
  (let ((match (keymap-exact-match (eli-keymap-instant eli) (key->string (eli-key eli)))))
    (unless (if match
		(progn
		  (format t "MATCH: ~A~%" match)
		  (funcall (symbol-function match) eli))
		) ;instant processing nil? continue
      (if (eli-interactive eli)
	  (funcall (eli-interactive eli) eli 1) ;returns nil/t to process keys in gtk
	  (progn
	    (vector-push-extend (eli-key eli) (eli-buffer eli)) ;append key
	    (let ((match (keymap-match (eli-keymap-top eli) (buffer->string (eli-buffer eli)))))
	      (render eli :match match)
	      (unless (listp match) (dispatch-match eli match)))
	    t)))))


(defun on-key-press (widget event)
  "Process a key from GTK; return key structure or nil for special keys"
  (let ((eli (gethash widget eli-map))))

  (let ((gtkkey (gdk-event-key-keyval event)))
    (setf (eli-key eli) (make-key gtkkey (gdk-event-key-state event)))
    (format t "ON-KEY-PRESS INSTANCE ~A ~A~%" (eli-instance eli) (eli-key eli))
    (or (modifier-p gtkkey) ;do not process modifiers, gtk will handle them
	(input-keystroke eli) ;let them decide if to continue with key process
	)))
  
(defun make-bar (window)
  (let ((eli (make-eli)))
    (unless eli-map
      (setf eli-map (make-hash-table)))
    (setf (gethash window eli-map) eli)
    
    (let ((bar (make-instance 'gtk-box :orientation :horizontal ))
	  (left (make-instance 'gtk-label :label "left"  ))
	  (middle (make-instance 'gtk-label :label "middle" ))
	  (entry (make-instance 'gtk-entry :label "entry"  )) 
	  (right (make-instance 'gtk-label :label "right" )))
      (setf (eli-bar eli) bar 
	    (eli-left eli) left
	    (eli-middle eli) middle
	    (eli-entry eli) entry
	    (eli-right eli) right)
      
      (gtk-box-pack-start    bar left :expand nil)
      (gtk-box-pack-start    bar middle :expand t)
      (gtk-box-pack-start    bar entry :expand t)
      (gtk-box-pack-start    bar right :expand nil)
      (format t "BAR: ~A ~A ~%" window eli))
    eli))


 





(defun fun1 (eli) (format t "fun1 instancd ~%")
       (reset eli))
(defun fun2 (eli) (format t "fun2")
       (reset eli :full t))
(defun fun3 (eli stage)
  (case stage
    (0 (format t "fun3: 0 instance ~A~%" (eli-instance eli))
       (use-entry eli t) 1)
    (1 (format t "fun3: 1 instance ~A~%" (eli-instance eli) ) nil)
    (2 (format t "fun3: 2  instance ~A~%" (eli-instance eli))
       (use-entry eli nil) t))
  )

(defun bind-keys (eli)
  (format t "binding keys for instance ~A~%" (eli-instance eli))
  (setf (eli-keymap-top eli)   (new-keymap))
  (bind (eli-keymap-top eli) "C-xC-c" 'app-quit)
  (bind (eli-keymap-top eli) "C-a" 'fun1)
  (bind (eli-keymap-top eli) "C-b" 'fun2)
  (bind (eli-keymap-top eli) "C-c" 'fun3)
  (setf (get 'fun3 'interactive) t)
  (setf (eli-keymap-instant eli) (new-keymap))
  (bind (eli-keymap-instant eli) "C-g" 'cmd-cancel)
  (bind (eli-keymap-instant eli)"BS" 'cmd-back-up)
  
  )
    

(defun test (inst)
  (let ((window nil))
       
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
	  (let (( eli (make-bar window)))
	    (setf (eli-instance eli) inst)
	    (let ((workspace (make-instance 'gtk-box :orientation :vertical))
		  (dummy (make-instance 'gtk-box ))
		  (bar (eli-bar eli)))
	      (gtk-box-pack-start workspace dummy)
	      (gtk-box-pack-end workspace bar :expand nil)
	      (gtk-container-add window workspace)
	      )
	    
	    (bind-keys eli)
	    (g-signal-connect window "key-press-event" #'on-key-press)
	    (g-signal-connect window "destroy"
			      (lambda (widget)
				(declare (ignore widget))
				(format t "done")
				(leave-gtk-main)))
	    
	    (gtk-widget-show-all window)
	    (reset eli :full t))
	  )))
    (run)))


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
