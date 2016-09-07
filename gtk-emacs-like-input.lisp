;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
;;; The key to using eli is the eli structure.  It contains the entire
;;; state of the input system, including the relevant gtk objects.
;;;
(defstruct (eli (:constructor construct-eli))
  bar	 ;; gtk container
  left           ;; gtk label on left side (keys as entered)
  middle         ;; gtk label in the middle 
  entry          ;; gtk text entry, hidden on start
  right          ;; gtk label on right (status)
  keymap-top     ;; top keymap
  keymap-instant ;; instant keymap (each character for cancel)
  buffer         ;; sequence of keys from command start
  key            ;; active key
  interactive    ;; interactive function in control
  window         ;; top window

)
;;; Key processing:
;;;
;;;   Find proper eli structure for this gtk widget (for multiple instances)
;;;   Create a key and store it in eli.  Modifiers are ignores as gtk
;;;   will attach them to the actual keys later.  
;;;  
;;;   Check the key against keymap-instant for cancel and emergency keystrokes.
;;;   Call instant binding if it exists which returns t or nil to gtk.
;;;
;;;   Otherwise, if interactive is active, invoke interactve handler with 1.
;;;
;;;   Otherwise, append the key to the buffer.  Try to match the buffer to
;;;   a binding via the keymap, and if any, even partial matches exist, call
;;;   render (to display partial matches, or assist the user to choose).

;;;   If a full match exists, check the binding type.  Symbols install an
;;;   interactive function and call to initialize it (with 0).  Functions
;;;   are funcalled and the environment is reset for next command.
;;;
;;;   Handlers
;;;
;;;   Normal handlers, bound with as a #' function in the keymap are invoked
;;;   the eli structure and
;;;   anything.
;;;
;;;   Interactive handlers are invoke with two parameters: the eli and the
;;;   stage keyword.  Stages are:
;;;   :initialize - when the handler is installed
;;;   :process    - for every keystroke after
;;;   :finalize   - when invoked by reset to let the handler clean up.
;;;
;;;   When the interactive handler is done, it should call (reset eli :full t)
;;;   to uninstall itself and _immediately_ return t.  Reset re-invokes the
;;;   handler with :finalize (from inside the handler!)  You've been warned.


(defun buffer->string (buffer)
  "convert a vector of keys into its string representation"
  (with-output-to-string (s)
    (loop for key across buffer do (write-key key s ))))

(defparameter eli-map nil)

(defun render (eli &key (match nil))
  "Visually update the information in the bar"
  (with-slots (buffer keymap-top left middle right) eli
    (let ((keystr(buffer->string buffer) ))
      (format t "RENDER ~A~%" keystr)
      (unless match ;if match not passed to us, calculate it
	(unless (zerop (length keystr)) ;if buffer length >0
	  (setf match (keymap-match keymap-top keystr))))
      ;; match now nil, list, or a hit
      (gtk-label-set-text left keystr)
      (gtk-label-set-text middle "")
      (gtk-label-set-text right
			  (if (consp match)
			      (format nil "~A matches" (length match))
			      (format nil ""))))))

(defun reset (eli &key (full nil))
  (with-slots (instance buffer entry left middle right interactive) eli
    "reset input state and visuals."
    (setf buffer (make-array 32 :fill-pointer 0 :adjustable t)
	  (gtk-entry-text entry) "")
    (gtk-widget-hide entry)
    (gtk-label-set-text left "")
    (gtk-label-set-text middle "")
    (gtk-label-set-text right "")
    
    (when full ; reset interactive stuff
      (and interactive
	   (funcall interactive eli :finalize))
      (setf interactive nil)))
  t)

(defun use-entry (eli on)
  (with-slots (middle entry) eli
    (format t "USE-ENTRY~%")
    (if on
	(progn
	  (gtk-widget-hide middle)
	  (gtk-widget-show entry)
	  (gtk-widget-grab-focus entry))
	(progn
	  (gtk-widget-hide entry)
	  (gtk-widget-show middle)))))

    
(defun dispatch-command (eli)
  "Attempt a dispatch on current keystr."
  (with-slots (buffer interactive key keymap-top) eli
    (vector-push-extend key buffer)	;append key to buffer
    (let ((match (keymap-match keymap-top (buffer->string buffer))))
      (if (consp match)
	  (progn (format t "----~%")
		 (loop for i in match do (format t "~A~%" (keymap-keystr-at keymap-top i)))
		 (format t "++++~%"))
	  
	  )
      
      (render eli :match match)
      
      (typecase match
	(null nil)
	(function 
	 (funcall match eli)		;regular binding
	 (reset eli))		;done with command
	(symbol			;install interactive
	 (funcall
	  (setf interactive (symbol-function match))
	  eli :initialize)))
      t)))

(defun dispatch-instant (eli)
  "if key is bound as instant, invoke binding.  Otherwise, return nil for further
processing"
  (with-slots (keymap-instant key) eli
    (let ((match (keymap-exact-match keymap-instant (key->string key))))
      (when match (funcall match eli)))))

(defun input-keystroke (eli)
  "process a keystroke."
  (with-slots (interactive) eli
    (unless (dispatch-instant eli)
      (if interactive
	  (funcall interactive eli :process) ;returns nil/t to process keys in gtk
	  (dispatch-command eli) ;otherwise, internal dispatch
	  ))))

(defun on-key-press (widget event)
  "Process a key from GTK; ignore modifier keys; process other keys in eli"
  (let ((gtkkey (gdk-event-key-keyval event)))
    (unless (modifier-p gtkkey ) ;do not process modifiers, gtk will handle them
      (let ((eli (gethash widget eli-map)))
	(unless eli
	  (error 'eli-error :message "eli:on-key-press: invalid window" :value widget))
	(setf (eli-key eli) (make-key gtkkey (gdk-event-key-state event)))
	(input-keystroke eli)))))
 
(defun make-eli (window)
  "Create an eli command bar; return eli"
  (let ((eli (construct-eli)))
    (with-slots (bar left middle entry right keymap-top keymap-instant) eli
      (unless eli-map
	(setf eli-map (make-hash-table)))
      (setf (gethash window eli-map) eli
	    (eli-window eli) window)
      (setf bar (make-instance 'gtk-box :orientation :horizontal )
	    left (make-instance 'gtk-label :label "left"  )
	    middle (make-instance 'gtk-label :label "middle" ) 
	    entry (make-instance 'gtk-entry :label "entry"  ) 
	    right (make-instance 'gtk-label :label "right" ))
      ;;note:
      (gtk-box-pack-start    bar left :expand nil)
      (gtk-box-pack-start    bar middle :expand t)
      (gtk-box-pack-start    bar entry :expand t)
      (gtk-box-pack-start    bar right :expand nil)
      ;; set up keymaps
      (setf keymap-top  (new-keymap)
	    keymap-instant (new-keymap))
      ;; and instant key processing
      (bind keymap-instant  "C-g" #'inst-cancel)
      (bind keymap-instant "BS" #'inst-back-up)
      (bind keymap-instant "TAB" #'inst-tab)
      ; default keystrokes
      (bind keymap-top  "C-xC-c" #'app-quit) 
)

    
    
   
    eli))

;;; Some helper functions that can be bound
(defun inst-cancel (eli)
  "Cancel command, reset"
  (reset eli :full t ))

(defun inst-tab (eli)
  (declare (ignore eli))
  (format t "TAB...~%")
  )
(defun inst-back-up (eli)
  "instant. BS the last keystroke"
  (with-slots (buffer interactive) eli
    (format t "int ~A SIZE: ~A~%" interactive (length buffer))
    (unless interactive ; on interactive return nil, it will handle
      (unless (zerop (length buffer))
	(vector-pop buffer)
	(render eli))
      t)))
(defun app-quit (eli)
  (gtk-widget-destroy (eli-window eli))
  (format t "quit done~%"))


(defun fun1 (eli) (format t "fun1~%")
       (reset eli))
(defun fun2 (eli) (format t "fun2~%")
       (reset eli :full t))
(defun fun3 (eli stage)
  (case stage
    (:initialize
       (use-entry eli t) 1)
    (:process
     (format t "fun3: 1 instance ~A~%" (eli-key eli) )
     nil ;let gtk work with the entry editor...
     )
    (:finalize
     (format t "fun3: finalizing~%")
     (use-entry eli nil)
     t))
  )


(defun bind-keys (eli)
  (with-slots (keymap-top keymap-instant) eli
    (bind keymap-top  "C-aabracadabraRET" #'fun1)
    (bind keymap-top  "C-aabracadilloRET" #'fun2)
    (bind keymap-top  "C-c" 'fun3) ;interactive - ' not #'
))
    

(defun test ()
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
	  (let (( eli (make-eli window)))
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
				(format t "done")))
	    
	    (gtk-widget-show-all window)
	    (reset eli :full t))
	  )))
    (run)))



