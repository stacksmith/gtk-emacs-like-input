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
  menubut        ;; for suggested matches
  keymap-top     ;; top keymap
  keymap-instant ;; instant keymap (each character for cancel)
  buffer         ;; sequence of keys from command start
  key            ;; active key
  interactive    ;; interactive function in control
  window         ;; top window
  match          ;; a list of indices of possible keymap matches, or function
 
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


;;; buffer functions - affect match...

(defun buffer-append-key (eli)
  (with-slots (key buffer keymap-top match) eli
    (vector-push-extend key buffer) 	        ;;append key to buffer
    (setf match (keymap-match keymap-top buffer)))) ;;and update match 

(defun buffer-bs (eli)
  (with-slots (buffer keymap-top match) eli
    (vector-pop buffer)
    (setf match (keymap-match keymap-top buffer))))

(defun buffer-reset (eli)
  (with-slots (buffer keymap-top match) eli
    (setf (fill-pointer buffer) 0)
    (setf match (keymap-match keymap-top buffer))))

(defun buffer-append (eli keyseq &key (start 0) (end (length keyseq)) (exit-on nil) )
  "append keyseq to the buffer, optionally terminating on an 'exit-on value."
  (with-slots (buffer keymap-top match) eli
    (loop for i from start to end
       for item =(elt keyseq i)
       until (= item exit-on)
       do (vector-push-extend item buffer))
    (setf match (keymap-match keymap-top buffer))))


(defun render (eli)
  "Visually update the information in the bar"
  (with-slots (buffer left middle menubut match) eli
    ;; match now nil, list, or a hit
    ;;    (gtk-label-set-text left (keyseq->string buffer) )
    (let ((string (html-escape (keyseq->string buffer))))
      (gtk-label-set-markup
       left
       (if match
	   (format nil  "<span foreground=\"#000088\">~A</span>" string)
	   (format nil  "<span foreground=\"#000000\">~A</span>" string))))
    (gtk-label-set-text middle "")
    (setf (gtk-button-label menubut)
	  (if (consp match)
	      (if (cdr match)
		  (format nil "~A possibilities" (length match))
		  (format nil "1 possibility"))
	      (format nil "")))))

(defun reset (eli &key (full nil))
  (with-slots (instance buffer entry left middle menubut interactive) eli
    "reset input state and visuals."
    (buffer-reset eli)
    (setf (gtk-entry-text entry) "")
    (gtk-widget-hide entry)
    (gtk-label-set-text left "")
    (gtk-label-set-text middle "")
    (setf (gtk-button-label menubut) "")
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

(defun dispatch-function (eli match)
  (with-slots (interactive) eli
    (typecase match
      (null nil)
      (function 
       (funcall match eli)		;regular binding
       (reset eli))		;done with command
      (symbol			;install interactive
       (funcall
	(setf interactive (symbol-function match))
	eli :initialize)))))

(defun dispatch-key (eli)
  "Attempt a dispatch on current keystr."
  (with-slots (buffer interactive key keymap-top match) eli
    (buffer-append-key eli)
    (render eli)
    (dispatch-function eli match)
    t)) ;done with keystroke for internal processing.

(defun dispatch-instant (eli)
  "if key is bound as instant, invoke binding.  Otherwise, return nil for further
processing"
  ;;TODO: perhaps a separate mechanism is called for instead of creating a damn array every 
  (with-slots (keymap-instant key) eli
    (let ((match (keymap-exact-match keymap-instant (vector key))))
;      (format t "INSTANT MATCH ~A~%" match)
      (when match (funcall match eli))))) ;if no match, when returns nil, ow whatever!

(defun input-keystroke (eli)
  "process a keystroke."
  (with-slots (interactive) eli
    (unless (dispatch-instant eli)
      (if interactive
	  (funcall interactive eli :process) ;returns nil/t to process keys in gtk
	  (dispatch-key eli) ;otherwise, internal dispatch
	  ))))

(defun on-key-press (eli widget event)
  "Process a key from GTK; ignore modifier keys; process other keys in eli"
  (declare (ignore widget))
  (let ((gtkkey (gdk-event-key-keyval event)))
    (format t "GTKKEY: ~A~%" gtkkey)
    (unless (modifier-p gtkkey ) ;do not process modifiers, gtk will handle them
      (setf (eli-key eli) (make-key gtkkey (gdk-event-key-state event)))
      (input-keystroke eli))))


(defun make-suggest-menu (eli widget)
  "create a pop-up menu from a match"
  (declare (ignore widget))
  (with-slots (keymap-top  match) eli
    (when (consp match)
      (popup-menu match		      ;it's a list of indices into keymap-top!
		  :transform (lambda (index)
			       (keyseq->string
				(keymap-keyseq-at keymap-top index)))
		  :on-click (lambda (index)
			      (format t "CLICKED ON ~A~%" index))))))

(defun make-eli (window)
  "Create an eli command bar; return eli"
  (let ((eli (construct-eli)))
    
    (with-slots (bar left middle entry menubut keymap-top keymap-instant buffer) eli
       (setf bar (make-instance 'gtk-box :orientation :horizontal )
	    left (make-instance 'gtk-label :label "left"   )
	    middle (make-instance 'gtk-label :label "middle" ) 
	    entry (make-instance 'gtk-entry :label "entry"  ) 
	    menubut (make-instance 'gtk-button :label "menubut" ))
      
      (gtk-box-pack-start    bar left :expand nil)
      (gtk-box-pack-start    bar middle :expand t)
      (gtk-box-pack-start    bar entry :expand t)
      (gtk-box-pack-start    bar menubut :expand nil)

      ;; set up keymaps
      (setf keymap-top  (new-keymap)
	    keymap-instant (new-keymap))
      ;; and instant key processing
      (bind keymap-instant "<C-g>" #'inst-cancel)
      (bind keymap-instant "<BS>" #'inst-back-up)
      (bind keymap-instant "<TAB>" #'inst-tab)
      ;; default keystrokes
      (bind keymap-top  "<C-x><C-c>" #'app-quit)
      ;; create buffer
      (setf buffer (make-array 32 :fill-pointer 0 :adjustable t) )
      
      (eli-signal-connect menubut "clicked" make-suggest-menu (widget))
      (eli-signal-connect window "key-press-event" on-key-press (widget event))
      eli)))

;;; Some helper functions that can be bound
(defun inst-cancel (eli)
  "Cancel command, reset"
  (reset eli :full t ))
;;;
;;; Tab completion.
;;;
;;; This is a little complicated. Basically, if there are partial matches,
;;; we find the longest string which starts all the mathches.  We then
;;; inject the keys into dispatch-key, as if they were typed. If there
;;; is a <RET>, we stop before it, to avoid trouble.
(defun inst-tab (eli)
  (with-slots (keymap-top buffer key match) eli
    (when (consp match)
      ;; determine if all matches have a common beginning sequence
      (let* ((first-keystr (keymap-keyseq-at keymap-top (first match)))
	     (buf-len (length buffer))
	     (tab-by
	      (if (cdr match)
		  (loop for i in (cdr match) ;loop for all matches but first
		     for ks = (keymap-keyseq-at keymap-top i) ;get keystrs
		     with ref = first-keystr
		     minimize (mismatch ks ref :start1 buf-len :start2 buf-len))
		  (length first-keystr))))
	;;insert remaining keys, terminating on <RET> to avoid running

	(buffer-append eli first-keystr :start buf-len :end (1- tab-by)
		       :exit-on #xff0d)
	(render eli)
	))
    t))

(defun inst-back-up (eli)
  "instant. BS the last keystroke"
  (with-slots (buffer interactive keymap-top match) eli
    (format t "int ~A SIZE: ~A~%" interactive (length buffer))
    (unless interactive ; on interactive return nil, it will handle
      (unless (zerop (length buffer))
	(vector-pop buffer) ;since we are modifying buffer
	(setf match (keymap-match keymap-top buffer)) ;update matches
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
    (bind keymap-top  "<C-a>abracadabra<RET>what" #'fun1)
    (bind keymap-top  "abracadabra<RET>what" #'fun1)
    (bind keymap-top  "<C-a>abracadillo<RET>" #'fun2)
    (bind keymap-top  "<C-a>abracddillo<RET>" #'fun2)
    (bind keymap-top  "<C-c>" 'fun3) ;interactive - ' not #'
))
    


  
       
(defun test (&key (stdout *standard-output*))
  (let ((gtk::*main-thread* nil))
    (within-main-loop
      (setf *standard-output* stdout) ;enable output in this thread
      ;; create a window with a command line on the bottom.
      (let*((window (make-instance 'gtk-application-window
				  :title "eli-test"
				  :type :toplevel
				  :border-width 0
				  :default-width 640
				  :default-height 480) )
	    (eli (make-eli window))
	    (workspace (make-instance 'gtk-box :orientation :vertical))
	    (dummy (make-instance 'gtk-box ))
	    (bar (eli-bar eli)))
	(gtk-box-pack-start workspace dummy)
	(gtk-box-pack-end workspace bar :expand nil)
	(gtk-container-add window workspace)
	  
	
	(bind-keys eli)
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignore widget))
			    (format t "done")))
	(gtk-widget-show-all window)
	(reset eli :full t))))
  )
 


