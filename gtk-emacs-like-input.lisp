;;;; gtk-emacs-like-input.lisp

(in-package #:gtk-emacs-like-input)
(defparameter *window* nil)




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
	  (cmd-bar (make-instance 'gtk-box :orientation :horizontal))
	  (cmd-static (make-instance 'gtk-label :label "test"
				     :expand nil))
	  (cmd-editor (make-instance 'gtk-entry :label "edit")))
      (gtk-box-pack-start cmd-bar cmd-static :expand nil)
      (gtk-box-pack-end cmd-bar cmd-editor)
      (gtk-box-pack-start workspace dummy)
      (gtk-box-pack-end workspace cmd-bar :expand nil)
      (gtk-container-add *window* workspace)

      (g-signal-connect cmd-editor "key-press-event" 'on-key-press)
      )
      

    (g-signal-connect *window* "destroy"
		      (lambda (widget)
			(declare (ignore widget))
			(format t "done")
			(leave-gtk-main)))
      
    (gtk-widget-show-all *window*)
      
      
      ))





