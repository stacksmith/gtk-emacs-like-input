(in-package :gtk-emacs-like-input)


(defun split-seq (seq &key (separators " ") (test #'char=) (default-value '("")))
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (char) (find char seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
	     then (position-if (complement #'sep) seq :start j)
	     as j = (position-if #'sep seq :start (or i 0))
	     while i
	     collect (subseq seq i j)
	     while j)
          default-value))))

(defun html-escape (string)
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
            do (case char
                 ((#\&) (write-string "&amp;" out))
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 (t (write-char char out)))))
    (coerce output 'simple-string)))



(defmacro eli-signal-connect (instance detailed-signal handler (&rest parameters))
  "like gtk-signal-connect, but
1) specifies the handler's parameters;
2) calls the handler with eli in front of the parameters"
  `(g-signal-connect ,instance ,detailed-signal
		     (lambda (,@parameters) (,handler eli ,@parameters))))



;;;=============================================================================
;;;
;;; Popup menu...
;;;
;;; create and pop up a menu for a list of items.
;;;
;;; transform:
;;; - nil if items are strings
;;; - f(item) to convert each item to a string
;;;
;;; on-click
;;; - f(index) 
(defun popup-menu (item-seq &key (transform nil) on-click (on-cancel nil) )
  "Create a pop-up menu from a sequence of strings or (or items that can be
converted to strings using the 'transform' lambda.  Execute 'on-click' 
lambda when the user selects an item, passing it the index."
  (let ((menu (gtk-menu-new)))
    ;; There is no way to tell at the end if the user clicked on a menu or
    ;; if it was closed by clicking away - both wind up with active index 0.
    ;; As a workaround, create a hidden menu-item for index 0.
    (gtk-menu-shell-append menu (gtk-menu-item-new-with-label "fake"))
    ;; append each item in the-seq to the menu, transforming as needed.
    (mapc (lambda (label)
	    (let ((menu-item (gtk-menu-item-new-with-label
			      (if transform
				  (funcall transform label)
				  label))))
	      (gtk-menu-shell-append menu menu-item)
	      (gtk-widget-show menu-item)))
	  item-seq)
    ;; establish click handling
    (g-signal-connect menu "selection-done"
		      (lambda (menu)
			;; remember that index 0 means no active menu item.
			(let ((click-index (gtk-menu-active menu)))
			  (if (zerop click-index)
			      (and on-cancel (funcall on-cancel))
			      (funcall on-click (1- click-index))))))
     ;; pop it up
    (gtk-menu-popup menu :activate-time (gtk-get-current-event-time))
    (format t "ON START ~A SELECTED~%" (gtk-menu-active menu))
    menu))
