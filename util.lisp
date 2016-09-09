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

(defun make-popup-menu (item-list &optional transform)
  "create a pop-up menu from a list of strings.  If transform is specified,
it must be a lambda that takes the item and returns a string."
  (let ((menu (gtk-menu-new)))
    (mapc
     (lambda (label)
       (if transform (setf label (funcall transform label)))
       (let ((item (gtk-menu-item-new-with-label label)))
	 (gtk-menu-shell-append menu item)
	 (gtk-widget-show item)))
     item-list)
    menu))
