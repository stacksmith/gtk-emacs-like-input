(in-package :gtk-emacs-like-input)

;;; Key bindings are stored in keymaps.  A keymap is an entity consisting
;;; of an array of strings containing textual descriptions of keys,
;;; and a matching array of function symbols.
;;;
;;; 
(defstruct (keymap
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(loop for name across (keymap-keystrs struct)
		   for i from 0
		   for symbol across (keymap-symbols struct) do
		     (format stream "~A: \"~A\" '~A~%" i name symbol)))))
  keystrs symbols)
(defun new-keymap ()
  (make-keymap :keystrs (make-array 5 :fill-pointer 0 :adjustable t)
	       :symbols (make-array 5 :fill-pointer 0 :adjustable t)))

(defun keymap-high-index (keymap)
  "Return the highest index of this keymap"
  (1- (fill-pointer (keymap-keystrs keymap))))

(defun keymap-symbol-at (keymap index)
  "Return symbol at index of keymap"
  (elt (keymap-symbols keymap) index))

(defun keymap-keystr-at (keymap index)
  "Return keystr at index of keymap"
  (elt (keymap-keystrs keymap) index))

(defun bind (keymap keystr symbol)
  "bind a keystr to a symbol in keymap"
  (vector-push-extend keystr (keymap-keystrs keymap))
  (vector-push-extend symbol (keymap-symbols keymap)))

(defun btest ()
  (setf *keymap-top* (new-keymap))
  (bind *keymap-top* "C-xC-y" 'y)
  (bind *keymap-top* "C-xC-b" 'b)
  (bind *keymap-top* "C-z" 'z))

(defun keymap-match (map keystr)
  "Search for a match for a keystr in a keymap.  Return one of:
-nil - if not matching at all
-list of partial match indices
-symbol of bound to match"
  (let ((keystr-length (length keystr)))
    (loop for i from (keymap-high-index map) downto 0
       for k = (elt (keymap-keystrs map) i)
       for mismatch = (mismatch keystr k)
       unless mismatch return (keymap-symbol-at map i) end	;nil mismatch = actual match
       if (>=  mismatch keystr-length) collect i ;collect partials
	 )))



