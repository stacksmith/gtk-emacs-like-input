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
		(loop for name across (keymap-names struct)
		   for i from 0
		   for symbol across (keymap-symbols struct) do
		     (format stream "~A: \"~A\" '~A~%" i name symbol)))))
  names symbols)
(defun new-keymap ()
  (make-keymap :names (make-array 5 :fill-pointer 0 :adjustable t)
	       :symbols (make-array 5 :fill-pointer 0 :adjustable t)))


(defun bind (map keys symbol)
  (vector-push-extend keys (keymap-names map))
  (vector-push-extend symbol (keymap-symbols map)))

(defun btest ()
  (setf *keymap-top* (new-keymap))
  (bind *keymap-top* "C-x C-y" 'y)
  (bind *keymap-top* "C-x C-b" 'b)
  (bind *keymap-top* "C-z" 'z))

(defun indices-of (map string)
  "returns a list of indices of possible bindings."
  (let ((length (length string)))
    (loop for string2 across (keymap-names map)
       for index from 0
       for mismatch = (mismatch string string2)
       when (or (not mismatch)
		(= length mismatch))
       collect index)))
 
(defun keymap-symbol-at (map index)
  (elt (keymap-symbols map) index))

(defun keymap-name-at (map index)
  (elt (keymap-names map) index))
