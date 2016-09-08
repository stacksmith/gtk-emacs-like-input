(in-package :gtk-emacs-like-input)

;;; Key bindings are stored in keymaps.
;;;
;;; We associate keystrs like "M-C-xCk-hello" with symbols or functions
;;; Note: for historical reasons we refer to the rhs as symbol, but
;;; it may be a function pointer
;;;
;;; Implementation: A keymap is an entity consisting
;;; of an array of strings containing textual descriptions of keys,
;;; and a matching array of function symbols.
;;;
;;; We also use intermediate indices into the arrays.
;;;
(defstruct (keymap
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(loop for name across (keymap-keystrs struct)
		   for i from 0
		   for symbol across (keymap-symbols struct) do
		     (format stream "~A: \"~A\" '~A~%" i (keys->string name) symbol)))))
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




(defun bind (keymap str symbol)
  "bind a str to a symbol in keymap"
  (let* ((keylist (parse-keystr str))
	 (keyarr (make-array (length keylist) :initial-contents keylist)))
    (vector-push-extend keyarr (keymap-keystrs keymap))
    (vector-push-extend symbol (keymap-symbols keymap))))

(defun keymap-match (map keystr)
  "Search for a match for a keystr in a keymap.  Return one of:
-nil - if not matching at all
-list of partial match indices
-symbol of bound to match"
  (let ((keystr-length (length keystr)))
    (loop for i from (keymap-high-index map) downto 0
       for k = (elt (keymap-keystrs map) i)
       for mismatch = (mismatch keystr k)
	 
       unless mismatch return (keymap-symbol-at map i) end ;nil mismatch = actual match
       if (>=  mismatch keystr-length) collect i ;collect partials
	 )))

(defun keymap-exact-match (map keystr)
  "return matching symbol or nil"
  (loop for i from (keymap-high-index map) downto 0 ;backwards
     for k = (elt (keymap-keystrs map) i)
     when (equalp keystr k)
       do (return (keymap-symbol-at map i))
     finally (return nil)))

