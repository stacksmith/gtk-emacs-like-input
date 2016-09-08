(in-package :gtk-emacs-like-input)

;;; Key bindings are stored in keymaps.
;;;
;;; We associate stringified keyseq like "<M-C-x><C-k>hello" with bvals,
;;; which may be symbols or functions.
;;;
;;; Implementation: A keymap is an entity consisting of a resizable
;;; array of key-seq (with possible modifiers)  and a matching array of
;;; function bvals.
;;;
;;; We also use intermediate indices into the arrays.
;;;
;;; 
(defstruct (keymap
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(loop for keyseq across (keymap-keyseqs struct)
		   for i from 0
		   for bval across (keymap-bvals struct) do
		     (format stream "~A: \"~A\" '~A~%" i (keyseq->string keyseq) bval)))))
  keyseqs bvals)
(defun new-keymap ()
  (make-keymap :keyseqs (make-array 5 :fill-pointer 0 :adjustable t)
	       :bvals (make-array 5 :fill-pointer 0 :adjustable t)))

(defun keymap-high-index (keymap)
  "Return the highest available index of this keymap"
  (1- (fill-pointer (keymap-keyseqs keymap))))

(defun keymap-bval-at (keymap index)
  "Return symbol at index of keymap"
  (elt (keymap-bvals keymap) index))

(defun keymap-keyseq-at (keymap index)
  "Return keyseq at index of keymap"
  (elt (keymap-keyseqs keymap) index))

(defun bind (keymap str bval)
  "bind a str to a symbol in keymap"
  (let* ((keylist (kbd str))
	 (keyarr (make-array (length keylist) :initial-contents keylist)))
    (vector-push-extend keyarr (keymap-keyseqs keymap))
    (vector-push-extend bval (keymap-bvals keymap))))

(defun keymap-match (map keyseq)
  "Search for a match for a keyseq in a keymap.  Return one of:
-nil - if not matching at all
-list of partial match indices
-symbol of bound to match"
  (let ((keyseq-length (length keyseq)))
    (loop for i from (keymap-high-index map) downto 0
       for k = (elt (keymap-keyseqs map) i)
       for mismatch = (mismatch keyseq k)
       unless mismatch return (keymap-bval-at map i) end ;nil mismatch = actual match
       if (>=  mismatch keyseq-length) collect i ;collect partials
	 )))

(defun keymap-exact-match (map keyseq)
  "return matching symbol or nil"
  (loop for i from (keymap-high-index map) downto 0 ;backwards
     for k = (elt (keymap-keyseqs map) i)
     when (equalp keyseq k)
       do (return (keymap-bval-at map i))
     finally (return nil)))

