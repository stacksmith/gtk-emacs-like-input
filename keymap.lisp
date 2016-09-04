(in-package :gtk-emacs-like-input)

;;; Key bindings are stored in a tree of hashtables of keys and values.
;;; key: a key with modifiers, or a string
;;; value: another map or a symbol of function to execute (not nil!)
;;;
;;; A full binding like "M-x C-Q test" corresponds to a keymap containing
;;; M-x, which in turn contains another keymap containing C-Q, which has
;;; "test" bound to some function.


;;; a sub-command is a string representation of a portion of a command description
;;; consisting of one of:
;;; - modifier character (one of CMASHh) followed by a -;
;;; - a string containing a single character, converted into key
;;; - a string containing a character name, converted into key
;;; - a string command used as a binding
;;; The parser returns a subseq after removing whatever it parsed.
(defun parse-sub-command (string key)
  "parse emacs-command string at index updating key, returning 2 values"
  (case (length string)
    (1 (incf key (char-code (char string 0)));last char must be char
       (setf string nil)) 
    (t (if (eq #\- (char string 1)) ; command formed as "?-..."
	   (progn ; attempt to set modifier
	     (case (char string 0) ;dispatch on the letter preceding #\-
	       (#\C (incf key mod-control-mask))
	       (#\M (incf key mod-meta-mask))
	       (#\A (incf key mod-alt-mask))
	       (#\S (incf key mod-shift-mask))
	       (#\s (incf key mod-super-mask))
	       (#\H (incf key mod-hyper-mask))
	       (t (signal 'kbd-parse-error :string string)))
	     (setf string (subseq string 2)))
	   (progn ; not a -, remainder must be convertible to a key
	     (let ((result (gtkcode-name->gtkcode string)))
	       (if result
		   (incf key result) ; add keycode into result
		   (if (zerop key)
		       (setf key string)
		       (signal 'kbd-parse-error :string string)
		       )))
	     (setf string nil)))))
  (values string key))

;;; a command - a string representing exactly a keystroke with modifiers,
;;; or a string representing some command
;;; possibly dividable into one or more sub-commands
(defun parse-command (string)
  "parse emacs-command string, returning key"
  (let ((key 0))
    (loop while string do
	 ;(format t "Parse-command: a~A ~A ~%" string key)
	 (multiple-value-setq (string key)
	   (parse-sub-command string key)))
    key))

(defun split-string-into-two (str)
  "split a space-separated command from the left of the string, returning left and right parts.
If not splittable, left part contains the entire string and right - nil."
  (let* ((pos (position #\space str)))
    (values (subseq str 0 pos) ;
     (and pos (string-left-trim '(#\space) (subseq str (1+ pos)))))))


(defun eli-bind (command-string value &key (top *keymap-top*))
  (let ((ass (car top))
	(key nil))		   ;pretend the first one is an association...
    (loop for command in (split-seq command-string) do
	 (setf key (parse-command command))
	 (let ((found (assoc key (cdr ass) :test #'equalp)))
	   (format t "ass is ~A~%" ass)
	   
	   (if found
	       (setf ass found)
	       (let ((new-binding (acons key nil (cdr ass))))
		 (format t "new binding is ~A~%" new-binding)
		 (setf (cdr ass) new-binding)
		 
		 (setf ass (car new-binding)) ;so we can keep on going
		 ))))
    (setf (cdr ass) value)
    ))

(defun find-binding (command-string &key (top *keymap-top*))
  (let ((ass (car top)))
    (loop for command in (split-seq command-string) do
	 (format t "Command ~A~%" command)
	 (let ((key (parse-command command))
	       (binding (cdr ass)))
	   (setf ass (and (listp binding) ;must be a list
			  (assoc key binding :test #'equalp)))))
    (and (symbolp (cdr ass)) ;
	 (cdr ass))))

(defun binding-locate (key keymap)
  "find a binding for key in keymap, or return nil.  The binding may be
another keymap, or a function pointer"
  (assoc key (cdr keymap) :test #'equalp))
