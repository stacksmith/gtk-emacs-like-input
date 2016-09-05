(in-package :gtk-emacs-like-input)

;;; Key bindings are stored in a tree of alists.
;;; key: a key (possibly with modifiers), or a string
;;; value: another map or a symbol of function to execute (not nil!)
;;;
;;; A full binding like "M-x C-Q" corresponds to a keymap containing
;;; M-x, which in turn contains another keymap containing C-Q

(define-condition eli-error (error)
  ((what
    :initarg :message
    :accessor eli-error-what
    :initform nil
    :documentation "What went wrong.")
   (where
    :initarg :message
    :accessor eli-error-where
    :initform nil
    :documentation "Where it went wrong.")
   (value
    :initarg :value
    :accessor eli-error-value
    :initform nil
    :documentation "The offending value.")))

(defmethod print-object ((object eli-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "ELI error in ~A: ~A, offending value [~A]~%"
	    (eli-error-where object)
	    (eli-error-what object)
            (eli-error-value object))))

;;; a sub-command is a string representation of a portion of a command description
;;; consisting of one of:
;;; - modifier character (one of CMASHh) followed by a -;
;;; - a string containing a single character, converted into key
;;; - a string containing a character name, converted into key
;;; 
;;; The parser returns a subseq after removing whatever it parsed.

(defun parse-sub-command (string key)
  "parse emacs-command string at index updating key, returning 2 values"
  (let ((first (char string 0)))
    (case (length string)
      (1 (incf key (char-code first));last char must be THE char
	 (setf string nil)) ;nil signals completion 
      (t (if (eq #\- (char string 1)) ; command formed as ".-."
	     (progn ; attempt to set modifier
	       (incf key (char->modmask first)) ;may raise
	       (setf string (subseq string 2)))
	     (progn ; not a -, remainder must be convertible to a key or string
	       (let ((gtkcode (gtkcode-name->gtkcode string)))
		 (if gtkcode
		     (incf gtkcode )  ;add keycode into result
		     (error 'eli-error
			    :where "parse-sub-command"
			    :what "String is not a key"
			    :value string)))
	       (setf string nil)))))) ;done parsing
  (values string key))

;;; A command is a string with an emacs-like key description
;;; or a string representing some command
;;; possibly dividable into one or more sub-commands
(defun parse-command (string)
  "parse emacs-command string, returning key"
  (let ((key 0))
    (loop while string
	 while key
       do
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


(defun bind (command-string value &key (top *keymap-top*))
  "traverse binding tree following command-string, creating the path as needed.
Finally, set the found or new binding to value.  Error if a binding along the
path is bound to a value; i.e. if 'M-x' is bound, 'M-x M-y' is not valid"
  (unless (symbolp value)
    (error 'eli-error :where "bind" :what "Attempted to bind a non-symbol"
	   :value value))
  (loop for command in (split-seq command-string)
     for key = (parse-command command)
     with binding = (car top) ;get binding from alist
     with found = nil
     do
       (if (setf found (assoc key (cdr binding)))
	   (setf binding found) ;subcommand found, set as curent binding
	   (setf binding (car ;next time, binding is first of new alist
			  (setf (cdr binding) (acons key nil (cdr binding))))))
     finally (setf (cdr binding) value))) ;at the end, set dest of new binding.
    

(defun binding-of (command-string &key (top *keymap-top*))
  "traverse binding tree following command string, returning the final
binding value or nil.  If :keymap requested, keymaps will also be returned"
  (loop for command in (split-seq command-string)
     for key = (parse-command command)
     with binding = (car top)
     do
       (setf binding (assoc key (cdr binding)))
     finally (return (cdr binding))))

(defun binding-name (binding)
  "return a string representation of this binding"
  (typecase (car binding)
    (integer (key->string (car binding)))
    (otherwise (error 'eli-error
		      :where "binding-name"
		      :what "Invalid binding key"
		      :value (car binding)))))

(defun binding-print (&key (binding (car *keymap-top*)) (prefix ""))
  "print the binding and sub-bindings"
  (let ((new-prefix (concatenate 'string prefix " " (binding-name binding))))
    (typecase (cdr binding)
      (cons
       (loop for b in (cdr binding)
	  do (binding-print :binding b :prefix new-prefix)))
      (otherwise
         (format t "~A ~A~%" new-prefix (cdr binding))))))


(defun binding-locate (key keymap)
  "find a binding for key in keymap, or return nil.  The binding may be
another keymap, or a function symbol"
  (assoc key (cdr keymap)))

