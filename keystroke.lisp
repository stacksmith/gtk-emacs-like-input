(in-package :gtk-emacs-like-input)
;;; Keystroke
;;;

(defconstant MOD-CONTROL-BIT 24)
(defconstant MOD-META-BIT    25)
(defconstant MOD-ALT-BIT     26)
(defconstant MOD-SHIFT-BIT   27)
(defconstant MOD-SUPER-BIT   28)
(defconstant MOD-HYPER-BIT   29)

(defconstant MOD-CONTROL-MASK (ash 1 MOD-CONTROL-BIT) )
(defconstant MOD-META-MASK    (ash 1 MOD-META-BIT) ) 
(defconstant MOD-ALT-MASK     (ash 1 MOD-ALT-BIT) ) 
(defconstant MOD-SHIFT-MASK   (ash 1 MOD-SHIFT-BIT) ) 
(defconstant MOD-SUPER-MASK   (ash 1 MOD-SUPER-BIT) ) 
(defconstant MOD-HYPER-MASK   (ash 1 MOD-HYPER-BIT) )


(deftype key () '(integer 0 #x3FFFFFFFF))

(defparameter keyval-spec (byte 24 0))
(defparameter keymod-spec (byte 8 24))

(defmacro key-val (key)
  "return keyval of the key"
  `(ldb keyval-spec ,key))
(defmacro key-mod (key)
  "return mod flags of key, not settable"
  `(mask-field keymod-spec ,key))

(defun key-char (key)
  "return a Lisp character corresponding to this key"
  (and (<  (key-val key) char-code-limit)
       (code-char (key-val key))))

(defun write-key (key stream)
  "print the stringified key to stream"
  (when (logbitp mod-control-bit key) (write-sequence "C-" stream ))
  (when (logbitp mod-meta-bit    key) (write-sequence "M-" stream ))
  (when (logbitp mod-alt-bit     key) (write-sequence "A-" stream ))
  (when (logbitp mod-shift-bit   key) (write-sequence "S-" stream ))
  (when (logbitp mod-super-bit   key) (write-sequence "s-" stream ))
  (when (logbitp mod-hyper-bit   key) (write-sequence "H-" stream ))
  (write-sequence (gtkcode->gtkcode-name (key-val key)) stream)
  ;(unless (zerop (key-mod key)) (write-char #\space stream))
  key)

(defun key->string (key)
  "Convert a key into a string representation"
  (with-output-to-string (s) (write-key key s)))

;;; Associate chars used as modifiers in command-strings to modmasks
;;; ah, defconstant reference in a quoted form seems to be a symbol, not value
(defparameter *char-modmask* `((#\C . ,mod-control-mask)
			       (#\M . ,mod-meta-mask)
			       (#\A . ,mod-alt-mask)
			       (#\S . ,mod-shift-mask)
			       (#\s . ,mod-super-mask)
			       (#\H . ,mod-hyper-mask)))
(defun char->modmask (char)
  "return a modmask for a character.  If not a mask character, error"
  (or (cdr (assoc char *char-modmask*))
      (error 'eli-error :message "char->modmask: char is not a mask-appropriage"
	      :value char)))
;;;
;;; We only care about control and meta (alt key).
;;; Shift is already processed for us.
(defun make-key (val &optional (gtk-modifiers nil)) 
  "create a key using the gtk modifier list"
  (dolist (modifier gtk-modifiers)
    (case modifier 
      (:control-mask (incf val MOD-CONTROL-MASK))
      (:mod1-mask    (incf val MOD-META-MASK))))
  val)

(defun key-reader (stream char)
  "read textual character representations like <C-M-x> and return keys"
  (declare (ignore char))
  (let ((key 0)
	(name (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for i upto 80 
       for c1 = (read-char stream)
       for c2 = (peek-char nil stream)
       while (char= #\- c2) do
	 (if (char= #\> c1) ;prevent <> or <C-> type errors
	     (error "unexpected >" ))
	 (read-char stream) ;skip the -
	 (incf key (char->modmask c1))
       finally (vector-push-extend c1 name))
    ;; now collect the rest of the name
    (loop for c = (read-char stream)
       until (char= c #\>) do
	 (vector-push-extend c name))
    (let ((gtkcode (gtkcode-name->gtkcode name)))
      (if gtkcode
	  (incf key gtkcode)
	  (error "~A is not a valid gtk key name" name)))
    key))
(set-macro-character #\< #'key-reader) 




