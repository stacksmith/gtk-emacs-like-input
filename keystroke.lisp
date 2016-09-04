(in-package :gtk-emacs-like-input)
;;; Keyboard
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

(defun key-str (key)
  "Convert a key into a string representation"
    (concatenate 'string
	       (when (logbitp mod-control-bit key) "C-")
	       (when (logbitp mod-meta-bit    key) "M-")
	       (when (logbitp mod-alt-bit     key) "A-")
	       (when (logbitp mod-shift-bit   key) "S-")
	       (when (logbitp mod-super-bit   key) "s-")
	       (when (logbitp mod-hyper-bit   key) "H-")
	       (gtkcode->gtkcode-name (key-val key))))
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
      (signal 'kbd-parse-error :string (string char))))
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



(defun keystroke-setup (widget)     ;; wiring
    (g-signal-connect widget "key-press-event" 'on-key-press)
   ; (g-signal-connect widget "key-release-event" 'on-key-release)
)






