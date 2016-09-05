(in-package :gtk-emacs-like-input)
(define-condition eli-error (error)
  ((message
    :initarg :message
    :initform nil
    :accessor message)
   (value :initarg :value  :initform nil  :accessor value)))

(defmethod print-object ((object eli-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "ELI error: ~A. value: ~A~%" (message object) (value object)))
  (format t "2~%" )    
    )


