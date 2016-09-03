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



