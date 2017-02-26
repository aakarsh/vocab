(defvar decorate:line-char "-")

(defun decorate:line (&optional ommit)
  (let ((line (string:repeat decorate:line-char 100)))
    (if (not ommit)
        (concat "\n" line  "\n")
      line)))


(provide 'decorate)