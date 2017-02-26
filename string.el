(defmacro string:set (str v)
  `(setq k ,str ,v))

(defmacro string:concat(str  &rest s)
  `(setq ,str (concat ,str ,@s)))

(defun string:repeat(c n)
  (let ((r ""))
  (while (> n 0)
    (string:concat r c)
    (setq n (- n 1)))r))

(defun string:ltrim (str)
  (let ((trim-pos (string-match "\\s +$" str)))
    (if trim-pos
        (substring str 0 trim-pos)
      str)))

(defun buffer:current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun string:rtrim (str)
  (let ((trim-pos (string-match "[^ \t]+" str)))
    (if trim-pos
        (substring str trim-pos)
      str)))

(defun string:empty (str)
  (equalp (length (string:trim str)) 0))

(defun string:trim (str)
  (string:rtrim (string:ltrim str)))

(defun buffer:cond-loop(condp action)
  (save-excursion
    (goto-char 0)
    (while (< (point) (point-max))
      (let ((line (buffer:current-line)))
        (if (funcall condp line )
            (funcall action line))
        (forward-line)))))

(defun delete-empty-lines()
  (interactive)
    (buffer:cond-loop 'string:empty 'kill-line ))

(provide 'string)
