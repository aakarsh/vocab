(require 'vocabulary-words)
(require 'questions)
(require 'cl)


(defgroup vocabulary nil
  "Info subsystem.")


(defcustom vocabulary-mode-hook nil
  "Hooks run when `vocabulary-mode' is called."
  :type 'hook
  :group 'vocabulary)

(defvar vocabulary:etymology-dom nil)

(defvar vocabulary:etymology-xml-file "./vocabulary-words-etymologies.xml")


(defstruct vocabulary:word 
  (word "word")
  (meanings '())
  example
  (difficulty 0))

(defstruct vocabulary:question  
  word
  qmeaning
  ans:num
  ameaning
  meanings
  (difficulty 10))

(defun vocabulary:number-words ()
  (length vocabulary:line-list))

;; represents vocab file
(defvar vocabulary:file "./vocabulary-words.el")

(defun vocabulary:load-words()
  "Reloads the vocabulary file with all the words we want to learn"
  (load-file vocabulary:file))

(defun vocabulary:insert-line ()
  "Inserts emtpy line"
  (interactive)
  (move-end-of-line 1)
  (insert "\n(\"\" (\"\") \"\")")
  (move-beginning-of-line 1)
  (indent-for-tab-command)
  (search-forward "\"")
)

(defun vocabulary:insert-num-list (start end) 
  (interactive "nStart :\nnEnd :")
  (map 'list (lambda (s) (insert (concat (number-to-string s) ".\n"))) (number-sequence start end)))

(defun vocabulary:take (start count )
  (let* ((cdrlist (nthcdr start vocabulary:word-list))
         (cdrlistlen (length cdrlist)))          
         (butlast cdrlist (- cdrlistlen count) )))

(defun vocabulary:word-compare-difficulty( w1 w2)
  (> (vocabulary:word-difficulty w1)   
     (vocabulary:word-difficulty w2)))


(defun vocabulary:get-word (line) 
  (car line))

(defun vocabulary:get-meanings (line)
  (cadr line))

(defun vocabulary:get-ex (line)
  (nth 2 line)
)
(defun vocabulary:num-words (list)
  (length  list))

(defun vocabulary:get-line (list i)
  (nth list i))

(defun vocabulary:get-random-line ()
  (nth (random (length vocabulary:line-list))  vocabulary:line-list ))

(defun vocabulary:get-random-ex ()
  (vocabulary:get-ex (vocabulary:get-random-line)))

(defun vocabulary:get-n-random-line (n )
  (let ((retval))
    (dotimes (i n) 
      (let* ((rl (vocabulary:get-random-line)))
      (setq retval (cons  rl retval))))
    retval))


(defun vocabulary:update-line (word)
  "Shit way to find and update the file but sort of works."
  (save-excursion
  (with-current-buffer (find-file-noselect  vocabulary:file)
    (goto-char 1)
    (if (search-forward (vocabulary:word-word word) (point-max) t)
        (progn
          (beginning-of-line)
          (kill-line 1)
          (insert "   ")
          (insert (vocabulary:word->line word))          
          (save-buffer)
        )))))

;; need to update the word in file
(defun vocabulary:word->line (word)
  "Good enough for now"  
  (format "(\"%s\"  %S \"%s\" %d ) \n" 
          (vocabulary:word-word word)
          (vocabulary:word-meanings word)
          (vocabulary:word-example word)
          (vocabulary:word-difficulty word)))


(defun vocabulary:words->question (words anum)
  (let ((word (nth anum words))
        (meanings (mapcar 'vocabulary:word-meanings words )))
    (make-vocabulary:question 
     :word word ;; need to be word
     :meanings meanings
     :ans:num anum
     :ameaning (nth anum meanings)     
     )))


(defun vocabulary:line->word (line)
  (let* ((word (vocabulary:get-word line))
         (meanings (vocabulary:get-meanings line))
         (example (vocabulary:get-ex line))
         (retval)
         )
    (setq retval (make-vocabulary:word 
                  :word word
                  :meanings meanings
                  :example example))
  retval
))

(defun vocabulary:lines->words(lines)
  (mapcar 'vocabulary:line->word lines))

(defun vocabulary:lines->question (lines anum)
    (vocabulary:words->question (vocabulary:lines->words lines) anum))


(defun vocabulary:word->question(word)
  (vocabulary:word->question-1 word 
                               (vocabulary:get-n-random-line 3)))

(defun vocabulary:word->question-1(word lines)
  "Takes a word and a random line list and converts it to vocabulary question format "
  (let* 
      ((lines (mapcar 'vocabulary:word-meanings (vocabulary:lines->words lines)))
       (split-pos (random (length lines)))
       (split-lines (list:split  split-pos lines))
       (meanings (append (append (car split-lines) 
                                 (list (vocabulary:word-meanings word)) 
                                 (cadr split-lines )))))
       (make-vocabulary:question 
        :word word ;; need to be word
        :meanings meanings
        :ans:num split-pos
        :ameaning (nth split-pos meanings))))
     
(defun vocabulary:question-check-answer-p (question n)
 (if n
     (= (vocabulary:question-ans:num question)  n)))

(defun vocabulary:get-question (lines nanum)
  (let* ((qline (nth nanum lines))
         (qword (vocabulary:get-word qline))
         (qans (vocabulary:get-meanings qline))
         (retval))
    (setq retval (list 
                  (cons 'qword  qword)
                  (cons 'anum  nanum)
                  (cons 'aexample (vocabulary:get-ex qline))
                  (cons 'avals  (map 'list 'vocabulary:get-meanings lines))))
    retval))

(defun vocabulary:question-qword (question)
  (cdr (assoc 'qword question)))

(defun vocabulary:question-qanswers (question)
  (cdr (assoc 'avals question)))

(defun vocabulary:question-anum (question)
  (cdr (assoc 'anum question)))
(defun vocabulary:question-aexample (question)
  (cdr (assoc 'aexample question)))

(defun vocabulary:question-qanswer (question)
  (nth (vocabulary:question-anum question) (vocabulary:question-qanswers question)))

(defun vocabulary:question-to-string (question)
  (let ((retval "") (i 1))
    (concat "Define the word : " 
           (upcase (vocabulary:word-word (vocabulary:question-word question)))  "\n\n" 
           (dolist (elt (vocabulary:question-meanings question) retval)
             (setq retval (concat retval (number-to-string i) "."  (mapconcat 'identity elt " , " )  "\n\n"))
             (setq i (+ i 1))))))

(defvar vocabulary:show-etymology nil)
(defun vocabulary:question-to-answer-string (question)
  (let* (
         (qword (vocabulary:question-word question))
         (real-word (vocabulary:word-word qword ))
         )
    (concat "'"  (upcase real-word) "'" " : " 
            (list:join (vocabulary:word-meanings qword))
            "\n\nExample :\n" 
            (vocabulary:word-example qword )
            (if vocabulary:show-etymology
                (progn
                  "\n\nEtymology :\n\n\t"
                  (vocabulary:etymology-for-word real-word)))
)))

(defun vocabulary:get-random-question ()
  (vocabulary:lines->question (vocabulary:get-n-random-line 4) (random 4)))

;; I need to work on actually working rather than just writing emacs modes
(defun vocabulary:get-random-question-string ()
  (vocabulary:question-to-string (vocabulary:get-random-question)))

(defun vocabulary:ask-question ()
  (let ((l  (vocabulary:get-n-random-line 4)))
    (print (concat "Define: " (vocabulary:get-word (nth (random 4) l))))
    (dolist (l l) (print (vocabulary:get-meanings l)))))


(defun vocabulary:make-deck(config)
  (let* ((line-list (vocabulary:ask-range config vocabulary:line-list))
         (word-list (vocabulary:lines->words line-list))
         (question-heap (q:question-heap (mapcar  'vocabulary:word->question word-list))))
    (make-q:deck :config config  :question-heap question-heap )))


(defun vocabulary:start-test()
  (interactive)  
  (vocabulary:load-words)  ;; we load data
  (let* ((deck-config (q:prompt-deck-configuration (length vocabulary:line-list) 40))
        (deck    (vocabulary:make-deck deck-config ) ))
      (q:question-loop deck)
    ))



(defun vocabulary:mapwords(&optional f)
  (mapcar f (vocabulary:lines->words (subseq vocabulary:line-list 3504))))



(defun vocabulary:etymology-load-dom()
  (interactive)
  (if (not vocabulary:etymology-dom)  
      (with-current-buffer (find-file vocabulary:etymology-xml-file)
        (setq vocabulary:etymology-dom (dom:make-from-buffer)))
    vocabulary:etymology-dom))
    
(defun vocabulary:etymology-for-word(word)
  (let ((root-element (dom-document-element  (vocabulary:etymology-load-dom))))
    (catch 'return 
      (dolist (node (dom-element-child-nodes root-element))
        (let ((node-name (dom-node-name node)) (node-text (dom-node-text-content node)))
        (if (and (string= "word" node-name) (string= word node-text ))
            (progn 
                (message "Found  %s " node-text )
                (throw 'return (dom-node-text-content
                                (dom-node-next-sibling (dom-node-next-sibling node)))))))))))


(defun vocabulary:etymolgy-download-words()
    (interactive)
    (switch-to-buffer  (find-file "vocabulary-words-etymology.xml"))
    (goto-char (1- (point-max)))
    (lexical-let ((write-chunk 20) (count 0))
    (vocabulary:mapwords 
     (lambda(word)
       (let* ((real-word (vocabulary:word-word word))
              (etymology (vocabulary:etymology-lookup-online real-word)))
         (insert "<word>" real-word  "</word>" "\t\n" "<etymology>" 
                 (if etymology    
                     etymology 
                   "No Etymology Found" )
                 "\n</etymology>\n")
         (if (= 0 (% count write-chunk))
             (save-buffer))
         (incf count))))))

(defun vocabulary:download-full-etymology()
  (interactive)
    (with-current-buffer    (find-file "vocabulary-words-etymology.el")    
      (switch-to-buffer (current-buffer))
      (vocabulary:mapwords 
       (lambda(word)
           (let* ((real-word (vocabulary:word-word word))
                  (etymology (vocabulary:etymology-lookup-online real-word)))
             (insert "<word>" real-word  "</word>" "\t\n" "<etymolygy>" etymology "\n</etymology>\n")
)))))
                                        
(defvar vocabulary:mouse-map
"Keymap for vocab helper"
(let ((map (make-sparse-keymap)))
  (define-key  map [mouse-2] 'undefined) map)
)


(defvar vocabulary:mode-map
  (let ((map (make-sparse-keymap)))
;;    (define-key map [?\C-c ?$] 'vocabulary:insert-new-element)
;;    (define-key map [?\C-c ?$] 'vocabulary:insert-new-element)
    map))

(defvar vocabulary:mode)

(define-minor-mode vocabulary:minor-mode vocabulary:mode)

(defvar vocabulary-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
;;    (define-key map "n" 'vocabulary:next-question)
;;    (define-key map "p"  'vocabulary:previous-question)
    (define-key map "q"  'vocabulary:quit)
))

(defun vocabulary-mode()
  " 
TODO : Currently this is not of much use anyway
A mode to make it easier to test your vocabulary" 
    (kill-all-local-variables)
    (setq major-mode 'vocabulary-mode)
    (setq mode-name "Vocabulary Quiz")
    (setq tab-width 8)
    (use-local-map vocabulary-mode-map)
    (set-syntax-table text-mode-syntax-table)
    (setq local-abbrev-table text-mode-abbrev-table)
    (setq case-fold-search t)
;;    (setq buffer-read-only t)
    (make-local-variable 'vocabulary-current-question)
    (make-local-variable 'vocabulary-previous-question)
    (make-local-variable 'vocabulary-previous-question-feedback)
;;    (vocabulary-set-mode-line)
    (run-mode-hooks 'vocabulary-mode-hook)
  )

(fset 'vocabulary:insert-new-element
   "\C-e\C-m  \"\",(\"\"),\"\",\C-a\C-s\"")

(fset 'vocabulary:next-element
   "\C-s\"\C-m")


;; (defun vocabulary:etymology-lookup-online(word &optional n)
;;   (if (not n)
;;       (setq n 0))
;;   (let ((buffer))
;;     (setq dom (www:url-dom  (format "http://www.etymonline.com/index.php?search=%s&searchmode=phrase" word)))
;;     (nth 0 (mapcar 'dom-element-text-content (dom-document-get-elements-by-tag-name dom  "dd")))))

(defun vocabulary:etymology-lookup()
  (interactive)
  (let ((lookup-word)
        (buffer (get-buffer-create "*Word Etymology*")))
    (setq lookup-word (substring-no-properties (thing-at-point 'word)))
    (with-current-buffer buffer
      (buffer:clear-current-buffer)
      (insert 
       (vocabulary:etymology-lookup-online lookup-word)))    
    (pop-to-buffer buffer)))



(provide 'vocabulary-helpers)
