(require 'heap)
(require 'vocabulary-helpers)
(require 'decorate)
(require 'insult)

(defstruct q:deck 
  config
  (question-heap '())
  (statistics nil)
  (vocabulary-deck-p t)
  (current-score (make-deck:score)))

(defstruct q:deck-configuration   
  (size 40)
  (number 1)  
  (total 0))

(defstruct deck:score   (asked-questions  0)  (correct-answered 0))

;; Have the accessors work with all dependent variables.
(defun deck:score-errors(s)
  (- (deck:score-asked-questions  s ) (deck:score-correct-answered s)))
(defun deck:score-error-percent(s)
  (math:percent (deck:score-errors s) (deck:score-asked-questions s)))
(defun deck:score-correct-percent(s)
  (math:percent (deck:score-correct-answered s) (deck:score-asked-questions s)))

(defun q:deck-configuration-range-start (config)
  (* (q:deck-configuration-number config) 
      (q:deck-configuration-size config)))

(defun q:deck-configuration-range-end (config)
 (1- (+ (q:deck-configuration-range-start config) (q:deck-configuration-size config))))

(defun q:prompt-deck-configuration(&optional total deck-size)
  (let* ((deck-size 
          (if deck-size
              deck-size
            (string-to-number (read-from-minibuffer "Deck Size : "))))b
         (max (/ total deck-size))
         (number (string-to-number 
                  (read-from-minibuffer 
                   (format "Pick a deck number [%s- %s] : " 1 max)))))
     (make-q:deck-configuration
      :size deck-size
      :total total
      :number number)))      

(defun q:deck-range(config)
  (list  (q:deck-configuration-range-start  config)
         (q:deck-configuration-range-end  config)))

(defun vocabulary:ask-range (config list)  
  (list:take-range (q:deck-range config)   list))


(defun q:question-heap(questions)
  (let ((deck-heap (heap-create 'q:question-compare-difficulty (length questions))))
    (dolist (question questions deck-heap)
      (heap-add deck-heap question))))


(defun q:question-compare-difficulty (q1 q2)
  (> (q:question-difficulty q1)   
     (q:question-difficulty q2)))

(defun q:question-difficulty (q)
  (cond ((vocabulary:question-p q)
         (vocabulary:word-difficulty (vocabulary:question-word q)))
        ((q:mcq-p q)
         (q:mcq-difficulty q))))
         
(defun q:score-update (score correct)
  (if correct
      (incf (deck:score-correct-answered score)))
  (incf (deck:score-asked-questions score)))

(defun q:deck-pop-top-question (deck)
  (heap-delete-root (q:deck-question-heap deck)))

(defun q:deck-push-question(deck question)
  (heap-add (q:deck-question-heap deck) question))

(defun q:score-line(deck)
  (let ((score(q:deck-current-score deck)))
  (format   "Deck # %s  Questions Answered: %s \nCorrectly Answered: %s Correct Rate:%4.4s%%\nErrors:%s Error Rate:%4.4s%%" 
            (q:deck-configuration-number (q:deck-config deck))
            (deck:score-asked-questions score)
            (deck:score-correct-answered score)
            (deck:score-correct-percent score)
            (deck:score-errors score)
            (deck:score-error-percent score))))

(defun q:question-loop(deck)
  "This is unfortunately the ugliest part of this code perhaps I should be reading some scip stuff to correct this horrendosity"

  (let* ((question-buffer (get-buffer-create "*Question*"))         
         (cur-question nil)
         (cur-itercount 0)
         (read-answer nil)
         (prev-question nil)
         (prev-question-correct-p nil))    

    (switch-to-buffer question-buffer)
    (delete-other-windows)
    (with-current-buffer question-buffer
      (if (and question-buffer 
               (not (eq major-mode 'vocabulary-mode)))
          (vocabulary-mode))
      (progn
         (while t      
           (if (> cur-itercount 100) ;; Switch to next neck after going 100 words
               (progn 
                 (setq deck (q:deck-make-next deck))
                 (setq cur-itercount  0))
             (incf cur-itercount))
                       
           (setq cur-question (q:deck-pop-top-question deck))
           ;; ask question
           (q:fill-buffer-template cur-question 
                                   prev-question  prev-question-correct-p
                                   deck)
           (setq prev-question  cur-question)
           ;; check chosen answer
           (setq prev-question-correct-p (q:check-answer-p cur-question (q:read-answer)))
           ;;update the socre
           (q:score-update (q:deck-current-score deck) prev-question-correct-p)
           ;; adjust the difficutly
           (q:difficulty-adjust cur-question prev-question-correct-p)
;;           (message "%s" cur-question)
           ;; add back question to word heap
           (q:deck-push-question deck cur-question)
           ;; add back the current work 
           ;; may be what we are going to do here is just pop the question back in the heap 
           ;; persist question information
           (if (vocabulary:question-p cur-question)
               (vocabulary:update-line (vocabulary:question-word cur-question)))
)))))


(defun q:fill-buffer-template (cur-question prev-question prev-question-correct-p  deck)
  (buffer:clear-current-buffer)  
  (insert 
   (decorate:line t) "\n"
   (q:score-line deck)
   (decorate:line)
   (q:q->string cur-question))
  (if prev-question
      (progn
        (insert 
         (concat  (decorate:line)
                  (insult:feedback-text prev-question-correct-p)
                  (decorate:line)
                  (q:q->answer-string prev-question)              
                  )))
    (insert (decorate:line)))
  (goto-char 1))

  
(defun q:q->answer-string (q)
  "Brain dead works"
  (cond 
   ((vocabulary:question-p q)
    (vocabulary:question-to-answer-string q))
   ((q:mcq-p q)
    (symbol-name (q:mcq-answer q)))
  ))


(defun q:difficulty-adjust(q correct)  
  (if correct
      (q:difficulty-decf q)
    (q:difficulty-incf q)))

(defun q:difficulty-decf(q)
  "Brain dead works"
  (cond 
   ((vocabulary:question-p q)
    (decf (vocabulary:word-difficulty (vocabulary:question-word  q))))
   ((q:mcq-p q)
    (decf (q:mcq-difficulty q))
  )))

(defun q:difficulty-incf(q)
  (cond 
   ((vocabulary:question-p q)
    (incf (vocabulary:word-difficulty (vocabulary:question-word  q))))
   ((q:mcq-p q)
    (incf (q:mcq-difficulty q))
  )))

(defun q:check-answer-p (q n)
  (cond 
    ((vocabulary:question-p q)
     (vocabulary:question-check-answer-p q n))
    ((q:mcq-p q)
     (q:mcq-check-answer-p q n)
     )))

;; (defun q:text-in-buffer(q pq pqf score deck)
;;   (cond 
;;     ((vocabulary:question-p q)
;;      (vocabulary:text-in-buffer q pq pqf score deck))
;;     ((q:mcq-p q)
;;      (q->check q)
;; )))


(defun q:q->string(q)
  (cond 
    ((vocabulary:question-p q)
     (vocabulary:question-to-string q))
    ((q:mcq-p q) 
     (q:mcq-string q))))

(defstruct q:mcq
  question
  answers '()
  answer
  (difficulty 0)
)


(defun  q:mcq-check-answer-p(mcq ans)
  "Index into answer array and return true or false"
  (if (numberp ans)
      (let ((r (list 'a 'b 'c 'd 'e)))
        (equal (q:mcq-answer mcq) (nth ans r)))))

(defun q:mcq-string(mcq)
  "String representation of question"
  (let ((answers (q:mcq-answers mcq)))
    (concat (q:mcq-question mcq) "\n\n"
            (format "\n\n A. %s \n\n B. %s \n\n C. %s \n\n D. %s \n\n E. %s \n\n" 
                    (cadr (assoc 'a answers))
                    (cadr (assoc 'b answers))
                    (cadr (assoc 'c answers))
                    (cadr (assoc 'd answers))
                    (cadr (assoc 'e answers)))
  )))
  

(defun q:deck-make-next(deck)
  (let ((deck-configuration (q:deck-config deck)))
    (vocabulary:make-deck 
                     (make-q:deck-configuration  
                      :size (q:deck-configuration-size deck-configuration)
                      :total (q:deck-configuration-total deck-configuration)
                      :number 
                      (if (> (1+ (q:deck-configuration-range-end deck-configuration))
                             (q:deck-configuration-total deck-configuration))
                          1
                        (1+ (q:deck-configuration-number deck-configuration)) ;; we have exceeded our range we circle back to 1 
                        )))))

(defun vocabulary:write-deck-statistics()
  "We Need to do  something to persist the deck statistics."
  (with-temp-buffer 
    (buffer:clear-current-buffer)
    (insert (format  "(setq deck-number %s )" 1))))

(defvar q:answer-prompt "Pick a number or die [ 1-5 ] :  ")

(defun q:read-answer()   
  "Reads the answer from the user using keypress no enter required"
  (let* ((key-char (read-event q:answer-prompt))
         (n1 (event:number-relative-to-1  key-char))
         (na (event:number-relative-to-a  key-char))
         (nj (event:number-relative-to-j  key-char))
         (one-to-four-p (lambda(n) (math:rangep -1 5 n))))
    (cond 
     ((funcall one-to-four-p n1)  n1)
     ((funcall one-to-four-p na)  na)
     ((funcall one-to-four-p nj)  nj)
     (t  nil))))


(provide 'questions)


