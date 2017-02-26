;;(require 'matrix)

(defun math:div-p(a b)
  (= (% a b) 0))

(defun math:div-ps(a b)
  (if (> a b)
      (math:div-p a b)
    (math:div-p b a)))

;; Memoized 
(defun math:factorial-1(n acc)
  (if (= n 0)
      acc
    (math:factorial-1 (1- n) (* n acc))))

(defun math:factorial(n)
  (math:factorial-1 n 1))

(defun math:float-div (r c)
  (/ r (float c)))

(defun math:cell-div(r c)
  (format "%.2f" (math:float-div r c)))

(defun math:cube(x) 
  (expt x 3))


(defvar fuzz-factor 1.0e-6)

(defun approx-equal (x y)
  (or (and (= x 0) (= y 0))
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         fuzz-factor)))

(defun math:percent-increase(p n)
  ( * (+ 1 (/ p 100.0)) n))

(defun math:percent-of (p n)
  (* (/ p 100.0) n))

(defun math:percent(n1 n2)
  (* 100 (math:float-div n1 n2)))

;; pointers?


(defun math:oddp(n)
  (not (= 0 (% n 2))))

(defun math:rangep (n1 n2 n)
  (if n
      (and (> n n1) (< n  n2) )
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For any problem there are two representations of the problem One is
;; the algebraic representation The other is the textual
;; representation Once a question is formulated questions can be asked
;; about several aspects of the answer
;; Simplistic definitions are related to each others in deep ways.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consider a simple algebraic fact
;; May be consider a simpler function as a way to prepare for the test
;; can you predict the next number of the sequence.
(defun math:delay (expr)
    (lambda()
      (lexical-let((e expr))
      (e))))
    
(defun math:additive-inverse(a)
  "We 'know' that adding a number to its additivie inverse results in zero"
  (* -1 a))

(defun math:multiplicative-identityp(a)
  "We have a initial proposition then we use it deduce other propositions"
  (eq a (math:additive-inverse a)))

(defvar math:error-tolerance .00000000000001)    

(defun math:variable? (x1)
  (symbolp x1))

(defun math:same-variable? (x1 x2)
  (and (math:variable? x1) (math:variable? x2) (eql x1 x2)))

(defun math:number? (e)
  (numberp e))

(defun  math:make-sum (e1 e2)
  (cond ((eql e1 0) e2)
        ((eql e2 0) e1)
        ((eql e1 e2)
         (math:make-prod 2 e1))
        (t
         (list '+ e1 e2))))

(defun math:sum?(e)
  (eql '+ (car e)))

(defun math:prod?(e)
  (eql '* (car e)))

(defun math:sum-car (e)
  (nth 1 e))
(defun math:sum-cdr (e)
  (nth 2 e))

(defun math:make-prod (e1 e2)
  (cond 
   ((or (eql e1 0) (eql e2 0)) 0)
   ((eql e1 1) e2)
   ((eql e2 1) e1)
        (t
         (list '* e1 e2))))

(defun math:prod-car (e)
  (nth 1 e))
(defun math:prod-cdr (e)
  (nth 2 e))


(defun math:deriv(expr v)
  (cond 
   ((math:number? expr) 0)
   ((math:variable? expr) 
    (if (math:same-variable? expr v) 1 0))
   ((math:sum? expr)
    (math:make-sum (math:deriv (math:sum-car expr) v)
              (math:deriv (math:sum-cdr expr) v)))
   ((math:prod? expr)
    (math:make-sum
     (math:make-prod (math:prod-car expr)
                   (math:deriv (math:prod-cdr expr) v))
     (math:make-prod (math:prod-cdr expr)
                   (math:deriv (math:prod-car expr) v))))
   (else 
    (error "Unkown expr " expr))))
      

(defun math:subs(f x v)
  (cond ((math:number? f) f)
        ((math:variable? f)
         (if (eql f x)
             v
           f))
        ((math:sum? f)
         (math:make-sum
          (math:subs (math:sum-car f) x v)
          (math:subs (math:sum-cdr f) x v)))
        ((math:prod? f)
         (math:make-prod
          (math:subs (math:prod-car f) x v)
          (math:subs (math:prod-cdr f) x v)))))

(defun math:eval (f x v)
  (eval (math:subs f x v)))
         
(defun math:estimate-root(f x x0)   
  "Find closest estimate of root "
  (- x0  (/ (math:eval  f x x0)
            (math:eval  (math:deriv f x) x x0))))

(defun math:within-tolerance(f x g)
  (> math:error-tolerance (abs (math:eval f x g))))

(defun math:root-iter(f x g)
  (if (math:within-tolerance f x g)
      g
    (math:root-iter f x (math:estimate-root f x g))))



(defun math:inverse(f n)
  (math:root-iter (list '+ f (* -1 n)) 'x 1.0))

(defun math:sqrt(n)
  (math:inverse '(* x x) n))

(defun math:cubert(n)
  (math:inverse '(* x (* x x))  n))

(provide 'math)