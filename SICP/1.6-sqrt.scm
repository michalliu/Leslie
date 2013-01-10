;; SICP sqrt.scm
;; (load "sqrt.scm")

(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; #1.6
;; 据说，new-if是自己定义的过程，过程调用前对参数进行求值，从而和预想不一致
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))


(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
		guess
		(new-sqrt-iter (improve guess x)
			       x)))
;; Stack overflow
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

;; guile> (load "sqrt.scm")
;; guile> (sqrt 9)
;; 3.00009155413138
;; guile> (new-sqrt 9)
;; ERROR: Stack overflow
;; ABORT: (stack-overflow)
;; guile> 
;; 

(display (sqrt 9))
(display "\n")
