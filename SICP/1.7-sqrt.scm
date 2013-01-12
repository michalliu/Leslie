;; SICP sqrt.scm
;; (load "sqrt.scm")

;; #1.6
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
  (sqrt-iter 1.0 x ))




;; #1.7
(define (new-good-enough? guess pre-guess)
  (< (abs (/ (- guess pre-guess) pre-guess)) 0.001))

(define (new-sqrt-iter guess x pre-guess)
  (if (new-good-enough? guess pre-guess)
      guess
      (new-sqrt-iter (improve guess x)
		     x guess)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x 0.9))


 

;; 9
(display (sqrt 9))
(display "\n")
(display (new-sqrt 9))
(display "\n")

;; 99
(display (sqrt 99))
(display "\n")
(display (new-sqrt 99))
(display "\n")

;; 9999
(display (sqrt 9999))
(display "\n")
(display (new-sqrt 9999))
(display "\n")

;; 0.00000001
(display (sqrt 0.00000001))
(display "\n")
(display (new-sqrt 0.00000001))
(display "\n")

;; if num is too small, error
;; 0.0312501065624275			
;; 9.79973446376897e-4


;; 9999999999999
(display (sqrt 9999999999999))
(display "\n")
(display (new-sqrt 9999999999999))
(display "\n")

;; 结论：采用监视猜测值从一次迭代到下一次迭代的变化情况，当改变值相对于猜测值的比率很小的时候就结束。
;; 对于很大的数，这种策略依然奏效，但对于很小的数，这种策略无法工作.
