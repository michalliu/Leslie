;; #1.8
;; 实现类似求平方根过程的求立方根过程。
;; (x/(y*y) + 2*y)/3，y是x的立方根近似值

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

;; 获取更好的近似值
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
		 x)))

(define (cube-root x)
  (cube-iter 1.0 x ))

(display "\n cube-root 27: ")
(display (cube-root 27))
(display "\n cube-root 8 : ")
(display (cube-root 8))
(display "\n cube-root 9 : ")
(display (cube-root 9))
(display "\n")


;; re-define cube-root with local-defined
(define (new-cube-root x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (cube-iter guess)
    (if (good-enough? guess)
	guess
	(cube-iter (improve guess))))
  (cube-iter 1.0))

(display "\n new-cube-root 27: ")
(display (new-cube-root 27))
(display "\n new-cube-root  8: ")
(display (new-cube-root 8))
(display "\n new-cube-root  9: ")
(display (new-cube-root 9))
(display "\n")
