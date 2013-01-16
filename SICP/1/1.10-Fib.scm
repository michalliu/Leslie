;; #1.10

;; n!
(define (fib n)
  (if (= n 1)
      1
      (* n (fib (- n 1)))))

(define (base-two n)
  (if (= n 0)
      1
      (* 2 (base-two (- n 1)))))

(display "\n fib 6:")
(display (fib 6))
(display "\n fib 3:")
(display (fib 3))

(display "\n base-two 2 :")
(display (base-two 2))
(display "\n base-two 10:")
(display (base-two 10))
(display "\n base-two 8:")
(display (base-two 8))


(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))


(display "\n (A 1 10):")
(display (A 1 10))
(display "\n (A 2 4) :")
(display (A 2 4))
(display "\n (A 3 3) :")
(display (A 3 3))
(display "\n")

(display "\n(f 4)=2*4:")
(define (f n) (A 0 n))			;f=2n
(display (f 4))

(display "\n(g 10)=1024:")
(define (g n) (A 1 n))			;f=(base-two n)
(display (g 10))

(display "\n(h 3)=8*2=16:")
(define (h n) (A 2 n))			;f=(base-two (f (- n 1)))
(display (h 3))

(display "\n(h 5)=(* (base-two n) (fib (- n 1))):")
;;(display (* (base-two (* 5 5))))

(display "\n (h 5):")
;;(display (h 5))
(display "\n")

(define (k n) (* 5 n n))		;k=(* (square n))
(display "\n (h 1):")
(display (h 1))
(display "\n (h 2):")
(display (h 2))
(display "\n (h 3):")
(display (h 3))
(display "\n (h 4):")
(display (h 4))
(display "\n")


;; h
;;        { 2                      ;(n -1)
;; h(n) = {
;;        { (base-two (h (- n 1))) ;(n > 1)
