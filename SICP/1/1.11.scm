;; #1.11
;; 函数f由以下规则定义：
;; 如果n < 3, f(n)=n;
;; 如果n>=3 , f(n)=f(n-1)+2f(n-2)+3f(n-3)
;; 请写一个采用递归计算f的过程；
;; 再写一个采用迭代计算f的过程;


;; 树形递归:要维护一条命令链条

(define (call-self n)
  (cond ((< n 3) n)
	(else (+ (call-self (- n 1)) (* 2 (call-self (- n 2))) (* 3 (call-self (- n 3)))))))

(display "\nf(1)=1: ")
(display (call-self 1))

(display "\nf(2)=2: ")
(display (call-self 2))

(display "\nf(3)=4: ")
(display (call-self 3))

(display "\nf(4)=11: ")
(display (call-self 4))

(display "\nf(5)=25: ")
(display (call-self 5))

(display "\n")

;; 迭代：只维护固定的几个变量

(define (call-self-again n)
  (cc 2 1 0  n))

(define (cc prev1 prev2 prev3 n)
  (cond ((< n 3) n)
	((= n 3) (+ prev1 (* 2 prev2) (* 3 prev3)))
	(else (cc (+ prev1 (* 2 prev2) (* 3 prev3))   prev1 prev2 (- n 1)))))

(display "\nf(1)=1: ")			;(cc 2 1 0 1)->1
(display (call-self-again 1))

(display "\nf(2)=2: ")			;(cc 2 1 0 2)->2
(display (call-self-again 2))

(display "\nf(3)=4: ")			;(cc 2 1 0 3)->4
(display (call-self-again 3))

(display "\nf(4)=11: ")		        ;(cc 2 1 0 4)->(cc 4 2 1 3)->11
(display (call-self-again 4))

(display "\nf(5)=25: ")			;(cc 2 1 0 5)->(cc 4 2 1 4)->(cc 11 4 2 3)->11+8+6
(display (call-self-again 5))

(display "\nf(6):")
(display (call-self 6))
(display "\nff(6):")
(display (call-self-again 6))

(display "\nf(7):")
(display (call-self 7))
(display "\nff(7):")
(display (call-self-again 7))


(display "\nf(17):")
(display (call-self 17))
(display "\nff(17):")
(display (call-self-again 17))
(display "\n")


(display "\nf(28):")
(display (call-self 28))
(display "\nff(28):")
(display (call-self-again 28))
(display "\n")












  
