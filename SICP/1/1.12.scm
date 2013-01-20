;; #1.12
;; 下面的数值模式称为“帕斯卡三角形”，也叫“杨辉三角”：
;;                 1
;;                1 1
;;               1 2 1
;;              1 3 3 1
;;             1 4 6 4 1
;;             ........
;;  三角形边上的数都是1，内部的每个数是位于它上面的两个数之和。
;;  请写一个过程，它采用递归计算过程计算出帕斯卡三角形。
;;
;;
;; 1.输入参数n，代表有几行，第n行有n个数字
;; 2.

;; 计算第n行，第m个数字
(define (YH-compute-num n m)
  (cond ((= m 1) 1)
	((= m n) 1)
	(else (+ (YH-compute-num (- n 1) (- m 1)) (YH-compute-num (- n 1) m)))))

;; 打印第n行，第m个数字
(define (YH-print-num n m)
  (display (YH-compute-num n m))
  (cond ((< m n) (display " "))
	(else (display "\n"))))

;; 打印n个空格
(define (YH-print-space m f)
  (cond ((>= m 0) (cond ((= m 0)
			 (if (= f 1) 
			     (display " ")
			     (display "  ")))
			 (else (display "  ") (YH-print-space (- m 1) f))))))

;; 打印第n行,k=1
(define (YH-print-line n k)
  (cond ((<= k n) (YH-print-num n k) (YH-print-line n (+ k 1)))
	(else (display "\n"))))

;; 寻找近似的最小整数
(define (find-num n m)
  (if (< (- n m) 1) m
      (find-num n (+ m 1))))

;; k=1,j是总共几行，以更美观方式打印第n行
(define (YH-pretty-line n j)
  (if (integer? (/ (- j n) 2))
      (YH-print-space (find-num (/ (- j n) 2) 0) 1)
      (YH-print-space (/ (- j n) 2) 0))
  (YH-line n))

(define (YH-line n)
  (YH-print-line n 1))

;; 打印前n行
(define (YH-print-prev-line n k)
  (cond ((<=  n k) (YH-line n) (YH-print-prev-line (+ n 1) k))
	(else (display "\n"))))

(define (YH-pretty-print-prev-line n k)
  (cond ((<= n k) (YH-pretty-line n k) (YH-pretty-print-prev-line (+ n 1) k))
	(else (display "\n"))))


(define (YH-prev-line k)
  (YH-print-prev-line 1 k))

(define (YH-pretty-prev-line k)
  (YH-pretty-print-prev-line 1 k))

(define (help )
  (display "Usage : \n")
  (display "    (load \"1.12.scm\")         加载此模块定义过程\n")
  (display "    (YH-compute-num n m)      计算杨辉三角第n行第m个数字\n")
  (display "    (YH-line n)               打印杨辉三角第n行\n")
  (display "    (YH-prev-line n)          打印杨辉三角前n行\n")
  (display "    (YH-pretty-prev-line n)   以更美观方式打印杨辉三角前n行\n")
  (display "    (help)                    获取帮助信息\n"))

(help)



;; guile log
;;
;;
;; guile> (load "1.12.scm")
;;Usage : 
;;    (load "1.12.scm")         加载此模块定义过程
;;    (YH-compute-num n m)      计算杨辉三角第n行第m个数字
;;    (YH-line n)               打印杨辉三角第n行
;;    (YH-prev-line n)          打印杨辉三角前n行
;;    (YH-pretty-prev-line n)   以更美观方式打印杨辉三角前n行
;;    (help)                    获取帮助信息
;;guile> (YH-compute-num 6 3)
;;10
;;guile> (YH-line 6)
;;1 5 10 10 5 1
;;
;;guile> (YH-prev-line 6)
;;1
;;
;;1 1
;;
;;1 2 1
;;
;;1 3 3 1
;;
;;1 4 6 4 1
;;
;;1 5 10 10 5 1
;;
;;
;;guile> (YH-pretty-prev-line 6)
;;      1
;;
;;     1 1
;;
;;    1 2 1
;;
;;   1 3 3 1
;;
;;  1 4 6 4 1
;;
;; 1 5 10 10 5 1
;;
;;
;;guile> 
