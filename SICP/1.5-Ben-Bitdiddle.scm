;; 通过以下方法来检测解释器采用正则序或者应用序来求值
(define (p) (p))			;如果是应用序，将在此死循环

(define (test x y)
  (if (= x 0)
      (display "正则序，先完全展开，再归约求值\n")
      y))

(test 0 (p))

