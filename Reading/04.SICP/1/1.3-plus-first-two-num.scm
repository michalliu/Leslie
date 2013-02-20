;; 定义过程，以三个数为参数，返回其中较大的两个之和
;; defined a process, plus the first two max num

(define (plus-first-two-num a b c)
  (cond ((and (> a b) (> b c) (+ a b)))
	((and (> a b) (< b c) (+ a c)))
	((and (< a b) (> a c) (+ a b)))
	((and (< a b) (< a c) (+ b c)))))

(display "\nInput: 1 2 3\n")
(display (plus-first-two-num 1 2 3))

(display "\nInput: 12 4 6\n")
(display (plus-first-two-num 12 4 6))
(display "\n")
