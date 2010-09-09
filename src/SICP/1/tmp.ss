(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (mul-2 a b)
  (cond ((= b 0) 0)
		((even? b) (double (mul-2 a (halve b))))
		(else (+ a (mul-2 a (- b 1))))))

(mul-2 5 3)
(mul-2 5 2)
(mul-2 5 4)
(mul-2 5 16)

(use slib)


(trace mul-2)
