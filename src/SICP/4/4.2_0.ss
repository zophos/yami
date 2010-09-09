(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))


(define (test a b)
  (unless (= b 0)
		  (/ a b)
		  (begin (display "exception: returning 0")
				 (newline)
				 0)))


(define (factorial n)
  (unless (= n 1)
		  (* n (factorial (- n 1)))
		  1))

;(factorial 5)
