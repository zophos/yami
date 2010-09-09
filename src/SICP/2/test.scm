(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add a b)
  (if (= a 0) b
	  (add (sub-1 a) (add-1 b))))
	  
  
