(define (cons x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)
		  (else
		   (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(car (cons 1 2))
(cdr (cons 1 2))

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (n-divide v d)
  (define (iter v n)
	(cond 
	 ((= v 0) n)
	 ((= (remainder v d) 0) 
	  (iter (/ v d) (+ n 1)))
	 (else
	  n)))
  (iter v 0))
(n-divide 18 2)
				  
(define (car z)
  (n-divide z 2))
		  
(define (cdr z)
  (n-divide z 3))
		  
(car (cons 1 2))
(cdr (cons 1 2))
(car (cons 1 0))
(cdr (cons 0 2))
(cdr (cons 0 0))


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(add-1 zero)
(add-1 (add-1 zero))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

((one (lambda (x) (+ x 1))) 0)
((two (lambda (x) (+ x 1))) 0)

(((add one zero) (lambda (x) (+ x 1))) 0)
(((add two one) (lambda (x) (+ x 1))) 0)
(((add two zero) (lambda (x) (+ x 1))) 0)

(define (add n1 n2)
  (lambda (f) (lambda (x) 
				((n1 f) ((n2 f) x)))))


(((add one zero) (lambda (x) (+ x 1))) 0)
(((add two one) (lambda (x) (+ x 1))) 0)
(((add two zero) (lambda (x) (+ x 1))) 0)
(((add two three) (lambda (x) (+ x 1))) 0)


(define (mul n1 n2)
  (lambda (f) (lambda (x) 
				((n1 (n2 f)) x))))

(((mul one zero) (lambda (x) (+ x 1))) 0)
(((mul two one) (lambda (x) (+ x 1))) 0)
(((mul two zero) (lambda (x) (+ x 1))) 0)
(((mul two three) (lambda (x) (+ x 1))) 0)

