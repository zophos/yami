(define (sum-integers a b)
  (if (> a b)
	  0
	  (+ a (sum-integers (+ a 1) b))))

(sum-integers 10 20)

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
	  0
	  (+ (cube a) (sum-cubes (+ a 1) b))))
  
(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
	  0
	  (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b ))))

(* 8 (pi-sum 1 100))
(* 8 (pi-sum 1 1000))
(* 8 (pi-sum 1 10000))

(define (sum term a next b)
  (if (> a b)
	  0
	  (+ (term a)
		 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define inc 10)
(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	(+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	 dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (x k)
	(cond ((= k 0) (f a))
		  ((= k n) (f b))
		  ((odd? k) (* 4 (f (+ a (* k h)))))
		  (else
		   (* 2 (f (+ a (* k h)))))))
  (* (/ h 3)
	 (sum x 0 inc n)))

(simpson  cube 0 1 100)
(simpson  cube 0 1 1000)


(define (sum2 term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

(sum2 identity 1 inc 10)

;ex1.31		
(define (product term a next b)
  (if (> a b)
	  1
	  (* (term a)
		 (product term (next a) next b))))


(define (factrial n)
  (product identity 1 inc n))

(factrial 5)
(factrial 4)

(define (plus2 n)
  (+ n 2))

(define (wallis-term n)
  (/ (/ (* (- n 1) (+ n 1)) n) n))

(* 4.0 (product wallis-term 3 plus2 100))

(* 4.0 (product wallis-term 3 plus2 500))

;ex1.32
(define (myaccumulate combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (combiner (term a)
				(myaccumulate combiner null-value term (next a) next b))))

(define (sum3 term a next b)
  (myaccumulate + 0 term a next b))

(sum3 identity 1 inc 10)

(define (product2 term a next b)
  (myaccumulate * 1 term a next b))

(* 4.0 (product2 wallis-term 3 plus2 100))

(* 4.0 (product2 wallis-term 3 plus2 500))


(define (myaccumulate2 combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum4 term a next b)
  (myaccumulate2 + 0 term a next b))

(sum4 identity 1 inc 10)

(define (product3 term a next b)
  (myaccumulate2 * 1 term a next b))
(* 4.0 (product3 wallis-term 3 plus2 100))
(* 4.0 (product3 wallis-term 3 plus2 500))

;ex1.33
(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n )n)
		((divides? test-divisor n ) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate p combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (if (p a)
		  (combiner (term a)
					(filtered-accumulate p combiner null-value term (next a) next b))
		  (filtered-accumulate p combiner null-value term (next a) next b))))

(define (ex1-33-a a b)
  (filtered-accumulate prime? + 0 square a inc b))
(ex1-33-a 2 3)
(ex1-33-a 2 4)
(ex1-33-a 2 5)
(ex1-33-a 2 6)
(ex1-33-a 2 7)

(define (ex1-33-b n)
  (define (p-1-33-b a)
	(= 1 (gcd a n)))
  (filtered-accumulate p-1-33-b * 1 identity 2 inc (- n 1)))

(ex1-33-b 8)

(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
	   a
	   (lambda (x) (+ x 4))
	   b))
(* 8 (pi-sum2 1 100))

(define (integral2 f a b dx)
  (* (sum f 
		  (+ a (/ dx 2.0))
		  (lambda x (+ x dx))
		  b)
	 dx))

(integral cube 0 1 0.01)
((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f x y)
  (define (f-helper a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
  (f-helper (+ 1 (* x y))
			(- 1 y)))

(define (f x y)
  ((lambda (a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
		(b (- 1 y)))
	(+ (* x (square a))
	   (* y b)
	   (* a b))))

(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))
(f f)
		
	  
   
	 
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
		midpoint
		(let ((test-value (f midpoint)))
		  (cond ((positive? test-value)
				 (search f neg-point midpoint))
				((negative? test-value)
				 (search f midpoint pos-point))
				(else midpoint))))))
				
			  
(define (average x y) 
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
		   (error "Values ar not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
					  1.0
					  2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (mysqrt x)
  (fixed-point (lambda (y) (/ x y))
			   1.0))
;(mysqrt 2)

(define (mysqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
			   1.0))

(mysqrt 2)


(fixed-point (lambda (y) (+ 1  (/ 1 y))) 1.0)

(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (display next)
	  (newline)
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

(fixed-point2 (lambda (x) (/ (log 1000)  (log x))) 1.1)

(fixed-point2 (lambda (x) (average x (/ (log 1000)  (log x)))) 1.1)


(define (cont-frac n d k)
  (define (iter i)
	(if (> i k)
		0
		(/ (n k) (+ (d k) (iter (+ i 1))))))
  (iter 1))

(fixed-point (lambda (y) (+ 1  (/ 1 y))) 1.0))
;1/Φ = 2/(1+√5) = (√5-1)/2 = 0.61803398874989484820
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)


(define (cont-frac2 n d k)
  (define (iter i r)
	(if (< i 1)
		r
		(iter (- i 1) (/ (n i) (+ (d i) r)))))
  (iter k 0))

(/ 1 (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 10))
(/ 1 (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 20))
(/ 1 (cont-frac2 (lambda (i) 1.0) (lambda (i) 1.0) 100))


(exp 1)
(+ 2 (cont-frac2 (lambda (i) 1.0) (lambda (i) 
									(if (= (remainder (+ i 1) 3) 0)
										(* (/ (+ i 1) 3) 2.0)
										1.0))
									10))

(+ 2 (cont-frac2 (lambda (i) 1.0) (lambda (i) 
									(if (= (remainder (+ i 1) 3) 0)
										(* (/ (+ i 1) 3) 2.0)
										1.0))
									20))

(+ 2 (cont-frac2 (lambda (i) 1.0) (lambda (i) 
									(if (= (remainder (+ i 1) 3) 0)
										(* (/ (+ i 1) 3) 2.0)
										1.0))
									100))

(tan 1)

(define (tan-cf x k)
  (define (iter i)
	(cond ((> i k) 0)
		  ((= i 1) (/ x (- 1 (iter 2))))
		  (else
		   (/ (* x x) (- (- (* i 2) 1)  (iter (+ i 1)))))))
  (iter 1))

(* 1.0 (tan-cf 1 1))
(* 1.0 (tan-cf 1 10))

		
;1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square n) (* n n))
(define (average x y) 
  (/ (+ x y) 2))

((average-damp square) 10)

(define (mysqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(mysqrt 2)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cube-root 2)
(cube-root 3)
(cube-root 8)

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))

(define (cube x) (* x x x))
((deriv cube) 5)


(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (mysqrt x)
  (newtons-method (lambda (y) (- (square y) x))
				 1.0))
(mysqrt 2.0)


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (mysqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
							average-damp
							1.0))
(mysqrt 2.0)
(define (mysqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
							newton-transform
							1.0))
(mysqrt 2.0)

;ex1.40

(define (cubic a b c)
  (lambda (y) 
	(+ (cube y) (* a (square y)) (* b y) c)))

(newtons-method (cubic 0 0 1) 1)

(newtons-method (cubic 0 0 -1) 1)
(newtons-method (cubic 0 0 8) 1)
(newtons-method (cubic 2 3 8) 1)
;ex1.41
(define (double f)
  (lambda (x)
	(f (f x))))

(define (inc x) (+ x 1))
((double inc) 5)
(((double double) inc) 5)
(use slib)
(require 'trace)
(trace double)
(((double (double double)) inc) 5)
((((double double) double) inc) 5)


;ex1.42
(define (compose f g)
    (lambda (x) (f (g x))))
((compose square inc) 6)

;ex1.43
(define (repeated f n)
  (lambda (x)
	(if (<= n 1) (f x)
		(f ((repeated f (- n 1)) x)))))

(define (repeated f n)
  (lambda (x)
	(if (<= n 1) (f x)
		((compose f (repeated f (- n 1))) x))))

(define (repeated f n)
  (if (<= n 1) (lambda (x) (f x))
	  (compose f (repeated f (- n 1)))))


(define (repeated f n)
  (lambda (x)
	(define (iter g m x)
	  (if (= m 1) (g x)
		  (iter (compose f g) (- m 1) x)))
	(iter f n x)))


(define (repeated f c)
  (define (iter result i)
	(if (= i 1) result
		(iter (compose result f) (- i 1))))
  (iter f c))
	

((repeated square 1) 5)
((repeated square 2) 5)
((repeated square 3) 5)


;exp1.44	  
(define (smooth f)
  (lambda (x)
	(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;bad
;(define (n-fold-smooth f n)
;  (repeated (smooth f) n))


((n-fold-smooth inc 5) 5)

((n-fold-smooth square 2) 2)
((n-fold-smooth square 3) 4.0)

(define (mysqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(mysqrt 2)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cube-root 8)

(define (n4-root x)
  (fixed-point (average-damp (lambda (y) (/ x (cube y)))) 1.0))
;(n4-root 2)

(define (n4-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y)))) 1.0))
(n4-root 16)
(n4-root 2)

;(define (g-root n x)
;  (fixed-point ((repeated average-damp (+ 1(floor (/ (log n) (log 2)))))
;				(lambda (y) (/ x (n-exp (- n 1) y))))
;			   1.0))

(define (g-root n x)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
				(lambda (y) (/ x (n-exp (- n 1) y))))
			   1.0))

(define (n-exp n x)
  (if (= n 1) x
	  (* x (n-exp (- n 1) x))))

(define (t n)
 (+ 1 (/ (log n) (log 2))))
(t 5)
(g-root 4 2)
(g-root 5 5)

;ex1.46
(define (iterative-improve guess improve)
  (lambda (y)
	(if (guess y) y
		((iterative-improve guess improve) (improve y)))))

(define (mysqrt x)
  ((iterative-improve (lambda (y) (< (abs (- (square y) x)) 0.001))
					  (lambda (y) (average y (/ x y)))
					  )
   1.0)
  )


(mysqrt 2)


(define (fixed-point f first-guess)
  ((iterative-improve (lambda (y) (< (abs (- y (f y))) tolerance))
					  f) first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
					
  
