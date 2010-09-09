;(require (lib "trace.ss"))
;(trace square)

;(trace test)
;(trace p)
;(define (p) (p))
;(define (test x y)
;  (if (= x 0)
;	  0
;	  y))
;(test 0 (p))

(define (abs x)
  (cond ((> x 0) x)
		((= x 0) 0)
		((< x 0) (- x))))
 

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

(define (mul2 a b c)
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
		((and (>= a b) (<= b c)) (+ (square a) (square c)))
		(else (+ (square b) (square a)))))

(mul2 1 3 5)
(mul2 3 3 5)
(mul2 5 3 5)
(mul2 5 3 1)
(mul2 3 3 3)
(mul2 5 1 1)




(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
				 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(my-sqrt 9)

(my-sqrt (+ 100 37))

;ex1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))
;(trace sqrt-iter2)
;(define (my-sqrt2 x)
;  (sqrt-iter2 1.0 x))

;(my-sqrt2 2)

;ex1.7
(sqrt 0.000001)
(my-sqrt 0.000001)


(define x 10000000000000)
(sqrt x)
;(my-sqrt x)

;(trace sqrt-iter)


(define (sqrt-iter3 pre-guess guess x)b
  (if (good-enough3? pre-guess guess x)
	  guess
	  (sqrt-iter3 guess (improve guess x)
				  x)))

;(define (good-enough3? pre-guess guess x)
;  (< (abs (- 1.0(/ pre-guess guess))) 0.001))
;  (< (abs (- pre-guess guess)) 0.001))

(define (good-enough3? pre-guess guess x)
  (< (/ (abs (- pre-guess guess)) guess) 0.00001))

(define (good-enough3? pre-guess guess x)
  (< (abs (- 1 (/ pre-guess guess))) 0.00001))


(define (my-sqrt3 x)
  (sqrt-iter3 10000000000 1.0 x))

(my-sqrt3 2)

(sqrt 0.000001)
(my-sqrt3 0.000001)

(sqrt x)
(my-sqrt3 x)

(sqrt 1E+50)
;(my-sqrt 1.0E+100)
(my-sqrt3 1.0E+100)
(my-sqrt3 1.0E+50)

(sqrt 1e-200)
(my-sqrt3 1e-200)


;ex 1.8
(define (cube x)
  (* x x x))

(define (good-enough-cube? guess x)
  (< (abs (/ (- (cube guess) x) x)) 0.00001))


(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root (improve-cube guess x)
                 x)))

(cube-root 1.0 8)
(cube-root 1.0 27)


(define (square x)
  (* x x))

(define (square2 x)
  (exp (double (log x))))

(define (double x) 
  (+ x x))

(square 1.11313)
(square2 1.11313)


(define (my-sqrt2 x)
  (define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
	(average guess (/ x guess)))
  (define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))


(define (my-sqrt3 x)
  (define (good-enough? guess)
	(< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
	(average guess (/ x guess)))
  (define (sqrt-iter guess)
	(if (good-enough? guess)
		guess
		(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
