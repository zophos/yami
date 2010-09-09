(use util.stream)
;(require srfi/41/streams)
;(require srfi/40/stream)


;; (define (stream-ref s n)
;;    (if (= n 0)
;;  	  (stream-car s)
;;  	  (stream-ref (stream-cdr s) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;; 	  the-empty-system
;; 	  (cons-stream (proc (stream-car s))
;; 				   (stream-map proc (stream-cdr s)))))

;; (define (stream-for-each proc s)
;;   (if (stream-null? s)
;; 	  'done
;;  	  (begin (proc (stream-car s))
;;  			 (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; (define (stream-car stream) (car stream))
;; (define (stream-cdr stream) (force (cdr stream)))



(stream-car (stream-cons 2  (stream-cons 1 stream-null)))
(stream-cdr (stream-cons 2  (stream-cons 1 stream-null)))
(define cons-stream stream-cons)

(define the-empty-system stream-null)

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


(define (stream-enumerate-interval low high)
  (if (> low high)
	  the-empty-system
	  (cons-stream
	   low
	   (stream-enumerate-interval (+ low 1) high))))

;; (define (stream-filter pred stream)
;;   (cond ((stream-null? stream) the-empty-system)
;; 		((pred (stream-car stream))
;; 		 (cons-stream (stream-car stream)
;; 					  (stream-filter pred
;; 									 (stream-cdr stream))))
;; 		(else
;; 		 (stream-filter pred (stream-cdr stream)))))



(stream-car
 (stream-cdr
  (stream-filter prime?
				 (stream-enumerate-interval 10000 1000000))))

;; (define (force delayed-object)
;;   (delayed-object))
;ex3.50
(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-system
	  (cons-stream
	   (apply proc (map stream-car argstreams))
	   (apply my-stream-map
			  (cons proc (map stream-cdr argstreams))))))


(define s1  (cons-stream 1 the-empty-system))
(define s11  (cons-stream 11 the-empty-system))
(define s2  (cons-stream 22 (cons-stream 2 the-empty-system)))
(define s33  (cons-stream 33 (cons-stream 3 the-empty-system)))

(apply +  '(1 22))
(stream-car (my-stream-map + s1 s11))
(stream-car (stream-cdr (my-stream-map + s2 s33)))

;ex3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)

(stream-ref x 7)

;ex 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))


(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
						 seq))

(stream-ref y 7)

(display-stream z)

(display sum)

(display-stream seq)
; 3.3.2
(define (stream-head s n)
  (define (iter s i)
	(if (>= i n)
		'done
		(begin
		  (display (stream-car s))
		  (display " ")
		  (iter (stream-cdr s) (+ i 1)))))
  (iter s 0))


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(stream-head integers 10)


(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
				 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
		   (lambda (x)
			 (not (divisible? x (stream-car stream))))
		   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))
(
define fibs
  (cons-stream 0
			   (cons-stream 1
							(add-streams (stream-cdr fibs)
										 fibs))))
(stream-ref fibs 8)
(stream-ref fibs 9)
(stream-ref fibs 10)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))


(stream-head double 10)

(define (prime? n)
  (define (iter ps)
	(cons ((> (square (stream-car ps)) n) true)
		  ((divisible? n (stream-car ps)) false)
		  (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream 
   2
   (stream-filter prime? (integers-starting-from 3))))

(stream-head primes 10)

;ex3.53
(define s (cons-stream 1 (add-streams s s)))
(stream-head s 10)

;ex3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(stream-head (mul-streams integers integers) 10)

(define factorials (cons-stream 1 
								(mul-streams factorials
											 integers)))
											 
(stream-head factorials 10)


;ex3.55
(define (partial-sums s)
  (define (iter ps)
	(cons-stream (stream-car ps)
				 (iter (add-streams (stream-cdr ps) s))))
  (iter s))

(stream-head (partial-sums integers) 10)

;ex3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		 (let ((s1car (stream-car s1))
			   (s2car (stream-car s2)))
;; 		   (newline)
;; 		   (display s1car)
;; 		   (display " ")
;; 		   (display s2car)
;; 		   (newline)
		   (cond ((< s1car s2car)
				  (cons-stream s1car (merge (stream-cdr s1) s2)))
				 ((> s1car s2car)
				  (cons-stream s2car (merge s1 (stream-cdr s2))))
				 (else
				  (cons-stream s1car
							   (merge (stream-cdr s1)
									  (stream-cdr s2)))))))))

(stream-head fibs 10)
(stream-head primes 10)

(stream-head (merge primes fibs) 10)

;; ;1 2 3 4 5 6 8 9 10 12....
;;(define S (cons-stream 1 (merge (scale-stream S 2)
;;                                (merge (scale-stream S 3)
;;                                       (scale-stream S 5)))))
;;http://note.golden-lucky.net/2006/07/gauche-mit-scheme-sicp-ex.html
(define S (stream-cons 
           1
           (merge (scale-stream (stream-cons 0 S) 2)
                  (merge (scale-stream (stream-cons 0 (stream-cons 0 S)) 3)
                         (scale-stream (stream-cons 0 (stream-cons 0 S)) 5)))))					   
							
(stream-head S 10)
(stream-head (scale-stream S 2) 10)
(stream-head (stream-filter (lambda (x) (not (= x 0))) S ) 10)

;ex3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(/ 10.0 7)
(stream-head (expand 1 7 10) 10)
(/ 30.0 8)
(stream-head (expand 3 8 10) 10)
;ex3.59
(define (integrate-series ss)
  (define (iter s n)
	(cons-stream (/ (stream-car s) n)
				 (iter (stream-cdr s) (+ n 1))))
  (iter ss 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(stream-head exp-series 10)

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(stream-head cosine-series 10)
(stream-head sine-series 10)
;ex3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
			   (add-streams (mul-series s1 (stream-cdr s2))
							(scale-stream (stream-cdr s1) (stream-car s2)))))

(stream-head (mul-series cosine-series cosine-series) 10)
(stream-head (mul-series sine-series sine-series) 10)

(stream-head (add-streams (mul-series cosine-series cosine-series)
						  (mul-series sine-series sine-series)) 10)
				  
;ex3.61
(define (invert-unit-series s)
  (let ((x stream-null))
	(set! x
		  (cons-stream 1 (stream-map - (mul-series (stream-cdr s) x))))
	x))

(stream-head (invert-unit-series exp-series) 10)

(stream-head (mul-series exp-series (invert-unit-series exp-series)) 10)

;ex3.62
(define (div-series n d)
  (if (= (stream-car d) 0)
	  (error "divine by zero")
	  (mul-series n (invert-unit-series d))))

(define tan-series (div-series sine-series cosine-series))

(stream-head tan-series 10)


	  
