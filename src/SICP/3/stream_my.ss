(define true #t)
(define false #f)

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
     (lambda ()
       (if (not already-run?)
           (begin (set! result (proc))
                    (set! already-run? #t)
                    result)
           result))))

;; define delay
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define the-empty-system '())

(define (stream-null? s) (null? s))

;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      (apply proc (map stream-car argstreams))
      (apply stream-map
               (cons proc (map stream-cdr argstreams)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-system)
          ((pred (stream-car s))
           (cons-stream (stream-car s)
                             (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line s)
  (newline)
  (display s))

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
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-system
	  (cons-stream
	   (apply proc (map stream-car argstreams))
	   (apply stream-map
			  (cons proc (map stream-cdr argstreams))))))


(define s1  (cons-stream 1 the-empty-system))
(define s11  (cons-stream 11 the-empty-system))
(define s2  (cons-stream 22 (cons-stream 2 the-empty-system)))
(define s33  (cons-stream 33 (cons-stream 3 the-empty-system)))

(apply +  '(1 22))
(stream-car (stream-map + s1 s11))
(stream-car (stream-cdr (stream-map + s2 s33)))

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
(define fibs
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
	(cond ((> (square (stream-car ps)) n) true)
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
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
;; (define S (stream-cons 
;;            1
;;            (merge (scale-stream (stream-cons 0 S) 2)
;;                   (merge (scale-stream (stream-cons 0 (stream-cons 0 S)) 3)
;;                          (scale-stream (stream-cons 0 (stream-cons 0 S)) 5)))))					   
							
(stream-head S 100)
(stream-head (scale-stream S 2) 10)
;(stream-head (stream-filter (lambda (x) (not (= x 0))) S ) 10)
;ex3.57
;memo化しているなら 1 1 2 3 ... に対して 0 1 2 3 4 回
;指定ないと 一章と一緒 φ^n
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
  (let ((x the-empty-system))
	(set! x
		  (cons-stream 1 (stream-map - (mul-series (stream-cdr s) x))))
	x))

(stream-head (invert-unit-series exp-series) 10)

(stream-head (mul-series exp-series (invert-unit-series exp-series)) 10)

(stream-head (mul-series cosine-series (invert-unit-series cosine-series)) 10)

;ex3.62
(define (div-series n d)
  (if (= (stream-car d) 0)
	  (error "divine by zero")
	  (mul-series n (invert-unit-series d))))

(define tan-series (div-series sine-series cosine-series))

(stream-head tan-series 10)

;3.5.3
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
	(cons-stream 1.0
				 (stream-map (lambda (guess)
							   (sqrt-improve guess x))
							 guesses)))
  guesses)

(stream-head (sqrt-stream 2) 10)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-head pi-stream 10)

;s2s0 - 2s2s1 + s2s2 - s2s2 + 2s2s1 -s1s1
; s2s0 - s1s1
; s0 - 2s1 + s2

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
		(s1 (stream-ref s 1))
		(s2 (stream-ref s 2)))
	(cons-stream (- s2 (/ (square (- s2 s1))
						  (+ s0 (* -2 s1) s2)))
				 (euler-transform (stream-cdr s)))))

(stream-head (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (cons-stream s 
			   (make-tableau transform
							 (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car 
			  (make-tableau transform s)))

(stream-head (accelerated-sequence euler-transform
								   pi-stream) 11)

;ex3.63
;ex3.64
(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
		(s1 (stream-ref s 1)))
	(if (> tolerance (abs (- s0 s1)))
		s1
		(stream-limit (stream-cdr s) tolerance))))

(stream-limit (sqrt-stream 2) 0.0000001)

(define (mysqrt  x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(mysqrt 3 0.001)
(mysqrt 4 0.001)
;ex.3.65
(log 2)

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(stream-head ln2-stream 10)
(stream-head (euler-transform ln2-stream) 10)
(stream-head (accelerated-sequence euler-transform
								   ln2-stream) 10)

(define (interleave s1 s2)
  (if (stream-null? s1)
	  s2
	  (cons-stream (stream-car s1)
				   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(pairs (stream-cdr s) (stream-cdr t)))))
n
(stream-head (pairs integers integers) 100)


(define int-pairs (pairs integers integers))

(stream-head int-pairs 100)

(stream-head 
 (stream-filter (lambda (pair)
				  (prime? (+ (car pair) (cadr pair))))
				int-pairs)
 100)


;ex3.66
(define (stream-find s target)
  (define (iter s n)
	(if (> n 50000) 
		'not_found
		(if (equal? target (stream-car s))
			n
			(iter (stream-cdr s) (+ n 1)))))
  (iter s 0))


(stream-find int-pairs '(1 100)) ; 197

(stream-find int-pairs '(1 10)) ; 17
(stream-find int-pairs '(9 10)) ; 766
(stream-find int-pairs '(10 10)) ; 1022

(stream-find int-pairs '(1 11)) ; 19
(stream-find int-pairs '(10 11)) ; 1534
(stream-find int-pairs '(11 11)) ; 2046

(stream-find int-pairs '(4 5)) ; 22
(stream-find int-pairs '(5 6)) ; 46
(stream-find int-pairs '(6 7)) ; 94
(stream-find int-pairs '(7 8)) ; 190
(stream-find int-pairs '(8 9)) ; 382


(stream-find int-pairs '(8 8)) ; 254
(stream-find int-pairs '(9 9)) ; 510
(stream-find int-pairs '(8 10)) ; 638
;(stream-find int-pairs '(99 100))

;ex3.67
(define (pairs2 s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave
	(stream-map (lambda (x) (list x (stream-car t)))
				(stream-cdr s))
	(interleave
	 (stream-map (lambda (x) (list (stream-car s) x))
				 (stream-cdr t))
	 (pairs2 (stream-cdr s) (stream-cdr t))))))


(stream-head (pairs2 integers integers) 100)




;; (define (triples s t u)
;;   (cons-stream 
;;    (list (stream-car s) (stream-car t) (stream-car u))
;;    (interleave
;; 	(stream-map (lambda (y) (list (stream-car s) (stream-car t) y))
;; 						(stream-cdr u))
;; 	(interleave
;; 	 (stream-map (lambda (pair) (cons (stream-car s) pair))
;; 				 (pairs (stream-cdr t) (stream-cdr u)))
;; 	 (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))
;ex3.69
(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
	(stream-map (lambda (pair) (cons (stream-car s) pair))
				(stream-cdr (pairs t u)))
	(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(stream-head (triples integers integers integers) 100)

(stream-find (triples integers integers integers) '(1 2 3))
(stream-find (triples integers integers integers) '(1 2 5))
(stream-find (triples integers integers integers) '(1 3 5))
;(stream-find (triples integers integers integers) '(5 12 13))
(stream-find (triples integers integers integers) '(6 8 10))

;; (stream-head
;;  (stream-filter
;;   (lambda (triple)
;;  	(let ((i (car triple))
;;  		  (j (cadr triple))
;;   		  (k (caddr triple)))
;;   	  (= (square k) (+ (square i) (square j)))))
;;     (triples integers integers integers)) 
;;   3)


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		 (let ((s1car (stream-car s1))
			   (s2car (stream-car s2)))
		   (cond 
			((equal? s1car s2car)
			 (cons-stream s1car
						  (merge-weighted (stream-cdr s1)
										  (stream-cdr s2) weight)))
			((< (weight s1car) (weight s2car))
			 (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
			(else
			 (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))


(define (weighted-pairs s t weight)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (merge-weighted
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
	weight)))
  
(stream-head (pairs integers integers) 10)

(stream-head (weighted-pairs integers integers (lambda (x) (apply + x))) 10)

(define (check370 x)
  (cond ((= (remainder x 2) 0) #f)
		((= (remainder x 3) 0) #f)
		((= (remainder x 5) 0) #f)
		(else #t))) 


(check370 1)
(check370 2)
(check370 3)
(check370 7)

(stream-head
 (stream-filter
  (lambda (x)
	(let ((i (car x))
		  (j (cadr x)))
	  (and (check370 i) (check370 j))))
  (weighted-pairs integers integers 
				  (lambda (x)
					(let ((i (car x))
						  (j (cadr x)))
					  (+ (* 2 i) (* 3 j) (* 5 i j)))))
  )
  100)

((lambda (x)
  (let ((i (car x))
		(j (cadr x)))
	(+ (* 2 i) (* 3 j) (* 5 i j))))
 '(1 59))

((lambda (x)
  (let ((i (car x))
		(j (cadr x)))
	(+ (* 2 i) (* 3 j) (* 5 i j))))
 '(7 11))
;3.71
(define (cube n) (* n n n))
(define (ramanujan-weight pair)
  (+ (cube (car pair))
	 (cube (cadr pair))))
(define ramanujan-pairs
  (weighted-pairs integers integers ramanujan-weight))

(stream-head ramanujan-pairs 10)
(define (ramanujans-iter s w)
  (let ((neww (ramanujan-weight (stream-car s))))
	(if (= w neww)
		(cons-stream neww
					 (ramanujans-iter (stream-cdr s) 0))
		(ramanujans-iter (stream-cdr s) neww))))

(define ramanujans (ramanujans-iter ramanujan-pairs 0))

(stream-head ramanujans 10)
  
; 3.72

(define (weight-372 pair)
  (+ (square (car pair))
	 (square (cadr pair))))
  
(define pairs-372
  (weighted-pairs integers integers weight-372))

(define (numbers-372-iter s w n)
  (let ((neww (weight-372 (stream-car s))))
	(if (= w neww)
		(if (= n 1)
			(cons-stream neww
						 (numbers-372-iter (stream-cdr s) 0 0))
			(numbers-372-iter (stream-cdr s) neww 1))
		(numbers-372-iter (stream-cdr s) neww 0))))


(define numbers-372 (numbers-372-iter pairs-372 0 0))

(stream-head numbers-372 10)


;;			
(define (integral integrand initial-value dt)
  (define int
	(cons-stream initial-value
				 (add-streams (scale-stream integrand dt)
							  int)))
  int)
					 

(define (list->stream l)
  (if (pair? l)
	  (cons-stream (car l)
				   (list->stream (cdr l)))
	  the-empty-system))

(display-stream (list->stream '(1 2 3)))

(display-stream (integral (list->stream '(1 2 3)) 0 1))
;ex3.73
(define (RC R C dt)
  (lambda (i v0)
	(add-streams
	 (scale-stream i R)
	 (integral (scale-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))
	
(stream-head (RC1 ones 0) 10)

(display-stream (RC1 (list->stream '(1.0 0.9 0.81 0.729 0.6561)) 0))

;ex3.74 
(define sense-data
  (list->stream
   '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))


(define (sign-change-detector i l)
  (cond ((and (>= l 0) (< i 0))
		 -1)
		((and (< l 0) (>= i 0))
		 1)
		(else
		 0)))
		  

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
						(stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))
(display-stream zero-crossings)

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-stream zero-crossings)

(define (make-zero-crossings2 input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
	(cons-stream (sign-change-detector avpt last-avpt)
				 (make-zero-crossings2 (stream-cdr input-stream)
									  (stream-car input-stream)
									  avpt))))

(define zero-crossings2 (make-zero-crossings2 sense-data 0 0))
(display-stream zero-crossings2)
;ex3.76
(define (smooth s)
  (cons-stream
   (average (stream-car s) (stream-car (stream-cdr s)))
   (smooth (stream-cdr s))))

(define (make-zero-crossings3 input-stream)
  (make-zero-crossings
   (smooth input-stream)
   0))

(define zero-crossings3 (make-zero-crossings3 sense-data))
(display-stream zero-crossings3)

(define (smooth st)
  (stream-map average st (stream-cdr st)))
;  (stream-map (lambda (x y) (/ (+ x y) 2)) st (stream-cdr st)))

;;3.5.4

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral delayed-integrand initial-value dt)
  (define int
	(cons-stream initial-value
				 (let ((integrand (force delayed-integrand)))
				   (add-streams (scale-stream integrand dt)
							  int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; gauche で動作するように修正した solve 手続き
(define (solve f y0 dt)
  (define dy (stream-map f y))
  (define y (integral (delay dy) y0 dt))
  y)


(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;ex3.77
(define (integral integrand initial-value dt)
  (cons-stream initial-value
			   (if (stream-null? integrand)
				   the-empty-system
				   (integral (stream-cdr integrand)
							 (+ (* dt (stream-car integrand))
								initial-value)
							 dt))))
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral delayed-integrand initial-value dt)
	(cons-stream initial-value
				   (if (stream-null? delayed-integrand)
					   the-empty-system
					   (let ((integrand (force delayed-integrand)))
						 (integral (delay (stream-cdr integrand))
								   (+ (* dt (stream-car integrand))
									  initial-value)
								   dt)))))
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)


(define (solve-2nd a b dt y0 dy0)
  (define ddy (add-streams (scale-stream dy a)
						   (scale-stream y b)))
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  y)

(stream-head (solve-2nd 1 2 0.001 1 0) 10)
(stream-ref (solve-2nd 3 -2 0.001 2 3) 1000)
(stream-ref (solve-2nd 1 6 0.001 3 -1) 1000)
(stream-ref (solve-2nd 0 1 0.001 1 1) 1000)

(define (solve-2nd-2 f dt y0 dy0)
  (define ddy (stream-map f dy y))
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  y)

(stream-head (solve-2nd-2 (lambda (dy y) (+ (* 1 dy) (* 2 y) )) 0.001 1 0) 10)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
	(define dvC (scale-stream iL (/ -1 C)))
	(define diL (add-streams
				 (scale-stream vC (/ 1 L))
				 (scale-stream iL (- (/ R L)))))
	(define iL (integral (delay diL) iL0 dt))
	(define vC (integral (delay dvC) vC0 dt))
	(cons vC iL)
	;(stream-map cons vC iL)
	))

(stream-head
 (car 
 ((RLC 1 1 0.2 0.1) 10 0))
 50)

(stream-head
 (cdr
 ((RLC 1 1 0.2 0.1) 10 0))
 50)
   

(define random-init 1)

(define (rand-update x)
  (modulo (+ (* 2423707 x) 7) 100000001))


(define rand
  (let ((x random-init))
        (lambda ()
          (set! x (rand-update x))
          x)))
(rand)	
   
(define random-numbers
  (cons-stream random-init
			   (stream-map rand-update random-numbers)))

(stream-head random-numbers 10)

(define (map-succesive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-succesive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-succesive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
					   random-numbers))


(stream-head cesaro-stream 10)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
	(cons-stream
	 (/ passed (+ passed failed))
	 (monte-carlo
	  (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
	  (next (+ passed 1) failed)
	  (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
			  (monte-carlo cesaro-stream 0 0)))

(stream-head pi 100)

;ex3.81



(define (rand-3-81 request-stream random-init)
  (if (stream-null? request-stream)
	  the-empty-system
	  (cond ((eq? 'generate (stream-car request-stream))
			 (let ((new-number (rand-update random-init)))
			   (cons-stream new-number
							(rand-3-81 (stream-cdr request-stream) new-number))))
			
			((number? (stream-car request-stream))
			 (let ((new-number (rand-update (stream-car request-stream))))
			   (cons-stream 
				new-number
				(rand-3-81 (stream-cdr request-stream) new-number))))
			(else
			 (display (stream-car request-stream))
			 (error "aaa")))))

(define rs (cons-stream	'generate
  		   (cons-stream 'generate
		   (cons-stream 1
		  (cons-stream 'generate
		  (cons-stream 'generate
		  (cons-stream '100
		  (cons-stream 'generate
		  the-empty-system))))))))

(define rs2 (list->stream '(generate generate 1 generate generate 100 generate)))

(display-stream (rand-3-81 rs 1))
(display-stream (rand-3-81 rs2 1))
				

(define (random)
  (let ((a (rand))
                (b (rand)))
        (if (> a b)
                (/ (* 1.0 b) a)
                (/ (* 1.0 a) b))))
		

(define (random-in-range low high)
  (let ((range (- high low)))
        (+ low (* range (random)))))

(define (random-stream-in-range a b)
  (cons-stream
   (random-in-range a b)
   (random-stream-in-range a b)))

(stream-head (random-stream-in-range 0 10) 10)
   


(define (circle-P-stream x1 x2 y1 y2)
  (define (circle-experiment)
	(let ((x (random-in-range x1 x2))
		  (y (random-in-range y1 y2))
		  (cx (/ (+ x1 x2) 2))
		  (cy (/ (+ y1 y2) 2))
		  (r (/ (- x2 x1) 2)))
	  (>= (square r)
		  (+ (square (- x cx))
			 (square (- y cy))))))
  (cons-stream
   (circle-experiment)
   (circle-P-stream x1 x2 y1 y2)))

(stream-head (circle-P-stream 2 8 4 10) 100)
(stream-head 
 (monte-carlo (circle-P-stream 2 8 4 10) 0 0) 100)

(define (estimate-integral P x1 x2 y1 y2)
  (if (not (= (- x2 x1) (- y2 y1))) (error "not square")
	  (scale-stream
	   (monte-carlo (P x1 x2 y1 y2) 0 0)
	   (* 1.0 (- x2 x1)  (- y2 y1)))))


(stream-head (estimate-integral circle-P-stream 2 8 4 10) 100)
(stream-head (estimate-integral circle-P-stream -1 1 -1 1) 100)


(define nyo 
  (stream-map cons random-numbers
			  random-numbers))

(stream-head nyo 10)
			  

