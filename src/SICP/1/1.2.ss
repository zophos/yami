(define (factorial n)
  (if (= n 1)
	  1
	  (* n (factorial (- n 1)))))
(factorial 5)
end


(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
 (if (> counter max-count)
	 product
	 (fact-iter (* counter product)
				(+ counter 1)
				max-count)))
(factorial2 6)

(define (plus1 a b)
  (if (= a 0)
	  b
	  (inc (plus1 (dec a) b))))

(define (plus2 a b)
  (if (= a 0)
	  b
	  (plus2 (dec a) (inc b))))

(define (inc a)
  (+ a 1))
(define (dec a)
  (- a 1))

;(use slib)
;(require 'trace)
;(trace plus1)
;(trace plus2)

(plus1 4 5)
(plus2 4 5)


(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
				 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(f 1)
(f 2)
(f 3)
(f 10)

(define (g n) (A 1 n))
(g 1)
(g 2)
(g 3)
(g 4)
(g 10)

(define (h n) (A 2 n))
(h 1)
(h 2)
(h 3)

(h 4)

;(h 5)
;(h 10)

  
(define (k n) (* 5 n n))


(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
	  bne
	  (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount
					 (- kinds-of-coins 1))
				 (cc (- amount 
						(first-denomination kinds-of-coins))
					 kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))
			  
			  
(count-change 100)
(count-change 6)
(count-change 11)

;ex. 1.11

(define (f n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		((= n 2) 2)
		(else
		 (+ 
		  (f (- n 1))
		  (* 2 (f (- n 2)))
		  (* 3 (f (- n 3)))))))n

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(define (f2 n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
	  c
	  (f-iter (+
			   a
			   (* 2 b)
			   (* 3 c))
			  a
			  b
			  (- count 1))))

(f2 3)
(f2 2)
(f2 1)
(f2 0)
(f2 4)
(f2 5)

(define (pascal-tri n k)
  (cond 
   ((= n 1) 1)
   ((= k 1) 1)
   ((= k n) 1)
   (else
	(+ (pascal-tri (- n 1) k)
	   (pascal-tri (- n 1) (- k 1))))))

(pascal-tri 1 1)
(pascal-tri 2 1)
(pascal-tri 3 2)
(pascal-tri 4 2)
(pascal-tri 5 3)

			   
			   



(sqrt 5)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
	  angle
	  (p (sine (/ angle 3.0)))))

(use slib)
(require 'trace)
(trace p)

(sine 12.15) ; p x 5

(sine 1) ; p x 3

(sine 100) ;p x 7
;1.2.4
(define (expt b n)
  (if (= n 0)
	  1
	  (* b (expt b (- n 1)))))

(expt 5 3)

(define (expt2 b n)
  (expt2-iter b n 1))

(define (expt2-iter b counter product)
  (if (= counter 0)
	  product
	  (expt2-iter b (- counter 1) (* b product))))

(expt2 5 3)


(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))
(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(fast-expt 5 3)
(fast-expt 2 5)

;ex 1.16

(define (fast-expt2 b n)
  (fast-expt2-iter b n 1))

;(define (odd? n)
;  (= (remainder n 2) 1))


(define (fast-expt2-iter b n a)
  (cond 
   ((= n 0) a)
   ((even? n)
	(fast-expt2-iter (square b) (/ n 2) a))
;   よくある間違い
;	(fast-expt2-iter b (/ n 2) (* a (square b))))
   (else 
	(fast-expt2-iter b (- n 1) (* a b)))))
					 

(fast-expt2 5 1)
(fast-expt2 5 2)
(fast-expt2 5 3)
(fast-expt2 5 4)

(define (mul a b)
  (if (= b 0)
	  0
	  (+ a (mul a (- b 1)))))

(mul 5 2)

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(double 2)
(halve 4)

(define (mul2 a b)
  (cond ((= b 0) 0)
		((even? b) (double (mul2 a (halve b))))
		(else (+ a (mul2 a (- b 1))))))

(mul2 5 2)
(mul2 5 3)
(mul2 4 4)



(define (mul3 a b)
  (mul3-iter a b 0))


(define (mul3-iter a b r)
  (cond 
   ((= b 0) r)
   ((even? b)
	(mul3-iter (double a) (halve b) r))
   (else 
;	(mul3-iter a (- b 1 ) (+ r a)))))
	(mul3-iter (double a) (halve (- b 1 )) (+ r a)))))
;http://www.pi-sliderule.net/SlideRule/Others/Noumin.aspx
(mul3 5 2)
(mul3 5 3)
(mul3 4 4)

(mul3 2 15)

(define (fib3 n)
  (fib3-iter 1 0 0 1 n))

(define (fib3-iter a b p q count)
  (cond ((= count 0) b)
		((even? count)
		 (fib3-iter a
					b
					(+ (* p p) (* q q))
					(+ (* q q) (* 2 p q))
					(/ count 2)))
		(else 
		 (fib3-iter (+ (* b q) (* a q) (* a p))
					(+ (* b p) (* a q))
					p
					q
					(- count 1)))))


(fib3 1)
(fib3 5)
(fib3 6)
(fib3 2)
(fib3 3)
(fib3 4)
(fib3 7)

(define (gcd a b)
  (if (= b 0)
	  a
	  (gcd b (remainder a b))))

(gcd 24 18)

(gcd 206 40)
(trace gcd)
206 40
40 6
6 4
4 2
2 0
;ifで14回
(remainder 206 40)
(remainder 40 (remainder 206 40))
(remainder (remainder 206 40)
		   (remainder 40 (remainder 206 40)))
(remainder (remainder 40 (remainder 206 40))
		   (remainder (remainder 206 40)
				c	  (remainder 40 (remainder 206 40))))

;結果を返す際 4回
(remainder (remainder 206 40)
		   (remainder 40 (remainder 206 40)))
; 1.2.6
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

(prime? 7)
(prime? 8)
(prime? 100)
(prime? 101)

(use math.mt-random)
(define m (make <mersenne-twister> :seed (sys-time)))
(mt-random-integer m 10)
(define (random a) (mt-random-integer m a))
(random 10)
(random 1)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m))
					m))
		(else
		 (remainder (* base (expmod base (- exp 1) m))
					m))))

(define (fermat-test n)
  (define (try-it a)
;	(display a)
;	(newline)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

(use slib)
(require 'trace)
(trace fermat-test)
(untrace fermat-test)

(fast-prime? 7 10)
(fast-prime? 8 10)
(fast-prime? 101 10)
	
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(runtime)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
	  (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 100)

(define (search-for-prime low high)
  (define (iter high n)
	(timed-prime-test n)
	(if (> (+ n 2) high)
		(display "\nend\n")
		(iter high (+ n 2))))
  (iter high low))

;(search-for-prime 1001 1101)
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

;(search-for-prime 10001 10101)		  
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

;(search-for-prime 100001 100101)		  
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(search-for-prime 1000001 1000051)		  
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

(sqrt 10.0)

(search-for-prime 1000000001 1000000051)
(search-for-prime 1000000001 1000000051)		  
(search-for-prime 10000000001 10000000051)


;(* 1.0 (/ (+ 133 82 80)  (+ 38 30 26)))
;(* 1.0 (/ (+ 241 275 268) (+ 133 82 80)))
;(* 1.0 (/ (+ 785 760 762) (+ 241 275 268)))

(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n )n)
		((divides? test-divisor n ) test-divisor)
		(else (find-divisor2 n (next test-divisor)))))


(define (next n)
  (if (= n 2) 3
	  (+ n 2)))
(next 2)
(next 3)
(next 5)

(define (prime2? n)
  (= n (smallest-divisor2 n)))


(define (timed-prime-test2 n)
  (newline)
  (display n)
  (start-prime-test2 n (runtime)))

(define (start-prime-test2 n start-time)
  (if (prime2? n)
	  (report-prime (- (runtime) start-time))))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test2 1009)
(timed-prime-test2 1013)
(timed-prime-test2 1019)


(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(timed-prime-test2 1000003)
(timed-prime-test2 1000033)
(timed-prime-test2 1000037)

(trace next)
(untrace next)

(define (smallest-divisor3 n)
  (find-divisor3-2 n))

(define (find-divisor3-2 n)
  (cond ((> (square 2) n ) n)
		((divides? 2 n ) 2)
  		(else (find-divisor3 n 3))))

(define (find-divisor3 n test-divisor)
  (cond ((> (square test-divisor) n )n)
		((divides? test-divisor n ) test-divisor)
		(else (find-divisor3 n (+ test-divisor 2)))))


(define (prime3? n)
  (= n (smallest-divisor3 n)))


(define (timed-prime-test3 n)
  (newline)
  (display n)
  (start-prime-test3 n (runtime)))

(define (start-prime-test3 n start-time)
  (if (prime3? n)
	  (report-prime (- (runtime) start-time))))


(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(timed-prime-test2 1000003)
(timed-prime-test2 1000033)
(timed-prime-test2 1000037)
(timed-prime-test3 1000003)
(timed-prime-test3 1000033)
(timed-prime-test3 1000037)

(define (timed-fast-prime-test n times)
  (newline)
  (display n)
  (start-fast-prime-test n times (runtime)))

(define (start-fast-prime-test n times start-time)
  (if (fast-prime? n times)
	  (report-prime (- (runtime) start-time))))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

(timed-fast-prime-test 1009 1)
(timed-fast-prime-test 1013 1)
(timed-fast-prime-test 1019 1000)

(timed-fast-prime-test 1000003 1)
(timed-fast-prime-test 1000033 1)
(timed-fast-prime-test 1000037 1000)

(/ (log 1000000) (log 1000))

(fast-prime? 561 100)

(define (carmichael-test n i)
  (cond ((<= n i) #t)
		((= (expmod i n n) i) (carmichael-test n (+ i 1)))
		(else #f)))


(carmichael-test 561 1)
(carmichael-test 1105 1)
(carmichael-test 1729 1)
(carmichael-test 2465 1)
(carmichael-test 2821 1)
(carmichael-test 6601 1)

; k != 1 and k != n -1 and r == 1
(define (apply-trivial-check k n r)
  (if (and (not (= k 1))
           (not (= k (- n 1)))
           (= r 1))
      0
      r))

(define (remainder-or-trivial k m)
  (apply-trivial-check k m (remainder (square k) m)))


(define (remainder-check k n)
  (define (remainder-check-sub k n r)
	(if (= r 1) (if (or (= k 1) (= k (- n 1))) r
					0)
		r)
	)
  (remainder-check-sub k n (remainder (square k) n)))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder-check (expmod2 base (/ exp 2) m)
							   m))
		(else
		 (remainder (* base (expmod2 base (- exp 1) m))
					m))))

(define (miller-rabin-test n)
    (define (try-it a)
	  (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))



(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
		((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
		(else #f)))

(miller-rabin-prime? 7 10)
(miller-rabin-prime? 10007 10)
(miller-rabin-prime? 10009 10)
(miller-rabin-prime? 10037 10)
(miller-rabin-prime? 1000003 10)
(miller-rabin-prime? 1000033 10)
(miller-rabin-prime? 1000037 10)





(miller-rabin-prime? 561 10)
(miller-rabin-prime? 8 10)

(miller-rabin-prime? 1105 10)
(miller-rabin-prime? 1729 10)
(miller-rabin-prime? 2465 10)
(miller-rabin-prime? 2821 10)
(miller-rabin-prime? 6601 10)



