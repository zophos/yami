(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 60)

(define new-withdraw 
  (let ((balance 100))
	(lambda (amount)
	  (if (>= balance amount)
	  (begin (set! balance (- balance amount))
			 balance)
	  "Insufficient funds"))))

(new-withdraw 50)
(new-withdraw 60)

(define (make-withdraw balance)
  (lambda (amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)
(W2 40)
(W1 40)

(define (make-account balance)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch m)
	(cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
					   m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 50)


(define acc2 (make-account 100))
((acc2 'deposit) 0)

		  
;ex3.1

(define (make-accumulator sum)
  (lambda (add)
	(begin (set! sum (+ sum add))
		   sum)))

(define A (make-accumulator 5))
(A 10)
(A 10)

(define (make-accumulator sum)
  (lambda (add)
	(begin (set! sum (+ sum add))
		   sum)))

(define A (make-accumulator 5))
(A 10)
(A 10)


;ex3.2
(define (make-monitored f)
  (let ((calls 0))
	(define (how-many-calls?)
	  calls)
	(define (reset-count)
	  (set! calls 0)
	  calls)
	(define (dispatch m)
	  (cond ((eq? m 'how-many-calls?) (how-many-calls?))
			((eq? m 'reset-count) (reset-count))
			(else
			 (set! calls (+ calls 1))
			 (f m))))
	dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
;ex3.3

(define (make-account balance password)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch p m)
	(if (eq? p password)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  (else (error "Unknown request -- MAKE-ACCOUNT"
						   m)))
		(lambda (n)
		  "Incorrect passsword")))
  dispatch)

(define aac (make-account 100 'secret-password))
((aac 'secret-password 'withdraw) 40)
((aac 'some-other-password 'deposit) 50)

;ex3.4

(define (make-account balance password)
  (let ((count-of-bad-password 0))
	(define (withdraw amount)
	  (if (>= balance amount)
		  (begin (set! balance (- balance amount))
				 balance)
		  "Insufficient funds"))
	(define (deposit amount)
	  (set! balance (+ balance amount))
	  balance)
	(define (call-the-cops)
	  (error "tuuhou"))
	(define (dispatch p m)
	  (if (eq? p password)
		  (begin 
			(set! count-of-bad-password 0)
			(cond ((eq? m 'withdraw) withdraw)
				  ((eq? m 'deposit) deposit)
				  (else (error "Unknown request -- MAKE-ACCOUNT"
							   m))))
		  (begin
			(set! count-of-bad-password (+ count-of-bad-password 1))
			(if (>= count-of-bad-password 7)
				(lambda (n)
				  (call-the-cops))
				(lambda (n)
				  "Incorrect passsword")))))
  dispatch)
  )


(define aac (make-account 100 'secret-password))
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'some-other-password 'deposit) 50)
((aac 'secret-password 'deposit) 50)

;3.1.2


(define random-init 1)

(define (rand-update x)
  (modulo (+ (* 2423707 x) 7) 100000001))


(define rand
  (let ((x random-init))
	(lambda ()
	  (set! x (rand-update x))
	  x)))


(define (estimate-pi trails)
  (sqrt (/ 6 (monte-carlo trails cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
	(cond ((= trails-remaining 0)
		   (/ trails-passed trails))
		  ((experiment)
		   (iter (- trails-remaining 1) (+ trails-passed 1)))
		  (else
		   (iter (- trails-remaining 1) trails-passed))))
  (iter trails 0))

(define (random-gcd-test trails inital-x)
  (define (iter trails-remaining trails-passed x)
	(let ((x1 (rand-update x)))
	  (let ((x2 (rand-update x1)))
		(cond ((= trails-remaining 0)
			   (/ trails-passed trails))
			  ((= (gcd x1 x2) 1)
			   (iter (- trails-remaining 1)
					 (+ trails-passed 1)
					 x2))
			  (else 
			   (iter (- trails-remaining 1)
					 trails-passed
					 x2))))))
  (iter trails 0 inital-x))

(define (estimate-pi2 trails inital-x)
  (sqrt (/ 6 (random-gcd-test trails inital-x))))

;ex3.5
;(use math.mt-random)
;(define m (make <mersenne-twister> :seed (sys-time)))
;(mt-random-real m)

(define (random)
  (let ((a (rand))
		(b (rand)))
	(if (> a b)
		(/ (* 1.0 b) a)
		(/ (* 1.0 a) b))))

;(define (random-in-range low high)
;  (let ((range (- high low)))
;	(+ low (* range (mt-random-real m)))))

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (* range (random)))))


(define (estimate-integral P x1 x2 y1 y2 trails)
  (if (not (= (- x2 x1) (- y2 y1))) (error "not square")
	  (* (- x2 x1)  (- y2 y1)  (monte-carlo trails (P x1 x2 y1 y2)))))

(define (square x)
  (* x x))

(define (circle-P x1 x2 y1 y2)
  (define (circle-experiment)
	(let ((x (random-in-range x1 x2))
		  (y (random-in-range y1 y2))
		  (cx (/ (+ x1 x2) 2))
		  (cy (/ (+ y1 y2) 2))
		  (r (/ (- x2 x1) 2)))
	  (>= (square r)
		  (+ (square (- x cx))
			 (square (- y cy))))))
  circle-experiment
  )


;(* 1.0 (estimate-integral circle-P 2 8 4 10 100))
(* 1.0 (estimate-integral circle-P -1 1 -1 1 10000))
;(* 1.0 (estimate-integral circle-P 1 -1 1 -1 10000))


;ex3.6
(define random-init 1)

(define (rand-update x)
  (modulo (+ (* 2423707 x) 7) 100000001))

(define rand
  (let ((x random-init))
	(define (generate)
	  (set! x (rand-update x))
	  x)
	(define (reset)
	  (set! x random-init))
	(define (dispatch m)
	  (cond ((eq? m 'generate) (generate))
			((eq? m 'reset) (reset))
			(else (error "Error method: " m))))
	dispatch))

(rand 'generate)
(rand 'reset)
	  
  
;3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
	(set! balance (- balance amount))
	balance))

(define W (make-simplified-withdraw 25))

(W 20)
(W 10)

(define (make-decrementer balance)
  (lambda (amount)
	(- balance amount)))

(define D (make-decrementer 25))

(D 20)
(D 10)

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))

(W1 20)
(W1 20)
(W2 20)

(define (make-account balance)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch m)
	(cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
					   m))))
  dispatch)

(define peter-acc (make-account 100))
(define paul-acc (make-account 100))

((peter-acc 'withdraw) 10)
((paul-acc 'withdraw) 0)

(define peter-acc (make-account 100))
(define paul-acc peter-acc)


((peter-acc 'withdraw) 10)
((paul-acc 'withdraw) 0)

(define (factorial n)
  (define (iter product counter)
	(if (> counter n)
		product
		(iter (* counter product)
			  (+ counter 1))))
  (iter 1 1))

(factorial 5)

(define (factorial n)
  (let ((product 1)
		(counter 1))
	(define (iter)
	  (if (> counter n)
		  product
		  (begin (set! product (* counter product))
				 (set! counter (+ counter 1))
				 (iter))))
	(iter)))

(factorial 5)

(define (factorial-bad n)
  (let ((product 1)
		(counter 1))
	(define (iter)
	  (if (> counter n)
		  product
		  (begin 
				 (set! counter (+ counter 1))
				 (set! product (* counter product))
				 (iter))))
	(iter)))

(factorial-bad 5)

;ex3.7
(define (make-account balance password)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  (define (dispatch p m)
	(if (eq? p password)
		(cond ((eq? m 'withdraw) withdraw)
			  ((eq? m 'deposit) deposit)
			  (else (error "Unknown request -- MAKE-ACCOUNT"
						   m)))
		(lambda (n)
		  "Incorrect passsword")))
  dispatch)

(define (make-joint acc acc-password joint-password)
  (define (dispatch p m)
	(if (eq? p joint-password)
		(acc acc-password m)
		(lambda (n)
		  "Incorrect joint-passsword")))
  dispatch)
  

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 0)
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 0)
((paul-acc 'rosebut 'withdraw) 0)
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 0)
((paul-acc 'rosebud 'deposit) 20)
((peter-acc 'open-sesame 'withdraw) 0)

;ex3.8
(define f
  (let ((value 0)
		(pre 0))
	(lambda (new)
	  (begin
		(set! pre value)
		(set! value new)
		pre))))

(f 1)
(f 0)

(+ (f 0) (f 1))



(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
	(lambda (amount)
	  (if (>= balance amount)
		  (begin (set! balance (- balance amount))
				 balance)
		  "Insufficient funds"))))

(define (make-withdraw initial-amount)
  ((lambda (balance)
	 (lambda (amount)
	  (if (>= balance amount)
		  (begin (set! balance (- balance amount))
				 balance)
		  "Insufficient funds")))
   initial-amount))
   
