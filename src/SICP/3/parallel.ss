;ex3.38

(define true #t)
(define false #f)
;a
(define balance 100)
(define (peter) (set! balance (+ balance 10)))
(define (paul) (set! balance (- balance 20)))
(define (mary) (set! balance (-  balance (/ balance 2))))

(define balance 100)
(peter) (paul) (mary)
(newline)
;45

(define balance 100)
(peter) (mary) (paul) 
(newline)
;35

(define balance 100)
(paul) (peter) (mary)
(newline)
;45

(define balance 100)
(paul) (mary) (peter) 
(newline)
;50

(define balance 100)
(mary) (peter) (paul) 
(newline)
;40

(define balance 100)
(mary) (paul) (peter) 
(newline)
;40

;35 40 45 50 

;b
;30 55 60 70 80 90 110

;; (use gauche.threads)
;; (define (parallel-execute . procs)
;;   (let ((threads (map make-thread procs)))
;;     (map thread-start! threads)
;;     (map thread-join! threads)))

(define (parallel-execute . args)
  "each arg is a thunk, to be run in a separate thread"
  (let ((threads ()))
    (dolist (thunk args)
      (push! threads (thread-start! (make-thread thunk))))
    (map thread-join! threads)))

(define (make-serializer/no-dw)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex-lock! mutex)
        (let ((val (apply p args)))
          (mutex-unlock! mutex)
          val))
      serialized-p)))

;; same using dynamic-wind
;; note: this function returns a function A which takes a function B
;; and returns a new function C that is a sync'd variant of function B.
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (proc)
      (lambda args
        (dynamic-wind
          (lambda () (mutex-lock! mutex))
          (lambda () (apply proc args))
          (lambda () (mutex-unlock! mutex)))))))


(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
				  (lambda () (set! x (+ x 1)))
 )
;(parallel-execute (lambda () (set! x (+ x 1)))
;				  (lambda () (set! x (* x x)))
;				  )
(display x)


(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
				  (s (lambda () (set! x (+ x 1))))
				  )
;(parallel-execute (s (lambda () (set! x (+ x 1))))
;				  (s (lambda () (set! x (* x x))))
;				  )
(display x)



(use srfi-1)
(list-tabulate 100
               (lambda (i)
                 (let ((x 10)
                       (s (make-serializer)))
                   (parallel-execute (s (lambda () (set! x (* x x))))
                                     (s (lambda () (set! x (+ x 1)))))
                   x)))

(list-tabulate 100
               (lambda (i)
                 (let ((x 10))
                   (parallel-execute (lambda () (set! x (* x x)))
                                     (lambda () (set! x (+ x 1))))
                   x)))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

;ex3.39
(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
				  (s (lambda () (set! x (+ x 1))))
				  )
(display x)
; 100 101 121

;ex3.40
;(* 100 100 100) = 1000000
;(* 1000 1000)   = 1000000

;(* 10 10 10)    = 1000
;(* 10 10 100)   = 10000
;(* 10 100 100)  = 100000
;(* 10 10)       = 100
;(* 10 1000)     = 10000

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
					   (account2 'balance))))
	((account1 'withdraw) difference)
	((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amountt)
  (let ((s (account 'serializer))
		(d (account 'deposit)))
	((s d) amountt)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
		(serializer2 (account2 'serializer)))
	((serializer1 (serializer2 exchange))
	 account1 account2)))


(define (make-serializer)
  (let ((mutex (make-mutex)))
	(lambda (p)
	  (define (serialized-p . args)
		(mutex 'acquire)
		(let ((val (apply p args)))
		  (mutex 'release)
		  val))
	  serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
	(define (the-mutex m)
	  (cond ((eq? m 'acquire)
			 (if (test-and-set! cell)
				 (the-mutex 'acquire)))
			((eq? m 'release) (clear! cell))))
	the-mutex))

(define (test-and-set! cell)
  (if (car cell)
	  true
	  (begin (set-car! cell true)
			 false)))

(define (clear! cell)
  (set-car! cell false))

;ex3.47

(define (make-semaphore n)
  (let ((mutex (make-mutex))
		(semaphore 0))
	(define (dispatch m)
	  (cond ((eq? m 'acquire)  
			 (mutex 'acquire)
			 (if (< semaphore n) 
				 (begin
				   (set! semaphore (+ 1 semaphore))
				   (mutex 'release)
				   true)
				 (begin
				   (mutex 'release)
				   false)
				 ))
			((eq? m 'release)
			 (if (= semaphore 0) 
				 (begin
				   (mutex 'release)
				   false)
				 (begin
				   (set! semaphore (- semaphore 1))
				   (mutex 'release)
				   true)
				 ))))
	dispatch))

(define semaphore (make-semaphore 2))

(semaphore 'acquire)
(semaphore 'acquire)
(semaphore 'acquire)

(semaphore 'release)
(semaphore 'release)
(semaphore 'release)
			 
			 
	


(define (make-semaphore2 n)
  (let ((semaphore 0)
		(cell (list false)))
	(define (dispatch m)
	  (cond ((eq? m 'acquire)  
			 (if (test-and-set! cell)
				 (dispatch 'acquire)
				 (if (< semaphore n) 
					 (begin
					   (set! semaphore (+ 1 semaphore))
					   (clear! cell)
					   true)
					 (begin
					   (clear! cell)
					   false)
					 )))
			((eq? m 'release)
			 (if (test-and-set! cell)
				 (dispatch 'release)
				 (if (= semaphore 0) 
					 (begin
					   (clear! cell)
					   false)
					 (begin
					   (set! semaphore (- semaphore 1))
					   (clear! cell)
					   true)
					 )))))

	dispatch))


(define semaphore2 (make-semaphore2 2))

(semaphore2 'acquire)
(semaphore2 'acquire)
(semaphore2 'acquire)

(semaphore2 'release)
(semaphore2 'release)
(semaphore2 'release)

;ex3.48

(define (make-sequence)
  (let ((sequence 0)
		(mutex (make-mutex)))
	(define (gen)
	  (mutex 'acquire)
	  (set! sequence (+ sequence 1))
	  (mutex 'release)
	  sequence)
	gen))

(define sequence (make-sequence))


(define (make-account-with-number balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
		(number (sequence)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'number) sequence)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))


(define (deposit account amountt)
  (let ((s (account 'serializer))
		(d (account 'deposit)))
	((s d) amountt)))



(define (serialized-exchange-with-number account1 account2)
  (let ((number1 (account1 'number1))
		(number2 (account2 'number2))
		(serializer1 (account1 'serializer))
		(serializer2 (account2 'serializer))
		)
	(cond ((= number1 number2) (error "ERROR"))
		  ((< number1 number2)
		   (serializer2 (serializer1 exchange))
			  account1 account2)
		  (else
		   (serializer1 (serializer2 exchange))
			  account1 account2))))
