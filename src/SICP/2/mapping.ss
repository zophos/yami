(define (square x)  (* x x))

(define nil `())

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high) nil
	  (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(accumulate append
			nil
			(map 
			 (lambda (i)
			   (map (lambda (j) (list i j))
					(enumerate-interval 1 (- i 1))))
			 (enumerate-interval 1 10)))

(map 
 (lambda (i)
   (map (lambda (j) (list i j))
		(enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 10))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else  (filter predicate (cdr sequence)))))


(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap
				(lambda (i)
					   (map (lambda (j) (list i j))
							(enumerate-interval 1 (- i 1))))
					 (enumerate-interval 1 n)))))

(prime-sum-pairs 6)
(for-each (lambda (x) (display (car x)) (display " "))
	 (prime-sum-pairs 6))



(prime-sum-pairs 6)

(prime-sum-pairs 10)

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n  cons nil  mat))

(for-each
 (lambda (x) (newline)(display x))
 (transpose
  (prime-sum-pairs 6)))

(define (permutations s)
  (if (null? s) (list nil)
	  (flatmap (lambda (x)
				 (map (lambda (p)  (cons x p))
					  (permutations (remove x s))))
			   s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
		  seq))

(permutations (list 1 2 3))
(permutations (list 3))

;p2.40

(define (unique-pairs n)
  (flatmap 
   (lambda (i)
	 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
					 (enumerate-interval 1 n)))
(unique-pairs 6)

(define (prime-sum-pairs2 n)
  (map make-pair-sum
	   (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs2 6)


;p2.41
(define (unique-3-tuples n)
  (flatmap 
   (lambda (i)
	 (flatmap (lambda (j) 
			(map (lambda (k) 
				   (list i j k))
				  (enumerate-interval 1 (- j 1))))
			(enumerate-interval 1 (- i 1))))
	 (enumerate-interval 1 n)))


(unique-3-tuples 5)


(define (filter-sum-3-tuples n s)
  (filter (lambda (l) (= s 
						 (accumulate + 0 l)))
		  (unique-3-tuples n)))


(filter-sum-3-tuples 7 12)
(filter-sum-3-tuples 6 9)
(filter-sum-3-tuples 6 10)
(filter-sum-3-tuples 6 11)
(filter-sum-3-tuples 6 12)
(filter-sum-3-tuples 6 13)

(define (unique-tuples n d)
  (if (= d 0) (list '())
	  (filter (lambda (x)
				(or (> 2 (length x))
					(> (car x) (cadr x))))
			  (flatmap
			   (lambda (i)
				 (map (lambda (j) (cons j i))
					  (enumerate-interval d n)))
			   (unique-tuples (- n 1) (- d 1))))))

(unique-tuples 5 3)

(define (filter-sum-3-tuples2 n s)
  (filter (lambda (l) (= s 
						 (accumulate + 0 l)))
		  (unique-tuples n 3)))

(filter-sum-3-tuples 7 10)
(filter-sum-3-tuples 6 9)
(filter-sum-3-tuples 6 10)
(filter-sum-3-tuples 6 11)
(filter-sum-3-tuples 6 12)
(filter-sum-3-tuples 6 13)

(reverse (filter-sum-3-tuples2 7 10))
(reverse (filter-sum-3-tuples2 6 9))
(reverse (filter-sum-3-tuples2 6 10))
(reverse (filter-sum-3-tuples2 6 11))
(reverse (filter-sum-3-tuples2 6 12))
(reverse (filter-sum-3-tuples2 6 13))


(reverse (filter-sum-3-tuples2 7 12))


;p2.42

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))


(define (enumerate-queen-move k position)
  (filter (lambda (pos)
			(and (> (car pos) 0) (> (cdr pos) 0)))
		  (let ((x (car position))
				(y (cdr position)))
			(append
			 (map
			  (lambda (n) (cons x n))
			  (enumerate-interval 1 (- k 1)))
			 (flatmap
			  (lambda (m) (list (cons (+ x (- y m)) m)
								(cons (- x (- y m)) m)))
			  (enumerate-interval 1 (- k 1)))
			 ))))
				
;(enumerate-queen-move 5 (cons 2 5))

;(member (cons 1 2) (list (cons 1 2) (cons 2 3)))
(define contains? member)
  
(define (eq-position? x y)
  (and (= (car x) (car y)) (= (cdr x) (cdr y))))

(eq-position? (cons 1 2) (cons 1 2))
(eq-position? (cons 1 2) (cons 1 3))

(define (safe? k positions)
  (let ((k-queen (car positions))
		(rest-of-queens (cdr positions))
		)
	(null?
	 (filter
	  (lambda (x)
		(contains? x rest-of-queens))
	  (enumerate-queen-move k k-queen)))))


(safe? 2 (list (cons 1 2) (cons 3 1)))
(safe? 3 (list (cons 3 3) (cons 1 2) (cons 3 1)))
(safe? 3 (list (cons 2 3) (cons 1 2) (cons 1 3)))

(define (queens board-size)
  (define (queens-cols k)
	(if (= k 0)
		(list empty-board)
		(filter 
		 (lambda (positions) (safe? k positions))
		 (flatmap
		  (lambda (rest-of-queens)
			(map (lambda (new-row)
				   (adjoin-position new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		  (queens-cols (- k 1))))))
  (queens-cols board-size))

;(queens 8)

;(length (queens 9))
(map (lambda (x) (length (queens x))) (enumerate-interval 1 8 ))


;p2.43

(define (queens2 board-size)
  (queens-cols2 board-size board-size))

(define (queens-cols2 k board-size)
  (if (= k 0)
	  (list empty-board)
	  (filter 
	   (lambda (positions) (safe? k positions))
	   (flatmap
		(lambda (rest-of-queens)
		  (map (lambda (new-row)
				 (adjoin-position new-row k rest-of-queens))
			   (enumerate-interval 1 board-size)))
		(queens-cols2 (- k 1) board-size )))))


;(require (lib "trace.ss"))
;(trace queens-cols2)

(length (queens2 8))
;(queens2 3)

(define (queens-bad board-size)
  (queens-bad-col board-size board-size))

(define (queens-bad-col k board-size)
  (if (= k 0)
	  (list empty-board)
	  (filter 
	   (lambda (positions) (safe? k positions))
	   (flatmap
		(lambda (new-row)
		  (map (lambda (rest-of-queens)
				 (adjoin-position new-row k rest-of-queens))
			   (queens-bad-col (- k 1) board-size)))
		(enumerate-interval 1 board-size)))))


;(trace queens-bad-col)

(queens-bad 3)
 
;r+r^2+r^3....+r^n
;=r(1-r^n)/(1-r)
;n+n^2+n^3....+n^n
;=n(1-n^n)/(1-n)
;(n^n+1-n)/(n-1)

; (n^n-1)/(n-1)=1+n+..+n^(n-1)
; n=3のとき 
; n=8のとき (16777216-1)/7=2396745



