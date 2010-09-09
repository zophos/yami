(define nil '())
(cons  1 (cons 2 nil))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)

(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length2 odds)

(append squares odds)
(append odds squares)
(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))
(last-pair odds)
(last-pair (list 23 72 149 34))

(define (reverse l)
  (define (reverse-iter a b)
    (if (null? a) b
		(reverse-iter (cdr a) (cons (car a) b))))
  (reverse-iter l '()))

(reverse squares)

(define us-coins (list 50 25 10 5 1))
(define us-coins2 (list 1 50 25 10 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define except-first-denomination cdr)

(define first-denomination car)

(define no-more? null?)


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)
(cc 100 us-coins2)
(cc 10 uk-coins)
;(cc 100 uk-coins)

(define (same-parity x . y)
    (define (same-parity-iter parity l r)
      (cond ((null? l) (reverse r))
            ((= parity (remainder (car l) 2))
             (same-parity-iter parity (cdr l) (cons (car l) r)))
            (else 
             (same-parity-iter parity (cdr l) r))))
    (same-parity-iter (remainder x 2) y (list x)))

(define (same-parity . w)
  (let ((first-even? (even? (car w))))
    (define (same-parity? item)
      (let ((this-even? (even? item)))
        (or (and this-even? first-even?)
            (and (not this-even?) (not first-even?)))))
    (define (iter items result)
      (cond ((null? items) result)
            ((same-parity? (car items))
             (cons (car items) (iter (cdr items) result)))
            (else
             (iter (cdr items) result))))
    (iter w '())))

(define (same-parity x . y)
  (let ((x-parity (remainder x 2)))
	(define (same-parity? item)
	  (= x-parity (remainder item 2)))
	(define (iter items)
	  (cond ((null? items) '())
			((same-parity? (car items))
			 (cons (car items) (iter (cdr items))))
			(else 
			 (iter (cdr items)))))
	(cons x (iter y))))

	  

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))

(define (mymap proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (mymap proc (cdr items)))))

(mymap abs (list -10 2.5 -11.6 17))
(mymap (lambda (x) (* x x))
       (list 1 2 3 4))

(define (scale-list2 items factor)
  (mymap (lambda (x) (* x factor))
         items))

(scale-list2 (list 1 2 3 4 5) 10)
(define (square-list items)
  (if (null? items)
      '()
      (cons ((lambda (x) (* x x)) (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list2 items) 
  (map (lambda (x) (* x x)) items))
(square-list2 (list 1 2 3 4))

(define (square x) (* x x))

(define (square-list-r items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (square (car things))
                                 answer))))
  (iter items '()))

(square-list-r (list 1 2 3 4))

(define (square-list-b items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer (square (car things))
                                 ))))
  (iter items '()))

(square-list-b (list 1 2 3 4))


(define (square-list-bl items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer (list (square (car things)))
                                 ))))
  (iter items '()))

(square-list-bl (list 1 2 3 4))


(define (square-list-ba items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))
                                   ))))
  (iter items '()))

(square-list-ba (list 1 2 3 4))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

(define (my-for-each proc l)
  (if (null? l) '()
      (cons (proc (car l)) (my-for-each proc (cdr l)))))


(my-for-each (lambda (x) (newline) (display x)) (list 57 321 88))

(define (my-for-each proc l)
  (if (null? l) '()
      (cons (proc (car l)) (my-for-each proc (cdr l)))))

(define (my-for-each2 proc l)
  (if (null? l) '()
      (begin (proc (car l)) (my-for-each2 proc (cdr l)))))

(my-for-each2 (lambda (x) (newline) (display x)) (list 57 321 88))


(define (my-for-each3 proc l)
  (cond ((null? l) '())
        (else (proc (car l)) (my-for-each3 proc (cdr l)))))

(my-for-each3 (lambda (x) (newline) (display x)) (list 57 321 88))
