(define (square x)  (* x x))

(define nil `())

(define (fib x)
  (fib-iter 1 0 x))
(define (fib-iter a b count)
  (if (= count 0) b
	  (fib-iter (+ a b) a (- count 1))))
	  

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
		((not (pair? tree))
		 (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 (list 2 3)))

(fib 10)

(define (even-fibs n)
  (define (next k)
	(if (> k n)
		nil
		(let ((f (fib k)))
		  (if (even? f)  (cons f (next (+ k 1)))
			  (next (+ k 1))))))
  (next 0))

(even-fibs 10)

(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else  (filter predicate (cdr sequence)))))


(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))


(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high) nil
	  (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


(define (sum-odd-squares tree)
  (accumulate + 
			  0
			  (map square
				   (filter odd?
						   (enumerate-tree tree)))))

(sum-odd-squares  (list 1 (list 2 (list 3 4)) 5))


(define (even-fibs n)
  (accumulate cons
			  nil
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))

(even-fibs 10)
(filter even?
		(map fib
			 (enumerate-interval 0 10)))


(define (list-fib-squares n)
  (accumulate cons
			  nil
			  (map square
				   (map fib
						(enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
			  1
			  (map square
				   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))


;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))


(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2
			  seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y) )
			  0
			  sequence))

(length (list 1 2 3))
(length (list 2 3 4 5))
   

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
				(+ this-coeff (* x higher-terms))
				)
			  0
			  coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)

(define (count-leaves t)
  (accumulate + 0
			  (map
			   (lambda (x)
				 (cond ((null? x) 0)
					   ((not (pair? x)) 1)
					   (else 
						(count-leaves x)))) t)))


(count-leaves x)
   


(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)

(define m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define m2 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define m3 (list (list 1 2 ) (list 4 5)))

(define m4 (list (list 1 2 3) (list 4 5 6)))
(define m5 (list (list 1 2) (list 3 4) (list 5 6)))

(define v1 (list 1 2))
(define v2 (list 3 4))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product v1 v2)

(define (matrix-*-vector m v)
  (map (lambda (x) 	(dot-product x v)
			  )
	   m))

(matrix-*-vector m3 v2)


(define (transpose mat)
  (accumulate-n  cons nil  mat))

(transpose m3)
  



(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
		(map (lambda (x)
			   (matrix-*-vector cols x)
			   ) m)))

(matrix-*-matrix m3 m3)
(matrix-*-matrix m4 m5)
(matrix-*-matrix m5 m4)

(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest) result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))


(define (reverse sequence)
  (fold-right (lambda (x y) 
				(append y (list x))
					  )
			  nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y)
			   (cons y x)
			   )
			 nil sequence))
(reverse (list 1 2 3))


