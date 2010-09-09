(define true #t)
(define false #f)

(define (element-of-set? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(1 2 3))
(element-of-set? 4 '(1 2 3))

(define (adjoin-set x set)
  (if (element-of-set? x set)
	  set
	  (cons x set)))

(adjoin-set 1 '(1 2 3))
(adjoin-set 4 '(1 2 3))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) set2)))
		(else  (intersection-set (cdr set1) set2))))

(intersection-set '(1) '(1 2 3))
(intersection-set '(1 3) '(1 2 3))
(intersection-set '(4) '(1 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else
		 (cons (car set1)
			   (union-set (cdr set1) set2)))))


(union-set '(1) '(1 2 3))
(union-set '(1 3) '(1 2 3))
(union-set '(4) '(1 2 3))

;(define adjoin-set cons)
;(define union-set append)

(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
	  (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
			   (cons x1 (intersection-set (cdr set1)
										  (cdr set2))))
			  ((< x1 x2)
			   (intersection-set (cdr set1) set2))
			  ((< x2 x1)
			   (intersection-set set1 (cdr set2)))))))

(element-of-set? 1 '(1 2 3))
(element-of-set? 4 '(1 2 3))


(intersection-set '(1) '(1 2 3))
(intersection-set '(1 3) '(1 2 3))
(intersection-set '(4) '(1 2 3))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '(1 2 3))
(adjoin-set 4 '(1 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else
		 (let ((x1 (car set1)) (x2 (car set2)))
		   (cond ((= x1 x2)
				  (cons x1 (union-set (cdr set1)
									  (cdr set2))))
				 ((< x1 x2)
				  (cons x1 (union-set (cdr set1) set2)))
				 ((< x2 x1)
				  (cons x2 (union-set set1 (cdr set2)))))))))


(union-set '(1) '(1 2 3))
(union-set '(1 3) '(1 2 3))
(union-set '(4) '(1 2 3))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		((> x (entry set))
		 (element-of-set? x (right-branch set)))))

(define test-tree
  (make-tree 7 (make-tree 3 '(1 () ()) '(5 () ())) (make-tree 9 '() '(11 () ()))))
(define test-tree2
  '( 3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define test-tree3
  '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


(entry test-tree)
(left-branch test-tree)

(entry 
 (left-branch
  (left-branch test-tree)))

(element-of-set? 1 test-tree)
(element-of-set? 2 test-tree)

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
					(adjoin-set x (left-branch set))
					(right-branch set)))
		((> x(entry set))
		 (make-tree (entry set)
					(left-branch set)
					(adjoin-set x (right-branch set))))))
		 


(adjoin-set 1 test-tree)
(adjoin-set 4 test-tree)


(define (tree->list-1 tree)
  (if (null? tree) '()
	  (append (tree->list-1 (left-branch tree))
			  (cons (entry tree)
					(tree->list-1 (right-branch tree))))))
	  
(tree->list-1 (adjoin-set 4 test-tree))
(tree->list-1 test-tree2)
(tree->list-1 test-tree3)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
		result-list
		(copy-to-list (left-branch tree)
					  (cons (entry tree)
							(copy-to-list (right-branch tree)
										  result-list)))))
  (copy-to-list tree '()))

(tree->list-2 (adjoin-set 4 test-tree))
(tree->list-2 test-tree2)
(tree->list-2 test-tree3)

(quotient (- 10 1) 2)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))


(define (partial-tree elts n)
  (if (= n 0)
	  (cons '() elts)
	  (let ((left-size (quotient (- n 1) 2)))
		(let ((left-result (partial-tree elts left-size)))
		  (let ((left-tree (car left-result))
				(non-left-elts (cdr left-result))
				(right-size (- n (+ left-size 1))))
			(let ((this-entry (car non-left-elts))
				  (right-result (partial-tree (cdr non-left-elts)
											  right-size)))
			  (let ((right-tree (car right-result))
					(remaining-elts (cdr right-result)))
				(cons (make-tree this-entry left-tree right-tree)
					  remaining-elts))))))))

(list->tree '(1 2 3 4 5 6))
(list->tree '(1 3 5 7 9 11))

(define tree->list tree->list-2)
			
(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1) (tree->list set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1) (tree->list set2))))

          
(define (union-list l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else
         (let ((h1 (car l1))
               (h2 (car l2)))
           (cond ((= h1 h2) (cons h1 (union-list (cdr l1) (cdr l2))))
                 ((< h1 h2) (cons h1 (union-list (cdr l1) l2)))
                 ((< h2 h1) (cons h2 (union-list l1 (cdr l2)))))))))

(define (intersection-list l1 l2)
  (cond ((or (null? l1) (null? l2)) '())
        (else 
         (let ((h1 (car l1))
               (h2 (car l2)))
           (cond ((= h1 h2) (cons h1 (intersection-list (cdr l1) (cdr l2))))
                 ((< h1 h2) (intersection-list (cdr l1) l2))
                 ((< h2 h1) (intersection-list l1 (cdr l2))))))))

							  
					
				 
(define test-tree4 (list->tree '(1 2 3 4 5 6)))
(define test-tree5 (list->tree '(1 3 5 7 9 11)))

(union-set test-tree4 test-tree5)
(intersection-set test-tree4 test-tree5)

(define (key x) x)
		   
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
		((equal? given-key (key (car set-of-records)))
		 (car set-of-records))
		(else (lookup given-key (cdr set-of-records)))))




(lookup 1 '(4 3 2 1))		

(define (lookup2 given-key set-of-records)
    (if (null? set-of-records) false
		(let ((x (entry set-of-records)))
		  (cond ((= x given-key) (key x))
				((< x given-key)
				 (lookup2 given-key (right-branch set-of-records)))
				((> x given-key)
				 (lookup2 given-key (left-branch set-of-records)))))))

;(require (lib "trace.ss"))
;(trace lookup2)

(lookup2 1 test-tree5)
(lookup2 11 test-tree5)
(lookup2 2 test-tree5)
(lookup2 6 test-tree5)



