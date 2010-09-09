;ex 3.12

(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

(define (my-last-pair x)
  (if (null? (cdr x))
	  x
	  (my-last-pair (cdr x))))


(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x)

(define w (my-append! x y))

(cdr x)

(define (make-cycle x)
  (set-cdr! (my-last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
  (define (loop x y)
	(if (null? x)
		y
		(let ((temp (cdr x)))
		  (set-cdr! x y)
		  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b) ))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)
;ex3.16
(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b) ))

(count-pairs x)
(count-pairs z1)
(count-pairs z2)


(define x3 (cons (cons 'a 'b) (cons 'a 'b)))
(count-pairs x3)

(define ab (cons 'a 'b))


(define x4 (cons ab (cons 'c ab)))
(count-pairs x4)

(define ab2 (cons ab ab))

(define x7 (cons ab2 ab2))
(count-pairs x7)

(define c (list c ))
(define cc (cons 'foo (cons 'foo c)))
(set-car! c cc)
(define x-inf c)
;ex3.17
;(count-pairs cc)

(define ab (cons 'a 'b))
(define abc (append (list ab) (list 'c)))
(define cab (list 'c))
(append! cab (list ab))
(display abc)
(display bc)
(memq ab abc)
(memq ab cab)



(define (count-pairs2 x)
  (let ((memo '()))
	(define (iter x)
;	  (display x)
;	  (newline)
	  (cond ((or (not (pair? x)) (memq x memo))
			 0)
			(else
			 (if (null? memo)
				 (set! memo (list x))
;				 (append! memo (list x)))
				 (set! memo (cons x memo)))
			 (+ (iter (car x))
				(iter (cdr x))
				1))))
	(iter x)))

(define (count-pairs2 x)
  (let ((memo '()))
	(define (iter x)
	  (cond ((or (not (pair? x)) (memq x memo))
			 0)
			(else
			 (set! memo (cons x memo))
			 (+ (iter (car x))
				(iter (cdr x))
				1))))
	(iter x)))


(count-pairs2 z1)
(count-pairs2 z2)

(count-pairs2 x3)
(count-pairs2 x4)
(count-pairs2 x7)
(count-pairs2 x-inf)


;ex3.18
(define (rotate-list? x)
  (define tracelist '())
  (define (rotate-list-aux? x)
    (cond ((null? x) #f)
          ((null? (cdr x)) #f)
          ((memq x tracelist) #t)
          (else
           (set! tracelist (cons x tracelist))
           (rotate-list-aux? (cdr x)))))
  (rotate-list-aux? x))

(rotate-list? (list 'a 'b 'c))
(rotate-list? z)
(define y (list a b))
(define z (cons c y))
(set-cdr! y z)
(rotate-list? y)

;ex3.19
(define (loop-check? x)
  (define (check x0 x1)
    (cond ((eq? x0 x1) #t)
          ((null? (cdr x1)) #f)
          ((null? (cddr x1)) #f)
          (else (check (cdr x0) (cddr x1)))))
  (if (and (pair? x) (pair? (cdr x)))
      (check (cdr x) (cddr x))
      #f))

(loop-check? (list 'a 'b 'c))
(loop-check? z)


(define (mycons1 x y)
  (define (dispatch m)
	(cond ((eq? m 'car) x)
		  ((eq? m 'cdr) y)
		  (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (mycar z) (z 'car))
(define (mycdr z) (z 'cdr))
(define x (mycons1 1 2))
(mycar x)
(mycdr x)

(define (mycons2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
	(cond ((eq? m 'car) x)
		  ((eq? m 'cdr) y)
		  ((eq? m 'set-car!) set-x!)
		  ((eq? m 'set-cdr!) set-y!)
		  (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (myset-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (myset-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


(define x (mycons2 1 2))
(mycar x)
(mycdr x)
(myset-car! x 3)
(myset-cdr! x 4)
(mycar x)
(mycdr x)

(define x (mycons2 1 2))
(define z (mycons2 x x))
(myset-car! (mycdr z) 17)
(mycar x)

;queue

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
	  (error "FRONT called with an empty queue" queue)
	  (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
	(cond ((empty-queue? queue)
		   (set-front-ptr! queue new-pair)
		   (set-rear-ptr! queue new-pair)
		   queue)
		  (else
		   (set-cdr! (rear-ptr queue) new-pair)
		   (set-rear-ptr! queue new-pair)
		   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
		 (error "DELETE! called with an empty queue" queue))
		(else
		 (set-front-ptr! queue (cdr (front-ptr queue)))
		 queue)))

(define q1 (make-queue))
(display q1)
(insert-queue! q1 'a)
(display q1)
(insert-queue! q1 'b)
(display q1)
(delete-queue! q1)
(display q1)
(delete-queue! q1)
(display q1)

;ex3.21
(define (print-queue queue)
  (display (front-ptr queue)))

(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;ex 3.22
(define (make-queue)
  (let ((front-ptr '())
		(rear-ptr '()))
	(define (set-front-ptr! item)
	  (set! front-ptr item)
	  )
	(define (set-rear-ptr! item) (set! rear-ptr item))
	(define (empty-queue?) 
		(null? front-ptr))
	(define (front-queue)
		(if (empty-queue?)
			(error "FRONT called with an empty queue")
			(car front-ptr)))
	(define (insert-queue! item)
		(let ((new-pair (cons item '())))
		  (cond ((empty-queue?)
				 (set-front-ptr! new-pair)
				 (set-rear-ptr! new-pair)
				 dispatch)
				(else
				 (set-cdr! rear-ptr new-pair)
				 (set-rear-ptr! new-pair)
				 dispatch))))
	(define (delete-queue!)
		(cond ((empty-queue?)
			   (error "DELETE! called with an empty queue"))
			  (else
			   (set-front-ptr! (cdr front-ptr))
			   dispatch)))
	(define (dispatch m)
	  (cond ((eq? m 'empty-queue?) (empty-queue?))
			  ((eq? m 'front-queue) (front-queue))
			  ((eq? m 'insert-queue!) insert-queue!)
			  ((eq? m 'delete-queue!) (delete-queue!))
			  (else (error "Undefined operation -- QUEUE" m))))
	dispatch)
  )

(define q2 (make-queue))
(q2 'empty-queue?)
((q2 'insert-queue!) 'a)
(q2 'front-queue)
(q2 'empty-queue?)
((q2 'insert-queue!) 'b)
(q2 'front-queue)
(q2 'delete-queue!)
(q2 'front-queue)
((q2 'insert-queue!) 'c)
(q2 'front-queue)
((q2 'delete-queue!) 'delete-queue!)
(q2 'empty-queue?)


;ex 3.23
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (make-item value)
  (cons value (cons '() '())))

(define (set-next-item! item next)
  (set-cdr! (cdr item) next))

(define (next-item item)
  (cddr item))

(define (prev-item item)
  (cadr item))

(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))

(define (value-of-item item)
  (car item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "empty queue")
      ((value-of-item (front-ptr queue)))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "empty queue")
      ((value-of-item (rear-ptr queue)))))

(define (rear-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-prev-item! new-item (rear-ptr queue))
           (set-next-item! (rear-ptr queue) new-item)
           (set-rear-ptr! queue new-item)
           queue))))

(define (front-insert-queue! queue value)
  (let ((new-item (make-item value)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-next-item! new-item (front-ptr queue))
           (set-front-ptr! queue new-item)
           queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty queue"))
        (else
         (set-front-ptr! queue (next-item (front-ptr queue)))
         queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "empty queue"))
        (else
         (set-rear-ptr! queue (prev-item (rear-ptr queue)))
         queue)))

(define (display-queue queue)
  (define (display-queue-internal q)
    (cond ((eq? q (rear-ptr queue))
           (display " ")
           (display (value-of-item q)))
          (else
           (begin (display " ")
                  (display (value-of-item q))
                  (display-queue-internal (next-item q))))))
  (if (empty-queue? queue)
      (display "empty queue\n")
      (begin
        (display "(")
        (display-queue-internal (front-ptr queue))
        (display ")\n"))))


(define q1 (make-queue))
(display-queue q1)
(rear-insert-queue! q1 'b)
(display-queue q1)

(front-insert-queue! q1 'a)
(display-queue q1)

(rear-insert-queue! q1 'c)
(display-queue q1)

(front-insert-queue! q1 'Z)
(display-queue q1)

(front-delete-queue! q1)
(display-queue q1)

(rear-delete-queue! q1)
(display-queue q1)

;3.3.3 table
;1-d
(define (lookup key table)
  (let ((record (myassoc key (cdr table))))
	(if record
		(cdr record)
		#f)))

(define (myassoc key records)
  (cond ((null? records) #f)
		((equal? key (caar records)) (car records))
		(else (myassoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (myassoc key (cdr table))))
	(if record
		(set-cdr! record value)
		(set-cdr! table
				  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define t1 (make-table))
(display t1)

(insert! 'c 3 t1)
(display t1)
(insert! 'b 2 t1)
(display t1)
(insert! 'a 1 t1)
(display t1)

(lookup 'a t1)
(lookup 'b t1)
(lookup 'c t1)
(lookup 'd t1)
;2-d

(define (lookup2 key-1 key-2 table)
  (let ((subtable (myassoc key-1 (cdr table))))
	(if subtable
		(let ((record (myassoc key-2 (cdr subtable))))
		  (if record
			  (cdr record)
			  #f))
		#f)))


(define (insert2! key-1 key-2 value table)
  (let ((subtable (myassoc key-1 (cdr table))))
	(if subtable
		(let ((record (myassoc key-2 (cdr subtable))))
		  (if record
			  (set-cdr! record value)
			  (set-cdr! subtable
						(cons (cons key-2 value)
							  (cdr subtable)))))
		(set-cdr! table
				  (cons (list key-1
							  (cons key-2 value))
						(cdr table)))))
  'ok)



(define t2 (make-table))
(display t2)
(insert2! 'math '+ 43 t2)
(display t2)
(insert2! 'math '- 45 t2)
(display t2)
(insert2! 'letters 'a 97 t2)
(display t2)
(lookup2 'math '+ t2)
(lookup2 'math '/ t2)
(lookup2 'letters 'a t2)
(lookup2 'letters 'z t2)


(define true #t)
(define false #f)
(define (make-table)
  (let ((local-table (list '*table*)))
	(define (lookup key-1 key-2)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if record
				  (cdr record)
				  false ))
			false)))
	(define (insert! key-1 key-2 value)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if record
				  (set-cdr! record value)
				  (set-cdr! subtable 
							(cons (cons key-2 value)
								  (cdr subtable)))))
			(set-cdr! local-table
					  (cons (list key-1
								  (cons key-2 value))
							(cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (error "Unknown operation -- TABLE" m))))
	dispatch))

;(define operation-table (make-table))
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc!))

;ex3.24

(define (make-table2 same-key?)
  (define (myassoc2 key records)
	(cond ((null? records) #f)
		  ((same-key? key (caar records)) (car records))
		  (else (myassoc2 key (cdr records)))))


  (let ((local-table (list '*table*)))
	(define (lookup key-1)
	  (let ((record (myassoc2 key-1 (cdr local-table))))
		(if record
			(cdr record)
			false )))
	(define (insert! key-1 value)
	  (let ((record (myassoc2 key-1 (cdr local-table))))
		(if record
			(set-cdr! record value)
			(set-cdr! local-table
					  (cons (cons key-1 value)
							(cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (error "Unknown operation -- TABLE" m))))
	dispatch))

(define (ambiguous-equal? k t)
  (cond ((= k t) #t)
		((= (+ k 1) t) #t)
		((= (- k 1) t) #t)
		(else #f)))

(define t324 (make-table2 ambiguous-equal?))

((t324 'insert-proc!) 1 10) 
((t324 'insert-proc!) 5 100) 
((t324 'lookup-proc) 1)
((t324 'lookup-proc) 2)

((t324 'insert-proc!) 6 1000)
((t324 'lookup-proc) 5)
((t324 'lookup-proc) 6)
((t324 'lookup-proc) 7)

;ex3.25
(define (lookup-m keys table)
  (cond ((null? keys) (error "LOOKUP - null keys"))
		((null? (cdr keys))
		 (let ((key (car keys)))
		   (let ((record (assoc key (cdr table))))
			 (if record
				 (cdr record)
				 #f))))
		(else
		 (let ((key (car keys))
			   (cdrkeys (cdr keys)))
		   (let ((subtable (assoc key (cdr table))))
			 (if subtable
				 (lookup-m cdrkeys subtable)
				 #f))))))
					

(define (insert-m! keys value table)
  (cond ((null? keys) (error "INSERT - null keys"))
		((null? (cdr keys))
		 (let ((key (car keys)))
		   (let ((record (assoc key (cdr table))))
			 (if record
				 (set-cdr! record value)
				 (set-cdr! table
						   (cons (cons key value) (cdr table)))))))
		(else
		 (let ((key (car keys))
			   (cdrkeys (cdr keys)))
		   (let ((subtable (assoc key (cdr table))))
			 (cond (subtable
					(insert-m! cdrkeys value subtable))
				   (else 
					(let ((newsubtable (cons key '())))
					  (set-cdr! table
								(cons newsubtable
									  (cdr table)))
					  (insert-m! cdrkeys value newsubtable)))))))))


(define (make-table)
  (list '*table*))


(define tm1 (make-table))
(insert-m! '(a) 1 tm1)
(display tm1)
(insert-m! '(b c) 3 tm1)
(display tm1)
(lookup-m '(a) tm1)
(lookup-m '(b c) tm1)
(lookup-m '(b) tm1)


;ex3.26

(define (key tree) (car tree))
;(define (set-key! tree new-key) (set-car! tree new-key))
(define (value tree) (cadr tree))
(define (set-value! tree new-value) (set-car! (cdr tree) new-value))
(define (left-branch tree) (caddr tree))
(define (set-left-branch! tree branch) (set-car! (cddr tree) branch))
(define (right-branch tree) (cadddr tree))
(define (set-right-branch! tree branch) (set-car! (cdddr tree) branch))
(define (make-subtree key value left right)
  (list key value left right))
(define (make-init-tree) (list '*tree*))

(define (lookup-t given-key tree)
  (define (lookup-t-sub subtree)
	(if (null? subtree) #f
		(let ((k (key subtree)))
		  (cond ((= k given-key) (value subtree))
				((< k given-key)
				 (lookup-t-sub (right-branch subtree)))
				(else
				 (lookup-t-sub (left-branch subtree)))))))
  (lookup-t-sub (cdr tree)))
	  
  

(define (insert-t! given-key given-value tree)
  (define (insert-t-sub! subtree)
	(if (null? subtree)
		(make-subtree given-key given-value '() '())
		(let ((k (key subtree)))
		  (cond ((= k given-key) (set-value! subtree given-value))
			  ((< k given-key)
			   (if (null? (right-branch subtree))
				   (set-right-branch! subtree
									  (make-subtree given-key given-value '() '()))
				   (insert-t-sub! (right-branch subtree))))
			  (else
			   (if (null? (left-branch subtree))
				   (set-left-branch! subtree
									  (make-subtree given-key given-value '() '()))
				   (insert-t-sub! (left-branch subtree))))))))
  (if (null? (cdr tree))
	  (set-cdr! tree 
				(make-subtree given-key given-value '() '()))
	  (insert-t-sub! (cdr tree))))


(define tree1 (make-init-tree))
(display tree1)
(lookup-t 10 tree1)
(insert-t! 10 'a tree1)
(display tree1)
(lookup-t 10 tree1)
(insert-t! 0 'b tree1)
(insert-t! 2 'c tree1)
(insert-t! 20 'd tree1)
(insert-t! 100 'e tree1)
(display tree1)
(lookup-t 10 tree1)
(lookup-t 0 tree1)
(lookup-t 2 tree1)
(lookup-t 20 tree1)
(lookup-t 100 tree1)
(lookup-t 1001 tree1)

(insert-t! 100 'f tree1)
(lookup-t 100 tree1)

(insert-t! 2 'g tree1)
(lookup-t 2 tree1)

(display tree1)

;ex3.27

(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (myassoc key (cdr table))))
	(if record
		(cdr record)
		#f)))

(define (myassoc key records)
  (cond ((null? records) #f)
		((equal? key (caar records)) (car records))
		(else (myassoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (myassoc key (cdr table))))
	(if record
		(set-cdr! record value)
		(set-cdr! table
				  (cons (cons key value) (cdr table)))))
  'ok)


(define (memoize f)
  (let ((table (make-table)))
	(lambda (x)
	  (let ((previously-computed-result (lookup x table)))
		(or previously-computed-result
			(let ((result (f x)))
			  (insert! x result table)
			  result))))))


(define memo-fib
  (memoize (lambda (n)
			 (cond ((= n 0) 0)
				   ((= n 1) 1)
				   (else (+ (memo-fib (- n 1))
							(memo-fib (- n 2))))))))

(memo-fib 5)

(memo-fib 50)

;(define memo-fib2 (memoize fib))
;(memo-fib2 50)
