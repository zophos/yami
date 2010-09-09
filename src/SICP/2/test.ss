(define (unique-pairs n)
  (flatmap 
   (lambda (i)
	 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
					 (enumerate-interval 1 n)))
(unique-pairs 6)


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

(unique-tuples 6 4)
			  
