(define (square x) (* x x))
(define pi (atan  0 -1.0))
(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
                ((predicate (car sequence))
                 (cons (car sequence)
                           (filter predicate (cdr sequence))))
                (else  (filter predicate (cdr sequence)))))


(define (type-tag datum)
  (cond ((pair? datum)
		 (car datum))
		((number? datum)
		 'scheme-number)
		(else
		 (error  "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
	  contents
	  (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum)
		 (cdr datum))
		((number? datum)
		 datum)
		(else
		 (error  "Bad tagged datum -- TYPE-TAG" datum))))

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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (error "No method for these types -- APPLY-GENERIC"
				 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))

(define (install-scheme-number-package)
  (define (tag x)

	(attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (tag (/ x y))))

  (put 'negate '(scheme-number)
	   (lambda (x) (tag (- x))))

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get' make 'scheme-number) n))

(install-scheme-number-package)

(define s1 (make-scheme-number 1))
(define s2 (make-scheme-number 2))

(add s1 s2)
(sub s1 s2)
(mul s1 s2)
(div s1 s2)

(define (gcd a b)
  (if (= b 0)
	  a
	  (gcd b (remainder a b))))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))

  (define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))

  (define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
			  (* (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (* (numer x) (denom y))
			  (* (denom x) (numer y))))

  (define (tag x)
	(attach-tag 'rational x))
  
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define r1 (make-rational 1 2))
(define r2 (make-rational 3 2))

(add r1 r2)
(sub r1 r2)
(mul r1 r2)
(div r1 r2)

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
	(sqrt (+ (square (real-part z))
			 (square (imag-part z)))))
  (define (angle z)
	(atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))
  
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  'done )

(install-rectangular-package)
	
(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))
  (define (real-part z)
	(* (magnitude z) (cos (angle z))))
  (define (imag-part z)
	(* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)))
		  (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  'done )
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



(define (install-complex-package)
  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
					   
  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))
					   
  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	   (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
	   (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
	   (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
	   (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
	   (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)

(define c1 (make-complex-from-real-imag 1 0))
(define c2 (make-complex-from-mag-ang 1 (/ pi 2)))


(add c1 c2)
(sub c1 c2)
(mul c1 c2)
(div c1 c2)

(magnitude c1)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(magnitude c1)

(add 1 2)
(add r1 r2)
(add c1 c2)

(define (install-equ?-package)
  (put 'equ? '(scheme-number scheme-number)
	   (lambda (x y)
		 (= x y)))

  (put 'equ? '(rational rational)
	   (lambda (x y)
		 (and
		  (= (car x) (car y))
		  (= (cdr x) (cdr y)))))

  (put 'equ? '(complex complex)
	   (lambda (x y)
		 (and
		  (= (real-part x) (real-part y))
		  (= (imag-part x) (imag-part y)))))
	   
  (put 'equ? '(scheme-number rational)
	   (lambda (x y)
		 (and
		  (= x (car y))
		  (= 1 (cdr y)))))


  (put 'equ? '(rational scheme-number)
	   (lambda (y x)
		 (and
		  (= x (car y))
		  (= 1 (cdr y)))))


  (put 'equ? '(scheme-number complex)
	   (lambda (x y)
		 (and
		  (= x (real-part y))
		  (= 0 (imag-part y)))))

  (put 'equ? '(complex scheme-number)
	   (lambda (y x)
		 (and
		  (= x (real-part y))
		  (= 0 (imag-part y)))))
  
  (put 'equ? '(rational complex)
	   (lambda (x y)
		 (and
		  (= (/ (car x) (cdr x)) (real-part y))
		  (= 0 (imag-part y)))))

  (put 'equ? '(complex rational)
	   (lambda (y x)
		 (and
		  (= (/ (car x) (cdr x)) (real-part y))
		  (= 0 (imag-part y)))))
  'done)




(define (equ? x y)
  (apply-generic 'equ? x y))

(install-equ?-package)

(equ? 1 1)
(equ? s1 1)
(equ? 1 2)

(equ? r1 r2)
(equ? r1 r1)

(equ? c1 c2)
(equ? c2 c2)

(equ? 2 r2)

(define r3 (make-rational 4 2))
(equ? 2 r3)
(equ? r3 2)

(equ? 1 c1)
(equ? c1 1)
(equ? 1 c2)

(define c3 (make-complex-from-mag-ang 2 0))

(equ? r3 c3)
(equ? c3 r3)
(equ? r3 c2)
(equ? c2 r3)


(define (=zero? x)
  (equ? 0 x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (install-polynomial-package)
  (define (make-poly variable term-list)
	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))


  (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (add-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- ADD-POLY"
			   (list p1 p2))))


  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (mul-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- MUL-POLY"
			   (list p1 p2))))


  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
	   (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
		((empty-termlist? L2) L1)
		(else
		 (let ((t1 (first-term L1)) (t2 (first-term L2)))
		   (cond ((> (order t1) (order t2))
				  (adjoin-term
				   t1 (add-terms (rest-terms L1) L2)))
				 ((< (order t1) (order t2))
				  (adjoin-term
				   t2 (add-terms L1 (rest-terms L2))))
				 (else
				  (adjoin-term
				   (make-term (order t1)
							  (add (coeff t1) (coeff t2)))
				   (add-terms (rest-terms L1)
							  (rest-terms L2)))))))))
				 
  
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
	  (the-empty-termlist)
	  (add-terms (mul-term-by-all-terms (first-term L1) L2)
				 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
	  (the-empty-termlist)
	  (let ((t2 (first-term L)))
		(adjoin-term
		 (make-term (+ (order t1) (order t2))
					(mul (coeff t1) (coeff t2)))
		 (mul-term-by-all-terms t1 (rest-terms L))))))


(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
	  term-list
	  (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? termlist) (null? termlist))

(define (make-term order coeff)
  (list order coeff))

(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(make-polynomial 'x 
				 '((1 1))
				 )
(make-polynomial 'x '((100 1) (2 2) (0 1)))



(add 
 (make-polynomial 'x 
				  '((1 1)))
 (make-polynomial 'x 
				  '((1 1)))
 )
(mul
 (make-polynomial 'x 
				  '((1 1)))
 (make-polynomial 'x 
				  '((1 1)))
 )

;ex2.87
(define (install-polynomial-package)
  (define (make-poly variable term-list)
	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))


  (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (add-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- ADD-POLY"
			   (list p1 p2))))


  ;; (define (sub-poly p1 p2)
  ;; 	(if (same-variable? (variable p1) (variable p2))
  ;; 		(make-poly (variable p1)
  ;; 				   (add-terms (term-list p1)
  ;; 							  (map (lambda (x)
  ;; 									 ; 手抜き
  ;; 									 ; 多項式係数を考慮していない
  ;; 									 (make-term (order x) (- (coeff x))))
  ;; 								   (term-list p2))))
  ;; 		(error "Polys not in same var -- SUB-POLY"
  ;; 			   (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (negate-term L) (map 
 						   (lambda (x) (make-term (order x) 
												  (negate (coeff x)))) L))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (mul-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- MUL-POLY"
			   (list p1 p2))))

  (define (div-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (div-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- DIV-POLY"
			   (list p1 p2))))


  (define (=zero-poly? p)
	(let ((terms (term-list p)))
	  (= (length terms)
		 (length (filter (lambda (x) (=zero? x))
						 (map coeff terms))))))



  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero-poly? '(polynomial)
	   =zero-poly?)

  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial polynomial)
	   (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'div '(polynomial polynomial)
	   (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'negate '(polynomial)
	 (lambda (p) (tag (negate-poly p))))

  (put 'make 'polynomial
	   (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)



(length (cddr (make-polynomial 'x 
				  '((1 1)))))

(length (filter (lambda (x) (= 0 x))
				(map cadr 
					 (cddr
					  (make-polynomial 'x 
									   '((1 1))))))
				)

(length (filter (lambda (x) (= 0 x))
				(map cadr 
					 (cddr
					  (make-polynomial 'x 
									   '((1 0))))))
				)


(define (=zero? x)
  (if (eq? (type-tag x) 'polynomial)
	  ((get '=zero-poly? '(polynomial)) (contents x))
	  (equ? 0 x)))

(=zero? (make-polynomial 'x 
				  '((1 1))))

(=zero? (make-polynomial 'x 
				  '((1 0))))


(=zero? (make-polynomial 'x 
				  '()))

(=zero? (make-polynomial 'x '((5 0) (4 0))))

(define c (make-polynomial 'y '((5 0) (4 0))))

(=zero? (make-polynomial 'x (list (list 5 c) (list 4 0))))


(sub 
 (make-polynomial 'x 
				  '((1 2)))
 (make-polynomial 'x 
				  '((1 1)))
 )

(sub 
 (sub  (make-polynomial 'x '((1 1)))
	   (make-polynomial 'x '((1 1))))
 (make-polynomial 'x 
				  '((1 1)))
)


(define (div-terms L1 L2)
  (if (empty-termlist? L1)
	  (list (the-empty-termlist) (the-empty-termlist))
	  (let ((t1 (first-term L1))
			(t2 (first-term L2)))
		(if (> (order t2) (order t1))
			(list (the-empty-termlist) L1)
			(let ((new-c (div (coeff t1) (coeff t2)))
				  (new-o (- (order t1) (order t2))))
			  (let ((rest-of-result
					 (div-terms
					  (add-terms L1
								 (mul-terms L2 (list (make-term new-o (negate new-c)))))
					  L2)
					 ))
				(cons (adjoin-term (make-term new-o new-c) (car rest-of-result))
							(cdr rest-of-result))
				))))))



(div (make-polynomial 'x '((1 1)))
		   (make-polynomial 'x '((1 1))))

(div (make-polynomial 'x '((5 1) (0 -1)))
		   (make-polynomial 'x '((2 1)(0 -1))))
						 
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
;  (define (make-rat n d)
;	(let ((g (gcd n d)))
;	  (cons (/ n g) (/ d g))))
  (define (make-rat n d)
	  (cons n d))
  (define (add-rat x y)
	(make-rat (add (mul (numer x) (denom y))
				   (mul (numer y) (denom x)))
			  (mul (denom x) (denom y))))

  (define (sub-rat x y)
	(make-rat (sub (mul (numer x) (denom y))
				   (mul (numer y) (denom x)))
			  (mul (denom x) (denom y))))

  (define (mul-rat x y)
	(make-rat (mul (numer x) (numer y))
			  (mul (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (mul (numer x) (denom y))
			  (mul (denom x) (numer y))))

  (define (tag x)
	(attach-tag 'rational x))
  
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

;ex 2.93

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rt (make-rational p2 p1))
(add rt rt)

(define (gcd-terms a b)
  (if (empty-termlist? b)
	  a
	  (gcd-terms b (remainder-terms a b))))

(define (remainder-terms a b)
	(cadr (div-terms a b)))

(div-terms '((3 1)(0 1)) '((2 1)(0 1)))
(remainder-terms '((3 1)(0 1)) '((2 1)(0 1)))

(gcd-terms '((3 1)(0 1)) '((2 1)(0 1)))
(gcd-terms '((5 2) (3 2) (2 2) (0 2)) '((4 1) (2 2) (0 1)))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))


  (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (add-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- ADD-POLY"
			   (list p1 p2))))


  ;; (define (sub-poly p1 p2)
  ;; 	(if (same-variable? (variable p1) (variable p2))
  ;; 		(make-poly (variable p1)
  ;; 				   (add-terms (term-list p1)
  ;; 							  (map (lambda (x)
  ;; 									 ; 手抜き
  ;; 									 ; 多項式係数を考慮していない
  ;; 									 (make-term (order x) (- (coeff x))))
  ;; 								   (term-list p2))))
  ;; 		(error "Polys not in same var -- SUB-POLY"
  ;; 			   (list p1 p2))))

  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (negate-term L) (map 
						   (lambda (x) (make-term (order x) 
												  (negate (coeff x)))) L))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))


  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (mul-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- MUL-POLY"
			   (list p1 p2))))

  (define (div-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (div-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- DIV-POLY"
			   (list p1 p2))))


  (define (=zero-poly? p)
	(let ((terms (term-list p)))
	  (= (length terms)
		 (length (filter (lambda (x) (=zero? x))
						 (map coeff terms))))))

  (define (gcd-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (gcd-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- GCD-POLY"
			   (list p1 p2))))


  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero-poly? '(polynomial)
	   =zero-poly?)

  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial polynomial)
	   (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'div '(polynomial polynomial)
	   (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'gcd-poly '(polynomial polynomial)
	   (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'negate '(polynomial)
	 (lambda (p) (tag (negate-poly p))))

  (put 'make 'polynomial
	   (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (greatest-common-divisor p1 p2)
  (let ((t1 (type-tag p1))
		(t2 (type-tag p2)))
	(cond ((and (eq? t1 'polynomial) (eq? t2 'polynomial))
		   (gcd-poly p1 p2))
		  ((and (eq? t1 'scheme-number) (eq? t2 'scheme-number))
		   (gcd p1 p2))
		  (else 
		   (error "greatest-common-divisor Error" (list p1 p2))))))

(define (gcd-poly x y) (apply-generic 'gcd-poly x y))
(greatest-common-divisor 8 12)

(define p1 (make-polynomial 'x '((4 1)(3 -1)(2 -2)(1 2))))
(define p2 (make-polynomial 'x '((3 1)(1 -1))))

(greatest-common-divisor p1 p2)

(div-terms '((4 1)(3 -1)(2 -2)(1 2)) '((2 -1)(1 1)))
(div-terms '((3 1)(1 -1)) '((2 -1)(1 1)))

;ex2.95

(define p1 (make-polynomial 'x '((2 1)(1 -2)(0 1))))
(define p2 (make-polynomial 'x '((2 11)(0 7))))
(define p3 (make-polynomial 'x '((1 13)(0 5))))


(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
(use slib)
(require 'trace)
;(trace remainder-terms)

(greatest-common-divisor q1 q2)
(gcd-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))

(remainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))
;(untrace gcd-terms)
;(untrace remainder-terms)
(define (pseudoremainder-terms p q)
  (let ((o1 (caar p))
		(o2 (caar q))
		(c  (cadar q)))
	(let ((factor (expt c (+ 1 o1 (- o2)))))
	  (cadr (div-terms 
			 (map (lambda (x) (list (car x) (mul factor (cadr x)))) p)
			 q)))))v

(remainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))


(pseudoremainder-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
	  a
	  (gcd-terms b (pseudoremainder-terms a b))))




(greatest-common-divisor q1 q2)

(define (accumulate op initial sequence)
  (if (null? sequence) initial
          (op (car sequence)
                  (accumulate op initial (cdr sequence)))))

(accumulate gcd 1458 '(-2916 1458))
(accumulate gcd 0 '(10 4 6))

(define (mul-coeff-term a n)
  (map (lambda (x) (list (car x) (* (cadr x) n))) a))
(define (div-coeff-term a n)
  (map (lambda (x) (list (car x) (/ (cadr x) n))) a))


(define (gcd-terms a b)
  (if (empty-termlist? b)
	  (let ((coeffs (map coeff a)))
		(let ((div-number (accumulate gcd 0 coeffs)))
		  (div-coeff-term a div-number)))

	  (gcd-terms b (pseudoremainder-terms a b))))



(greatest-common-divisor q1 q2)

;ex 2.97

(define (reduce-terms n d)
  (let ((gcd-term (gcd-terms n d)))
	(let ((g1 (coeff (first-term  gcd-term)))
		  (o2 (order (first-term  gcd-term)))
		  (on (order (first-term  n)))
		  (dn (order (first-term  d))))
	  (let ((mul-value
			 (if (> on dn) 
				 (expt g1 (+ 1 on (- o2)))
				 (expt g1 (+ 1 dn (- o2)))
				 )))

		(let ((mn (mul-coeff-term n mul-value))
			  (md (mul-coeff-term d mul-value)))

		  (let ((tn (car (div-terms mn gcd-term)))
				(td (car (div-terms md gcd-term))))
			(let ((div-number
				   (gcd
					(accumulate gcd 0 (map coeff tn))
					(accumulate gcd 0 (map coeff td)))))
			  (list
			   (div-coeff-term tn div-number)
			   (div-coeff-term td div-number)))))))))
								 
				  
(gcd-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))
(reduce-terms '((4 11) (3 -22) (2 18) (1 -14) (0 7)) '((3 13) (2 -21) (1 3) (0 5)))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
	(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))


  (define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (add-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- ADD-POLY"
			   (list p1 p2))))


  ;; (define (sub-poly p1 p2)
  ;; 	(if (same-variable? (variable p1) (variable p2))
  ;; 		(make-poly (variable p1)
  ;; 				   (add-terms (term-list p1)
  ;; 							  (map (lambda (x)
  ;; 									 ; 手抜き
  ;; 									 ; 多項式係数を考慮していない
  ;; 									 (make-term (order x) (- (coeff x))))
  ;; 								   (term-list p2))))
  ;; 		(error "Polys not in same var -- SUB-POLY"
  ;; 			   (list p1 p2))))

  (define (negate-poly p)
    (make-poly (variable p) (negate-term (term-list p))))
  (define (negate-term L) (map 
						   (lambda (x) (make-term (order x) 
												  (negate (coeff x)))) L))


  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))


  (define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (mul-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- MUL-POLY"
			   (list p1 p2))))

  (define (div-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (div-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- DIV-POLY"
			   (list p1 p2))))


  (define (=zero-poly? p)
	(let ((terms (term-list p)))
	  (= (length terms)
		 (length (filter (lambda (x) (=zero? x))
						 (map coeff terms))))))

  (define (gcd-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1)
				   (gcd-terms (term-list p1)
							  (term-list p2)))
		(error "Polys not in same var -- GCD-POLY"
			   (list p1 p2))))

  (define (reduce-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(let ((reduced
			  (reduce-terms (term-list p1)
							(term-list p2))))
		  (list
		   (tag (make-poly (variable p1) (car reduced)))
		   (tag (make-poly (variable p1) (cadr reduced)))))
		(error "Polys not in same var -- REDUCE-POLY"
			   (list p1 p2))))




  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero-poly? '(polynomial)
	   =zero-poly?)

  (put 'add '(polynomial polynomial)
	   (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial polynomial)
	   (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
	   (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'div '(polynomial polynomial)
	   (lambda (p1 p2) (tag (div-poly p1 p2))))

  (put 'gcd-poly '(polynomial polynomial)
	   (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'reduce '(polynomial polynomial)
	   (lambda (p1 p2) (reduce-poly p1 p2)))

  (put 'negate '(polynomial)
	 (lambda (p) (tag (negate-poly p))))


  (put 'make 'polynomial
	   (lambda (var terms) (tag (make-poly var terms))))
  'done)


(install-polynomial-package)

(define (reduce-poly x y) (apply-generic 'reduce x y))


(define p1 (make-polynomial 'x '((2 1)(1 -2)(0 1))))
(define p2 (make-polynomial 'x '((2 11)(0 7))))
(define p3 (make-polynomial 'x '((1 13)(0 5))))

(reduce-poly (mul p2 p1) (mul p1 p3))
(reduce-poly (mul p2 p3) (mul p2 p1))
(reduce-poly (mul p1 p3) (mul p2 p3))

(define (reduce-integers n d)
  (let ((g (gcd n d)))
	(list (/ n g) (/ d g))))

(reduce-integers 10 2)

(define (install-scheme-number-package)
  (define (tag x)
	(attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (tag (/ x y))))

  (put 'reduce '(scheme-number scheme-number)
	   reduce-integers)

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)


(define (reduce x y) (apply-generic 'reduce x y))

(reduce 10 5)
(reduce (mul p1 p2) (mul p1 p3))


(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
	(reduce n d))
  (define (add-rat x y)
	(make-rat (add (mul (numer x) (denom y))
				   (mul (numer y) (denom x)))
			  (mul (denom x) (denom y))))

  (define (sub-rat x y)
	(make-rat (sub (mul (numer x) (denom y))
				   (mul (numer y) (denom x)))
			  (mul (denom x) (denom y))))

  (define (mul-rat x y)
	(make-rat (mul (numer x) (numer y))
			  (mul (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (mul (numer x) (denom y))
			  (mul (denom x) (numer y))))

  (define (tag x)
	(attach-tag 'rational x))
  
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)


(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(add 
 (make-rational 10 2)
 (make-rational 10 2))

(make-rational 10 2)

(make-rational p2 p1)
(define rf1 (make-rational p1 p2))
(display rf1)


(define rf2 (make-rational p3 p4))
(display rf2)

(add rf1 rf2)
(reduce p1 p2)
(reduce p3 p4)
(reduce p1 p4)
(reduce p2 p4)



(sub 
 (make-polynomial 'x 
				  '((1 2)))
 (make-polynomial 'x 
				  '((1 1)))
 )
