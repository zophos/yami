(define (square x) (* x x))
(define pi (atan  0 -1.0))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
	  (car datum)
	  (error  "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
	  (cdr datum)
	  (error  "Bad tagged datum -- CONTENTS" datum)))
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

(define (type-tag datum)
  (cond ((pair? datum)
		 (car datum))
		((number? datum)
		 'scheme-number)
		(else
		 (error  "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
;  (if (number? contents)
  (if (eq? type-tag 'scheme-number)
	  contents
	  (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum)
		 (cdr datum))
		((number? datum)
		 datum)
		(else
		 (error  "Bad tagged datum -- TYPE-TAG" datum))))

(add 1 2)
(add r1 r2)
(add c1 c2)

(define (install-equ?-package)
  (put 'equ? '(scheme-number scheme-number)
	   (lambda (x y)
		 (= x y)))

  (put 'equ? '(rational rational)
	   (lambda (x y)
		 (=
		  (* (car x) (cdr y))
		  (* (cdr x) (car y)))))

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

(=zero? 0)
(=zero? (make-rational 0 1))
(=zero? (make-complex-from-mag-ang 0 0))

(=zero? 1)
(=zero? (make-rational 1 0))
(=zero? (make-complex-from-mag-ang 1 0))



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

  (define (add-complex-to-schemenum z x)
	(make-from-real-imag (+ (real-part z) x)
						 (imag-part z)))


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

  (put 'add '(complex scheme-number)
		(lambda (z x) (tag (add-complex-to-schemenum z x))))

  'done)

(install-complex-package)



(add (make-complex-from-mag-ang 1 0) 1)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (make-coercion-table)
  (let ((local-table (list '*coercion-table*)))
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

(define coercion-table (make-coercion-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion  (coercion-table 'insert-proc!))

(put-coercion 'scheme-number 'complex scheme-number->complex)
(get-coercion 'scheme-number 'complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (if (= (length args) 2)
			  (let ((type1 (car type-tags))
					(type2 (cadr type-tags))
					(a1 (car args))
					(a2 (cadr args)))
				(let ((t1->t2 (get-coercion type1 type2))
					  (t2->t1 (get-coercion type1 type2)))
				  (cond (t1->t2
						 (apply-generic op (t1->t2 a1) a2))
						(t2->t1
						 (apply-generic op a1 (t2->t1 a2)))
						(error "No method for these types"
							   (list op type-tags)))))
			  (error "No method for these types"
					 (list op type-tags)))))))





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

(install-complex-package)

(add 2 (make-complex-from-real-imag 2 1))

;ex2.81

(define (exp x y) (apply-generic 'exp x y))

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

  (put 'exp '(scheme-number scheme-number)
	   (lambda (x y) (tag (expt x y))))

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)


(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)


(put-coercion 'scheme-number 'scheme-number
			  scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

(exp 2 2)
;(exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 2 0))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (if (= (length args) 2)
			  (let ((type1 (car type-tags))
					(type2 (cadr type-tags))
					(a1 (car args))
					(a2 (cadr args)))
				(if (eq? type1 type2)
					(error "No method for these types"
						   (list op type-tags))
					 (let ((t1->t2 (get-coercion type1 type2))
						   (t2->t1 (get-coercion type1 type2)))
					   (cond (t1->t2
							  (apply-generic op (t1->t2 a1) a2))
							 (t2->t1
							  (apply-generic op a1 (t2->t1 a2)))
							 (error "No method for these types"
									(list op type-tags)))))
			  (error "No method for these types"
					 (list op type-tags))))))))
(exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 2 0))

(define (id x) x)

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else  (filter predicate (cdr sequence)))))


(define (try-coercion type-tags args)
  (try-coercion-sub type-tags type-tags args)
  )

(define (try-coercion-sub check tags args)

  (if (null? check) #f
	  (let ((type (car check)))
		(let ((coercion-list
			   (filter (lambda (x) (if (eq? #f x) #f #t))
					   (map (lambda (x)
							  (get-coercion x type)) tags))))
		  (if (= (length coercion-list) (length tags))
			  (do-coercion coercion-list args)
			  (try-coercion-sub (cdr check) tags args))))))


(define (do-coercion coercion-list args)
  (if (null? coercion-list)
	  '()
	  (cons 
	   ((car coercion-list) (car args))
	   (do-coercion (cdr coercion-list) (cdr args)
						))))


(try-coercion '(scheme-number complex) '(1 (complex rectangular 2.0 . 0.0)))
(try-coercion '(rational complex) '(1 (complex rectangular 2.0 . 0.0)))

(define (get-coercion-list type tags)
  (filter (lambda (x) (if (eq? #f x) #f #t))
		  (map (lambda (x)
				 (get-coercion x type)) tags)))

(get-coercion-list 'scheme-number '(scheme-number complex)) 


(define (apply-generic op . args)
  (display "op: ")
  (display op)
  (newline)
  (display "args: ")
  (display args)
  (newline)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (let ((coercion-args  (try-coercion type-tags args)))


			(if (not (eq? #f coercion-args))
				(apply apply-generic (cons op coercion-args))
				(error "No method for these types"
						   (list op type-tags))))))))

(add (make-complex-from-real-imag 31 0) (make-complex-from-real-imag 1 0))
(add 31 (make-complex-from-real-imag 1 0))


(define (install-scheme-number-package)

  (define (raise-number x)
	(make-rational (contents x) 1))

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

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  
  (put 'raise-number 'scheme-number
	   (lambda (x) (raise-number x)))

  'done)

(install-scheme-number-package)

(define (raise-number x)
  ((get 'raise-number (type-tag x)) x))
(raise-number 5)


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

  (define (raise-number x)
	(make-complex-from-real-imag
	 (/ (numer (contents x)) (denom (contents x))) 0))


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

  (put 'raise-number 'rational
	   (lambda (x) (raise-number x)))

  'done)

(install-rational-package)

(raise-number (make-rational 1 2))



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

(install-complex-package)

(define
  package-types '(scheme-number rational complex))

(define (compare-type x y types)
  (if (null? types)
	  (error "compare-type error" x y)
	  (let ((type (car types)))
		(cond ((and (eq? x type) (eq? y type))
			   'equal)
			  ((eq? x type) 'lesser)
			  ((eq? y type) 'greater)
			  (else 
			   (compare-type x y (cdr types)))))))

(compare-type 'scheme-number 'scheme-number package-types)
(compare-type 'complex 'scheme-number package-types)
(compare-type 'scheme-number 'complex package-types)

(define (type-level type)
  (define (type-level-sub type types level)
	(cond ((null? types) (error 'no type' type))
		  ((eq? type (car types)) level)
		  (else
		   (type-level-sub type (cdr types) (+ level 1)))))
  (type-level-sub type package-types 0))

(type-level 'scheme-number)
(type-level 'rational)
(type-level 'complex)

(define (max-type-level types)
  (define (max-type-level-sub types max)
	(if (null? types)
		max
		(let ((level (type-level (car types))))
		  (if (> level max)
			  (max-type-level-sub (cdr types) level)
			  (max-type-level-sub (cdr types) max)))))
  (max-type-level-sub types -1))

(define (max-type-level types)
  (apply max (map type-level types)))

(max-type-level '(scheme-number complex))
(max-type-level '(rational scheme-number))

(define (equal-level? types)
  (apply = (map type-level types)))
  
(equal-level? '(scheme-number complex))
(equal-level? '(rational scheme-number))
(equal-level? '(rational rational rational))

(define (raise-level max arg)
  (if (= max (type-level (type-tag arg))) arg
	  (raise-level max
				   (raise-number arg))))

(raise-level 2 (make-rational 2 3))

(raise-level 2 1)

			  


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(if (= 1 (length args))
		(let ((proc (get op type-tags)))
		  (if proc
			  (apply proc (map contents args))
			  (error "No method for these types -- APPLY-GENERIC"
					 (list op type-tags))))
		(if (equal-level? type-tags)
			(let ((proc (get op type-tags)))
			  (if proc
				  (apply proc (map contents args))
				  (error "No method for these types -- APPLY-GENERIC"
						 (list op type-tags))))
			(let ((max-level (max-type-level type-tags)))
			  (apply apply-generic (cons op
					 (map 
					  (lambda (x) (raise-level max-level x))
					  args))))))))

(add 1 (make-rational 1 2))

  


(define (install-scheme-number-package)

  (define (raise-number x)
	(make-rational (contents x) 1))

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

  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  
  (put 'raise-number 'scheme-number
	   (lambda (x) (raise-number x)))

  'done)

(install-scheme-number-package)



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

  (define (raise-number x)
	(make-complex-from-real-imag
	 (/ (* 1.0 (numer (contents x))) (denom (contents x))) 0))

  (define (project-number x)
	(make-scheme-number
	 (numer (contents x))))


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

  (put 'raise-number 'rational
	   (lambda (x) (raise-number x)))

  (put 'project-number 'rational
	   (lambda (x) (project-number x)))

  'done)

(install-rational-package)


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

  (define (project-number z)
	(make-rational (round->exact (* (real-part z) 2310)) 2310))

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

  (put 'project-number 'complex
	   (lambda (x) (project-number x)))

  'done)

(install-complex-package)

(define (project-number x)
  ((get 'project-number (type-tag x)) x))


(project-number (make-rational 1 2))

(raise-number
 (project-number (make-rational 1 2)))

(equ? 
 (raise-number
  (project-number (make-complex-from-real-imag 1.5 0)))
 (make-complex-from-real-imag 1.5 0))

(define (drop x)
  (if (= (type-level (type-tag x)) 0) x
	  (let ((project-n (project-number x)))
		(let ((raise-n (raise-number project-n)))
		  (if (equ? x raise-n)
			  (drop project-n)
			  x)))))

(drop 1)

(drop (make-complex-from-real-imag 1 0))


(drop (make-complex-from-real-imag 1 2))


(drop (make-complex-from-real-imag 1.51 0))
(drop (make-complex-from-real-imag 1.5 0))
(drop (make-rational 1 1))
(drop (make-rational 1 2))
					  
;ex2.86
;(ά)
