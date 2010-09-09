(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))


(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (make-rat n d)
  (let ((g (gcd n d)))
	(cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

;ex 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
	(let ((tn (/ n g))
		  (td (/ d g)))
	  (cond ((= td 0)
			 (error "divided by zero"))
			((> td 0)
			 (cons tn td))
			(else
			 (cons (- tn) (- td)))))))
			 
(print-rat (add-rat one-third one-third))

(print-rat (make-rat 1 2))
(print-rat (make-rat 2 -4))
(print-rat (make-rat -2 -6))
(print-rat (make-rat -8 2))



;ex 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segmant s e)
  (cons s e))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
  
(define (print-segment s)
  (display "(")
  (print-point
   (start-segment s))
  (display ",")
  (print-point 
   (end-segment s))
  (display ")"))


(define p1 (make-point 1 2))
(define p2 (make-point 2 5))

(define s1 (make-segmant p1 p2))
(print-segment s1)

(define (midpoint-segment s)
  (let ((spx (x-point (start-segment s)))
		(spy (y-point (start-segment s)))
		(epx (x-point (end-segment s)))
		(epy (y-point (end-segment s))))
	(make-point (/ (+ spx epx) 2)
				(/ (+ spy epy) 2))))

(print-point (midpoint-segment s1))



;ex 2.3
;segment
(define (square x) (* x x))

(define (length-segment s)
  (let ((spx (x-point (start-segment s)))
		(spy (y-point (start-segment s)))
		(epx (x-point (end-segment s)))
		(epy (y-point (end-segment s))))
	(sqrt (+ (square (- spx epx))
			 (square (- spy epy))))))

(length-segment s1)

(define (make-rectangle s)
  s)


(define (make-sides x y)
  (cons (abs x) (abs y)))

(define (x-sides p)
  (car p))
(define (y-sides p)
  (cdr p))


(define (sides-rectangle r)
  (let ((spx (x-point (start-segment r)))
		(spy (y-point (start-segment r)))
		(epx (x-point (end-segment r)))
		(epy (y-point (end-segment r))))
	(make-sides 
	 (- epx spx)
	 (- epy spy))))

(define (preimeter-rectangle r)
  (let ((sides (sides-rectangle r)))
	(* 2 (+ (x-sides sides) (y-sides sides)))))

(define (area-rectangle r)
  (let ((sides (sides-rectangle r)))
	(* (x-sides sides) (y-sides sides))))



(preimeter-rectangle (make-rectangle s1))
		
(area-rectangle (make-rectangle s1))


(define (make-rectangle2 p1 p2)
  (make-segmant p1 p2))

(preimeter-rectangle (make-rectangle2 p1 p2))
		
(area-rectangle (make-rectangle2 p1 p2))

