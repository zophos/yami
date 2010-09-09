(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x
				(make-interval (/ 1.0 (upper-bound y))
							   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))


(define (lower-bound x)  (car x))

(define (upper-bound x)  (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (upper-bound x))
                 (- (upper-bound y) (lower-bound x))))


(sub-interval (make-interval -1 2) (make-interval -1 2))
  
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-parcent c p)
  (make-center-width c (* c (/ p 100))))

(define (parcent i)
  (* (/ (width i) (center i)) 100))



(define i (make-center-width 5 5))
(parcent i)

(define i (make-center-parcent 100 10))
(parcent i)
