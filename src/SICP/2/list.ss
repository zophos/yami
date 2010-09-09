; p50
(define nil '())
;; (define (scale-list items factor)
;;   (if (null? items)
;;       nil
;;       (cons (* (car items) factor)
;;             (scale-list (cdr items) factor))))
;; (scale-list (list 1 2 3 4 5) 10)

;(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

;(map (lambda (x y) (+ x (* 2 y)))
;     (list 1 2 3)
;     (list 4 5 6))

; p51
;(define (map proc items)
;  (if (null? items)
;      nil
;      (cons (proc (car items))
;            (map proc (cdr items)))))
;(map abs (list -10 2.5 -11.6 17))

;(map (lambda (x) (* x x))
;     (list 1 2 3 4))

; 2.21
(define (square x) (* x x))

(define (square-list items)
 (if (null? items)
     nil
     (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))

; 2.22

(define (square-list items)
 (define (iter things answer)
   (if (null? things)
       answer
      (iter (cdr things)
             (cons (square (car things)) 
                   answer))))
 (iter items nil))
(square-list (list 1 2 3 4))

(define (square-list items)
 (define (iter things answer)
   (if (null? things)
       answer
       (iter (cdr things)
             (cons answer 
                   (square (car things))))))
 (iter items nil))

(square-list (list 1 2 3 4))

(define (square-list items)
 (define (iter things answer)
   (if (null? things)
       answer
       (iter (cdr things)
             (append answer 
                   (list (square (car things)))))))
 (iter items nil))

(square-list (list 1 2 3 4))

(define (square-list items)
 (define (iter things answer)
   (if (null? things)
       (reverse answer)
       (iter (cdr things)
             (cons (square (car things))
				   answer))))
 (iter items nil))

(square-list (list 1 2 3 4))


; 2.23

(define (for-each proc list)
 (if (null? list) nil
     (cons (proc (car list)) (for-each proc (cdr list)))))

(define (for-each proc list)
 (cond ((null? list) nil)
       (else
        (proc (car list)) (for-each proc (cdr list)))))

(define (for-each proc list)
 (if (null? list) (display "")
     (begin (proc (car list))
			(for-each proc (cdr list)))))

(for-each (lambda (x) (newline) (display x))
         (list 57 321 88))
