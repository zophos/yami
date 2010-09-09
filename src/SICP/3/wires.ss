(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))

(define (inverter input output)
  (define (invert-input)
	(let ((new-value (logical-not (get-signal input))))
	  (after-delay inverter-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
		((= s 1) 0)
		(else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
	(let ((new-value
		   (logical-and (get-signal a1) (get-signal a2))))
	  (after-delay and-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


	
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
		((and (= s1 0) (= s2 1)) 0)
		((and (= s1 1) (= s2 0)) 0)
		((and (= s1 0) (= s2 0)) 0)
		(else (error "Invalid signal" s1 s2))))

;ex3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value
		   (logical-or (get-signal a1) (get-signal a2))))
	  (after-delay or-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


	
(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
		((and (= s1 0) (= s2 1)) 1)
		((and (= s1 1) (= s2 0)) 1)
		((and (= s1 0) (= s2 0)) 0)
		(else (error "Invalid signal" s1 s2))))

;ex3.29
;2* invert-delay + and-gate-delay

;ex3.30
;half-adder-delay = max(and-delay, or-delay+and-delay, and-delay+invert-delay+and-delay) = max(or-delay+and-delay, and-delay+invert-delay+and-delay)
;full-adder-delay = 2* half-adder-delay + or-delay

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define (call-each procedures)
  (if (null? procedures)
	  'done
	  (begin 
		((car procedures))
		(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
				  action 
				  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
	  'done
	  (let ((first-item (first-agenda-item the-agenda)))
;		(newline)
;		(display first-item)
		(first-item)
		(remove-first-agenda-item! the-agenda)
		(propagate))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

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



(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
	(or (null? segments)
		(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
	(let ((q (make-queue)))
	  (insert-queue! q action)
	  (make-time-segment time q)))
  (define (add-to-segments! segments)
	(if (= (segment-time (car segments)) time)
		(insert-queue! (segment-queue (car segments))
					   action)
		(let ((rest (cdr segments)))
		  (if (belongs-before? rest)
			  (set-cdr! 
			   segments
			   (cons (make-new-time-segment time action)
					 (cdr segments)))
			  (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
	(if (belongs-before? segments)
		(set-segments!
		 agenda
		 (cons (make-new-time-segment time action)
			   segments))
		(add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
	(delete-queue! q)
	(if (empty-queue? q)
		(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
	  (error "Agenda is empty -- FIRST-AGENDA-ITEM")
	  (let ((first-seg (first-segment agenda)))
		(set-current-time! agenda (segment-time first-seg))
		(front-queue (segment-queue first-seg)))))
						   

(define (probe name wire)
  (add-action! wire 
			   (lambda ()
				 (newline)
				 (display name)
				 (display " ")
				 (display (current-time the-agenda))
				 (display " New-value = ")
				 (display (get-signal wire)))))
;
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
  
				 
(define input-1 (make-wire))
(define input-2 (make-wire))
;(define sum (make-wire))
(define carry (make-wire))
(define sum (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

  
(set-signal! input-1 1)

(propagate)
(set-signal! input-2 1)
(propagate)

;ex3.31
(use slib)
(require 'trace)

(define (make-wire-tmp)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	(trace accept-action-procedure!)
	dispatch))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(display the-agenda)  
				 
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire-tmp))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(display the-agenda)  
;; (0 
;;  (2  
;;   (#<closure (inverter invert-input #f)>) 
;;   #<closure (inverter invert-input #f)>)
;;  (3 
;;   (#<closure  (and-gate and-action-procedure #f)
;;    > #<closure (and-gate and-action-procedure #f)
;;    > #<closure (and-gate and-action-procedure #f)
;;    > #<closure (and-gate and-action-procedure #f)
;;    >) #<closure (and-gate and-action-procedure #f)>)
;;  (5 
;;   (#<closure  (or-gate or-action-procedure #f)
;;    > #<closure (or-gate or-action-procedure #f)
;;    >) #<closure (or-gate or-action-procedure #f)>)
;;  )

  
(set-signal! input-1 1)

(propagate)
(set-signal! input-2 1)
(propagate)

;;;;
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures)))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	(trace accept-action-procedure!)
	dispatch))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
  
				 
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

  
(set-signal! input-1 1)

(propagate)
(set-signal! input-2 1)
(propagate)

;;
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
  
(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))
(probe 'input-1 input-1)
(probe 'input-2 input-2)
(probe 'output output)

(or-gate input-1 input-2 output)
(display the-agenda)
(propagate)
(set-signal! input-1 1)
(display the-agenda)
(propagate)

;(5 (10 (#<closure (or-gate or-action-procedure #f)>) #<closure (or-gate or-action-procedure #f)>))

;ex3.30

(define (ripple-carry-adder a-list b-list s-list c-output)
  (define (iter as bs ss cn-1)
    (cond ((null? as)
		   'ok)
		  (else
		   (let ((an (car as))
				 (bn (car bs))
				 (sn (car ss))
				 (cn (make-wire)))
			 (if (null? (cdr as))
				 (full-adder an bn cn-1 sn c-output)
				 (full-adder an bn cn-1 sn cn))
			 (iter (cdr as) (cdr bs) (cdr ss) cn)))))
  (iter a-list b-list s-list (make-wire)))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define a0 (make-wire))
(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))

(define b0 (make-wire))
(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))

(define s0 (make-wire))
(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))

(define a (list a0 a1 a2 a3))
(define b (list b0 b1 b2 b3))
(define c (make-wire))
(define s (list s0 s1 s2 s3))

(probe 's0 s0)
(probe 's1 s1)
(probe 's2 s2)
(probe 's3 s3)
(probe 'c c)

(ripple-carry-adder a b s c)

(set-signal! a0 1)
(propagate) 

(set-signal! b0 1)
(propagate)
(set-signal! a1 1)
(propagate)

(set-signal! b3 1)
(propagate)
(set-signal! a3 1)
(propagate)


;;test


(define (make-wire-verbose)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (display proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc)
	  )
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define the-agenda (make-agenda))
(define inverter-delay 2)

(define input (make-wire-verbose))
(define output (make-wire))

(probe 'output output)

(inverter input output)
(propagate)
(set-signal! input 1)
(propagate)
(use srfi-42)

(define x 10)

(define (diff xs) 
  (list-ec (:parallel (: x xs) (: y (cdr xs))) (- y x)))

(diff '(1 2))


(use gauche.threads)
(define (parallel-execute . procs)
  (let ((threads (map make-thread procs)))
    (map thread-start! threads)
    (map thread-join! threads)))
(parallel-execute (lambda () (set! x (* x x)))
				  (lambda () (set! x (+ x 1))))
