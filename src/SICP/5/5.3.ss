(load "./5.2.ss")

;ex5.21

(define (count-leaves-a tree)
  (define count-leaves-machine-a
	(make-machine
	 '(tree continue val tmp)
	 (list (list 'null? null?) (list 'not not) (list 'pair? pair?)
		   (list 'car car) (list 'cdr cdr) (list '+ +))
	 '(start

	   (assign continue (label done))
	   loop
	   (test (op null?) (reg tree))
	   (branch (label null))
	   
	   (assign tmp (op pair?) (reg tree))
	   (test (op not) (reg tmp))
	   (branch (label pair))

	   (save continue)
	   (assign continue (label after-1))
	   (save tree)
	   (assign tree (op car) (reg tree))
	   (goto (label loop))
	   after-1
	   (restore tree)
	   (assign tree (op cdr) (reg tree))
	   (assign continue (label after-2))
	   (save val)
	   (goto (label loop))
	   after-2
	   (assign tmp (reg val))
	   (restore val)
	   (restore continue)
	   (assign val
			   (op +) (reg val) (reg tmp))
	   (goto (reg continue))
	   pair
	   (assign val (const 1))
	   (goto (reg continue))
	   null
	   (assign val (const 0))
	   (goto (reg continue))
	   done)))
  ((count-leaves-machine-a 'stack) 'initialize)
	(set-register-contents! count-leaves-machine-a 'tree tree)
	(start count-leaves-machine-a)
	(display (get-register-contents count-leaves-machine-a 'val))
	(newline))


(count-leaves-a '())
(count-leaves-a '(a))

(count-leaves-a '(1 2))
(count-leaves-a '(1 (2 3)))
	  
(define x '((1 2) 3 4))
(count-leaves-a (list x x))

(define (count-leaves-b tree)
  (define count-leaves-machine-b
	(make-machine
	 '(tree continue n tmp)
	 (list (list 'null? null?) (list 'not not) (list 'pair? pair?)
		   (list 'car car) (list 'cdr cdr) (list '+ +))
	 '(start
	   (assign n (const 0))
	   (assign continue (label done))

	   iter
	   (test (op null?) (reg tree))
	   (branch (label null))
	   
	   (assign tmp (op pair?) (reg tree))
	   (test (op not) (reg tmp))
	   (branch (label pair))

	   (save tree)
	   (save continue)
	   (assign continue (label after-car))
	   (assign tree (op car) (reg tree))
	   (goto (label iter))

	   after-car
	   (restore continue)
	   (restore tree)
	   (assign tree (op cdr) (reg tree))
	   (goto (label iter))

	   pair
	   (assign n (op +) (reg n) (const 1))
	   (goto (reg continue))

	   null
	   (goto (reg continue))
	   done)))
  ((count-leaves-machine-b 'stack) 'initialize)
  (set-register-contents! count-leaves-machine-b 'tree tree)
  (start count-leaves-machine-b)
  (display (get-register-contents count-leaves-machine-b 'n))
  (newline))


(count-leaves-b '())
(count-leaves-b '(a))

(count-leaves-b '(1 2))
(count-leaves-b '(1 (2 3)))
(define x '((1 2) 3 4))
(count-leaves-b (list x x))


										;ex5.22

(define (append-m x y)
  (define append-machine
	(make-machine
	 '(x y val continue)
	 (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
	 '(start
	   (assign continue (label done))
	   iter
	   (test (op null?) (reg x))
	   (branch (label null))
	   (save x)
	   (save continue)
	   (assign x (op cdr) (reg x))
	   (assign continue (label after-iter))
	   (goto (label iter))

	   after-iter
	   (restore continue)
	   (restore x)
	   (assign x (op car) (reg x))
	   (assign val (op cons) (reg x) (reg val))
	   (goto (reg continue))

	   null
	   (assign val (reg y))
	   (goto (reg continue))
	   done)))
  (set-register-contents! append-machine 'x x)
  (set-register-contents! append-machine 'y y)
  ;(append-machine 'trace-on)
  (start append-machine)

  (display (get-register-contents append-machine 'val))
  (newline))


(define x '(1 2))
(append-m x x)
(display x)

(append-m '(a b) '(c d))

(append-m '() '(c d))
(append-m '(a) '())

(define (append!-m x y)
  (define append!-machine
	(make-machine
	 '(x y val)
	 (list (list 'null? null?) (list 'set-cdr! set-cdr!) (list 'cdr cdr))
	 '(start
	   (save x)
	   iter
	   (assign val (op cdr) (reg x))
	   (test (op null?) (reg val))
	   (branch (label last-pair-done))
	   (assign x (op cdr) (reg x))
	   (goto (label iter))
	   last-pair-done
	   (perform (op set-cdr!) (reg x) (reg y))
	   (restore x)
	   (assign val (reg x))
	   done)))
  (set-register-contents! append!-machine 'x x)
  (set-register-contents! append!-machine 'y y)
  ;(append!-machine 'trace-on)
  (start append!-machine)
  (display (get-register-contents append!-machine 'val))
  (newline))
	   

(define x '(a b))
(define y '(c d))

(append!-m x y)
(display x)

(define x '(a b))
(define y '(c d))
(append! x y)



	



(vector-ref '#(0 1 2 3 4 5) 3)
(vector-ref '#(0 1 2 3 4 5) 0)
(vector-ref '#(0 1 2 3 4 5) 6)

(define v0 '#(0 1 2 3 4 5))
(vector-set! v0 0 1)
(display v0)

(define the-cars (make-vector 128))
(define the-cdrs (make-vector 128))





;;gc

'(begin-garbage-collection
  (assign free (const 0))
  (assign scan (const 0))
  (assign old (reg root))
  (assign relocate-continue (label reassign-root))
  (goto (label relocate-old-result-in-new))

  reassign-root
  (assign root (reg new))
  (goto (label gc-loop))

  gc-loop
  (test (op =) (reg scan) (reg fee))
  (branch (label gc-flip))
  (assign old (op vector-ref) (reg new-cars) (reg scan))
  (assign relocate-continue (label update-car))
  (goto (label relocate-old-result-in-new))

  update-car
  (perform
   (op vector-set!) (reg new-cars) (reg scan) (reg new))
  (assign old (op vector-ref) (reg new-cdrs) (reg scan))
  (assign relocate-continue (label update-cdr))
  (goto (label relocate-old-result-in-new))

  update-cdr
  (perform
   (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
  (assign scan (op +) (reg scan) (const 1))
  (goto (label gc-loop))

  relocate-old-result-in-new
  (test (op pointor-to-pair?) (reg old))
  (branch (label pair))
  (assign new (reg old))
  (goto (reg relocate-continue))
  
  pair
  (assign oldcr (op vector-ref) (reg the-cars) (reg old))
  (test (op broken-heart?) (reg oldcr))
  (branch (label already-moved))
  (assign new (reg free))

  (assign free (op +) (reg free) (const 1))
  
  (perform (op vector-set!)
		   (reg new-cars) (reg new) (reg oldcr))
  (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
  (perform (op vector-set!)
		   (reg new-cdrs) (reg new) (reg oldcr))

  (perform (op vector-set!)
		   (reg the-cars) (reg old) (const broken-heart))
  (perform
   (op vector-set!) (reg the-cdrs) (reg old) (reg new))
  (goto (reg relocate-continue))

  already-moved
  (assign new (op vector-ref) (reg the-cdrs) (reg old))
  (goto (reg relocate-continue))


  gc-flip
  (assign temp (reg the-cdrs))
  (assign the-cdrs (reg new-cdrs))
  (assign new-cdrs (reg temp))
  (assign temp (reg the-cars))
  (assign the-cars (reg new-cars))
  (assign new-cars (reg temp))

  )



