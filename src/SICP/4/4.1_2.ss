;;2度評価しないこと
(define apply-in-underlying-scheme apply)
(define eval-in-underlying-scheme eval)
;;2度評価しないこと

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   arguments
		   (procedure-environment procedure))))
		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
	  (eval (if-consequent exp) env)
	  (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
		(else (eval (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
					   (eval (assignment-value exp) env)
					   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
	                (eval (definition-value exp) env)
					env)
  'ok)

;;4.1.2
(define true #t)
(define false #f)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
		((string? exp) true)
		(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
	  (cadr exp)
	  (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
	  (caddr exp)
	  (make-lambda (cdadr exp)
				   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))
  
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clauses? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
	  'false
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(if (cond-else-clauses? first)
			(if (null? rest)
				(sequence->exp (cond-actions first))
				(error "ELSE clause isn't last -- COND->IF"
					   clauses))
			(make-if (cond-predicate first)
					 (sequence->exp (cond-actions first))
					 (expand-clauses rest))))))

;;ex4.2

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (if (< (length vars) (length vals))
		  (error "Too many arguments supplied" vars vals)
		  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (car vals))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable -- SET!" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (scan vars vals)
	  (cond ((null? vars)
			 (add-binding-to-frame! var val frame))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(scan (frame-variables frame)
		  (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cadr cadr)
		(list 'caddr caddr)
		(list 'cons cons)
		(list 'null? null?)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '= =)
		(list '< <)
		(list '> >)
		(list 'display display)
		(list 'newline newline)
		(list 'assoc assoc)
		(list 'eq? eq?)
		(list 'equal? equal?)
		(list 'list list)
		(list 'map map)
		;...
		))

(define (primitive-procedure-names)
  (map car 
	   primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
	   primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
		 (extend-environment (primitive-procedure-names)
							 (primitive-procedure-objects)
							 the-empty-environment)))
	(define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))

(define the-global-environment (setup-environment))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
	(let ((output (eval input the-global-environment)))
	  (announce-output output-prompt)
	  (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
	  (display (list 'compound-procedure
					 (procedure-parameters object)
					 (procedure-body object)
					 '<procedure-env>))
	  (display object)))

(define the-global-environment (setup-environment))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))


(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (eval-and-conds (cdr exp) env))

(define (eval-and-conds conds env)
  (if (null? conds)
	  true
	  (if (eval (car conds) env)
		  (eval-and-conds (cdr conds) env)
		  false)))



(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (eval-or-conds (cdr exp) env))

(define (eval-or-conds conds env)
  (if (null? conds)
	  false
	  (if (eval (car conds) env)
		  true
		  (eval-or-conds (cdr conds) env))))

;ex4.5
(cond ((assoc 'b '((a 1) (b 2))) => cadr)
	  (else false))

(define (expand-clauses clauses)
  (if (null? clauses)
	  'false
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(cond ((cond-else-clauses? first)
			   (if (null? rest)
				   (sequence->exp (cond-actions first))
				   (error "ELSE clause isn't last -- COND->IF"
						  clauses)))
			  ((eq? (cadr first) '=>)
			   (make-if (cond-predicate first)
						(list (caddr first) (cond-predicate first))
						(expand-clauses rest)))
			  (else 
			   (make-if (cond-predicate first)
						(sequence->exp (cond-actions first))
						(expand-clauses rest)))))))

(driver-loop)
(+ 1 2)
(cond ((assoc 'b '((a 1) (b 2))) true)
	  (else false))

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
	  (else false))

(cond ((assoc 'b '((a 1) (b 2))) => (lambda (x) (+ 3 (cadr x))))
	  (else false))
end
;ex4.6

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 (eval (let->combination exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))


;(define (let-clauses exp) (cdr exp))
;
;(define (let-bindings clauses) (car clauses))

;(define (let-body clauses) (cdr clauses))

;(define (let->combination exp)
;  (expand-let-clauses (let-clauses exp)))

;(define (expand-let-clauses clauses)
;  (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
; 		(map cadr (let-bindings clauses)))))

(define (let->combination exp)
  (cons (make-lambda
		 (map car (cadr exp)) (cddr exp))
		(map cadr (cadr exp))))
			  

(let ((a 3) (b 2))
  (+ a b))
(let ()
  (+ 1 2))

(driver-loop)
(lambda (a b) (+ a b))
(let ((a 3) (b 2))
  (+ a b))
(let ()
  (+ 1 2))
end

;ex4.7
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 ;(display exp)
		 ;(newline)
		 (eval (let->combination exp) env))
		((let*? exp)
		 (eval (let*->nested-lets exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-clauses exp) (cdr exp))

(define (let*-bindings clauses) (car clauses))

(define (let*-body clauses) (cadr clauses))

(define (make-let clauses body) (list 'let clauses body))

(define (let*->nested-lets exp)
  (let ((clauses (let*-clauses exp)))
	(let ((bindings (let*-bindings clauses))
		  (body (let*-body clauses)))
	  (define (iter bindings)
		(if (null? bindings)
			;(make-let '() body)
			body
			(make-let (list (car bindings))
					  (iter (cdr bindings)))
		))
	  (iter bindings)
	  )))
  


(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

(driver-loop)
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))
end

;ex4.8
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 ;(display exp)
		 (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 ;(display exp)
		 ;(newline)
		 (eval (let->combination exp) env))
		((let*? exp)
		 (eval (let*->nested-lets exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))


;; (define (let->combination exp env)
;;   (if (pair? (cadr exp))
;; 	  (cons (make-lambda
;; 			 (map car (cadr exp)) (cddr exp))
;; 			(map cadr (cadr exp)))
;; 	  (begin
;; 		;(display (cons (cadr exp) (map car (caddr exp))))
;; 		;(newline)
;; 		;(display (cons (cadr exp) (map cadr (caddr exp))))
;; 		;(newline)
;; 		(eval-definition
;; 		 (list 'define (cons (cadr exp) (map car (caddr exp)))
;; 			   (cadddr exp)) env)
;; 		(cons (cadr exp)
;; 			  (map cadr (caddr exp))))))
		

(define (let->combination exp)
  (if (pair? (cadr exp))
	  (cons (make-lambda
			 (map car (cadr exp)) (cddr exp))
			(map cadr (cadr exp)))
	  (make-begin
	   (list
		(list 'define (cons (cadr exp) (map car (caddr exp)))
			  (sequence->exp
			   (cdddr exp)))
		(cons (cadr exp)
			  (map cadr (caddr exp)))))))
		

  
(driver-loop)
(let ((x 3) (y 2))
  (* x y))
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
;	(display a)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
(fib 7)
(fib 8)
(fib 9)
end
;4.9

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 ;(display exp)
		 (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 ;(display exp)
		 ;(newline)
		 (eval (let->combination exp) env))
		((let*? exp)
		 (eval (let*->nested-lets exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((while? exp) (eval (while->if exp) env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))


(define i 0)
(while (< i 10)
	   (display i)
	   (newline)
	   (set! i (+ i 1)))

(define (while? exp) (tagged-list? exp 'while))

(define (sequence->exp seq)
  ;(display seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))


(define (while->if exp)
  (make-if (cadr exp)
		   (sequence->exp (append (cddr exp) (list exp)))
		   'ok))

(driver-loop)
(define i 0)
(while (< i 10)
	   (display i)
	   (newline)
	   (set! i (+ i 1)))
end		   


;ex4.11
(define (make-frame variables values)
  (define (iter variables values)
	(if (null? variables)
		'()
		(cons (cons (car variables) (car values))
			  (iter (cdr variables) (cdr values)))))
  (list (iter variables values)))

(define (frame-variables frame)
  (map car (car frame)))

(define (frame-values frame)
  (map cdr (car frame)))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))


(define f (make-frame (list 'a 'b 'd) (list 1 2 4)))
(add-binding-to-frame! 'c 3 f)
(display f)
(frame-variables f)
(frame-values f)


(define f (make-frame '() '()))
(add-binding-to-frame! 'c 4 f)
(display f)
(frame-variables f)
(frame-values f)

(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan l)
	  (cond ((null? l)
			 (env-loop (enclosing-environment env)))
			((eq? var (caar l))
			 (cdar l))
			(else (scan (cdr l)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (car frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
	(define (scan l)
	  (cond ((null? l)
			 (env-loop (enclosing-environment env)))
			((eq? var (caar l))
			 (set-cdr! (car l) val))
			(else (scan (cdr l)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable -- SET!" var)
		(let ((frame (first-frame env)))
		  (scan (car frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (scan l)
	  (cond ((null? l)
			 (add-binding-to-frame! var val frame))
			((eq? var (caar l))
			 (set-cdr! (car l)  val))
			(else (scan (cdr l)))))
	(scan (car frame))))
		  

(define-variable! 'true true (extend-environment '(a)
							 '(1)
							 the-empty-environment))

(define (setup-environment2)
  (let ((initial-env
		 (extend-environment '(a)
							 '(1)
							 the-empty-environment)))
	(define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))


(define the-global-environment2 (setup-environment2))

(define-variable! 'b 1 the-global-environment2)
(display the-global-environment2)

(set-variable-value! 'b 2 the-global-environment2)
(display the-global-environment2)


(set-variable-value! 'd 2 the-global-environment2)

(lookup-variable-value 'a the-global-environment2)
(lookup-variable-value 'b the-global-environment2)
(display the-global-environment2)

;;ex4.12

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (car vals))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable -- SET!" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (scan vars vals)
	  (cond ((null? vars)
			 (add-binding-to-frame! var val frame))
			((eq? var (car vars))
			 (set-car! vals val))
			(else (scan (cdr vars) (cdr vals)))))
	(scan (frame-variables frame)
		  (frame-values frame))))


(driver-loop)
(define i 0)
(while (< i 10)
	   (display i)
	   (newline)
	   (set! i (+ i 1)))
end		   

(define (scan var val frame null-func eq-func)
  (define (iter vars vals)
	(cond ((null? vars)
		   (null-func var val))
		  ((eq? var (car vars))
			(eq-func vals val))
		  (else (iter
				   (cdr vars) (cdr vals)))))
  (iter  (frame-variables frame)
		 (frame-values frame)))


  
(define (lookup-variable-value var env)
  (define (null-func var val)
	(env-loop (enclosing-environment env)))
  (define (eq-func vals val)
	(car vals))
  (define (env-loop env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan 
		   var
		   0
		   frame
		   null-func
		   eq-func
		   ))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (null-func var val)
	(env-loop (enclosing-environment env)))
  (define (eq-func vals val)
	(set-car! vals val))
  (define (env-loop env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable -- SET!" var)
		(let ((frame (first-frame env)))
		  (scan 
		   var
		   val
		   frame
		   null-func
		   eq-func
		   ))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
	(define (null-func var val)
	  (add-binding-to-frame! var val frame))
	(define (eq-func vals val)
	  (set-car! vals val))
	(scan var val 
		  frame
		  null-func
		  eq-func
		  )))


(define (scan var vars vals)
  (cond ((null? vars) '())
        ((eq? var (car vars)) vals)
        (else
          (scan var (cdr vars) (cdr vals)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
                  (if (null? result-of-scan)
                      (env-loop (enclosing-environment env))
                      (car result-of-scan))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
                  (if (null? result-of-scan)
                      (env-loop (enclosing-environment env))
                      (set-car! result-of-scan val))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
       (let ((result-of-scan (scan var (frame-variables frame) (frame-values frame))))
            (if (null? result-of-scan)
                (add-binding-to-frame! var val frame)
                (set-car! result-of-scan val)))))


(driver-loop)
(define i 0)
(while (< i 10)
	   (display i)
	   (newline)
	   (set! i (+ i 1)))
end		   

;ex4.13
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 (eval-definition exp env))
		((unbind!? exp)
		 (eval-unbind exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 (eval (let->combination exp) env))
		((let*? exp)
		 (eval (let*->nested-lets exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((while? exp) (eval (while->if exp) env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (unbind!? exp)
  (tagged-list? exp 'unbind!))

(define (eval-unbind exp env)
  (unbind-variable! (cadr exp)
					env)
  'ok)

;手抜き
(define (unbind-variable! var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (set-car! vars '*garbage*)
			 )
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))


(driver-loop)
(define i 0)
(display i)
(unbind! i)
(display i)
end

;ex4.14
(driver-loop)
(define (my-map proc items)
  (if (null? items)
	  '()
	  (cons (proc (car items)) 
			(my-map proc (cdr items)))))
(my-map (lambda (x) (* x x))  '(1 2 3))

(map (lambda (x) (* x x))  '(1 2 3))
end

;ex4.16
;a
(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (let ((val (car vals)))
			   (if (eq? val '*unassigned*)
				   (error "Unassigned variable" var)
				   val)))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))


;b
;; (define (make-clauses-ex4.16 vars)
;;   (if (null? vars)
;; 	  '()
;; 	  (cons (list (car vars) ''*unassigned*) (make-clauses-ex4.16 (cdr vars)))))


;; (make-clauses-ex4.16 '(u v))
;; (make-clauses-ex4.16 '(u v w))

;; (define (make-sets-ex4.16 vars vals)
;;   (if (null? vars)
;; 	  '()
;; 	  (cons (list 'set! (car vars) (car vals)) (make-sets-ex4.16 (cdr vars) (cdr vals)))))


;; (make-sets-ex4.16 '() '())
;; (make-sets-ex4.16 '(u v) '(a b))

;; (make-sets-ex4.16 '(u v) '((+ 1 2) b))
;; (make-sets-ex4.16 '(u v) '((display 1) (display 2)))

;; (append (list 'let
;; 			(make-clauses-ex4.16 '(u v)))
;; 	  (make-sets-ex4.16 '(u v) '((display 1) (display 2))))

;; (eval 
;;  (append (list 'let
;; 			   (make-clauses-ex4.16 '(u v)))
;; 		 (make-sets-ex4.16 '(u v) '((display 1) (display 2))))
;;  the-global-environment)


;; (define (make-let-ex4.16 vars vals body)
;;   (if (null? vars)
;; 	  body
;; 	  (list (append (list 'let
;; 					(make-clauses-ex4.16 vars))
;; 			  (make-sets-ex4.16 vars vals)
;; 			  body))
;; 	  ))
				
  

;; (define (scan-out-defines proc-body)
;;   (define (iter body vars vals)
;; 	(if (null? body)
;; 		(make-let-ex4.16 vars vals body)
;; 		(let ((exp (car body)))
;; 		  (if (definition? exp)
;; 			  (iter 
;; 			   (cdr body)
;; 			   (cons (definition-variable exp) vars)
;; 			   (cons (definition-value exp) vals)

;; 			   )
;; 			  (make-let-ex4.16 vars vals body)))))
;;   (iter proc-body '() '()))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let binds body)
  (cons 'let (cons binds body)))

(define (make-assignment var val)
  (list 'set! var val))

(define (filter p ls)
  (if (null? ls)
      '()
      (if (p (car ls))
          (cons (car ls) (filter p (cdr ls)))
          (filter p (cdr ls)))))

(define (scan-out-defines body)
  (let ((defs (filter definition? body))
        (rest (filter (lambda (x) (not (definition? x))) body)))
    (if (null? defs)
        body
        (let ((vars (map definition-variable defs))
              (vals (map definition-value defs)))
          (list (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
                          (append (map make-assignment vars vals)
                                  rest)))))))



(scan-out-defines
 '((define u (display v)) (define v (+ 1 2)) (display "aaa")))


(eval 
 (scan-out-defines
  '((define u (display v)) (define v (+ 1 2)) (display "aaa")))
 the-global-environment)

(scan-out-defines
  '((define (even? n)
	 (if (= n 0)
		 true
		 (odd? (- n 1))))
  (define (odd? n)
	(if (= n 0)
		false
		(even? (- n 1))))
  (even? x)))
 


;c
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-body p) (caddr p))

;1
(define (make-procedure parameters body env)
;  (display (scan-out-defines body))
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-body p) (caddr p))

(driver-loop)
(let ((a 3) (b 2))
  (+ a b))
(let ((x 1))
  x)


(define (f x)
  (define (even? n)
	(if (= n 0)
		true
		(odd? (- n 1))))
  (define (odd? n)
	(if (= n 0)
		false
		(even? (- n 1))))
  (even? x))

(f 3)
end  
  
;2
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-body p) 
  ;(display (scan-out-defines (caddr p)))
  (scan-out-defines (caddr p)))


(driver-loop)
(let ((a 3) (b 2))
  (+ a b))
(let ((x 1))
  x)


(define (f x)
  (define (even? n)
	(if (= n 0)
		true
		(odd? (- n 1))))
  (define (odd? n)
	(if (= n 0)
		false
		(even? (- n 1))))
  (even? x))

(f 3)
end  
 
;ex 4.18 
;http://d.hatena.ne.jp/rucifer_hacker/20090331/1238480075

(let ((a 1))
  (define (f x)
	(define b (+ a x))
	(define a 5)
	(+ a b))
  (f 10))

;ex4.20

(define (f x)
  (letrec ((even? 
			(lambda (n)
			  (if (= n 0)
				  true
				  (odd? (- n 1)))))
		   (odd? 
			(lambda(n)
			  (if (= n 0)
				  false
				  (even? (- n 1))))))
	(even? x)))
(f 3)
			


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 (eval-definition exp env))
		((unbind!? exp)
		 (eval-unbind exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
		 (make-procedure (lambda-parameters exp)
						 (lambda-body exp)
						 env))
		((let? exp)
		 (eval (let->combination exp) env))
		((let*? exp)
		 (eval (let*->nested-lets exp) env))
		((letrec? exp)
		 (eval (letrec->let exp) env))
		((begin? exp)
		 (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((while? exp) (eval (while->if exp) env))
		((application? exp)
		 (apply (eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
		 (error "Unknown expression type -- EVAL" exp))))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((bindings (cadr exp))
		(body (cddr exp)))
	(let ((vars (map car bindings))
		  (vals (map cadr bindings)))
	  (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
					  (append (map make-assignment vars vals)
							  body)))))

(letrec->let 
 (quote
  (letrec ((even? 
			(lambda (n)
			  (if (= n 0)
				  true
				  (odd? (- n 1)))))
		   (odd? 
			(lambda(n)
			  (if (= n 0)
				  false
				  (even? (- n 1))))))
	(even? x))))


(driver-loop)
(define (f x)
  (letrec ((even? 
			(lambda (n)
			  (if (= n 0)
				  true
				  (odd? (- n 1)))))
		   (odd? 
			(lambda(n)
			  (if (= n 0)
				  false
				  (even? (- n 1))))))
	(even? x)))
(f 3)
end
;ex4.21

((lambda(n)
   ((lambda (fact)
	  (fact fact n))
	(lambda (ft k)
	  (if (= k 1)
		  1
		  (* k (ft ft  (- k 1)))))))
 10)


((lambda(n)
   (define (a ft k)
	 (display ft)
	 (if (= k 1)
		 1
		 (* k (ft ft  (- k 1)))))
   ((lambda (fact)
	  (fact fact n))
	a))
 10)



((lambda(n)
   ((lambda (fib)
	  (fib fib n))
	(lambda (fb k)
	  (cond ((= k 0) 0)
			((= k 1) 1)
			(else 
			 (+ (fb fb (- k 1)) (fb fb (- k 2))))))))
 10)
	 

(define (f x)
  ((lambda (even? odd?)
	 (even? even? odd? x))
   (lambda (ev? od? n)
	 (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
	 (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 2)
(f 3)

