;;2度評価しないこと
(define apply-in-underlying-scheme apply)
(define eval-in-underlying-scheme eval)
;;2度評価しないこと
(define true #t)
(define false #f)

(define (actual-value exp env)
;  (display exp)
  (force-it (eval exp env)))


(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-procedure 
		  procedure 
		  (list-of-arg-values arguments env)))
		((compound-procedure? procedure)
		 (eval-sequence
		  (procedure-body procedure)
		  (extend-environment
		   (procedure-parameters procedure)
		   (list-of-delayed-args arguments env)
		   (procedure-environment procedure))))
		(else
		 (error
		  "Unknown procedure type -- APPLY" procedure))))



(define (list-of-arg-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (actual-value (first-operand exps) env)
			(list-of-arg-values (rest-operands exps)
								env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
	  '()
	  (cons (delay-it (first-operand exps) env)
			(list-of-delayed-args (rest-operands exps)
								  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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
			(make-let '() body)
			(make-let (list (car bindings))
					  (iter (cdr bindings)))
		))
	  (iter bindings)
	  )))
  



(define (let->combination exp env)
  (if (pair? (cadr exp))
	  (cons (make-lambda
			 (map car (cadr exp)) (cddr exp))
			(map cadr (cadr exp)))
	  (begin
		;(display (cons (cadr exp) (map car (caddr exp))))
		;(newline)
		;(display (cons (cadr exp) (map cadr (caddr exp))))
		;(newline)
		(eval-definition
		 (list 'define (cons (cadr exp) (map car (caddr exp)))
			   (cadddr exp)) env)
		(cons (cadr exp)
			  (map cadr (caddr exp))))))
		

(define (let->combination exp)
  (if (pair? (cadr exp))
	  (cons (make-lambda
			 (map car (cadr exp)) (cddr exp))
			(map cadr (cadr exp)))
	  (make-begin
	   (list
		(list 'define (cons (cadr exp) (map car (caddr exp)))
			  (cadddr exp))
		(cons (cadr exp)
			  (map cadr (caddr exp)))))))
		



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


;c
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-body p) (caddr p))

;1
(define (make-procedure parameters body env)
;  (display (scan-out-defines body))
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-body p) (caddr p))
  
;2
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-body p) 
  (scan-out-defines (caddr p)))


(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((bindings (cadr exp))
		(body (cddr exp)))
	(let ((vars (map car bindings))
		  (vals (map cadr bindings)))
	  (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
					  (append (map make-assignment vars vals)
							  body)))))

;;4.2.2



(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define the-global-environment (setup-environment))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 (eval-definition exp env))
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
		 (apply (actual-value (operator exp) env)
				(operands exp)
				env))
		(else
		 (error "Unknown expression type -- EVAL" exp))))



(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
;	(display input)
	(let ((output 
		   (actual-value input the-global-environment)))
	  (announce-output output-prompt)
	  (user-print output)))
  (driver-loop))

(define (force-it obj)
  (if (thunk? obj)
	  (actual-value (thunk-value obj) (thunk-env obj))
	  obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
		 (let ((result (actual-value
						(thunk-exp obj)
						(thunk-env obj))))
		   (set-car! obj 'evaluated-thunk)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		((evaluated-thunk? obj)
		 (thunk-value obj))
		(else obj)))

(driver-loop)
(define (try a b)
  (if (= a 0) 1 b))
(try 0 (/ 1 0))
end

;4.27

(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define w (id (id 10)))
count
w
count


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define w (id (id 10)))
count
w
count
end

;ex4.28

(driver-loop)
((if true + -) 1 2)
end
;ex4.29
(define (force-it obj)
  (if (thunk? obj)
	  (actual-value (thunk-value obj) (thunk-env obj))
	  obj))


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))
(square (id 10))
count
end



(define (force-it obj)
  (cond ((thunk? obj)
		 (let ((result (actual-value
						(thunk-exp obj)
						(thunk-env obj))))
		   (set-car! obj 'evaluated-thunk)
		   (set-car! (cdr obj) result)
		   (set-cdr! (cdr obj) '())
		   result))
		((evaluated-thunk? obj)
		 (thunk-value obj))
		(else obj)))


(driver-loop)
(define count 0)
count
(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))
(square (id 10))
count
end

;ex4.30
(driver-loop)
(define (for-each proc items)
  (if (null? items)
	  'done
	  (begin (proc (car items))
			 (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
		  (list 57 321 88))
end

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
		(else (eval (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

(driver-loop)
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
	e 
	x)
  (p (set! x (cons x '(2)))))
(p1 1)
(p2 1)
end

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
		(else (actual-value (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

(driver-loop)
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
	e 
	x)
  (p (set! x (cons x '(2)))))
(p1 1)
(p2 1)
end

;2009/10/05

;4.2.3
(driver-loop)
(define (cons x y)
  (lambda(m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(define (list-ref items n)
  (if (= n 0)
	  (car items)
	  (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
	  '()
	  (cons (proc (car items))
			(map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
	   items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
		((null? list2) list1)
		(else (cons (+ (car list1) (car list2))
					(add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))

(define integers (cons 1 (add-lists ones integers)))

(list-ref integers 17)

(define (integral integrand initial-value dt)
  (define int
	(cons initial-value
		  (add-lists (scale-list integrand dt)
					 int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

(list-ref (solve (lambda (x) x) 1 0.001) 1000)
end
;ex4.32
;この定義ではconsのcarの部分も遅延評価する
;以前は (cons-stream <a> <b>) => (cons <a> (delay <b>))
;x が未定義の場合に (cons-stream x <b>)はできない.

(driver-loop)
(define y (cons x (* x x)))
(define x 2)
(display (car y))
(display (cdr y))
end

;ex4.33

(driver-loop)
(car '(a b c))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp env))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) 
		 (eval-definition exp env))
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
		 (apply (actual-value (operator exp) env)
				(operands exp)
				env))
		(else
		 (error "Unknown expression type -- EVAL" exp))))


(define (text-of-quotation exp env)
  (if (list? (cadr exp))
      (eval (make-quotation-list (cadr exp)) env)
      (cadr exp)))

(define (make-quotation-list l)
  (if (null? l)
      '()
      (let ((first-list (car l))
            (rest-list (cdr l)))
           (list 'cons (list 'quote first-list) 
				 (make-quotation-list rest-list)))))

(driver-loop)
(car '(a b c))
(cdr '(a b c))
end

(driver-loop)
(car '((b c)))
(car (car '((b c))))
(car (car (cdr '(a 
				 (b c)
				 )
			   )))


