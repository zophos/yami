(load "./5.2.ss")
;;4
;(define apply-in-underlying-scheme apply)

(define true #t)
(define false  #f)
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

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

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

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (get-global-environment)
  the-global-environment)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))


(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list '= =)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '> >)
		(list '< <)
		(list 'display display)
		(list 'newline newline)
		(list 'assoc assoc)
		(list 'eq? eq?)
		(list 'equal? equal?)
		(list 'list list)
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

(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (if (< (length vars) (length vals))
		  (error "Too many arguments supplied" vars vals)
		  (error "Too few arguments supplied" vars vals))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define (make-procedure parameters body env)
  (list ' procedure parameters body env))


;;5.4.1

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))




(define 
  eceval-body
  '(
	;; 5.4.4

	read-eval-print-loop
	(perform (op initialize-stack))
	(perform 
	 (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
	print-result
	(perform
	 (op announce-output) (const ";;; EC-Eval value:"))
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

	;;5.4.1
	eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg
							exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))
	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))
	

	ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
	ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
	ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
	ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure)
			(reg unev) (reg exp) (reg env))
	(goto (reg continue))

	ev-application
	(save continue)
	(save env)
	(assign unev (op operands) (reg exp))
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))

	ev-appl-did-operator
	(restore unev)
	(restore env)
	(assign argl (op empty-arglist))
	(assign proc (reg val))
	(test (op no-operands?) (reg unev))
	(branch (label apply-dispatch))
	(save proc)

	ev-appl-operand-loop
	(save argl)
	(assign exp (op first-operand) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label ev-appl-last-arg))
	(save env)
	(save unev)
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label eval-dispatch))

	ev-appl-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label ev-appl-operand-loop))
	
	ev-appl-last-arg
	(assign continue (label ev-appl-accum-last-arg))
	(goto (label eval-dispatch))
	ev-appl-accum-last-arg
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(restore proc)
	(goto (label apply-dispatch))

	apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))

	primitive-apply
	(assign val (op apply-primitive-procedure)
			(reg proc)
			(reg argl))
	(restore continue)
	(goto (reg continue))
	
	compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
			(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

	;;5.4.2  

	ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))

	ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
	ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
	ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))

	;;5.4.3
	ev-if
	(save exp)
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch))

	ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
	ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
	ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))
	
	ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev)
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch))
	ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev)
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch))
	ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	;;5.4.4

	
	unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	unknown-procedure-type
	(restore continue)
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
	))

(define eceval-operations
  (list
   (list 'adjoin-arg adjoin-arg)
   (list 'announce-output announce-output)
   (list 'application? application?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'assignment-value assignment-value)
   (list 'assignment-variable assignment-variable)
   (list 'assignment? assignment?)
   (list 'begin-actions begin-actions)
   (list 'begin? begin?)
   (list 'compound-procedure? compound-procedure?)
   (list 'define-variable! define-variable!)
   (list 'definition-value definition-value)
   (list 'definition-variable definition-variable)
   (list 'definition? definition?)
   (list 'empty-arglist empty-arglist)
   (list 'extend-environment extend-environment)
   (list 'first-exp first-exp)
   (list 'first-operand first-operand)
   (list 'get-global-environment get-global-environment)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'if-predicate if-predicate)
   (list 'if? if?)
   (list 'lambda-body lambda-body)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda? lambda?)
   (list 'last-exp? last-exp?)
   (list 'last-operand? last-operand?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'make-procedure make-procedure)
   (list 'no-operands? no-operands?)
   (list 'operands operands)
   (list 'operator operator)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-parameters procedure-parameters)
   (list 'prompt-for-input prompt-for-input)
   (list 'quoted? quoted?)
   (list 'read read)
   (list 'rest-exps rest-exps)
   (list 'rest-operands rest-operands)
   (list 'self-evaluating? self-evaluating?)
   (list 'set-variable-value! set-variable-value!)
   (list 'text-of-quotation text-of-quotation)
   (list 'true? true?)
   (list 'user-print user-print)
   (list 'variable? variable?)
   )
)


(define the-global-environment (setup-environment))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   eceval-body))

;(start eceval)
;; (define (append x y)
;;   (if (null? x)
;; 	  y
;; 	  (cons (car x)
;; 			(append (cdr x) y))))

;; (append '(a b c) '(d e f))
;; quit
;ex5.23
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clauses? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))



(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


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

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (cons (make-lambda
                 (map car (cadr exp)) (cddr exp))
                (map cadr (cadr exp))))

(define eceval-operations
  (list
   (list 'adjoin-arg adjoin-arg)
   (list 'announce-output announce-output)
   (list 'application? application?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'assignment-value assignment-value)
   (list 'assignment-variable assignment-variable)
   (list 'assignment? assignment?)
   (list 'begin-actions begin-actions)
   (list 'begin? begin?)
   (list 'compound-procedure? compound-procedure?)
   (list 'define-variable! define-variable!)
   (list 'definition-value definition-value)
   (list 'definition-variable definition-variable)
   (list 'definition? definition?)
   (list 'empty-arglist empty-arglist)
   (list 'extend-environment extend-environment)
   (list 'first-exp first-exp)
   (list 'first-operand first-operand)
   (list 'get-global-environment get-global-environment)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'if-predicate if-predicate)
   (list 'if? if?)
   (list 'lambda-body lambda-body)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda? lambda?)
   (list 'last-exp? last-exp?)
   (list 'last-operand? last-operand?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'make-procedure make-procedure)
   (list 'no-operands? no-operands?)
   (list 'operands operands)
   (list 'operator operator)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-parameters procedure-parameters)
   (list 'prompt-for-input prompt-for-input)
   (list 'quoted? quoted?)
   (list 'read read)
   (list 'rest-exps rest-exps)
   (list 'rest-operands rest-operands)
   (list 'self-evaluating? self-evaluating?)
   (list 'set-variable-value! set-variable-value!)
   (list 'text-of-quotation text-of-quotation)
   (list 'true? true?)
   (list 'user-print user-print)
   (list 'variable? variable?)
   (list 'cond? cond?)
   (list 'cond->if cond->if)
   (list 'let? let?)
   (list 'let->combination let->combination)
   )
)

(define 
  eceval-body
  '(
	;; 5.4.4

	read-eval-print-loop
	(perform (op initialize-stack))
	(perform 
	 (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
	print-result
	(perform
	 (op announce-output) (const ";;; EC-Eval value:"))
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

	;;5.4.1
	eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))

	;;ex5.23
	(test (op cond?) (reg exp))
	(branch (label ev-cond))
	(test (op let?) (reg exp))
	(branch (label ev-let))

	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))
	

	;;ex5.23
	ev-cond
	(assign exp (op cond->if) (reg exp))
	(goto (label eval-dispatch))

	ev-let
	(assign exp (op let->combination) (reg exp))
	(goto (label eval-dispatch))


	ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
	ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
	ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
	ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure)
			(reg unev) (reg exp) (reg env))
	(goto (reg continue))

	ev-application
	(save continue)
	(save env)
	(assign unev (op operands) (reg exp))
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))

	ev-appl-did-operator
	(restore unev)
	(restore env)
	(assign argl (op empty-arglist))
	(assign proc (reg val))
	(test (op no-operands?) (reg unev))
	(branch (label apply-dispatch))
	(save proc)

	ev-appl-operand-loop
	(save argl)
	(assign exp (op first-operand) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label ev-appl-last-arg))
	(save env)
	(save unev)
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label eval-dispatch))

	ev-appl-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label ev-appl-operand-loop))
	
	ev-appl-last-arg
	(assign continue (label ev-appl-accum-last-arg))
	(goto (label eval-dispatch))
	ev-appl-accum-last-arg
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(restore proc)
	(goto (label apply-dispatch))

	apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))

	primitive-apply
	(assign val (op apply-primitive-procedure)
			(reg proc)
			(reg argl))
	(restore continue)
	(goto (reg continue))
	
	compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
			(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

	;;5.4.2  

	ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))

	ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
	ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
	ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))

	;;5.4.3
	ev-if
	(save exp)
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch))

	ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
	ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
	ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))
	
	ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev)
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch))
	ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev)
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch))
	ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	;;5.4.4

	
	unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	unknown-procedure-type
	(restore continue)
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
	))


(define the-global-environment (setup-environment))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   eceval-body))

;; (start eceval)
;; (if (eq? 0 0)
;; 	0
;; 	1)



;; (define a 0)
;; (eq? a 1)
;; (if (eq? a 0)
;; 	0
;; 	1)

;; (cond ((eq? 0 1) 1)
;; 	  ((eq? 0 2) 2)
;; 	  (else 0))
;; (define a 0)

;; (cond ((eq? a 1) 1)
;; 	  ((eq? a 2) 2)
;; 	  (else a))

;; (define (nyo a)
;;   (cond ((eq? a 1) 1)
;; 		((eq? a 2) 2)
;; 		(else a)))
;; (nyo 1)
;; (nyo 2)
;; (nyo 3)  

;; (define (nya)
;;   (let ((a 0))
;; 	(cond ((eq? a 1) 1)
;; 		  ((eq? a 2) 2)
;; 		  (else a))))
  
;; (nya)
;; (define (nya)
;;   (let ((a 1))
;; 	(cond ((eq? a 1) 1)
;; 		  ((eq? a 2) 2)
;; 		  (else a))))
;; (nya)
;; #q

;;ex 5.24

(define eceval-operations
  (list
   (list 'adjoin-arg adjoin-arg)
   (list 'announce-output announce-output)
   (list 'application? application?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'assignment-value assignment-value)
   (list 'assignment-variable assignment-variable)
   (list 'assignment? assignment?)
   (list 'begin-actions begin-actions)
   (list 'begin? begin?)
   (list 'compound-procedure? compound-procedure?)
   (list 'define-variable! define-variable!)
   (list 'definition-value definition-value)
   (list 'definition-variable definition-variable)
   (list 'definition? definition?)
   (list 'empty-arglist empty-arglist)
   (list 'extend-environment extend-environment)
   (list 'first-exp first-exp)
   (list 'first-operand first-operand)
   (list 'get-global-environment get-global-environment)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'if-predicate if-predicate)
   (list 'if? if?)
   (list 'lambda-body lambda-body)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda? lambda?)
   (list 'last-exp? last-exp?)
   (list 'last-operand? last-operand?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'make-procedure make-procedure)
   (list 'no-operands? no-operands?)
   (list 'operands operands)
   (list 'operator operator)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-parameters procedure-parameters)
   (list 'prompt-for-input prompt-for-input)
   (list 'quoted? quoted?)
   (list 'read read)
   (list 'rest-exps rest-exps)
   (list 'rest-operands rest-operands)
   (list 'self-evaluating? self-evaluating?)
   (list 'set-variable-value! set-variable-value!)
   (list 'text-of-quotation text-of-quotation)
   (list 'true? true?)
   (list 'user-print user-print)
   (list 'variable? variable?)
   (list 'cond? cond?)
   (list 'cond-clauses cond-clauses)
   (list 'cond-else-clauses? cond-else-clauses?)
   (list 'cond-predicate cond-predicate)
   (list 'cond-actions cond-actions)
   (list 'let? let?)
   (list 'let->combination let->combination)
   (list 'null? null?)
   (list 'car car)
   (list 'cdr cdr)
   )
)

(define 
  eceval-body
  '(
	;; 5.4.4

	read-eval-print-loop
	(perform (op initialize-stack))
	(perform 
	 (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
	print-result
	(perform
	 (op announce-output) (const ";;; EC-Eval value:"))
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

	;;5.4.1
	eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))

	;;ex5.23
	(test (op cond?) (reg exp))
	(branch (label ev-cond))
	(test (op let?) (reg exp))
	(branch (label ev-let))

	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))
	

	;;ex5.24
	ev-cond
	(save continue)
	(assign unev (op cond-clauses) (reg exp))
	
	ev-cond-loop
	(test (op null?) (reg unev))
	(branch (label ev-cond-null))
	(assign exp (op car) (reg unev))
	(test (op cond-else-clauses?) (reg exp))
	(branch (label ev-cond-actions))
	(save exp)
	(assign exp (op cond-predicate) (reg exp))
	(save unev)
	(save env)
	(assign continue (label ev-cond-predicate-done))
	(goto (label eval-dispatch))
	
	ev-cond-predicate-done
	(restore env)
	(restore unev)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-cond-actions))
	(assign unev (op cdr) (reg unev))
	(goto (label ev-cond-loop))

	ev-cond-actions
	(assign unev (op cond-actions) (reg exp))
	(goto (label ev-sequence))
	
	ev-cond-null
	(assign val (const #f))
	(restore continue)
	(goto (reg continue))
			

	




	;;ex5.23
	ev-let
	(assign exp (op let->combination) (reg exp))
	(goto (label eval-dispatch))


	ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
	ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
	ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
	ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure)
			(reg unev) (reg exp) (reg env))
	(goto (reg continue))

	ev-application
	(save continue)
	(save env)
	(assign unev (op operands) (reg exp))
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))

	ev-appl-did-operator
	(restore unev)
	(restore env)
	(assign argl (op empty-arglist))
	(assign proc (reg val))
	(test (op no-operands?) (reg unev))
	(branch (label apply-dispatch))
	(save proc)

	ev-appl-operand-loop
	(save argl)
	(assign exp (op first-operand) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label ev-appl-last-arg))
	(save env)
	(save unev)
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label eval-dispatch))

	ev-appl-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label ev-appl-operand-loop))
	
	ev-appl-last-arg
	(assign continue (label ev-appl-accum-last-arg))
	(goto (label eval-dispatch))
	ev-appl-accum-last-arg
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(restore proc)
	(goto (label apply-dispatch))

	apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))

	primitive-apply
	(assign val (op apply-primitive-procedure)
			(reg proc)
			(reg argl))
	(restore continue)
	(goto (reg continue))
	
	compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
			(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

	;;5.4.2  

	ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))

	ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
	ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
	ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))

	;;5.4.3
	ev-if
	(save exp)
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch))

	ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
	ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
	ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))
	
	ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev)
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch))
	ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev)
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch))
	ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	;;5.4.4

	
	unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	unknown-procedure-type
	(restore continue)
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
	))



(define the-global-environment (setup-environment))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   eceval-body))

;; (start eceval)
;; (define a 0)

;; (cond ((eq? a 1) 1)
;; 	  ((eq? a 2) 2)
;; 	  (else a))

;; (define (nyo a)
;;   (cond ((eq? a 1) 1)
;; 		((eq? a 2) 2)
;; 		(else a)))
;; (nyo 1)
;; (nyo 2)
;; (nyo 3)  

;; (define (nya)
;;   (let ((a 0))
;; 	(cond ((eq? a 1) 1)
;; 		  ((eq? a 2) 2)
;; 		  (else a))))
  
;; (nya)
;; (define (nya)
;;   (let ((a 1))
;; 	(cond ((eq? a 1) 1)
;; 		  ((eq? a 2) 2)
;; 		  (else a))))
;; (nya)
;; #q

;ex5.25
;;http://d.hatena.ne.jp/umeaji/20080221
;;http://himoiku.cocolog-nifty.com/blog/2008/05/sicp525_f6db.html



;;;;;; performance ;;;;;;;
(define 
  eceval-body
  '(
	;; 5.4.4

	read-eval-print-loop
	(perform (op initialize-stack))
	(perform 
	 (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
	print-result
	(perform
	 (op print-stack-statistics))
	(perform
	 (op announce-output) (const ";;; EC-Eval value:"))
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))

	;;5.4.1
	eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))

	;;ex5.23
	(test (op cond?) (reg exp))
	(branch (label ev-cond))
	(test (op let?) (reg exp))
	(branch (label ev-let))

	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))
	

	;;ex5.24
	ev-cond
	(save continue)
	(assign unev (op cond-clauses) (reg exp))
	
	ev-cond-loop
	(test (op null?) (reg unev))
	(branch (label ev-cond-null))
	(assign exp (op car) (reg unev))
	(test (op cond-else-clauses?) (reg exp))
	(branch (label ev-cond-actions))
	(save exp)
	(assign exp (op cond-predicate) (reg exp))
	(save unev)
	(save env)
	(assign continue (label ev-cond-predicate-done))
	(goto (label eval-dispatch))
	
	ev-cond-predicate-done
	(restore env)
	(restore unev)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-cond-actions))
	(assign unev (op cdr) (reg unev))
	(goto (label ev-cond-loop))

	ev-cond-actions
	(assign unev (op cond-actions) (reg exp))
	(goto (label ev-sequence))
	
	ev-cond-null
	(assign val (const #f))
	(restore continue)
	(goto (reg continue))
			

	




	;;ex5.23
	ev-let
	(assign exp (op let->combination) (reg exp))
	(goto (label eval-dispatch))


	ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
	ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
	ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
	ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure)
			(reg unev) (reg exp) (reg env))
	(goto (reg continue))

	ev-application
	(save continue)
	(save env)
	(assign unev (op operands) (reg exp))
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))

	ev-appl-did-operator
	(restore unev)
	(restore env)
	(assign argl (op empty-arglist))
	(assign proc (reg val))
	(test (op no-operands?) (reg unev))
	(branch (label apply-dispatch))
	(save proc)

	ev-appl-operand-loop
	(save argl)
	(assign exp (op first-operand) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label ev-appl-last-arg))
	(save env)
	(save unev)
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label eval-dispatch))

	ev-appl-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label ev-appl-operand-loop))
	
	ev-appl-last-arg
	(assign continue (label ev-appl-accum-last-arg))
	(goto (label eval-dispatch))
	ev-appl-accum-last-arg
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(restore proc)
	(goto (label apply-dispatch))

	apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))

	primitive-apply
	(assign val (op apply-primitive-procedure)
			(reg proc)
			(reg argl))
	(restore continue)
	(goto (reg continue))
	
	compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
			(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

	;;5.4.2  

	ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))

	ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
	ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
	ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))

	;;5.4.3
	ev-if
	(save exp)
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch))

	ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
	ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
	ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))
	
	ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev)
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch))
	ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev)
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch))
	ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform
	 (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))

	;;5.4.4

	
	unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	unknown-procedure-type
	(restore continue)
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))

	signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
	))



(define the-global-environment (setup-environment))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   eceval-body))

;; (start eceval)
;; (define (factorial n)
;;   (if (= n 1)
;; 	  1
;; 	  (* (factorial (- n 1)) n)))
;; (factorial 5)
;; (factorial 6)

;; (define (factorial-iter n)
;;   (define (iter product counter)
;; 	(if (> counter n)
;; 		product
;; 		(iter (* counter product)
;; 			  (+ counter 1))))
;;   (iter 1 1))
;; (factorial-iter 5)
;; (factorial-iter 6)

;; #q
;;ex5.26
;;http://www.serendip.ws/archives/3510

;;ex5.27
;;http://www.serendip.ws/archives/3515

;;ex5.28
;;http://www.serendip.ws/archives/3520

;;ex5.29
;;http://www.serendip.ws/archives/3525
;; (start eceval)
;; (define (fib n)
;;   (if (< n 2)
;;       n
;;       (+ (fib (- n 1)) (fib (- n 2)))))
;; (fib 3)
;; (fib 4)
;; (fib 5)
;; (fib 6)
;; #q

;;ex5.30
;;http://www.serendip.ws/archives/3572
