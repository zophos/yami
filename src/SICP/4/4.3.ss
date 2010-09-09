`;;2度評価しない
(define apply-in-underlying-scheme apply)
;;;
;(define eval-in-underlying-scheme eval)
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cadr cadr)
		(list 'caddr caddr)
		(list 'caar caar)
		(list 'cdar cdar)
		(list 'cons cons)
		(list 'null? null?)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '= =)
		(list '< <)
		(list '> >)
		(list '<= <=)
		(list '>= >=)
		(list 'display display)
		(list 'newline newline)
		(list 'assoc assoc)
		(list 'eq? eq?)
		(list 'equal? equal?)
		(list 'list list)
		(list 'map map)
		(list 'not not)
		(list 'remainder remainder)
		(list 'member member)
		(list 'memq memq)
		(list 'abs abs)
		(list 'length length)
		(list 'even? even?)
		(list 'odd? odd?)
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
							 the-empty-environment))
		)
	(define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))

(define the-global-environment (setup-environment))


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

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (cons (make-lambda
		 (map car (cadr exp)) (cddr exp))
		(map cadr (cadr exp))))


(define (and? exp)
  (tagged-list? exp 'and))

(define (analyze-and exp)
  (lambda (env succeed fail)
	(analyze-and-conds (cdr exp) env succeed fail)))

(define (analyze-and-conds conds env succeed fail)
  (if (null? conds)
	  (succeed true fail)
	  (let ((cond-proc (analyze (car conds))))
		(cond-proc env
				   (lambda (cond-value fail2)
					 (if (true? cond-value)
						 (analyze-and-conds (cdr conds) env succeed fail2)
						 (succeed false fail)))
				   fail))))
				   



(define (or? exp)
  (tagged-list? exp 'or))

(define (analyze-or exp)
  (lambda (env succeed fail)
	(analyze-or-conds (cdr exp) env succeed fail)))

(define (analyze-or-conds conds env succeed fail)
  (if (null? conds)
	  (succeed false fail)
	  (let ((cond-proc (analyze (car conds))))
		(cond-proc env
				   (lambda (cond-value fail2)
					 (if (true? cond-value)
						 (succeed true fail)
						 (analyze-and-conds (cdr conds) env succeed fail2)))
				   fail))))
				   


(define (analyze exp)
  (cond ((self-evaluating? exp)
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((lambda? exp) (analyze-lambda exp))
		((let? exp)
		 (analyze (let->combination exp)))
		((begin? exp) 
		 (display                (analyze-sequence (begin-actions exp)))
		 (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((amb? exp) (analyze-amb exp))
		((and? exp) (analyze-and exp))
		((or? exp) (analyze-or exp))
		((application? exp) (analyze-application exp))
		(else
		 (error "Unknown expression type -- ANALYZE" exp))))




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

;;;;


(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))


(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
	(succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
	(lambda (env succeed fail)
	  (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
	(succeed (lookup-variable-value exp env)
			 fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
		(bproc (analyze-sequence (lambda-body exp))))
	(lambda (env succeed fail)
	  (succeed (make-procedure vars bproc env)
			   fail))))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
		(cproc (analyze (if-consequent exp)))
		(aproc (analyze (if-alternative exp))))
	(lambda (env succeed fail)
	  (pproc env
			 (lambda (pred-value fail2)
			   (if (true? pred-value)
				   (cproc env succeed fail2)
				   (aproc env succeed fail2)))
			 fail))))


(define (analyze-sequence exps)
  (define (sequentially a b)
	(lambda(env succeed fail)
	  (a env 
		 (lambda (a-value fail2)
		   (b env succeed fail2))
		 fail)))
  (define (loop first-proc rest-procs)
	(if (null? rest-procs)
		first-proc
		(loop (sequentially first-proc (car rest-procs))
			  (cdr rest-procs))))
  (let ((procs (map analyze exps)))
	(if (null? procs)
		(error "Empty sequence -- ANALYZE"))
	(loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
		(vproc (analyze (definition-value exp))))
	(lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)
			   (define-variable! var val env)
			   (succeed 'ok fail2))
			 fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
	(lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)
			   (let ((old-value
					  (lookup-variable-value var env)))
				 (set-variable-value! var val env)
				 (succeed 'ok
						  (lambda()
							(set-variable-value! var
												 old-value
												 env)
							(fail2)))))
			 fail))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
		(aprocs (map analyze (operands exp))))
	(lambda (env succeed fail)
	  (pproc env
			 (lambda (proc fail2)
			   (get-args aprocs
						 env
						 (lambda (args fail3)
						   (execute-application
							proc args succeed fail3))
						 fail2))
			 fail))))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
	  (succeed '() fail)
	  ((car aprocs) env
	   (lambda (arg fail2)
		 (get-args (cdr aprocs)
				   env
				   (lambda (args fail3)
					 (succeed (cons arg args)
							  fail3))
				   fail2))
	   fail)))



(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
		 (succeed (apply-primitive-procedure proc args)
				  fail))
		((compound-procedure? proc)
		 ((procedure-body proc)
		  (extend-environment (procedure-parameters proc)
							  args
							  (procedure-environment proc))
		  succeed
		  fail))
		(else
		 (error
		  "Unknown procedure type -- EXECUTE-APPLICATION"
		  proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
	(lambda (env succeed fail)
	  (define (try-next choices)
		(if (null? choices)
			(fail)
			((car choices) env
			 succeed
			 (lambda ()
			   (try-next (cdr choices))))))
	  (try-next cprocs))))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
	(prompt-for-input input-prompt)
	(let ((input (read)))
	  (if (eq? input 'try-again)
		  (try-again)
		  (begin 
			(newline)
			(display ";;; Starting a new problem ")
			(ambeval input 
					 the-global-environment
					 (lambda (val next-alternative)
					   (announce-output output-prompt)
					   (user-print val)
					   (internal-loop next-alternative))
					 (lambda ()
					   (announce-output
						";;; There are no more value of")
					   (user-print input)
					   (driver-loop)))))))
  (internal-loop 
   (lambda()
	 (newline)
	 (display ";;; There is no current problem")
	 (driver-loop))))

(driver-loop)
(list (amb 1 2 3) (amb 'a 'b))
try-again
try-again
try-again
try-again
try-again

try-again
end


(driver-loop)
(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
		(b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))


(prime-sum-pair '(1 3 5 8) '(20 35 110))

try-again
try-again
try-again
try-again

(prime-sum-pair '(19 27 30) '(11 36 58))

end


;ex4.35-36
(driver-loop)
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between low high)))
	  (let ((k (an-integer-between low high)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

(a-pythagorean-triple-between 3 13)
try-again
try-again
try-again
try-again
try-again
try-again
try-again


(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 5)))
	(let ((i (an-integer-between 1 k)))
	  (let ((j (an-integer-between 1 k)))
		(require (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

(a-pythagorean-triple)
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
end
;ex4.37
;kについて探索しないでよい

(driver-loop)

(define (distinct? items)
  (cond ((null? items) true)
		((null? (cdr items)) true)
		((member (car items) (cdr items)) false)
		(else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
		(cooper (amb 1 2 3 4 5))
		(fletcher (amb 1 2 3 4 5))
		(miller (amb 1 2 3 4 5))
		(smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))

(multiple-dwelling)
try-again		  
end


(driver-loop)
(define (multiple-dwelling2)
  (let ((baker (amb 1 2 3 4 5))
		(cooper (amb 1 2 3 4 5))
		(fletcher (amb 1 2 3 4 5))
		(miller (amb 1 2 3 4 5))
		(smith (amb 1 2 3 4 5)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))
	(require (not (= baker 5)))
	(require (not (= cooper 1)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(require (> miller cooper))
;	(require (not (= (abs (- smith fletcher)) 1)))
	(require (not (= (abs (- fletcher cooper)) 1)))
	(list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))

(multiple-dwelling2)
try-again		  
try-again		  
try-again		  
try-again		  
try-again		  

end




(driver-loop)
(define (multiple-dwelling3)
  (let ((fletcher (amb 1 2 3 4 5)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(let ((cooper (amb 1 2 3 4 5)))
	  (require (not (= cooper 1)))
	  (require (not (= (abs (- fletcher cooper)) 1)))
	  (let ((baker (amb 1 2 3 4 5))
			(miller (amb 1 2 3 4 5))
			(smith (amb 1 2 3 4 5)))
		(require (not (= baker 5)))
		(require (> miller cooper))
		(require (not (= (abs (- smith fletcher)) 1)))
		(require
		 (distinct? (list baker cooper fletcher miller smith)))
		
		(list (list 'baker baker)
			  (list 'cooper cooper)
			  (list 'fletcher fletcher)
			  (list 'miller miller)
			  (list 'smith smith))))))

(multiple-dwelling3)
try-again		  

end

;ex4.41



(define (distinct? items)
  (cond ((null? items) true)
		((null? (cdr items)) true)
		((member (car items) (cdr items)) false)
		(else (distinct? (cdr items)))))


(define (multiple-dwelling4-condition items)
  (let ((baker (car items))
		(cooper (cadr items))
		(fletcher (caddr items))
		(miller (cadddr items))
		(smith  (car (cddddr items))))
	(cond ((= fletcher 5) false)
		  ((= fletcher 1) false)
		  ((= cooper 1) false)
		  ((= (abs (- fletcher cooper)) 1) false)
		  ((= baker 5) false)
		  ((< miller cooper) false)
		  ((= (abs (- smith fletcher)) 1) false)
;		  ((not (distinct? items)) false)
		  (else true))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (permutations s)
  (if (null? s) (list '())
	  (flatmap (lambda (x)
				 (map (lambda (p) (cons x p))
					  (permutations (remove x s))))
			   s)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else  (filter predicate (cdr sequence)))))



(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

;(permutations (list 1 2 3))

(define (multiple-dwelling4)
  (filter multiple-dwelling4-condition
		  (permutations (list 1 2 3 4 5))))

(multiple-dwelling4)

(driver-loop)
(and true true)
(and true false)
(and)
(or true false)
(or true true)
(or false true)
(or false false)
end


(driver-loop)

(define (xor a b)
  (if a (not b) b))

(define (require-ex4-42 cond1 cond2)
  (require (or (and cond1 (not cond2))
			   (and (not cond1) cond2))))

(define (ex4-42)
  (let ((betty (amb 1 2 3 4 5))
		(ethel (amb 1 2 3 4 5))
		(joan (amb 1 2 3 4 5))
		(kitty (amb 1 2 3 4 5))
		(mary (amb 1 2 3 4 5)))
	(require (xor  (= kitty 2) (= betty 3)))
	(require (xor  (= ethel 1) (= joan 2)))
	(require (xor  (= joan 3) (= ethel 5)))
	(require (xor  (= kitty 2) (= mary 4)))
	(require (xor  (= mary 4) (= betty 1)))
	(require
	 (distinct? 	(list betty ethel joan kitty mary)))
	(list betty ethel joan kitty mary)))

(ex4-42)
try-again
end

(driver-loop)

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
       (cons (car lst) (filter pred (cdr lst)))
       (filter pred (cdr lst)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
end

(driver-loop)
(define (ex4-43)
  (let (
		(moore (cons 'lorna 'mary-ann))
		(downing (cons 'melissa (amb 'lorna 'rosalind 'gabrielle)))
		(hall (cons 'rosalind (amb 'lorna 'gabrielle)))
		(barnacle (cons 'gabrielle 'melissa))
		(parker (cons 'mary-ann (amb 'lorna 'rosalind 'gabrielle)))
		)
	(let ((fathers 
		   (list moore downing hall barnacle parker)))
;	  (require
;	   (distinct? (map car fathers)))
	  (require
		(distinct? (map cdr fathers)))
	  (require 
	   (eq? (cdr parker)
			(car (car (filter (lambda (x) (eq? (cdr x) 'gabrielle)) fathers)))))
	  (list 'moore moore 'downing downing 'hall hall 'barnacle barnacle 'parker parker))))


(ex4-43)
try-again
end


(driver-loop)
(define (ex4-43-2)
  (let (
		(moore (cons 'lorna (amb 'rosalind 'gabrielle 'mary-ann)))
		(downing (cons 'melissa (amb 'lorna 'rosalind 'gabrielle 'mary-ann)))
		(hall (cons 'rosalind (amb 'lorna 'gabrielle 'mary-ann)))
		(barnacle (cons 'gabrielle 'melissa))
		(parker (cons 'mary-ann (amb 'lorna 'rosalind 'gabrielle)))
		)
	(let ((fathers 
		   (list moore downing hall barnacle parker)))
;	  (require
;	   (distinct? (map car fathers)))
	  (require
		(distinct? (map cdr fathers)))
	  (require 
	   (eq? (cdr parker)
			(car (car (filter (lambda (x) (eq? (cdr x) 'gabrielle)) fathers)))))
	  (list 'moore moore 'downing downing 'hall hall 'barnacle barnacle 'parker parker))))


(ex4-43-2)
try-again
try-again
end


(driver-loop)

(define (eight-queen-diag row1 rest-rows)
  (if (null? rest-rows) true
	  (if
	   (= (abs (- (car row1) (caar rest-rows)))
		  (abs (- (cdr row1) (cdar rest-rows)))) 
	   false
	   (eight-queen-diag row1 (cdr rest-rows)))))
		  
	 
(define (eight-queen-sub rows)
  (if (null? rows) true
	  (let ((first-row (car rows))
			(rest-rows (cdr rows)))
		(if 
		 (eight-queen-diag first-row rest-rows) 
		 (eight-queen-sub rest-rows)
		 false))))
		 




(define (4-queen)
  (let (
		(row1 (cons 1 (amb 1 2 3 4 )))
		(row2 (cons 2 (amb 1 2 3 4 )))
		(row3 (cons 3 (amb 1 2 3 4 )))
		(row4 (cons 4 (amb 1 2 3 4 )))
		)
	(let ((rows (list row1 row2 row3 row4)))
	  (require
	   (distinct? (map cdr rows)))
	  (require
	   (eight-queen-sub rows))
	  rows)))
	  
(4-queen)
try-again	  
try-again	  
try-again	 
end 


(driver-loop)

(define (eight-queen-diag row1 rest-rows)
  (if (null? rest-rows) true
	  (if
	   (= (abs (- (car row1) (caar rest-rows)))
		  (abs (- (cdr row1) (cdar rest-rows)))) 
	   false
	   (eight-queen-diag row1 (cdr rest-rows)))))
		  
	 
(define (eight-queen-sub rows)
  (if (null? rows) true
	  (let ((first-row (car rows))
			(rest-rows (cdr rows)))
		(if 
		 (eight-queen-diag first-row rest-rows) 
		 (eight-queen-sub rest-rows)
		 false))))


(define (n-queens n)
  (define (generate-positions k l)
	(if (> k n) l
		(generate-positions (+ k 1)  (cons (cons k (an-integer-between 1 n)) l))))
  (let ((rows (generate-positions 1 '())))
	  (require
	   (distinct? (map cdr rows)))
	  (require
	   (eight-queen-sub rows))
	  rows))
(n-queens 4)
try-again	  
try-again	  
try-again	 
end 



end


;; (driver-loop)
;; (define (eight-queen)
;;   (let (
;; 		(row1 (cons 1 (amb 1 2 3 4 5 6 7 8)))
;; 		(row2 (cons 2 (amb 1 2 3 4 5 6 7 8)))
;; 		(row3 (cons 3 (amb 1 2 3 4 5 6 7 8)))
;; 		(row4 (cons 4 (amb 1 2 3 4 5 6 7 8)))
;; 		(row5 (cons 5 (amb 1 2 3 4 5 6 7 8)))
;; 		(row6 (cons 6 (amb 1 2 3 4 5 6 7 8)))
;; 		)
;; 	(let ((rows-a (list row1 row2 row3 row4 row5 row6)))
;; 	  (require
;; 	   (distinct? (map cdr rows-a)))
;; 	  (require
;; 	   (eight-queen-sub rows-a))
;; 	  (let
;; 		  (
;; 		   (row7 (cons 7 (amb 1 2 3 4 5 6 7 8)))
;; 		   )
;; 		(let ((rows-b (list row1 row2 row3 row4 row5 row6 row7)))
;; 		  (require
;; 		   (distinct? (map cdr rows-b)))
;; 		  (require
;; 		   (eight-queen-sub rows-b))
		
;; 		  (let (
;; 				(row8 (cons 8 (amb 1 2 3 4 5 6 7 8)))
;; 				)
;; 			(let ((rows (list row1 row2 row3 row4 row5 row6 row7 row8)))
;; 			  (require
;; 			   (distinct? (map cdr rows)))
;; 			  (require
;; 			   (eight-queen-sub rows))
;; 			  rows)))))))
	  

;; (eight-queen)

(driver-loop)
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
		(parse-noun-phrase)
		(parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
		(parse-word articles)
		(parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) found-word)))

end 

(driver-loop)
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
;	(newline) (display "unparsed: " )(display *unparsed*) (newline)
	(require (null? *unparsed*))
	sent))
(parse '(the cat eats))
end

(driver-loop)
(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
		(parse-word prepositions)
		(parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence
		(parse-noun-phrase)
		(parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
;	(newline) (display "verb-maybe-extend: ")
;	(display verb-phrase) (newline)
	(amb verb-phrase
		 (maybe-extend (list 'verb-phrase
							 verb-phrase
							 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
;	(newline) (display "noun-maybe-extend: ")
;	(display noun-phrase) (newline)
	(amb noun-phrase
		 (maybe-extend (list 'noun-phrase
							 noun-phrase
							 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(parse '(the student with the cat sleeps in the class))
(parse '(the professor lectures to the student with the cat))
try-again
try-again
end

(driver-loop)
(parse '(the professor lectures to the student with the cat))
try-again
try-again
end


(driver-loop)
(parse '(the professor lectures to the student in the class with the cat))
try-again
try-again
try-again
try-again
try-again
end


;ex4.46
;	(set! *unparsed* (cdr *unparsed*))

;ex4.47

;; (driver-loop)
;; (define (parse-word word-list)
;;   (display *unparsed*)
;;   (newline)
;;   (display word-list)
;;   (newline)
;;   (require (not (null? *unparsed*)))
;;   (require (memq (car *unparsed*) (cdr word-list)))
;;   (let ((found-word (car *unparsed*)))
;; 	(set! *unparsed* (cdr *unparsed*))
;; 	(list (car word-list) found-word)))

;; (parse '(the cat eats))
;; try-again
;; end

;; (driver-loop)
;; (define (parse-verb-phrase)
;;   (amb (parse-word verbs)
;; 	   (list 'verb-phrase
;; 			 (parse-verb-phrase)
;; 			 (parse-prepositional-phrase))))
;; (parse '(the cat eats))
;; try-again
;; end



;ex4.49

(driver-loop)
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
	(amb verb-phrase
		 (maybe-extend (list 'verb-phrase
							 verb-phrase
							 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (let ((found-word (an-element-of (cdr word-list))))
	(list (car word-list) found-word)))
(parse-sentence)
try-again
try-again
try-again
try-again
try-again
end


;ex4.50
(define (analyze exp)
  (cond ((self-evaluating? exp)
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((lambda? exp) (analyze-lambda exp))
		((let? exp)
		 (analyze (let->combination exp)))
		((begin? exp) 
		 (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((amb? exp) (analyze-amb exp))
		((ramb? exp) (analyze-ramb exp))
		((and? exp) (analyze-and exp))
		((or? exp) (analyze-or exp))
		((application? exp) (analyze-application exp))
		(else
		 (error "Unknown expression type -- ANALYZE" exp))))


(define (ramb? exp) (tagged-list? exp 'ramb))

(use math.mt-random)
(define m (make <mersenne-twister> :seed (sys-time)))
(mt-random-integer m 10)
(define (random a) (mt-random-integer m a))
(use srfi-1)
(list-ref '(1 2) 1)
(take '(1 2) 1)
(drop '(1 2) 1)
(list-ref '(1 2 3) 2)
(random 0)
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
	(lambda (env succeed fail)
	  (define (try-next choices)
		(if (null? choices)
			(fail)
			(let ((i (random (length choices))))
			  ((list-ref choices i) env
			   succeed
			   (lambda ()
				 (try-next (append (take choices i) (drop choices (+ i 1)))
						   ))))))
	  (try-next cprocs))))

(driver-loop)
(ramb 1 2 3 4 5)
try-again
try-again
try-again
try-again
end

(driver-loop)
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
	(ramb verb-phrase
		  (maybe-extend (list 'verb-phrase
							  verb-phrase
							  (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
	(ramb noun-phrase
		  (maybe-extend (list 'noun-phrase
							  noun-phrase
							  (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
	(ramb verb-phrase
		 (maybe-extend (list 'verb-phrase
							 verb-phrase
							 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (random-word items)
  (require (not (null? items)))
  (ramb (car items) (an-element-of (cdr items))))
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (list (car word-list) (random-word (cdr word-list))))

(parse-sentence)
try-again
;try-again
;try-again
;try-again
;try-again
end

;ex4.51
(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
	(lambda (env succeed fail)
	  (vproc env
			 (lambda (val fail2)
			   (set-variable-value! var val env)
			   (succeed 'ok
						fail2))
			 fail))))


(define (analyze exp)
  (cond ((self-evaluating? exp)
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((permanent-set? exp) (analyze-permanent-set exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((lambda? exp) (analyze-lambda exp))
		((let? exp)
		 (analyze (let->combination exp)))
		((begin? exp) 
		 (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((amb? exp) (analyze-amb exp))
		((ramb? exp) (analyze-ramb exp))
		((and? exp) (analyze-and exp))
		((or? exp) (analyze-or exp))
		((application? exp) (analyze-application exp))
		(else
		 (error "Unknown expression type -- ANALYZE" exp))))

(driver-loop)
(define count 0)
(define p-count 0)

(let ((x (an-element-of '(a b c)))
	  (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (permanent-set! p-count (+ p-count 1))
  (require (not (eq? x y)))
  (list x y count p-count))
try-again
end

(driver-loop)
(define count 0)

(let ((x (an-element-of '(a b c)))
	  (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
try-again
end

;ex4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (analyze-if-fail exp)
  (let ((normal (analyze (cadr exp)))
		(alternative (analyze (caddr exp))))
	(lambda (env succeed fail)
	  (normal env
;			 (lambda (val fail2)
;			   (succeed val fail2))
			  succeed
			 (lambda ()
			   (alternative env succeed fail))))))
					  


(define (analyze exp)
  (cond ((self-evaluating? exp)
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((permanent-set? exp) (analyze-permanent-set exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((if-fail? exp) (analyze-if-fail exp))
		((lambda? exp) (analyze-lambda exp))
		((let? exp)
		 (analyze (let->combination exp)))
		((begin? exp) 
		 (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((amb? exp) (analyze-amb exp))
		((ramb? exp) (analyze-ramb exp))
		((and? exp) (analyze-and exp))
		((or? exp) (analyze-or exp))
		((application? exp) (analyze-application exp))
		(else
		 (error "Unknown expression type -- ANALYZE" exp))))

(driver-loop)
(if-fail (let ((x (an-element-of '(1 3 5))))
		   (require (even? x))
		   x)
		 'all-odd)
end

(driver-loop)
(if-fail (let ((x (an-element-of '(1 3 8))))
		   (require (even? x))
		   x)
		 'all-odd)
end


;ex4.53
(driver-loop)
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
			 (permanent-set! pairs (cons p pairs))
			 (amb))
		   pairs))
end

;ex4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))



(define (analyze exp)
  (cond ((self-evaluating? exp)
		 (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((permanent-set? exp) (analyze-permanent-set exp))
		((definition? exp) (analyze-definition exp))
		((require? exp) (analyze-require exp))
		((if? exp) (analyze-if exp))
		((if-fail? exp) (analyze-if-fail exp))
		((lambda? exp) (analyze-lambda exp))
		((let? exp)
		 (analyze (let->combination exp)))
		((begin? exp) 
		 (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((amb? exp) (analyze-amb exp))
		((ramb? exp) (analyze-ramb exp))
		((and? exp) (analyze-and exp))
		((or? exp) (analyze-or exp))
		((application? exp) (analyze-application exp))
		(else
		 (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
	(lambda (env succeed fail)
	  (pproc env
			 (lambda (pred-value fail2)
			   (if (not pred-value)
				   (fail)
				   (succeed 'ok fail2)))
			 fail))))

(driver-loop)
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
		(b (an-element-of list2)))
	(require (prime? (+ a b)))
	(list a b)))

(prime-sum-pair '(1 3 5 8) '(20 35 110))
try-again
try-again
try-again
end
