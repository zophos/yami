(load "./ece4compiler.scm")
;;ex5.45
;;http://www.serendip.ws/archives/3926
(compile-and-go
 '(define (factorial n)
	(if (= n 1)
		1
		(* (factorial (- n 1))  n))))
(factorial 4)
(factorial 5)
quit

(start-eceval)
(define (factorial n)
  (if (= n 1)
	  1
	  (* (factorial (- n 1))  n)))
(factorial 4)
(factorial 5)
q

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (fact n)
  (define fact-machine
	(make-machine
	 '(continue val n)
	 (list (list '= =) (list '- -) (list '* *))
	 '(start
       (assign continue (label fact-done))
	   fact-loop
	   (test (op =) (reg n) (const 1))
	   (branch (label base-case))
	   (save continue)
	   (save n)
	   (assign n (op -) (reg n) (const 1))
	   (assign continue (label after-fact))
	   (goto (label fact-loop))
	   after-fact
	   (restore n)
	   (restore continue)
	   (assign val (op *) (reg n) (reg val))
	   (goto (reg continue))
	   base-case
	   (assign val (const 1))
	   (goto (reg continue))
	   fact-done)))

  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (display (get-register-contents fact-machine 'val))
  (display ((fact-machine 'stack) 'print-statistics))
  (newline))

(fact 5)


;;ex5.46
;;http://www.serendip.ws/archives/3962
(compile-and-go
 '(define (fib n)
	(if (< n 2)
		n
		(+ (fib (- n 1)) (fib (- n 2))))))
(fib 3)
(fib 4)
quit

(start-eceval)
(define (fib n)
  (if (< n 2)
	  n
	  (+ (fib (- n 1)) (fib (- n 2)))))
(fib 3)
(fib 4)
q

(define (fib n)
  (define fib-machine
	(make-machine
	 '(continue n val)
	 (list (list '< <) (list '- -) (list '+ +))
	 '((assign continue (label fib-done))
	 fib-loop
	 (test (op <) (reg n) (const 2))
	 (branch (label immediate-answer))
	 (save continue)
	 (assign continue (label afterfib-n-1))
	 (save n)
	 (assign n (op -) (reg n) (const 1))
	 (goto (label fib-loop))
	 afterfib-n-1
	 (restore n)
	 (restore continue)
	 (assign n (op -) (reg n) (const 2))
	 (save continue)
	 (assign continue (label afterfib-n-2))
	 (save val)
	 (goto (label fib-loop))
	 afterfib-n-2
	 (assign n (reg val))
	 (restore val)
	 (restore continue)
	 (assign val
			 (op +) (reg val) (reg n))
	 (goto (reg continue))
	 immediate-answer
	 (assign val (reg n))
	 (goto (reg continue))
	 fib-done)
	 ))
  ((fib-machine 'stack) 'initialize)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (display (get-register-contents fib-machine 'val))
  (display ((fib-machine 'stack) 'print-statistics))
  (newline))

(fib 2)
(fib 3)



;;ex5.47
(compile-and-go
 '(begin (define (f x) (g x))
 ))
(define (g x) (+ x 2))
(f 1)
quit


(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
	(let ((compiled-linkage
		   (if (eq? linkage 'next) after-call linkage)))
	  (append-instruction-sequences
	   (make-instruction-sequence '(proc) '()
								  `((test (op primitive-procedure?) (reg proc))
									(branch (label ,primitive-branch))))
	   (make-instruction-sequence '(proc) '()
								  `((test (op compiled-procedure?) (reg proc))
									(branch (label ,compiled-branch))))
	   (parallel-instruction-sequences
		(cond ((and (eq? target 'val) (not (eq? compiled-linkage 'return)))
			   (make-instruction-sequence '(proc) all-regs
										  `((assign continue (label ,compiled-linkage))
											(save continue)
											(goto (reg compapp)))))
			  ((and (not (eq? target 'val))
					(not (eq? compiled-linkage 'return)))
			   (let ((proc-return (make-label 'proc-return)))
				 (make-instruction-sequence '(proc) all-regs
											`((assign continue (label ,proc-return))
											  (save continue)
											  (goto (reg compapp))
											  ,proc-return
											  (assign ,target (reg val))
											  (goto (label ,compiled-linkage))))))
			  ((and (eq? target 'val) (eq? compiled-linkage 'return))
			   (make-instruction-sequence '(proc continue) all-regs
										  '((save continue)
											(goto (reg compapp)))))
			  ((and (not (eq? target 'val)) (eq? compiled-linkage 'return))
			   (error "return linkage, target not val -- COMPILE"
					  target)))
		(parallel-instruction-sequences
		 (append-instruction-sequences
		  compiled-branch
		  (compile-proc-appl target compiled-linkage))
		 (append-instruction-sequences
		  primitive-branch
		  (end-with-linkage linkage
							(make-instruction-sequence '(proc argl)
													   (list target)
													   `((assign ,target
																 (op apply-primitive-procedure)
																 (reg proc)
																 (reg argl))))))))
	   after-call))))


(define eceval
  (make-machine
   '(exp env val proc argl continue unev compapp)
   eceval-operations
   (cons '(assign compapp (label compound-apply))
		 eceval-body)))

(compile-and-go
 '(begin (define (f x) (g x))
 ))
(define (g x) (+ x 2))
(f 1)
quit

;;ex5.48

(define 
  eceval-body2
  '(
	(assign compapp (label compound-apply))
	(branch (label external-entry))
	read-eval-print-loop
	(perform (op initialize-stack))
	(perform
	 (op prompt-for-input) (const ";;; EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
	external-entry
	(perform (op initialize-stack))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (reg val))
	print-result
	(perform (op print-stack-statistics))
	(perform
	 (op announce-output) (const ";;; EC-Eval value:"))
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
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

	(test (op compile-and-run?) (reg exp)) ;;ex5.48
	(branch (label ev-compile-and-run))

	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))

	ev-compile-and-run ;;ex5.48
	(assign exp (op compile-and-run-body) (reg exp))
	(perform (op compile-and-run) (reg exp))
	(goto (reg continue))


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
	(assign val (op make-procedure) (reg unev) (reg exp) (reg env))
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
	(test (op compiled-procedure?) (reg proc))
	(branch (label compiled-apply))
	(goto (label unknown-procedure-type))
	compiled-apply
	(restore continue)
	(assign val (op compiled-procedure-entry) (reg proc))
	(goto (reg val))
	primitive-apply
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	(restore continue)
	(goto (reg continue))
	compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment) (reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))

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

	unknown-expression-type
	(assign val (const unknown-expression-type-error))
	(goto (label signal-error))
	unknown-procedure-type
	(restore continue)
	(assign val (const unknown-procedure-type-error))
	(goto (label signal-error))
	signal-error
	(perform (op user-print) (reg val))
	(goto (label read-eval-print-loop))
	))

(define (compile-and-run expression)
  (newline)
  (display "compile-and-run: ")
  (display expression)
  (newline)
  (newline)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
	(set-register-contents! eceval 'val instructions)
	(set-register-contents! eceval 'flag true)
	(start eceval)))

(define (compile-and-run? proc)
  (tagged-list? proc 'compile-and-run))

(define (compile-and-run-body exp)
  (cadadr exp))

(define eceval-operations2
  (cons (list 'compile-and-run-body compile-and-run-body)
		(cons (list 'compile-and-run compile-and-run)
			  (cons (list 'compile-and-run? compile-and-run?)
					eceval-operations))))

(define eceval
  (make-machine
    '(exp env val proc argl continue unev compapp)
    eceval-operations2
	eceval-body2
	))

(define (compile-and-go expression)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
       (set! the-global-environment (setup-environment))
       (set-register-contents! eceval 'val instructions)
       (set-register-contents! eceval 'flag true)
       (start eceval)))

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(start-eceval)
(compile-and-run
 '(define (factorial n)
	(if (= n 1)
		1
		(* (factorial (- n 1))  n))))
(factorial 5)
q

 

;;ex5.48-2

(define (setup-environment2)
  (extend-environment
    (list 'compile-and-run)
    (list (list 'primitive compile-and-run))
    (setup-environment)))


(define eceval
  (make-machine
    '(exp env val proc argl continue unev compapp)
    eceval-operations
	eceval-body
	))

(define (start-eceval)
  (set! the-global-environment (setup-environment2))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(start-eceval)
(compile-and-run
 '(define (factorial n)
	(if (= n 1)
		1
		(* (factorial (- n 1))  n))))
(factorial 5)
q

;;ex5.49

;(define (empty-arglist) '())

(define (get-eceval3) eceval3)

(define eceval-operations3
  (cons (list 'get-eceval3 get-eceval3)
		(cons (list 'compile compile)
			  (cons (list 'statements statements)
					(cons (list 'assemble assemble)
						  eceval-operations)))))

(define eceval-body3
    '(
	  (assign machine (op get-eceval3))
	  (assign compapp (label compound-apply))
	  (branch (label external-entry))
	  read-eval-print-loop
	  (perform (op initialize-stack))
	  (perform
	   (op prompt-for-input) (const ";;; EC-Eval input:"))
	  (assign exp (op read))
	  (assign env (op get-global-environment))
	  (assign continue (label print-result))
	  ;;(goto (label eval-dispatch))
	  compile ;;ex5.49
	  (assign exp (op compile) (reg exp) (const val) (const return))
	  (assign exp (op statements) (reg exp))
	  assemble
	  (assign val (op assemble) (reg exp) (reg machine))
	  (goto (reg val))


	  external-entry
	  (perform (op initialize-stack))
	  (assign env (op get-global-environment))
	  (assign continue (label print-result))
	  (goto (reg val))
	  print-result
	  (perform (op print-stack-statistics))
	  (perform
	   (op announce-output) (const ";;; EC-Eval value:"))
	  (perform (op user-print) (reg val))
	  (goto (label read-eval-print-loop))
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
	  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
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
	  (test (op compiled-procedure?) (reg proc))
	  (branch (label compiled-apply))
	  (goto (label unknown-procedure-type))
	  compiled-apply
	  (restore continue)
	  (assign val (op compiled-procedure-entry) (reg proc))
	  (goto (reg val))
	  primitive-apply
	  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	  (restore continue)
	  (goto (reg continue))
	  compound-apply
	  (assign unev (op procedure-parameters) (reg proc))
	  (assign env (op procedure-environment) (reg proc))
	  (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
	  (assign unev (op procedure-body) (reg proc))
	  (goto (label ev-sequence))

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

	  unknown-expression-type
	  (assign val (const unknown-expression-type-error))
	  (goto (label signal-error))
	  unknown-procedure-type
	  (restore continue)
	  (assign val (const unknown-procedure-type-error))
	  (goto (label signal-error))
	  signal-error
	  (perform (op user-print) (reg val))
	  (goto (label read-eval-print-loop))
	  ))



(define eceval3
  (make-machine
   '(exp env val proc argl continue unev compapp machine)
   eceval-operations3
   eceval-body3
   ))

(define (start-eceval3)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval3 'flag false)
  (start eceval3))

(start-eceval3)
(define (factorial n)
  (if (= n 1)
	  1
	  (* (factorial (- n 1))  n)))
(factorial 4)
(factorial 5)
q

;;ex5.50
;;http://himoiku.cocolog-nifty.com/blog/2008/07/sicp550_f385.html
;;http://www.serendip.ws/archives/4020
