;5.5.6

;ex5.39
;http://www.serendip.ws/archives/3762
(define (lexical-address-frame-number lexical-address)
  (car lexical-address))

(define (lexical-address-displacement-number lexical-address)
  (cadr lexical-address))

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (lexical-address-lookup lexical-address env)
  (let ((frame-number (lexical-address-frame-number lexical-address))
        (displacement-number (lexical-address-displacement-number lexical-address)))
       (let ((frame (list-ref env frame-number)))
             (let ((value (list-ref (frame-values frame) displacement-number)))
                  (if (eq? value '*unassigned*)
                      (error "Unassigned variable")
                      value)))))

(define (lexical-address-set! lexical-address val env)
  (let ((frame-number (lexical-address-frame-number lexical-address))
        (displacement-number (lexical-address-displacement-number lexical-address)))
       (let ((frame (list-ref env frame-number)))
             (define (iter variables values count)
               (cond ((null? variables)
                      (error "Unbound variable - LEXICAL-ADDRESS-SET!"))
                     ((= count 0)
                      (set-car! values val))
                     (else
                       (iter (cdr variables) (cdr values) (- count 1)))))
             (iter (frame-variables frame) (frame-values frame) displacement-number))))

;ex5.40
(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
       (append-instruction-sequences
         (make-instruction-sequence '(env proc argl) '(env)
                                    `(,proc-entry
                                       (assign env (op compiled-procedure-env) (reg proc))
                                       (assign env
                                               (op extend-environment)
                                               (const ,formals)
                                               (reg argl)
                                               (reg env))))
         (compile-sequence (lambda-body exp) 'val 'return (cons formals ct-env)))))

(define (compile exp target linkage ct-env) ; ex5.40
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assingment exp target linkage ct-env)) ; ex5.40
        ((definition? exp)
         (compile-definition exp target linkage ct-env)) ; ex5.40
        ((if? exp) (compile-if exp target linkage ct-env)) ; ex5.40
        ((lambda? exp) (compile-lambda exp target linkage ct-env)) ; ex5.40
        ((begin? exp)
         (compile-sequence (begin-action exp)
                           target
                           linkage
                           ct-env)) ; ex5.40
        ((cond? exp) (compile (cond->if exp) target linkage ct-env)) ; ex5.40
        ((application? exp)
         (compile-application exp target linkage ct-env)) ; ex5.40
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
		(get-value-code
		 (compile (assignment-value exp) 'val 'next ct-env)))
	(end-with-linkage linkage
					  (preserving '(env)
								  get-value-code
								  (make-instruction-sequence '(env val) (list target)
															 `((perform (op set-variable-value!)
																		(const ,var)
																		(reg val)
																		(reg env))
															   (assign ,target (const ok))))))))

(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp))
		(get-value-code
		 (compile (definition-value exp) 'val 'next ct-env)))
	(end-with-linkage linkage
					  (preserving '(env)
								  get-value-code
								  (make-instruction-sequence '(env val) (list target)
															 `((perform (op define-variable!)
																		(const ,var)
																		(reg val)
																		(reg env))
															   (assign ,target (const ok))))))))


(define (compile-if exp target linkage ct-env)
  (let ((t-branch (make-label 'true-branch))
		(f-branch (make-label 'false-branch))
		(after-if (make-label 'after-if)))
	(let ((consequent-linkage
		   (if (eq? linkage 'next) after-if linkage)))
	  (let ((p-code (compile (if-predicate exp) 'val 'next ct-env))
			(c-code 
			 (compile 
			  (if-consequent exp) target consequent-linkage ct-env))
			(a-code 
			  (compile (if-alternative exp) target linkage ct-env)))
		(preserving '(env continue)
					p-code
					(append-instruction-sequences
					 (make-instruction-sequence '(val) '()
												`((test (op false?) (reg val))
												  (branch (label ,f-branch))))
					 (parallel-instruction-sequences
					  (append-instruction-sequences t-branch c-code)
					  (append-instruction-sequences f-branch a-code))
					 after-if))))))

(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry))
		(after-lambda (make-label 'after-lambda)))
	(let ((lambda-linkage
		   (if (eq? linkage 'next) after-lambda linkage)))
	  (append-instruction-sequences
	   (tack-on-instruction-sequence
		(end-with-linkage lambda-linkage
						  (make-instruction-sequence '(env) (list target)
													 `((assign ,target
															   (op make-compiled-procedure)
															   (label ,proc-entry)
															   (reg env)))))
		(compile-lambda-body exp proc-entry ct-env))
	   after-lambda))))

(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
	  (compile (first-exp seq) target linkage ct-env)
	  (preserving '(env continue)
				  (compile (first-exp seq) target 'next ct-env)
				  (compile-sequence (rest-exps seq) target linkage ct-env))))

(define (compile-application exp target linkage ct-env)
  (let ((proc-code (compile (operator exp) 'proc 'next ct-env))
		(operand-codes
		 (map (lambda (operand) (compile operand 'val 'next ct-env))
			  (operands exp))))
	(preserving '(env continue)
				proc-code
				(preserving '(proc continue)
							(construct-arglist operand-codes)
							(compile-procedure-call target linkage)))))


(parse-compiled-code
  (compile
    '(f 'x 'y)
    'val
    'next
	'()
	))


(parse-compiled-code
  (compile
    '((f) 'x 'y)
    'val
    'next
	'()
	))

(parse-compiled-code
  (compile
    '(f (g 'x) y)
    'val
    'next
	'()
	))

(parse-compiled-code
  (compile
    '(f (g 'x) 'y)
    'val
    'next
	'()
	))

;ex5.41

(define (find-variable val ct-env)
  (define (iter env count)
	(if (null? env)
		'not-found
		(let ((index (list-index (lambda (e) (eq? val e)) (car env))))
		  (if (integer? index)
			  (make-lexical-address count index)
			  (iter (cdr env) (+ count 1))))))
  (iter ct-env 0))
		   
(find-variable 'c '((y z) (a b c d e) (x y)))
(find-variable 'x '((y z) (a b c d e) (x y)))
(find-variable 'y '((y z) (a b c d e) (x y)))
(find-variable 'z '((y z) (a b c d e) (x y)))
(find-variable 'w '((y z) (a b c d e) (x y)))

;ex5.42
(define (compile exp target linkage ct-env) ; ex5.40
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage ct-env))
        ((assignment? exp)
         (compile-assingment exp target linkage ct-env)) ; ex5.40
        ((definition? exp)
         (compile-definition exp target linkage ct-env)) ; ex5.40
        ((if? exp) (compile-if exp target linkage ct-env)) ; ex5.40
        ((lambda? exp) (compile-lambda exp target linkage ct-env)) ; ex5.40
		((let? exp)
		 (compile (let->combination exp) target linkage ct-env))
        ((begin? exp)
         (compile-sequence (begin-action exp)
                           target
                           linkage
                           ct-env)) ; ex5.40
        ((cond? exp) (compile (cond->if exp) target linkage ct-env)) ; ex5.40
        ((application? exp)
         (compile-application exp target linkage ct-env)) ; ex5.40
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (compile-variable exp target linkage ct-env)
  (let ((addr (find-variable exp ct-env)))
       (end-with-linkage
         linkage
         (make-instruction-sequence
           '(env)
           (list target)
           (if (eq? addr 'not-found)
               `((assign ,target
                         (op lookup-variable-value)
                         (const ,exp)
                         (reg env)))
               `((assign ,target
                         (op lexical-address-lookup)
                         (const ,addr)
                         (reg env))))))))

(define (compile-assingment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next ct-env)))
       (let ((addr (find-variable var ct-env)))
            (end-with-linkage
              linkage
              (preserving
                '(env)
                get-value-code
                (make-instruction-sequence
                  '(env val)
                  (list target)
                  (if (eq? addr 'not-found)
                      `((perform (op set-variable-value!)
                                 (const ,var)
                                 (reg val)
                                 (reg env))
                        (assign ,target (const ok)))
                      `((perform (op lexical-address-set!)
                                 (const ,addr)
                                 (reg val)
                                 (reg env))
                            (assign ,target (const ok))))))))))

(parse-compiled-code
  (compile
    '(lambda (x y)
             (lambda (a b)
                     (+
                       (+ x a)
                       (* y b)
                       (set! x a)
                       (set! z b))))
    'val
    'next
    '()))

(define (make-assignment var val)
  (list 'set! var val))
(define (make-let clauses body) (list 'let clauses body))


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


(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
	;(display (scan-out-defines (lambda-body exp)))
	(append-instruction-sequences
	 (make-instruction-sequence
	  '(env proc argl)
	  '(env)
	  `(,proc-entry
		(assign env (op compiled-procedure-env) (reg proc))
		(assign env
				(op extend-environment)
				(const ,formals)
				(reg argl)
				(reg env))))
	 (compile-sequence
	  (scan-out-defines (lambda-body exp))
	  'val
	  'return
	  (cons formals ct-env)))))

(parse-compiled-code
  (compile
    '(lambda (x y)
             (define u (+ u x))
             (define v (- v y))
             (* u v))
    'val
    'next
    '()))

;ex5.44
;http://www.serendip.ws/archives/3862
