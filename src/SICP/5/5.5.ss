(load "./5.4.ss")
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
		 (compile-self-evaluating exp target linkage))
		((quoted? exp) (compile-quoted exp target linkage))
		((variable? exp)
		 (compile-variable exp target linkage))
		((assignment? exp)
		 (compile-assignment exp target linkage))
		((definition? exp)
		 (compile-definition exp target linkage))
		((if? exp) (compile-if exp target linkage))
		((lambda? exp) (compile-lambda exp target linkage))
		((begin? exp)
		 (compile-sequence (begin-actions exp)
						   target
						   linkage))
		((cond? exp) (compile (cond->if exp) target linkage))
		((application? exp)
		 (compile-application exp target linkage))
		(else
		 (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
		 (make-instruction-sequence '(continue) '()
									'((goto (reg continue)))))
		((eq? linkage 'next)
		 (empty-instruction-sequence))
		(else
		 (make-instruction-sequence '() '()
									`((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
			  instruction-sequence
			  (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
					(make-instruction-sequence '() (list target)
											   `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
					(make-instruction-sequence '() (list target)
											   `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
					(make-instruction-sequence '(env) (list target)
											   `((assign ,target
														 (op lookup-variable-value)
														 (const ,exp)
														 (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
		(get-value-code
		 (compile (assignment-value exp) 'val 'next)))
	(end-with-linkage linkage
					  (preserving '(env)
								  get-value-code
								  (make-instruction-sequence '(env val) (list target)
															 `((perform (op set-variable-value!)
																		(const ,var)
																		(reg val)
																		(reg env))
															   (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
		(get-value-code
		 (compile (definition-value exp) 'val 'next)))
	(end-with-linkage linkage
					  (preserving '(env)
								  get-value-code
								  (make-instruction-sequence '(env val) (list target)
															 `((perform (op define-variable!)
																		(const ,var)
																		(reg val)
																		(reg env))
															   (assign ,target (const ok))))))))


(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
		(f-branch (make-label 'false-branch))
		(after-if (make-label 'after-if)))
	(let ((consequent-linkage
		   (if (eq? linkage 'next) after-if linkage)))
	  (let ((p-code (compile (if-predicate exp) 'val 'next))
			(c-code 
			 (compile 
			  (if-consequent exp) target consequent-linkage))
			(a-code 
			  (compile (if-alternative exp) target linkage)))
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

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
				  (number->string (new-label-number)))))


(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
	  (compile (first-exp seq) target linkage)
	  (preserving '(env continue)
				  (compile (first-exp seq) target 'next)
				  (compile-sequence (rest-exps seq) target linkage))))



(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
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
		(compile-lambda-body exp proc-entry))
	   after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
	(append-instruction-sequences 
	 (make-instruction-sequence '(env proc argl) '(env)
								`(,proc-entry
								  (assign env (op compiled-procedure-env) (reg proc))
								  (assign env (op extend-environment)
										  (const ,formals)
										  (reg argl)
										  (reg env))))
	 (compile-sequence (lambda-body exp) 'val 'return))))
				
					
	
	

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
		(operand-codes
		 (map (lambda (operand) (compile operand 'val 'next))
			  (operands exp))))
	(preserving '(env continue)
				proc-code
				(preserving '(proc continue)
							(construct-arglist operand-codes)
							(compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
	(if (null? operand-codes)
		(make-instruction-sequence '() '(argl)
								   '((assign argl (const()))))
		(let ((code-to-get-last-arg
			   (append-instruction-sequences
				(car operand-codes)
				(make-instruction-sequence '(val) '(argl)
										   '((assign argl (op list) (reg val)))))))
		  (if (null? (cdr operand-codes))
			  code-to-get-last-arg
			  (preserving '(env)
						  code-to-get-last-arg
						  (code-to-get-rest-args
						   (cdr operand-codes))))))))
			  
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
		 (preserving '(argl)
					 (car operand-codes)
					 (make-instruction-sequence '(val argl) '(argl)
												'((assign argl
														  (op cons) (reg val) (reg argl)))))))
	(if (null? (cdr operand-codes))
		code-for-next-arg
		(preserving '(env)
					code-for-next-arg
					(code-to-get-rest-args (cdr operand-codes))))))

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
																(op apply-primitive-procedure
																	(reg proc)
																	(reg argl))))))))
	   after-call))))

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
		 (make-instruction-sequence '(proc) all-regs
									`((assign continue (label ,linkage))
									  (assign val (op compiled-procedure-entry)
											  (reg proc))
									  (goto (reg val)))))
		((and (not (eq? target 'val))
			  (not (eq? linkage 'return)))
		 (let ((proc-return (make-label 'proc-return)))
		   (make-instruction-sequence '(proc) all-regs
									  `((assign continue (label ,proc-return))
										(assign val (op compiled-procedure-entry)
												(reg proc))
										(goto (reg val))
										,proc-return
										(assign ,target (reg val))
										(goto (label ,linkage))))))
		((and (eq? target 'val) (eq? linkage 'return))
		 (make-instruction-sequence '(proc continue) all-regs
									'((assign val (op compiled-procedure-entry)
											  (reg proc))
									  (goto (reg val)))))
		((and (not (eq? target 'val)) (eq? linkage 'return))
		 (error "return linkage, target not val -- COMPILE"
				target))))
			   

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-regsiter? seq reg)
  (memq reg (registers-needed seq)))

(define (modified-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
	(make-instruction-sequence
	 (list-union (registers-needed seq1)
				 (list-difference (registers-needed seq2)
								  (registers-modified seq1)))
	 (list-union (registers-modified seq1)
				 (registers-modified seq2))
	 (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
	(if (null? seqs)
		(empty-instruction-sequence)
		(append-2-sequences (car seqs)
							(append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
		((memq (car s1) s2) (list-union (cdr s1) s2))
		(else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
		((memq (car s1) s2) (list-difference (cdr s1) s2))
		(else (cons (car s1) 
					(list-difference (cdr s1) s2)))))


(define (preserving regs seq1 seq2)
  (if (null? regs)
	  (append-instruction-sequences seq1 seq2)
	  (let ((first-reg (car regs)))
		(if (and (needs-regsiter? seq2 first-reg)
				 (modified-register? seq1 first-reg))
			(preserving (cdr regs)
						(make-instruction-sequence
						 (list-union (list first-reg)
									 (registers-needed seq1))
						 (list-difference (registers-modified seq1)
										  (list first-reg))
						 (append `((save ,first-reg))
								 (statements seq1)
								 `((restore ,first-reg))))
						 seq2)
			(preserving (cdr regs) seq1 seq2)))))


(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
			   (registers-needed seq2))
   (list-union (registers-modified seq1)
			   (registers-modified seq2))
   (append (statements seq1) (statements seq2))))
									 
			
(compile 
 '(define (factorial n)
	(if (= n 1)
		1
		(* (factorial (- n 1)) n)))
 'val
 'next)

;ex5.31
;http://www.serendip.ws/archives/3583
(define (parse-compiled-code lis)
  (if (not (null? lis))
      (begin
        (if (pair? (caar lis))
            (map (lambda (x)
                         (if (symbol? x)
                             (print x)
                             (print "  " x)))
                 (car lis))
            (print (car lis)))
        (parse-compiled-code (cdr lis)))))

(parse-compiled-code
 (compile 
  '(define (factorial n)
	 (if (= n 1)
		 1
		 (* (factorial (- n 1)) n)))
  'val
  'next))

(parse-compiled-code
  (compile
    '(f 'x 'y)
    'val
    'next))

(parse-compiled-code
  (compile
    '((f) 'x 'y)
    'val
    'next))

(parse-compiled-code
  (compile
    '(f (g 'x) y)
    'val
    'next))<

(parse-compiled-code
  (compile
    '(f (g 'x) 'y)
    'val
    'next))

;ex5.32
;http://www.serendip.ws/archives/3590

(length (caddr
 (compile 
  '(define (factorial n)
	 (if (= n 1)
		 1
		 (* (factorial (- n 1)) n)))
  'val
  'next)))

(length (caddr
 (compile 
  '(define (factorial-alt n)
	 (if (= n 1)
		 1
		 (* n (factorial (- n 1)))))
  'val
  'next)))

(parse-compiled-code
 (compile 
  '(define (factorial-alt n)
	 (if (= n 1)
		 1
		 (* n (factorial (- n 1)))))
  'val
  'next))


;ex5.34
;http://www.serendip.ws/archives/3618
(parse-compiled-code
 (compile 
  '(define (factorial n)
	 (define (iter product counter)
	   (if (> counter n)
		   product
		   (iter (* counter product)
				 (+ counter 1)))))
  'val
  'next))

;ex5.35
;http://www.serendip.ws/archives/3622
(define label-counter 15)
(parse-compiled-code
  (compile
    '(define (f x)
       (+ x (g (+ x 2))))
    'val
    'next))

;ex5.36
;http://www.serendip.ws/archives/3634

;ex5.37
;http://www.serendip.ws/archives/3693

;ex5.38
;http://www.serendip.ws/archives/3757

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

(parse-compiled-code
  (compile
    '((lambda (x y)
	   (+ x y))
	  1 2)
	
    'val
    'next
    '()))


;ex5.44
;http://www.serendip.ws/archives/3862
