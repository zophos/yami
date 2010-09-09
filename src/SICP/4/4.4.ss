;get put
(define true #t)
(define false #f)
(define (make-table)
  (let ((local-table (list '*table*)))
	(define (lookup key-1 key-2)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if record
				  (cdr record)
				  false ))
			false)))
	(define (insert! key-1 key-2 value)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if record
				  (set-cdr! record value)
				  (set-cdr! subtable 
							(cons (cons key-2 value)
								  (cdr subtable)))))
			(set-cdr! local-table
					  (cons (list key-1
								  (cons key-2 value))
							(cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (error "Unknown operation -- TABLE" m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;stream
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
     (lambda ()
       (if (not already-run?)
           (begin (set! result (proc))
                    (set! already-run? #t)
                    result)
           result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define the-empty-stream '())

(define (stream-null? s) (null? s))

;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-map proc s)
  (if (stream-null? s)
	  the-empty-stream
	  (cons-stream (proc (stream-car s))
				   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
			 (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
          ((pred (stream-car s))
           (cons-stream (stream-car s)
                             (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line s)
  (newline)
  (display s))


;4.4.4.1

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
	(if (eq? input 'end) (display "stopped.")
		(let ((q (query-syntax-process input)))
		  (cond ((assertion-to-be-added? q)
				 (add-rule-or-assertion! (add-assertion-body q))
				 (newline)
				 (display "Assertion added to data base.")
				 (query-driver-loop))
				(else
				 (newline)
				 (display output-prompt)
				 (display-stream
				  (stream-map
				   (lambda (frame)
					 (instantiate q
								  frame
								  (lambda(v f)
									(contract-quetion-mark v))))
				   (qeval q (singleton-stream '()))))
				 (query-driver-loop)))))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
	(cond ((var? exp)
		   (let ((binding (binding-in-frame exp frame)))
			 (if binding
				 (copy (binding-value binding))
				 (unbound-var-handler exp frame))))
		  ((pair? exp)
		   (cons (copy (car exp)) (copy (cdr exp))))
		  (else exp)))
  (copy exp))


;4.4.4.2
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
	(if qproc
		(qproc (contents query) frame-stream)
		(simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (stream-append-delayed
	  (find-assertions query-pattern frame)
	  (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
	  frame-stream
	  (conjoin (rest-conjuncts conjuncts)
			   (qeval (first-conjunct conjuncts)
					  frame-stream))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
	  the-empty-stream
	  (interleave-delayed
	   (qeval (first-disjunct disjuncts) frame-stream)
	   (delay (disjoin (rest-disjuncts disjuncts)
					   frame-stream)))))

(put 'or 'qeval disjoin)
			   
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (if (stream-null? (qeval (negated-query operands)
							  (singleton-stream frame)))
		 (singleton-stream frame)
		 the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (if (execute
		  (instantiate
		   call
		   frame
		   (lambda(v f)
			 (error "Unknown pat var -- LISP-VALUE" v))))
		 (singleton-stream frame)
		 the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define user-initial-environment (interaction-environment))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
		 (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

;4.4.4.3
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
					(check-an-assertion datum pattern frame))
				  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result 
		 (pattern-match query-pat assertion query-frame)))
	(if (eq? match-result 'failed)
		the-empty-stream
		(singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
		((equal? pat dat) frame)
		((var? pat) (extend-if-consistent pat dat frame))
		((and (pair? pat) (pair? dat))
		 (pattern-match (cdr pat)
						(cdr dat)
						(pattern-match (car pat)
									   (car dat)
									   frame)))
		(else 'failed)))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
	(if binding
		(pattern-match (binding-value binding) dat frame)
		(extend var dat frame))))

;4.4.4.4
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
					(apply-a-rule rule pattern frame))
				  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
	(let ((unify-result
		   (unify-match query-pattern
						(conclusion clean-rule)
						query-frame)))
	  (if (eq? unify-result 'failed)
		  the-empty-stream
		  (qeval (rule-body clean-rule)
				 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
	(define (tree-walk exp)
	  (cond ((var? exp)
			 (make-new-variable exp rule-application-id))
			((pair? exp)
			 (cons (tree-walk (car exp))
				   (tree-walk (cdr exp))))
			(else exp)))
	(tree-walk rule)))



(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
		((equal? p1 p2) frame)
		((var? p1) (extend-if-possible p1 p2 frame))
		((var? p2) (extend-if-possible p2 p1 frame)) ; ***
		((and (pair? p1) (pair? p2))
		 (unify-match (cdr p1)
					  (cdr p2)
					  (unify-match (car p1)
								   (car p2)
								   frame)))
		(else 'failed)))


(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
	(cond (binding 
		   (unify-match
			(binding-value binding) val frame))
		  ((var? val) ;***
		   (let ((binding (binding-in-frame val frame)))
			 (if binding
				 (unify-match
				  var (binding-value binding) frame)
				 (extend var val frame))))
		  ((depends-on? val var frame) ; ***
		   'failed)
		  (else (extend var val frame)))))


(define (depends-on? exp var frame)
  (define (tree-walk e)
	(cond ((var? e)
		   (if (equal? var e)
			   true
			   (let ((b (binding-in-frame e frame)))
				 (if b
					 (tree-walk (binding-value b))
					 false))))
		  ((pair? e)
		   (or (tree-walk (car e))
			   (tree-walk (cdr e))))
		  (else false)))
  (tree-walk exp))
;4.4.4.5

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
	  (get-indexed-assertions pattern)
	  (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
	(if s s the-empty-stream)))
						  
		   
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
	  (get-indexed-rules pattern)
	  (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (stream-append s1 s2)
  (if (stream-null? s1)
	  s2
	  (cons-stream
	   (stream-car s1)
	   (stream-append (stream-cdr s1) s2))))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
	  (add-rule! assertion)
	  (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
	(set! THE-ASSERTIONS
		  (cons-stream assertion old-assertions))
	'ok))


(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
	(set! THE-RULES (cons-stream rule old-rules))
	'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
	  (let ((key (index-key-of assertion)))
		(let ((current-assertion-stream
			   (get-stream key 'assertion-stream)))
		  (put key
			   'assertion-stream
			   (cons-stream assertion
							current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
	(if (indexable? pattern)
		(let ((key (index-key-of pattern)))
		  (let ((current-rule-stream
				 (get-stream key 'rule-stream)))
			(put key
				 'rule-stream
				 (cons-stream rule
							  current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
	  (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
	(if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;4.4.4.6

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
	  (force delayed-s2)
	  (cons-stream
	   (stream-car s1)
	   (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
	  (force delayed-s2)
	  (cons-stream
	   (stream-car s1)
	   (interleave-delayed (force delayed-s2)
						   (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
	  the-empty-stream
	  (interleave-delayed
	   (stream-car stream)
	   (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (type exp)
  (if (pair? exp)
	  (car exp)
	  (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
	  (cdr exp)
	  (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
	  '(always-true)
	  (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
		 (cons (map-over-symbols proc (car exp))
			   (map-over-symbols proc (cdr exp))))
		((symbol? exp) (proc exp))
		(else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
	(if (string=? (substring chars 0 1) "?")
		(list '?
			  (string->symbol
			   (substring chars 1 (string-length chars))))
		symbol)))

(define (tagged-list? exp tag)
  (if (pair? exp)
          (eq? (car exp) tag)
          false))


(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp)
  (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-quetion-mark variable)
  (string->symbol
   (string-append "?"
				  (if (number? (cadr variable))
					  (string-append (symbol->string (caddr variable))
									 "-"
									 (number->string (cadr variable)))
					  (symbol->string (cadr variable))))))


(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))


(query-driver-loop)
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))

(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))

(assert! (can-do-job (computer programmer)
            (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
            (administration big wheel)))

(assert! (rule (same ?x ?x)))

(assert! (rule (lives-near ?person-1 ?person-2)
			   (and (address ?person-1 (?town . ?rest-1))
					(address ?person-2 (?town . ?rest-2))
					(not (same ?person-1 ?person-2)))))

(assert! (rule (wheel ?person)
			   (and (supervisor ?middle-manager ?person)
					(supervisor ?x ?middle-manager))))

(assert! (rule (outranked-by ?staff-person ?boss)
			   (or (supervisor ?staff-person ?boss)
				   (and (supervisor ?staff-person ?middle-manager)
						(outranked-by ?middle-manager ?boss)))))

;ex4.57
(assert! (rule (replace-job ?job-1 ?job-2)
			   (or (can-do-job ?job-1 ?job-2)
				   (and (can-do-job ?job-1 ?job-m)
						(replace-job ?job-m ?job-2))
				   )))
(assert! (rule (replace ?person-1 ?person-2)
			   (and (job ?person-1 ?job-1)
					(job ?person-2 ?job-2)
					(or (replace-job ?job-1 ?job-2)
						(same ?job-1 ?job-2))
					(not (same ?person-1 ?person-2)))))

;ex4.58
(assert! (rule (big-shot ?person)
			   (and 
				(job ?person (?div-p . ?rest-p))
				(supervisor ?person ?supervisor)
				(job ?supervisor (?div-s . ?rest-s))
				(not (same ?div-p ?div-s))
				)
			   ))
;ex4.59
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

(assert!
 (rule (meeting-time ?person ?day-and-time)
	   (and (job ?person (?div . ?rest-p))
			(or (meeting whole-company ?day-and-time)
				(meeting ?div ?day-and-time)))))

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
			   (append-to-form ?v ?y ?z)))

;ex4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
			   (?x next-to ?y in ?z)))


;(assert! (rule (last-pair (?x . ?y) ?z)
;			   (last-pair ?y ?z)))
(assert! (rule (last-pair (?x . ?y) (?z))
			   (last-pair ?y (?z))))
(assert! (rule (last-pair (?y) (?y))))


(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson-of ?g ?s)
			   (and (son-of ?p ?s)
					(son-of ?g ?p))))

(assert! (rule (son-of ?m ?s)
			   (or 
				(son ?m ?s)
				(and (wife ?m ?w)
					 (son ?w ?s)))))

;ex4.68
(assert! (rule (reverse () ())))
(assert! (rule (reverse (?u . ?v) ?y)
			   (and (reverse ?v ?w)
					(append-to-form ?w (?u) ?y))))

;ex4.69

(assert! (rule ((great . ?rel) ?x ?y)
               (and (son-of ?x ?w)
                    (?rel ?w ?y))))


;; (assert! (rule ((son) ?x ?y)
;;                (son-of ?x ?y)))
					

(assert! (rule ((grandson) ?x ?y)
               (grandson-of ?x ?y)))

;; (rule (ends-with ?lis ?x)
;; 	  (last-pair ?lis (?x . ?unused)))

;; (rule ((great  . ?rel) ?a ?d)
;; 	  (and    
;; 	   (and    
;; 		(son ?a ?m)
;; 		(?rel  ?m ?d))
;; 	   (ends-with ?rel grandson)))

end


(query-driver-loop)
(job ?x (computer programmer))
(job ?x (computer ?type))
(job ?x (computer . ?type))
end

;ex4.55
(query-driver-loop)
;a
(supervisor ?x (Bitdiddle Ben))
;b
(job ?x (accounting . ?y))
;c
(address ?x (Slumerville . ?y))
end

(query-driver-loop)
(and (job ?person (computer programmer))
	 (address ?person ?where))
(or (supervisor ?x (Bitdiddle Ben))
	(supervisor ?x (Hacker Alyssa P)))

(and (supervisor ?x (Bitdiddle Ben))
	 (not (job ?x (computer programmer))))

(and (salary ?persion ?amount)
	 (lisp-value > ?amount 30000))
end

;ex4.56
(query-driver-loop)
;a
(and (supervisor ?x (Bitdiddle Ben))
	 (address ?x ?y))
;b
(and (salary (Bitdiddle Ben) ?ben-amount)
	 (salary ?x ?amount)
	 (lisp-value < ?amount ?ben-amount))
;c
(and (supervisor ?person ?supervisor)
	 (not (job ?supervisor (computer . ?x)))
	 (job ?supervisor ?y))

(assert! (married Minnie Mickery))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))

end

(query-driver-loop)

(lives-near ?x (Bitdiddle Ben))
(lives-near ?x ?y)
(wheel ?x)
(and (job ?x (computer programmer))
	 (lives-near?x (Bitdiddle Ben)))
(outranked-by ?person ?boss)
end

;ex4.57
(query-driver-loop)

(replace-job ?j (computer programmer trainee))
(replace-job (computer wizard) ?j)

(replace ?p (Fect Cy D))

(and (replace ?p1 ?p2)
	 (salary ?p1 ?s1)
	 (salary ?p2 ?s2)
	 (lisp-value < ?s1 ?s2))
end

;ex4.58
(query-driver-loop)

(and 
 (job ?person (?div-p . ?rest-p))
 (supervisor ?person ?supervisor)
 (job ?supervisor (?div-s . ?rest-s))
 (not (same ?div-p ?div-s))
 )

(big-shot ?p)

end

;ex4.59
(query-driver-loop)
(meeting ?d (Friday . ?t))
;; (and (job ?person (?div . ?rest-p))
;; 	 (or (meeting whole-company ?day-and-time)
;; 		 (meeting ?div ?day-and-time)))
;(meeting-time ?p ?dt)
(meeting-time (Hacker Alyssa P) ?dt)
(meeting-time (Hacker Alyssa P) (Wednesday ?t))
end

;ex4.60
;名前に順序を付けるなどする必要あり
(query-driver-loop)
(lives-near ?p1 ?p2)
end
;;
(query-driver-loop)
(append-to-form (a b) (c d) ?z)

(append-to-form (a b) ?y (a b c d))

(append-to-form ?x ?y (a b c d))
end

;ex4.61
(query-driver-loop)
(?x next-to ?y in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))
end

;ex4.62
(query-driver-loop)
(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))

(last-pair (?x) (3))
end

;ex4.63
(query-driver-loop)
(grandson-of ?g ?s)
(son-of ?m ?s)
end


(query-driver-loop)
(not (baseball-fan (Bitdiddle Ben)))
end


;ex4.65
(query-driver-loop)
(wheel ?who)
(supervisor ?middle-manager ?person)
;; (supervisor (Aull DeWitt) (Warbucks Oliver)) ; 0
;; (supervisor (Cratchet Robert) (Scrooge Eben)) ; 0
;; (supervisor (Scrooge Eben) (Warbucks Oliver)) ; 1
;; (supervisor (Bitdiddle Ben) (Warbucks Oliver)) ; 3
;; (supervisor (Reasoner Louis) (Hacker Alyssa P)) ; 0
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben)) ;0
;; (supervisor (Fect Cy D) (Bitdiddle Ben)) ; 0
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) ; 1
(supervisor ?x (Warbucks Oliver))
(supervisor ?x (Bitdiddle Ben))
end

;ex4.68
(query-driver-loop)
(reverse (1) ?y)
(reverse (1 2) ?y)
(reverse (1 2 3) ?x)
;(reverse ?x (1 2 3))
;loop
end

;ex4.69

(query-driver-loop)
((great grandson) ?g ?ggs)
;((?rel grandson) Adam Irad)
;(?relationship Adam Cain)
;(?relationship Adam Irad)
end


;ex4.70
;問題文中の定義では無限ストリームになる

;ex4.71
;無限ループの場合に結果を表示できなくなる場合がある
;Louisの定義

(define (interleave s1 s2)
  (if (stream-null? s1)
	  s2
	  (cons-stream (stream-car s1)
				   (interleave s2 (stream-cdr s1)))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (stream-append
	  (find-assertions query-pattern frame)
	  (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
	  the-empty-stream
	  (interleave
	   (qeval (first-disjunct disjuncts) frame-stream)
	   (disjoin (rest-disjuncts disjuncts)
				frame-stream))))

(query-driver-loop)
(married Mickery ?x)
end


(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (stream-append-delayed
	  (find-assertions query-pattern frame)
	  (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
	  the-empty-stream
	  (interleave-delayed
	   (qeval (first-disjunct disjuncts) frame-stream)
	   (delay (disjoin (rest-disjuncts disjuncts)
					   frame-stream)))))
;; (query-driver-loop)
;; (married Mickery ?x)
;; end

;ex4.72,4.73
;無限のストリームに対応するため.

;; (query-driver-loop)
;; (or (married Mickery ?x)
;; 	(same Mickery ?x))
;; end

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;; 	  the-empty-stream
;; 	  (interleave
;; 	   (stream-car stream)
;; 	   (flatten-stream (stream-cdr stream)))))



(define (flatten-stream stream)
  (if (stream-null? stream)
	  the-empty-stream
	  (interleave-delayed
	   (stream-car stream)
	   (delay (flatten-stream (stream-cdr stream))))))



;ex4.74
;a
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map
   stream-car
   (stream-filter 
	(lambda (f) (not (stream-null? f)))
	stream)))

;; (define (stream-flatmap proc s)
;;   (flatten-stream (stream-map proc s)))

;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;; 	  the-empty-stream
;; 	  (interleave-delayed
;; 	   (stream-car stream)
;; 	   (delay (flatten-stream (stream-cdr stream))))))

;b
;空ストリームと単一のストリームしかこないので, 影響しない.

;ex4.75
(query-driver-loop)
(job ?x (computer wizard))
(job ?x (computer programmer))
(and (job ?x ?j) (job ?anyone ?j))
end



(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (let ((result (qeval (uniqued-query operands)
						  (singleton-stream frame))))
	   (if (and (not (stream-null? result))
				(stream-null? (stream-cdr result)))
		   result
		   the-empty-stream)))
   frame-stream))

(define (uniqued-query exps) (car exps))

(put 'unique 'qeval uniquely-asserted)


(query-driver-loop)
(unique (job ?x (computer wizard)))
(unique (job ?x (computer programmer)))

(and (job ?x ?j) (job ?anyone ?j))
(and (job ?x ?j) (unique (job ?anyone ?j)))
end

(query-driver-loop)

(and (supervisor ?x ?y)
	 (unique (supervisor ?anyone ?y)))

(and (supervisor ?middle-manager ?person)
	 (supervisor ?x ?middle-manager))

(and (supervisor ?middle-manager ?person)
	 (unique (supervisor ?x ?middle-manager)))
end

;;;;
(use slib)
(require 'trace)

;;;
(query-syntax-process '(job ?x (computer programmer)))

(display-stream 
 (qeval (query-syntax-process '(job ?x (computer programmer)))
		(singleton-stream '())))

;; (display-stream 
;;  (simple-query (query-syntax-process '(job ?x (computer programmer)))
;; 		(singleton-stream '())))


;; (define (instantiate exp frame unbound-var-handler)
;;   (define (copy exp)
;; 	(cond ((var? exp)
;; 		   (let ((binding (binding-in-frame exp frame)))
;; 			 (if binding
;; 				 (copy (binding-value binding))
;; 				 (unbound-var-handler exp frame))))
;; 		  ((pair? exp)
;; 		   (cons (copy (car exp)) (copy (cdr exp))))
;; 		  (else exp)))
;;   (newline)
;;   (display "exp: ")
;;   (display exp)
;;   (newline)
;;   (display "frame: ")
;;   (display frame)
;;   (newline)
;;   (copy exp))
;(trace query-syntax-process)
(trace qeval)

(query-driver-loop)
(job ?x (computer programmer))
end

(untrace qeval)
(untrace query-syntax-process)




;; (define (instantiate exp frame unbound-var-handler)
;;   (define (copy exp)
;; 	(cond ((var? exp)
;; 		   (let ((binding (binding-in-frame exp frame)))
;; 			 (if binding
;; 				 (copy (binding-value binding))
;; 				 (unbound-var-handler exp frame))))
;; 		  ((pair? exp)
;; 		   (cons (copy (car exp)) (copy (cdr exp))))
;; 		  (else exp)))
;;   (copy exp))

;(trace conjoin)

(query-driver-loop)
(and (job ?person (computer programmer))
	 (address ?person ?where))
end

(untrace conjoin)

;4.4.4.3
(display-stream
 (find-assertions (query-syntax-process '(job ?x (computer programmer)))
				  (singleton-stream '())))


(fetch-assertions (query-syntax-process '(job ?x (computer programmer)))
				  (singleton-stream '()))


(check-an-assertion '(job (Hacker Alyssa P) (computer programmer))
					(query-syntax-process '(job ?x (computer programmer)))
					(singleton-stream '()))

(check-an-assertion '(job (Bitdiddle Ben) (computer wizard))
					(query-syntax-process '(job ?x (computer programmer)))
					(singleton-stream '()))

(pattern-match
			  (query-syntax-process '(job ?x (computer programmer)))
			  '(job (Hacker Alyssa P) (computer programmer))
			  (singleton-stream '()))


(pattern-match
			  (query-syntax-process '(job ?x ?y))
			  '(job (Hacker Alyssa P) (computer programmer))
			  (singleton-stream '()))

(binding-in-frame '(? y)
 (pattern-match
  (query-syntax-process '(job ?x ?y))
  '(job (Hacker Alyssa P) (computer programmer))
  (singleton-stream '())))

;4.4.4.4

(query-driver-loop)
(wheel ?p)
end

(fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(apply-rules (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
			   (query-syntax-process '(wheel ?p)) (singleton-stream '()))

(display-stream
 (stream-map 
  (lambda (frame)
	(instantiate
	 (query-syntax-process '(wheel ?p))
	 frame
	 (lambda (v f) (v))))
  (apply-a-rule (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '())))
				(query-syntax-process '(wheel ?p)) (singleton-stream '()))))

(rename-variables-in 
 (stream-car (fetch-rules  (query-syntax-process '(wheel ?p)) (singleton-stream '()))))



;(trace unify-match)
(query-driver-loop)
(wheel ?p)
end
;(untrace unify-match)

(unify-match 1 '(? x)
			 (singleton-stream '()))

(unify-match '(? x) 1
			 (singleton-stream '()))

(unify-match '(? x) '(? x)
			 (singleton-stream '()))

(query-syntax-process '(?x ?x))
;(?x ?x) (?y ?y)
(unify-match '((? x) (? x)) '((? y) (? y))
			 (singleton-stream '()))

(unify-match '((? y) (? x)) '((? x) (? y))
			 (singleton-stream '()))


(unify-match '(? x) 1
			 (extend '(? x) 2
					 (singleton-stream '())))

(unify-match '(? x) 2
			 (extend '(? x) 2
					 (singleton-stream '())))

(unify-match '((? x) (? y)) '((? y) 1)
			 (extend '(? x) 2
					 (singleton-stream '())))

(depends-on? '((? x) (? x)) '(? x) 
			 (singleton-stream '()))
