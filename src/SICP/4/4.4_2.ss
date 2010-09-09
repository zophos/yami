;ex4.76
(define (conjoin-ex4.76 conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-streams
        (qeval (first-conjunct conjuncts) frame-stream)
        (conjoin-ex4.76 (rest-conjuncts conjuncts) frame-stream))))

(define (merge-frame-streams s1 s2)
  (stream-flatmap (lambda (frame1)
					(stream-filter
					 (lambda (frame) (not (equal? frame 'failed)))
					 (stream-map
					  (lambda (frame2)
						(merge-frames frame1 frame2))
					  s2)))
                  s1))


(define (merge-frames frame1 frame2)
  (if (null? frame1)
      frame2
      (let ((var (caar frame1))
            (val (cdar frame1)))
		(let ((extension (extend-if-possible var val frame2)))
		  (if (equal? extension 'failed)
			  'failed
			  (merge-frames (cdr frame1) extension))))))

(put 'and 'qeval conjoin-ex4.76)

(query-driver-loop)
(and (address ?x ?y) (supervisor ?x ?z))

(and (address ?person-1 (?town . ?rest-1))
	 (address ?person-2 (?town . ?rest-2))
	 (not (same ?person-1 ?person-2)))
end

(put 'and 'qeval conjoin)
(query-driver-loop)
(and (address ?x ?y) (supervisor ?x ?z))


(and (address ?person-1 (?town . ?rest-1))
	 (address ?person-2 (?town . ?rest-2))
	 (not (same ?person-1 ?person-2)))
end

(define (first-binding frame)
  (car frame))

(define (rest-bindings frame)
  (cdr frame))


(define (conjoin2 conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
	  frame-stream
	(let ((join1 (qeval (first-conjunct conjuncts)
						frame-stream))
		  (join2 (conjoin2 (rest-conjuncts conjuncts)
						   frame-stream)))
	  (stream-flatmap (lambda (frame1)
						(stream-flatmap (lambda (frame2)
										  (let ((result (merge-frame frame1 frame2)))
											(if (eq? result 'false)
											  the-empty-stream
											  (singleton-stream result))))
									join2))
					  join1))))

; フレームのマージ
(define (merge-frame frame1 frame2)
  (if (null? frame1)
	frame2
	(let ((binding (first-binding frame1)))
	  (let ((var (binding-variable binding))
			(val (binding-value binding)))
		(let ((binding2 (binding-in-frame var frame2))) 
		  (if binding2
			  (let ((match-result (merge-variable val frame1 
												  (binding-value binding2) frame2
												  frame2)))
				(if (eq? match-result 'false)
				  'false
				  (merge-frame (rest-bindings frame1) match-result)))
			  (merge-frame (rest-bindings frame1)
						   (extend var val frame2))))))))

; 2つのフレームに同じ変数がバインドされている場合に
; それらに矛盾がないかをチェックし、可能であればマージする
; ユニファイと同じように、片方が未束縛だった場合に
; もう片方の変数の値をバインドしてやってたりします
(define (merge-variable val1 frame1 val2 frame2 frame)
  (cond ((equal? val1 val2) frame)
		((var? val1)
		 (merge-if-possible val1 frame1 val2 frame2 frame))
		((var? val2)
		 (merge-if-possible val2 frame2 val1 frame1 frame))
		((and (pair? val1) (pair? val2))
		 (merge-variable (cdr val1) frame1
						 (cdr val2) frame2
						 (merge-variable (car val1) frame1
										 (car val2) frame2
										 frame)))
		(else
		  'false)))

; extend-if-possibleのぱくり
(define (merge-if-possible val1 frame1 val2 frame2 frame)
  (let ((binding (binding-in-frame val1 frame1)))
	(cond (binding
			(merge-variable (binding-value binding) frame1
							val2 frame2
							frame))
		  ((var? val2)
		   (let ((binding (binding-in-frame val2 frame2)))
			 (if binding
			   (merge-variable val1 frame1
							   (binding-value binding) frame2
							   frame)
			   (extend val1 val2 frame))))
		  (else (extend val1 val2 frame)))))


(put 'and 'qeval conjoin2)

(query-driver-loop)
(and (address ?x ?y) (supervisor ?x ?z))

(and (address ?person-1 (?town . ?rest-1))
	 (address ?person-2 (?town . ?rest-2))
	 (not (same ?person-1 ?person-2)))
end
