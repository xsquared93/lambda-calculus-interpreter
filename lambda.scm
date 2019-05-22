(define apply-in-underlying-scheme apply)

(define (eval exp)
  (cond ((self-evaluating? exp) exp)
	((and (lambda? exp)
	      (application? (car (lambda-body exp)))
	      (lambda? (operator (car (lambda-body exp)))))
	 (make-lambda (lambda-parameters exp) (eval (car (lambda-body exp)))))
	((and (lambda? exp)
	      (application? (car (lambda-body exp)))
	      (not (lambda? (operator (car (lambda-body exp))))))
	 (make-lambda (lambda-parameters exp) (car (lambda-body exp))))
	((application? exp)
	 (apply (eval (operator exp))
		(list-of-values (operands exp))))))

(define (apply procedure arguments)
   (apply-beta-reduction procedure arguments))

(define (apply-beta-reduction procedure arguments)
  (cond ((lambda? procedure)
	 (if (not (pair? (car (lambda-body procedure))))
	     (car arguments)
	     (subst (car (lambda-body procedure)) (car arguments))))
	(error "Invalid procedure --APPLY-BETA-REDUCTION" procedure)))

(define (list-of-values exps)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps))
	    (list-of-values (cdr exps)))))

(define (self-evaluating? exp)
  (cond ((and (lambda? exp)
	      (not (application? (car (lambda-body exp))))) true)
	((variable? exp) true)
	(else false)))

(define (application? exp)
  (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (variable? exp)
  (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; examples

;; example 1
(define exp1 '((lambda (x) x) (lambda (y) y)))
(eval exp1)
;;value: (lambda (y) y)

;; example 2

(define exp2 '((lambda (x) x) y))
(eval exp2)
;;value: y

;; example 3

(define exp3 '((lambda (x) x) ((lambda (x) x) (lambda (z) ((lambda (x) x) z)))))
(eval exp3)
;;value: (lambda (z) z)

;; example 4
(define exp4 '((lambda (x) (x x)) y))
;; (eval exp4)
;; value: (y y)

;; example 5
(define exp5 '((lambda (x) (lambda (y) y)) d))
;; (eval exp5)
;; value: (lambda (y) y)
;; scratch

(define (subst body argument)
  (cond ((null? body) '())
	(else (cons argument
		    (subst (cdr body) argument)))))
		
		
(define (redex? exp)
  (if (and (application? exp)
	   (lambda? (operator exp)))
      true
      false))

(define (normal-form? exp)
  (if (and (not (redex? exp))
	   (not (redex? (car (lambda-body exp)))))
      true
      false))
