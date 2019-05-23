(define apply-in-underlying-scheme apply)

(define (eval exp)
  (cond ((variable? exp) exp)
	((lambda? exp)
	 (make-lambda (lambda-parameters exp)
		      (eval (first (lambda-body exp)))))
	((application? exp)
	 (apply (eval (operator exp))
		      (list-of-values (operands exp))))
	(else (error "Unknown Expression Type -- EVAL" exp))))

(define (apply procedure arguments)
  (subst (lambda-body procedure)
	 (arg arguments)))

(define (subst body arguments)
  (let ((solution (subst-helper body arguments)))
    (if (eq? (length solution) 1)
	(first solution)
	solution)))

(define (subst-helper body argument)
  (if (null? body)
      '()
      (cons argument
	    (subst-helper (cdr body) argument))))

(define (list-of-values exps)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps))
	    (list-of-values (rest-operands exps)))))

;;; data representation

;; representation of abstractions ie lambda expressions

(define (make-lambda parameters body)
  (list 'lambda parameters body))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

;; representation of applications

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; variables

(define (variable? exp) (symbol? exp))

;; selectors for operands

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (no-operands? ops) (null? ops))

;;; other

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; tests

;; example 1
(define exp1 '((lambda (x) x) (lambda (y) y)))
(eval exp1)
;;value: (lambda (y) y)

;; example 2

(define exp2 '((lambda (x) x) y))
(eval exp2)
;;value: y

;; example 3

(define exp3 '((lambda (x) x) ((lambda (y) y) (lambda (z) ((lambda (x) x) z)))))
(eval exp3)
;;value: (lambda (z) z)
