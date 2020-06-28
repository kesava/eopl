#lang eopl

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (math-exp (id symbol?) (rand1 expression?) (rand2 expression?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (lambda-exp (ids list?) (body expression?))
  (app-exp (rator expression?) (rand expression?)))

(define parse
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((number? datum) (lit-exp datum))
      ((pair? datum)
       (cond
         ((or (eq? (car datum) '+) (eq? (car datum) '*))
          (math-exp (car datum) (parse (cadr datum)) (parse (caddr datum))))
         ((eq? (car datum) 'if)
          (if-exp (parse (cadr datum)) (parse (caddr datum)) (parse (cadddr datum))))
         ((eq? (car datum) 'lambda)
          (lambda-exp (cadr datum) (parse (caddr datum))))
         (else (app-exp (parse (car datum)) (parse (cadr datum))))))
      (else (eopl:error 'parse "Invalid syntax")))))


(define unparse
  (lambda (exp)
    (cases expression exp
      (lit-exp (num) num)
      (var-exp (id) id)
      (math-exp (op rand1 rand2) (list op (unparse rand1) (unparse rand2)))
      (if-exp (test-exp true-exp false-exp) (list 'if (unparse test-exp) (unparse true-exp) (unparse false-exp)))
      (lambda-exp (ids body) (list 'lambda ids (unparse body)))
      (app-exp (rator rand) (list (unparse rator) (unparse rand))))))


(define-datatype mexp mexp?
  (numE (datum number?))
  (idE (datum symbol?))
  (plusE (l-exp mexp?) (r-exp mexp?))
  (multiE (l-exp mexp?) (r-exp mexp?))
  (appE (id symbol?) (arg mexp?)))

(define-datatype func func?
  (fd (name symbol?) (arg symbol?) (body mexp?)))



#|
(define interp
  (lambda (expp)
    (cases mexp expp
      (numE (num) num)
      (plusE (l r) (+ (interp l) (interp r)))
      (multiE (l r) (* (interp l) (interp r))))))
|#

