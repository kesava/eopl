#lang eopl
; Section 4.2
; Exercise 4.2.1 - use beta-reductions

; 1
; ((lambda (x) (x (y x)))
; z)
; => (z (y z))

; 2
; ((lambda (x) (x y))
;  (lambda (y) (x y)))
; => 
; ((lambda (z1) (z1 y))
;  (lambda (z2) (x z2)))
; =>
; ((lambda (z2) (x z2)) y)
; =>
; (x y)

; 3
; ((lambda (x)
;    (lambda (y) ((x y) z)))
;  (lambda (a) y))
; =>
; ((lambda (z1)
;    (lambda (z2) ((z1 z2) z)))
;  (lambda (a) y))
; =>
; ((lambda (z2) ((lambda (a) y) z2) z))
; =>
; ((lambda (a) y) z)
; =>
; y

; 4
; ((lambda (x)
;    (lambda (y)
;      ((lambda (x) (z x))
;       (lambda (y) (z y)))))
;  (lambda (y) y))
; =>
; (lambda (y)
;   ((lambda (z1) (z z1))
;    (lambda (z2) (z z2))))

; Exercise 4.2.2
(define-datatype lc-exp lc-exp?
  (lit-exp
   (datum number?))
  (var-exp
   (var symbol?))
  (lambda-exp
   (formal symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define var-exp?
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #t)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) #f)
      (else (eopl:error "lambda-exp?: error in syntax")))))

(define lambda-exp?
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #t)
      (app-exp (rator rand) #f)
      (else (eopl:error "lambda-exp?: error in syntax")))))
        

(define parse
  (lambda (datum)
    (cond
      ((number? datum) (make-lit-exp datum))
      ((symbol? datum) (make-var-exp datum))
      ((pair? datum)
       (if (eq? (car datum) 'lambda)
           (make-lambda-exp (caadr datum) (parse (caddr datum)))
           (make-app-exp (parse (car datum)) (parse (cadr datum)))))
      (else (eopl:error "parse: invalid concrete syntax")))))

(define beta-redex?
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) (and (lambda-exp? rator)
                                 (var-exp? rand)))
      (else (eopl:error "beta-redex? invalid concrete syntax")))))
      
(beta-redex? (parse '(lambda (x) (y z))))
; #f      
(beta-redex? (parse '((lambda (x) (y z)) z)))
; #t
(beta-redex? (parse '(lambda (x) ((lambda (x) (y x)) z))))
; #f
