#lang eopl

(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(define list-of-lc-exp?
  (lambda (l-ex)
    (and (list? l-ex) (apply and-l (map (lambda (x) (lc-exp? x)) l-ex)))))

(define-datatype lc-exp lc-exp?
  (lit-exp
   (datum number?))
  (var-exp
   (var symbol?))
  (app-exp
   (rator lc-exp?)
   (rands list-of-lc-exp?)))

(define  eval-exp
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) datum)
      (var-exp (var) (apply-env init-env var))
      (app-exp (rator rands)
               (let ((proc (eval-exp rator))
                     (args (eval-rands rands)))
                 (apply-proc proc args)))
      (else (eopl:error "Invalid AST: " exp)))))