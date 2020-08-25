#lang eopl
(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(define list-of-exp?
  (lambda (l-ex)
    (and (list? l-ex) (apply and-l (map (lambda (x) (exp? x)) l-ex)))))

(define-datatype exp exp?
  (lit-exp
   (datum number?))
  (var-exp
   (var symbol?))
  (app-exp
   (rator exp?)
   (rands list-of-exp?)))

(define  eval-exp
  (lambda (expr)
    (cases exp expr
      (lit-exp (datum) datum)
      (var-exp (var) (apply-env init-env var))
      (app-exp (rator rands)
               (let ((proc (eval-exp rator))
                     (args (eval-rands rands)))
                 (apply-proc proc args)))
      (else (eopl:error "Invalid AST: " exp)))))

(define make-ff
  (lambda (current-env)
    (let ((env current-env))
      (lambda (message)
        (case message
          ((empty?) (lambda () (null? env)))
          ((extend) (lambda (sym val) (begin
                                        (set! env (cons (list sym val) env))
                                        (make-ff env))))
          ((apply) (lambda (sym) (letrec ((findsym (lambda (envr) (if (null? envr)
                                                                  (eopl:error "No association for symbol: " sym)
                                                                  (if (eq? (caar envr) sym)
                                                                      (cadar envr)
                                                                      (findsym (cdr envr)))))))
                                   (findsym env))))
          ((print) (lambda () (display env)))
          (else (eopl:error "Invalid message for make-ff")))))))