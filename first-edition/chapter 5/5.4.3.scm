#lang eopl
(require "../define-record.rkt")
(require "../string-parser.rkt")
(require readline/readline)
(require "../util.rkt")

(define-record lit (datum))
(define-record varref (var))
(define-record lambda (formal body))
(define-record app (rator rand))
(define-record if (test-exp then-exp else-exp))
(define-record let (decls body))
(define-record decl (var exp))
(define-record proc (formals body))
(define-record closure (formals body env))

(define true-value?
  (lambda (x)
    (not (zero? x))))

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env env var))
      (if (test-exp then-exp else-exp)
          (if (true-value? (eval-exp test-exp env))
              (eval-exp then-exp env)
              (eval-exp else-exp env)))
      (app (rator rands)
               (let ((proc (eval-exp rator env))
                     (args (eval-rands rands env)))
                 (apply-proc proc args)))
      (let (decls body)
        (let ((vars (map decl->var decls))
              (exps (map decl->exp decls)))
          (let ((new-env (extend-env vars
                                     (eval-rands exps env)
                                     env)))
            (eval-exp body new-env))))
      (proc (formals body)
            (make-closure formals body env))
      (else (eopl:error "Invalid AST: " exp)))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-exp rand env)) rands)))

(define-record prim-proc (prim-op))

;(define apply-proc (lambda (f args) (f args)))

(define apply-proc
  (lambda (proc args)
    (displayln "proc: " proc "; args: " args)
    (variant-case proc
      (prim-proc (prim-op) (begin
                             (displayln prim-op "->" args)
                             ((apply-prim-op prim-op) args)))
      (closure (formals body env)
               (eval-exp body (extend-env formals args env)))
      (else (eopl:error "Invalid procedure: " proc)))))

(define ft->01
  (lambda (x)
    (if x
        1
        0)))

(define apply-prim-op
  (lambda (prim-op)
    (case prim-op
      ((+) (lambda (args) (+ (car args) (cadr args))))
      ((-) (lambda (args) (- (car args) (cadr args))))
      ((*) (lambda (args) (* (car args) (cadr args))))
      (else (eopl:error "Invalid  prim-op name: " prim-op)))))

(define prim-op-names '(+ - * add1 sub1 minus list car cdr cons equal greater less zero null eq))

(define init-env
  (extend-env
   prim-op-names
   (map make-prim-proc prim-op-names)
   (extend-ff 'emptylist '() the-empty-env)))

(define run
  (lambda (x)
    (eval-exp (parse x))))

(define parse character-string-parser)

; lang eopl doesnt support standard scheme read-line. So using a different package readline.
(define repl
  (lambda ()
    (write (eval-exp (parse (readline "-->")) init-env))
    (newline)
    (repl)))

; Exercise 5.4.1
;> (repl)
;let f = proc (y, z) *(y, z) in f(2, 8)
;16
;(proc (y, z) *(y, z))(2, 8)
;16