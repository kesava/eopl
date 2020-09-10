#lang eopl
(require "../define-record.rkt")
(require "../string-parser.rkt")
(require readline/readline)
(require "../util.rkt")

(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(define list-of-exp?
  (lambda (l-ex)
    (and (list? l-ex) (apply and-l (map (lambda (x) (exp? x)) l-ex)))))

(define exp?
  (lambda (exp)
    (or (lit? exp) (varref? exp) (app? exp))))

(define-record lit (datum))
(define-record varref (var))
(define-record lambda (formal body))
(define-record app (rator rand))
(define-record if (test-exp then-exp else-exp))

(define eval-exp
  (lambda (exp)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env init-env var))
      (app (rator rands)
               (let ((proc (eval-exp rator))
                     (args (eval-rands rands)))
                 (apply-proc proc args)))
      (else (eopl:error "Invalid AST: " exp)))))

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

(define-record prim-proc (prim-op))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (begin
                             ;(displayln prim-op "->" args)
                             (apply-prim-op prim-op args)))
      (else (eopl:error "Invalid procedure: " proc)))))

(define apply-prim-op
  (lambda (prim-op args)
    (case prim-op
      ((+) (+ (car args) (cadr args)))
      ((-) (- (car args) (cadr args)))
      ((*) (* (car args) (cadr args)))
      ((add1) (+ (car args) 1))
      ((sub1) (- (car args) 1))
      ((minus) (- 0 (car args))) ; Exercise 5.1.3
      ((list) args) ; Exercise 5.1.4
      ((car) (caar args)) ; Exercise 5.1.4
      ((cdr) (cdar args)) ; exercise 5.1.4
      ((cons) (cons (car args) (cadr args))) ; Exercise 5.1.4
      (else (eopl:error "Invalid  prim-op name: " prim-op)))))

(define prim-op-names '(+ - * add1 sub1 minus list car cdr cons))

(define init-env
  (extend-env
   prim-op-names
   (map make-prim-proc prim-op-names)
   (extend-ff 'emptylist '() the-empty-env)))

(define run
  (lambda (x)
    (eval-exp (parse x))))

; Exercise 5.1.1
(define local-parse
  (lambda (datum)
    (cond
      ((number? datum) ( datum))
      ((symbol? datum) (make-varref datum))
      ((pair? datum) (make-app (local-parse (car datum)) (map local-parse (cdr datum))))
      (else (eopl:error "parse: invalid concrete syntax")))))

(define read-eval-print-0
  (lambda ()
    (write (eval-exp (local-parse (readline "-->"))))
    (newline)
    (read-eval-print-0)))

; Exercise 5.1.2
(define parse character-string-parser)

; lang eopl doesnt support standard scheme read-line. So using a different package readline.
(define read-eval-print-1
  (lambda ()
    (write (eval-exp (parse (readline "-->"))))
    (newline)
    (read-eval-print-1)))
