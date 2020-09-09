#lang eopl
(require "define-record.rkt")
(require "string-parser.rkt")
(require readline/readline)

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

;(define-datatype exp exp?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (var symbol?))
;  (app-exp
;   (rator exp?)
;   (rands list-of-exp?)))

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

(define list-index
  (lambda (s los)
    (define helper
      (lambda (los n)
        (if (null? los)
            -1
            (if (equal? (car los) s)
                n
                (helper (cdr los) (+ 1 n))))))
    (helper los 0)))

(define ribassoc
  (lambda (s los v fail-value)
    (let ([index (list-index s los)])
      (if (eq? index -1)
          fail-value
          (vector-ref v index)))))

(define-datatype ff ff?
  (empty-ff)
  (extended-ff (sym symbol?) (val number?) (next-ff ff?))
  (extended-ff* (sym-list (list-of symbol?)) (var-list vector?) (next-ff ff?)))

(define (create-empty-ff) (empty-ff))
(define (extend-ff sym val f) (extended-ff sym val f))
(define (extend-ff* sym-list val-list ff) (extended-ff* sym-list (list->vector val-list) ff))

(define (apply-ff f symbol)
  (cases ff f
    (empty-ff () (eopl:error "Empty ff: no association for symbol" symbol))
    (extended-ff (sym val next-ff)
      (if (eq? sym symbol)
        val
        (apply-ff next-ff symbol)))
    (extended-ff* (sym-list val-vector ff)
                  (let ((val (ribassoc symbol sym-list val-vector '*fail*)))
                    (if (eq? val '*fail*)
                        (apply-ff ff symbol)
                        val)))
    (else (eopl:error "apply-ff: invalid finite function" f))))

(define the-empty-env (create-empty-ff))
(define extend-env extend-ff*)
(define apply-env apply-ff)

(define-datatype pproc pproc?
  (prim-proc (prim-op symbol?)))

(define apply-proc
  (lambda (proc args)
    (cases pproc proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
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
      (else (eopl:error "Invalid  prim-op name: " prim-op)))))

(define prim-op-names '(+ - * add1 sub1 minus))

(define init-env
  (extend-env
   prim-op-names
   (map make-prim-proc prim-op-names)
   the-empty-env))

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

; (read-eval-print-1)
; *(add1(2), -(6,4))
; 6

; Exercise 5.1.3
; Added above
; (read-eval-print-1)
; minus(+(minus(5), 9))
; -4