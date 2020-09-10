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

(define true-value?
  (lambda (x)
    (not (zero? x))))

(define eval-exp
  (lambda (exp)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env init-env var))
      (if (test-exp then-exp else-exp)
          (if (true-value? (eval-exp test-exp))
              (eval-exp then-exp)
              (eval-exp else-exp)))
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

(define ft->01
  (lambda (x)
    (if x
        1
        0)))

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
      ((equal) (ft->01 (= (car args) (cadr args)))) ; Exercise 5.2.2
      ((greater) (ft->01 (> (car args) (cadr args)))) ; Exercise 5.2.2
      ((less) (ft->01 (< (car args) (cadr args)))) ; Exercise 5.2.2
      ((zero) (ft->01 (= (car args) 0))) ; Exercise 5.2.2
      (else (eopl:error "Invalid  prim-op name: " prim-op)))))

(define prim-op-names '(+ - * add1 sub1 minus list car cdr cons equal greater less zero))

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
    (write (eval-exp (parse (readline "-->"))))
    (newline)
    (repl)))

; Exercise 5.2.2
;> (repl)
;equal(3,3)
;1
;zero(sub1(5))
;0
;if greater(2,3) then 5 else 6
;6