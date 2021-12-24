#lang eopl

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" sym)
        (let ((syms (car (car env)))
              (vals (cadr (car env)))
              (env (cdr env)))
          (let ((pos (rib-find-position sym syms)))
            (if (number? pos)
                (list-ref vals pos)
                (apply-env env sym)))))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define rib-find-position list-find-position)

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

(define true-value?
  (lambda (val) (cond
                  ((null? val) #f)
                  ((integer? val) (if (> val 0) #t #f))
                  (else (if val #t #f)))))
      

(define-datatype program program?
  (a-program
   (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (primapp-exp (prim primitive?)
               (rands (list-of expression?))))
  
(define-datatype primitive primitive?
  (add-prim)
  (subtract-prim)
  (mult-prim)
  (minus-prim)
  (list-prim)
  (car-prim)
  (cdr-prim)
  (cons-prim)
  (setcar-prim)
  (incr-prim)
  (decr-prim)
  (equal-prim)
  (zero-prim)
  (greater-prim)
  (print-prim))

(define eval-program
  (lambda (pgm)
    (cases program pgm
        (a-program (body)
                   (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      )))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (if (> (length args) 2) (eopl:error "+ takes only two args") (+ (car args) (cadr args))))
      (subtract-prim () (- (car args) (cadr args)))
      (minus-prim () (- 0 (car args)))
      (mult-prim () (* (car args) (cadr args)))
      (list-prim () args)
      (car-prim () (caar args))
      (setcar-prim () (cons (car args) (cdadr args)))
      (cdr-prim () (cdar args))
      (cons-prim () (cons (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (equal-prim () (= (car args) (cadr args)))
      (zero-prim () (= (car args) 0))
      (greater-prim () (> (car args) (cadr args)))
      (print-prim () (begin (map (lambda (x) (display x)) args)))
      )))

(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))

(define scanner-spec-3-1
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar-3-1
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (primitive ("+") add-prim)
    (primitive ("-") subtract-prim)
    (primitive ("*") mult-prim)
    (primitive ("print") print-prim)
    (primitive ("list") list-prim)
    (primitive ("car") car-prim)
    (primitive ("cdr") cdr-prim)
    (primitive ("cons") cons-prim)
    (primitive ("setcar") setcar-prim)
    (primitive ("minus") minus-prim)
    (primitive ("add1") incr-prim)
    (primitive ("equal?") equal-prim)
    (primitive ("zero?") zero-prim)
    (primitive ("greater?") greater-prim)
    (primitive ("sub1") decr-prim)))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1))


(define run
  (lambda (string)
    (eval-program
     (scan&parse string))))

(define repl
  (sllgen:make-rep-loop "-->" eval-program
                        (sllgen:make-stream-parser
                         scanner-spec-3-1
                         grammar-3-1)))

; setcar(1, cons(4, cons(8, emptylist)))
; (1 8)
