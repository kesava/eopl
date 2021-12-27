#lang eopl

(define iota
  (lambda (n)
    (iota-helper n 0)))

(define iota-helper
  (lambda (rest so-far)
    (if (zero? rest)
        '()
        (cons so-far (iota-helper (- rest 1) (+ so-far 1))))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals vector?)
   (env environment?))
  (recursively-extended-env-record
   (proc-names (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (bodies (list-of expression?))
   (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
          (let ((pos (rib-find-position sym syms)))
            (if (number? pos)
                (a-ref pos vals)
                (apply-env-ref old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (rib-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure
                                              (list-ref idss pos)
                                              (list-ref bodies pos)
                                              env)
                                             (apply-env-ref old-env sym)))))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) idss bodies)
           env)))))

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
  (let-exp (ids (list-of symbol?)) (rands (list-of expression?)) (body expression?))
  (letrec-exp (proc-names (list-of symbol?)) (idss (list-of (list-of symbol?))) (bodies (list-of expression?)) (letrec-body expression?))
  (proc-exp (ids (list-of symbol?)) (body expression?))
  (primapp-exp (prim primitive?)
               (rands (list-of expression?)))
  (varassign-exp (id symbol?) (rhs-exp expression?))
  (begin-exp (exp expression?) (exps (list-of expression?)))
  (define-exp (id symbol?) (exp expression?))
  (app-exp (rator expression?) (rands (list-of expression?))))
  
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

(define-datatype procval procval?
  (closure (ids (list-of symbol?))
           (body expression?)
           (env environment?)))

(define-datatype reference reference?
  (a-ref
   (position integer?)
   (vec vector?)))

(define eval-program
  (lambda (pgm)
    (cases program pgm
        (a-program (body)
                   (eval-expression body (init-env))))))

(define begin-eval
  (lambda (exps env acc)
    (if (null? exps)
        acc
        (begin-eval (cdr exps) env (eval-expression (car exps) env)))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively
                                    proc-names idss bodies env)))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim (vector->list args))))
      (proc-exp (ids body) (closure ids body env))
      (varassign-exp (id rhs-exp)
                     (begin
                       (setref!
                        (apply-env-ref env id)
                        (eval-expression rhs-exp env))
                       1))
      (begin-exp (exp exps)
                 (begin-eval exps env (eval-expression exp env)))
      (define-exp (id exp) (extend-env (list id) (vector (eval-expression exp env)) env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procval proc args)
                     (eopl:error "Procedure not defined?"))))
      )))

(define eval-rands
  (lambda (rands env)
    (list->vector (map (lambda (x) (eval-rand x env)) rands))))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (if (= (length ids) (vector-length args))
                   (eval-expression body (extend-env ids args env))
                   (eopl:error "Wrong number of arguments"))))))

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
      (print-prim () (display (car args)))
      )))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val)))))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))


(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     (vector 1 5 10 '())
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
    (expression ("let" (separated-list identifier "=" expression ",") "in" expression) let-exp)
    (expression ("letrec" (separated-list identifier "(" (separated-list identifier ",") ")" "=" expression ",") "in" expression) letrec-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (separated-list expression ",") ")") app-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression ("begin" expression ";" (separated-list expression ";") "end") begin-exp)
    (expression ("define" identifier "=" expression) define-exp)
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




; begin
; -->begin +(3,4);  end
; 7
; -->begin +(3,4); +(4,5) end
; 9
