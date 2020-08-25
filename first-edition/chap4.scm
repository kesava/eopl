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

(define app-exp?
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) #t)
      (else (eopl:error "lambda-exp?: error in syntax")))))

(define lambda-exp-formal
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) formal)
      (app-exp (rator rand) #f)
      (else (eopl:error "lambda-exp-formal: error in syntax")))))

(define lambda-exp-body
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) body)
      (app-exp (rator rand) #f)
      (else (eopl:error "lambda-exp-body: error in syntax")))))

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

(define unparse
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) datum)
      (var-exp (var) var)
      (lambda-exp (formal body) (list 'lambda (list formal) (unparse body)))
      (app-exp (rator rand) (list (unparse rator) (unparse rand)))
      (else (eopl:error "unparse: invalid abstract sybtax" exp)))))

(define free?
  (lambda (var exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (v) (eq? v var))
      (lambda-exp (formal body) (if (eq? formal var)
                                    #f
                                    (free? var body)))
      (app-exp (rator rand) (or (free? var rator)
                                (free? var rand))))))

(define beta-redex?
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) (and (lambda-exp? rator)
                                 (lc-exp? rand)))
      (else (eopl:error "beta-redex?: invalid concrete syntax")))))

(beta-redex? (parse '(lambda (x) (y z))))
; #f      
(beta-redex? (parse '((lambda (x) (y z)) z)))
; #t
(beta-redex? (parse '(lambda (x) ((lambda (x) (y x)) z))))
; #f

(define beta-redex-lambda
  (lambda (r-exp)
    (cases lc-exp r-exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) rator)
      (else (eopl:error "beta-redex-lambda: invalid concrete syntax")))))

(define beta-redex-var
  (lambda (r-exp)
    (cases lc-exp r-exp
      (lit-exp (datum) #f)
      (var-exp (var) #f)
      (lambda-exp (formal body) #f)
      (app-exp (rator rand) rand)
      (else (eopl:error "beta-redex-lambda: invalid concrete syntax")))))


(define gensym
  (let ([counter 1233])
    (lambda ([x 'g0])
      (if (number? x)
        (set! counter x)
        (begin
          (set! counter (+ counter 1))
          (string->symbol (string-append (symbol->string x) (number->string counter))))))))

(define substitute
  (lambda (e m x)
    (cases lc-exp e
      (lit-exp (datum) datum)
      (var-exp (var) (if (equal? e (parse x))
                         m
                         e))
      (lambda-exp (formal body)
                  (cond
                    ((equal? formal (parse x)) e)
                    ((not (free? x body)) e)
                    ((not (free? formal m)) (make-lambda-exp formal (substitute body m x)))
                    (else (let ((z (gensym)))
                            (make-lambda-exp z (substitute (substitute body (parse z) formal) m x))))))
      (app-exp (rator rand) (make-app-exp (substitute rator m x) (substitute rand m x)))
      (else (eopl:error "substitute: invalid concrete syntax")))))

(unparse (substitute (parse '(a b)) (parse 'c) 'b))
; (a c)
(unparse (substitute (parse '(lambda (a) (a b))) (parse 'a) 'b))
; (lambda (g01234) (g01234 a))

; Exercise 4.2.4
(define beta-reduce
  (lambda (r-exp)
    (if (beta-redex? r-exp)
        (let ((e (lambda-exp-body (beta-redex-lambda r-exp)))
              (m (beta-redex-var r-exp))
              (x (lambda-exp-formal (beta-redex-lambda r-exp))))
          (substitute e m x))
        r-exp)))

(unparse (beta-reduce (parse '((lambda (x) (y x)) z))))
; (y z)

(unparse (beta-reduce (parse '((lambda (x) (lambda (y) (x y))) (y w)))))
;(lambda (g01235) ((y w) g01235))


; Exercise 4.2.5
(define eta-redex?
  (lambda (exp)
    (if (lambda-exp? exp)
        (free? (lambda-exp-formal exp) (lambda-exp-body exp))
        #f)))

(eta-redex? (parse '(lambda (x) (y x))))
; #t
(eta-redex? (parse '(lambda (x) ((lambda (y) y) x))))
; #t
(eta-redex? (parse '((lambda (x) (y x)) z)))
; #f

(define answer?
  (lambda (exp)
    (not (app-exp? exp))))

(define reduce-once-appl
  (lambda (exp succeed fail)
    (cases lc-exp exp 
      (lit-exp (datum) (fail))
      (var-exp (var) (fail))
      (lambda-exp (formal body) (fail))
      (app-exp (rator rand)
               (if (and (beta-redex? exp) (answer? rand))
                   (succeed (beta-reduce exp))
                   (reduce-once-appl rator
                                    (lambda (reduced-rator) (succeed (make-app-exp reduced-rator rand)))
                                    (lambda () (reduce-once-appl rand
                                                                  (lambda (reduced-rand) (succeed (make-app-exp rator reduced-rand)))
                                                                  fail))))))))

; Exercise 4.3.1
(define reduce-history
  (lambda (exp n)
    (if (eq? n 0)
        '()
        (reduce-once-appl (if (lc-exp? exp)
                              exp
                              (parse exp))
                          (lambda (e) (cons (unparse e) (reduce-history e (- n 1))))
                          (lambda () '())))))

(reduce-history '((lambda (x) (x ((lambda (x) y) z))) w) 5)
; ((w ((lambda (x) y) z)) (w y))

(reduce-history '((lambda (x) (x x)) (lambda (x) (x x))) 5)
; (((lambda (x) (x x)) (lambda (x) (x x))) ((lambda (x) (x x)) (lambda (x) (x x))) ((lambda (x) (x x)) (lambda (x) (x x))) ((lambda (x) (x x)) (lambda (x) (x x))) ((lambda (x) (x x)) (lambda (x) (x x))))

; Exercise 4.3.2
(define reduce*
  (lambda (exp n)
    (let ((result (reduce-history exp n)))
      (if (eq? (length result) n)
          #f
          (car (reverse result))))))

(reduce* '((lambda (x) (x ((lambda (x) y) z))) w) 5)
; (w y)

(reduce* '((lambda (x) (x x)) (lambda (x) (x x))) 5)
; #f

; Exercise 4.3.3
; ribassoc

; Exercise 4.3.4
; reduce-once-appl with +

; Exercise 4.3.5
(define reduce-once-leftmost
  (lambda (exp succeed fail)
    (cases lc-exp exp 
      (lit-exp (datum) (fail))
      (var-exp (var) (fail))
      (lambda-exp (formal body) (if (beta-redex? body)
                                    (reduce-once-leftmost body
                                                          (lambda (reduced-body) (succeed (make-lambda-exp formal reduced-body)))
                                                          (fail))
                                    (fail)))
      (app-exp (rator rand)
               (cond
                 ((beta-redex? exp) (succeed (beta-reduce exp)))
                 (else (reduce-once-leftmost rator
                                             (lambda (reduced-rator) (succeed (make-app-exp reduced-rator rand)))
                                             (lambda () (reduce-once-leftmost rand
                                                                              (lambda (reduced-rand) (succeed (make-app-exp rator reduced-rand)))
                                                                              fail)))))))))

(define reduce-history-leftmost
  (lambda (exp n)
    (if (eq? n 0)
        '()
        (reduce-once-leftmost (if (lc-exp? exp)
                              exp
                              (parse exp))
                          (lambda (e) (cons (unparse e) (reduce-history-leftmost e (- n 1))))
                          (lambda () '())))))

(reduce-history-leftmost '((lambda (x) (x ((lambda (x) y) z))) w) 5)
; ((w ((lambda (x) y) z)) (w y))

(reduce-history-leftmost '((lambda (y) z) ((lambda (x) (x x)) (lambda (x) (x x)))) 5)
; (z)

; Exercise 4.4.2
; Exercise 4.4.3
; TODO: Need to get better at Y-combinator lambda calculus


(define for-each
  (lambda (proc lst)
    (if (null? lst)
        'done
        (begin
          (proc (car lst))
          (for-each proc (cdr lst))))))

(define displayln
  (lambda lst
    (begin
      (for-each display lst)
      (newline))))

; (displayln 2 "+" 2 "=" (+ 2 2))
; 2+2=4

(define read-eval-print
  (lambda ()
    (display "--> ")
    (write (eval (read)))
    (newline)
    (read-eval-print)))

; (read-eval-print)
; --> (+ 1 2)
; 3
; --> (car (cons "foo" 'foo))
; "foo"

; Exercise 4.5.1
(define reduce*-repl
  (lambda ()
    (display "--> ")
    (write (reduce* (read) 5))
    (newline)
    (reduce*-repl)))

; (reduce*-repl)
; '((lambda (x) (x ((lambda (x) y) z))) w)
; '(w y)

; Exercise 4.5.2
; Doesnt mutate global value, works only in local scope
(define set-cdr!
  (lambda (lst ncdr)
    (set! lst (cons (car lst) ncdr))))
          
(define reverse!
  (letrec ((loop
            (lambda (last ls)
              (let ((next (cdr ls)))
                (set-cdr! ls last)
                (if (null? next)
                    ls
                    (loop ls next))))))
  (lambda (ls)
    (if (null? ls)
        ls
        (loop '() ls)))))

; Exercise 4.6.1

(define make-stack
  (lambda (size)
    (let ((stk (make-vector size)) (pos 0))
      (lambda (message)
        (case message
          ((empty?) (lambda () (eq? pos 0)))
          ((push!) (lambda (x) (begin (vector-set! stk pos x) (set! pos (+ pos 1)))))
          ((pop!) (lambda () (if (eq? pos 0)
                                 (eopl:error "Stack empty")
                                 (begin
                                   (set! pos (- pos 1))
                                   (vector-ref stk pos)))))
          ((top) (lambda () (if (eq? pos 0)
                                (eopl:error "Stack empty")
                                (vector-ref stk (- pos 1)))))
          (else (eopl:error "stack: invalid message -- " message)))))))

(define s1 (make-stack 10))
((s1 'push!) 7)
((s1 'push!) 4)
((s1 'push!) 2)
((s1 'top))
; 2
((s1 'pop!))
; 2
((s1 'top))
; 4
((s1 'pop!))
; 4
((s1 'pop!))
; 7

; Exercise 4.6.3
(define next-symbol
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      (string->symbol (string-append "g" (number->string c))))))

(eq? (next-symbol) 'g1)
; #t

(eq? (next-symbol) 'g2)
; #t

(eq? (next-symbol) (next-symbol))
; #f

; Exercise 4.7.1 and 4.7.2
; Streams ADT




      
