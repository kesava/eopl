#lang eopl
  
; Exercise 3.1.1
(let ((x 5) (y 6) (z 7))
  (let ((x 13) (y (+ x y)) (z x))
    (- (+ x z) y)))
; 7

(let ((x 5) (y 6) (z 7))
  (+ (let ((z (+ x z))) (* z (+ x z)))
     (let ((z (* x y))) (+ z (* z (- z y))))))

; 954

; Exercise 3.1.2
(define extract-vars-exp
  (lambda (exp)
    (define helper
      (lambda (v-acc exp-acc lst)
        (cond
          ((null? lst) (list (reverse v-acc) (reverse exp-acc)))
          (else (helper (cons (car (car lst)) v-acc) (cons (cadr (car lst)) exp-acc) (cdr lst))))))
    (helper '() '() exp)))
          
(define let->application
  (lambda (let-exp)
    (if (eq? (car let-exp) 'let)
        (append (list (list 'lambda (car (extract-vars-exp (cadr let-exp))) (caddr let-exp))) (cadr (extract-vars-exp (cadr let-exp))))
        let-exp)))

(let->application '(let ((x 4) (y 3)) (let ((z 5)) (+ x (+ y z)))))
; ((lambda (x y) (let ((z 5)) (+ x (+ y z)))) (4 3))

(define any?
  (lambda (check-fn lst)
    (define helper
      (lambda (lst acc)
        (cond
          ((null? lst) #f)
          ((check-fn (car lst)) #t)
          (else (helper (cdr lst) acc)))))
    (helper lst '())))

; Exercise 3.1.4
(define flatten-lst
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (flatten (car lst)) (flatten-lst (cdr lst)))
            (cons (car lst) (flatten-lst (cdr lst)))))))

(define flatten
  (lambda (slst)
    (if (null? slst)
        '()
        (if (symbol? (car slst))
            (cons (car slst) (flatten (cdr slst)))
            (append (flatten-lst (car slst)) (flatten (cdr slst)))))))

(define filter-in
  (lambda (p lst)
    (if (null? lst)
        '()
        (if (p (car lst))
            (cons (car lst) (filter-in p (cdr lst)))
            (filter-in p (cdr lst))))))

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eq? var exp))
      ((eq? (car exp) 'if)
       (or (occurs-free? var (car exp))
            (occurs-free? var (cadr exp))
            (occurs-free? var (caddr exp))))
      ((eq? (car exp) 'lambda)
       (and (not (eq? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      ((or (eq? (car exp) 'letrec)
           (eq? (car exp) 'let))
       (and (not (any? (lambda (x) (eq? x var)) (car (extract-vars-exp (cadr exp)))))
            (not (any? (lambda (x) (occurs-free? var x)) (cadr (extract-vars-exp (cadr exp)))))
            (occurs-free? var (caddr exp))))
      (else (any? (lambda (x) (occurs-free? var x)) exp)))))

(occurs-free? 'x '(lambda (y) (+ c (lambda (z) (+ v b)))))
; #f

(occurs-free? 'x '(lambda (y) (+ c (lambda (z) (+ v x)))))
; #t

(occurs-free? 'x '(lambda (y) (+ c (lambda (x) (+ v x)))))
; #f

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eq? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (not (eq? (caadr exp) var))
            (occurs-free? var (caddr exp)))))
      ((or (eq? (car exp) 'let)
           (eq? (car exp) 'letrec))
       (or (occurs-bound? var (caddr exp))
           (and (any? (lambda (x) (eq? var x)) (caadr exp))
           (occurs-free? var (caddr exp)))))
      (else (any? (lambda (x) (occurs-bound? var x)) exp)))))

(occurs-bound? 'x '(lambda (x) (+ x y)))
; #t

(occurs-bound? 'y '(lambda (x) (+ x y)))
; #f

(occurs-bound? 'x '(lambda (y) (+ c (lambda (x) (+ v x)))))
; #t

(define extract-vars
  (lambda (exp)
    (define collector
      (lambda (exp vars)
        (cond
          ((symbol? exp)
           (if (memq exp vars)
               vars
               (cons exp vars)))
          ((eq? (car exp) 'lambda)
           (collector (caddr exp) vars))
          ((or (eq? (car exp) 'let)
               (eq? (car exp) 'letrec))
           (collector (caddr exp) vars))
          ; a bug in the following line
          ; a) need to collect unique vars
          (else (flatten (apply append (map (lambda (x) (collector x vars)) exp)))))))
    (collector exp '())))

(define free-vars
  (lambda (l-exp)
    (filter-in (lambda (x) (occurs-free? x l-exp)) (extract-vars l-exp))))

(free-vars '(lambda (x) (x y z)))
; (y z)

(free-vars '(lambda (y) (z c (lambda (x) (y v x)))))
; (z c v)

(define bound-vars
  (lambda (l-exp)
    (filter-in (lambda (x) (occurs-bound? x l-exp)) (extract-vars l-exp))))

(bound-vars '(lambda (x) (x y)))
; (x)

(bound-vars '(lambda (y) (z c (lambda (x) (y v x)))))
; (y x)

; Exercise 3.1.5
(define find-position
  (lambda (v lst)
    (define helper
      (lambda (position lst)
        (cond
          ((null? lst) -1)
          ((not (pair? lst)) -1)
          ((eq? (car lst) v) position)
          (else (helper (+ position 1) (cdr lst))))))
    (helper 0 lst)))

(define find-pd-from-plists
  (lambda (v plists)
    (define helper
      (lambda (plists depth)
        (cond
          ((null? plists) (list '() -1))
          ((> (find-position v (car plists)) -1) (list depth (find-position v (car plists))))
          (else (helper (cdr plists) (+ depth 1))))))
    (helper plists 0)))

(define (make-pairs list1 list2)
  (define (helper lst1 lst2 acc)
    (cond
      ((null? lst1) acc)
      (else (helper (cdr lst1) (cdr lst2) (cons (list (car lst1) (car lst2)) acc)))))
  (helper list1 list2 '()))



; Exercise 3.2.1
(define (and-proc . args)
  (cond
    ((null? args) #t)
    ((pair? (cdr args)) (if (car args) (apply and-proc (cdr args)) #f))
    (else (car args))))

(and-proc #t #t #t)
; #t
(and-proc #t #t #f)
; #f

(define (or-proc . args)
  (cond
    ((null? args) #f)
    (else (if (car args) (car args) (apply or-proc (cdr args))))))

(or-proc #f #f #f)
; #f
(or-proc #f #t #t)
; #t

; Exercise 3.3.1
(define (if->cond if-exp)
  (letrec ((helper (lambda (exp)
                  (cond
                    ((null? exp) '())
                    ((eq? (car exp) 'if)
                     (if (pair? (cadddr exp))
                         (cons (list (cadr exp) (caddr exp)) (helper (cadddr exp)))
                         (cons (list (cadr exp) (caddr exp)) (list (list 'else (cadddr exp))))))
                    (else exp)))))
    (cons 'cond (helper if-exp))))

(if->cond '(if a b c))
; (cond (a b) (else c))

(if->cond  '(if a b (if c d (if e f g))))
; (cond (a b) (c d) (e f) (else g))

(if->cond '(if a (if x b c) (if d e f)))
; (cond (a (if x b c)) (d e) (else f))

(define (cond->if exp)
  (letrec ((helper (lambda (exp)
                     (cond
                       ((null? exp) '())
                       ((eq? (car (cadr exp)) 'else) (list 'if (car (car exp)) (cadr (car exp)) (cadr (cadr exp))))
                       (else (list 'if (car (car exp)) (cadr (car exp)) (helper (cdr exp))))))))
    (helper (cdr exp))))
                
(cond->if '(cond (a b) (c d) (e f) (else g)))
; (if a b (if c d (if e f g)))

(cond->if '(cond (a (if x b c)) (d e) (else f)))
; (if a (if x b c) (if d e f))

(cond->if '(cond (a b) (else c)))
; (if a b c)

; Exercise 3.1.4 and Exercise 3.3.2
(define lexical-address
  (lambda (exp)
    (define varref-helper
      (lambda (v dp-pair)
        (list (list v ': (car dp-pair) (cadr dp-pair)))))
    (define exp-helper
      (lambda (plists exp)
         (cond
           ((null? exp) '())
           ((symbol? exp) (varref-helper exp (find-pd-from-plists exp plists)))
           ((eq? (car exp) 'lambda) (lambda-helper (cons (cadr exp) plists) (caddr exp)))
           ((eq? (car exp) 'if) (if-helper (cadr exp) (caddr exp) (cadddr exp) plists))
           ((eq? (car exp) 'let) (let-helper (cons (car (extract-vars-exp (cadr exp))) plists) (cadr (extract-vars-exp (cadr exp))) (caddr exp)))
           ((eq? (car exp) 'letrec) (letrec-helper (cons (car (extract-vars-exp (cadr exp))) plists) (cadr (extract-vars-exp (cadr exp))) (caddr exp)))
           ((eq? (car exp) 'cond) (cond-helper (cons (car (extract-vars-exp (cdr exp))) plists) (cadr (extract-vars-exp (cdr exp)))))
           (else (append (exp-helper plists (car exp)) (exp-helper plists (cdr exp)))))))
    (define if-helper
      (lambda (pred then-exp else-exp plists)
        (list 'if (append (exp-helper plists pred)
                (exp-helper plists then-exp)
                (exp-helper plists else-exp)))))
    (define lambda-helper
      (lambda (plists exp)
        (list 'lambda (car plists) (exp-helper plists exp))))
    (define let-helper
      (lambda (var-lists assign-lists aexp)
        (list 'let (make-pairs (car var-lists) assign-lists) (exp-helper var-lists aexp))))
    (define letrec-helper
      (lambda (var-lists assign-lists aexp)
        (list 'letrec (make-pairs (car var-lists) (map (lambda (x) (exp-helper (car var-lists) x)) assign-lists)) (exp-helper var-lists aexp))))
    (define cond-helper
      (lambda (condlists conseqlist)
        (list 'cond (make-pairs (car condlists) (map (lambda (x) (exp-helper (car condlists) x)) conseqlist)))))

    
    (exp-helper (list (list 'eq? 'cons '+ '<)) exp)))

; Section 3.4

(define-datatype binaryTree binaryTree?
  (null-node)
  (leaf-node (datum number?))
  (interior-node (key symbol?) (left-child binaryTree?) (right-child binaryTree?)))

(define tree-1 (interior-node 'foo (interior-node 'bar (leaf-node 1) (leaf-node 22)) (leaf-node 3)))

; Exercise 3.4.1

(define leaf-sum
  (lambda (tree)
    (cases binaryTree tree
      (leaf-node (value) value)
      (interior-node (key left-tree right-tree)
                (+ (leaf-sum left-tree) (leaf-sum right-tree)))
      (else (eopl:error "leaf-sum-1: invalid tree" tree)))))