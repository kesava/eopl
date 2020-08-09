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
          ((null? plists) (list -1 -1))
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

(lexical-address '(let ((x p)) (lambda (y x) (x (x y)))))
; (let ((x p)) (lambda (y x) ((x : 0 1) (x : 0 1) (y : 0 0))))

; Section 3.4

(define-datatype binaryTree binaryTree?
  (null-node)
  (leaf-node (datum number?))
  (interior-node (key symbol?) (left-child binaryTree?) (right-child binaryTree?)))

(define tree-1 (interior-node 'foo (interior-node 'bar (leaf-node 1) (leaf-node 22)) (leaf-node 3)))

(define leaf-node?
  (lambda (x)
    (cond
      ((not (binaryTree? x) #f))
      (else (cases binaryTree x
              (null-node () #f)
              (leaf-node (l) #t)
              (interior-node (k l r) #f))))))

; Exercise 3.4.1

(define leaf-sum
  (lambda (tree)
    (cases binaryTree tree
      (leaf-node (value) value)
      (interior-node (key left-tree right-tree)
                (+ (leaf-sum left-tree) (leaf-sum right-tree)))
      (else (eopl:error "leaf-sum-1: invalid tree" tree)))))


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

(unparse (parse '(lambda (x) (x y))))
; (lambda (x) (x y))

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

(define (union a b)
  (cond ((null? b) a)
        ((member (car b) a)
         (union a (cdr b)))
        (else (union (cons (car b) a) (cdr b)))))

(define free-vars-with-exp
  (lambda (exp)
    (cases lc-exp exp
      (lit-exp (datum) '())
      (var-exp (var) (list var))
      (lambda-exp (formal body) (remove formal (free-vars-with-exp body)))
      (app-exp (rator rand) (union (free-vars-with-exp rator) (free-vars-with-exp rand))))))

; Exercise 3.4.5

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

(free? 'x (parse '(lambda (y) (x (x y)))))
; #t
(free? 'y (parse '(lambda (y) (x (x y)))))
; #f

; Exercise 3.4.6

(define-datatype l-exp l-exp?
  (lt-exp
   (datum number?))
  (lex-info
   (id symbol?)
   (distane number?)
   (position number?))
  (if-exp
   (test-exp l-exp?)
   (then-exp l-exp?)
   (else-exp l-exp?))
  (ld-exp
   (formals list?)
   (body l-exp?))
  (ap-exp
   (rator l-exp?)
   (rands l-exp?)))

(define lex-parse
  (lambda (datum)
    (cond
      ((number? datum) (make-lt-exp datum))
      ((symbol? datum) (make-lex-info datum -1 -1))
      ((pair? datum) (cond
                       ((eq? (car datum) 'lambda)
                         (make-ld-exp (cadr datum) (lex-parse (caddr datum))))
                       ((eq? (car datum) 'if)
                         (make-if-exp (lex-parse (cadr datum)) (lex-parse (caddr datum)) (lex-parse (cadddr datum))))
                       ((pair? (cdr datum)) (make-ap-exp (lex-parse (car datum)) (lex-parse (cdr datum))))
                       (else (lex-parse (car datum)))))
      (else (eopl:error "parse: invalid concrete syntax")))))
  

(define lex-unparse
  (lambda (exp)
    (cases l-exp exp
      (lt-exp (datum) datum)
      (lex-info (v d p) (list v ': d p))
      (ld-exp (formals body) (list 'lambda formals (lex-unparse body)))
      (if-exp (test-exp then-exp else-exp) (list 'if (lex-unparse test-exp) (lex-unparse then-exp) (lex-unparse else-exp)))
      (ap-exp (rator rand) (list (lex-unparse rator) (lex-unparse rand)))
      (else (eopl:error "unparse: invalid abstract sybtax" exp)))))


(define lex-address
  (lambda (exp)
    (define get-lexical-address
      (lambda (v plists)
        (let ((dp-pair (find-pd-from-plists v plists)))
              (make-lex-info v (car dp-pair) (cadr dp-pair)))))
    (define exp-helper
      (lambda (plists exp)
        (cases l-exp exp
          (lt-exp (datum) datum)
          (lex-info (var d p) (get-lexical-address var plists))
          (if-exp (test-exp then-exp else-exp) (if-helper test-exp then-exp else-exp plists))
          (ld-exp (formals body) (lambda-helper (cons formals plists) body))
          (ap-exp (rator rands) (make-ap-exp (exp-helper plists rator) (exp-helper plists rands))))))
    (define if-helper
      (lambda (pred then-exp else-exp plists)
        (make-if-exp (append (exp-helper plists pred)
                (exp-helper plists then-exp)
                (exp-helper plists else-exp)))))
    (define lambda-helper
      (lambda (plists exp)
        (make-ld-exp (car plists) (exp-helper plists exp))))

    
    (exp-helper (list (list 'eq? 'cons '+ '<)) exp)))

(lex-unparse (lex-address (lex-parse '(lambda (y x) (x (x y))))))
; (lambda (y x) ((x : 0 1) ((x : 0 1) (y : 0 0))))

(lex-unparse (lex-address (lex-parse '(lambda (y x) (x (x y (lambda (z) (z x))))))))
; (lambda (y x) ((x : 0 1) ((x : 0 1) ((y : 0 0) (lambda (z) ((z : 0 0) (x : 1 1)))))))

; Section 3.6.2
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

(define ff1 (extend-ff* '(d x y) '(6 7 8) (create-empty-ff)))
; ff1
; #(struct:extended-ff* (d x y) #(6 7 8) #(struct:empty-ff))
(define ff2 (extend-ff* '(a b c) '(1 2 3) ff1))
; ff2
; #(struct:extended-ff* (a b c) #(1 2 3) #(struct:extended-ff* (d x y) #(6 7 8) #(struct:empty-ff)))
(apply-ff ff2 'd)
; 6
(define ff3 (extend-ff* '(d e) '(4 5) ff2))
; ff3
; #(struct:extended-ff* (d e) #(4 5) #(struct:extended-ff* (a b c) #(1 2 3) #(struct:extended-ff* (d x y) #(6 7 8) #(struct:empty-ff))))
(apply-ff ff3 'a)
; 1
(apply-ff ff3 'd)
; 4


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

  