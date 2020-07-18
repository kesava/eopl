#lang eopl
(require racket/trace)

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(remove-first 'a4 '(c1 a4 c1 a4))
; '(c1 c1 a4)
(remove-first 'x '())
; '()

; Exercise 2.2.2
; If the inner if's cons is removed, it produces remove-till-first
(define remove-till-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (cdr los)
            (remove-till-first s (cdr los))))))

(remove-till-first 'a4 '(c1 a4 c1 a4))
; '(c1 a4)
(remove-till-first 'x '())
; '()

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))


(remove 'a4 '(c1 a4 c1 a4))
; '(c1 c1)
(remove-first 'x '())
; '()

; Exercise 2.2.3
; Not sure what it means

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (cons (subst new old (car slist)) (subst new old (cdr slist)))
            (if (eq? old (car slist))
                (cons new (subst new old (cdr slist)))
                (cons (car slist) (subst new old (cdr slist))))))))

(subst 'a 'b '((b c) (b d)))
; ((a c) (a d))

; mutually recursive version
(define subst-mutually-recur
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-symbol-expression new old (car slist)) (subst new old (cdr slist))))))

(define subst-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eq? se old) new se)
        (subst-mutually-recur new old se))))

(subst-mutually-recur 'a 'b '((b c) (b d)))
; ((a c) (a d))

; Exercise 2.2.4
; Because subst-mutually-recur has a termination condition for list
; and subst-symbol-expression has a termination condition for symbol

; Exercise 2.2.5
(define subst-map
  (lambda (new old slist)
    (map (lambda (se)
           (if (symbol? se)
               (if (eq? se old) new se)
               (subst-map new old se)))
          slist)))

(subst-map 'a 'b '((b c) (b d)))
; ((a c) (a d))

(define list-sum
  (lambda (lon)
    (if (null? lon)
        0
        (+ (car lon) (list-sum (cdr lon))))))

(define partial-vector-sum
  (lambda (von n)
    (if (zero? n)
        0
        (+ (vector-ref von (- n 1)) (partial-vector-sum von (- n 1))))))

(define vector-sum
  (lambda (von)
    (partial-vector-sum von (vector-length von))))

; Exercise 2.2.7
(define duple
  (lambda (n x)
         (if (eq? n 0)
             '()
             (cons x (duple (- n 1) x)))))

(duple 2 3)
; (3 3)

(duple 4 '(ho ho))
; ((ho ho) (ho ho) (ho ho) (ho ho))

(duple 0 '(blah))
; ()

(define invert
  (lambda (2list)
    (if (null? 2list)
        '()
        (cons (list (cadr (car 2list)) (car (car 2list))) (invert (cdr 2list))))))

(invert '((a 1) (a 2) (b 1) (b 2)))
; ((1 a) (2 a) (1 b) (2 b))

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

(list-index '(a b c) (list 'b '(a b c)))
; 1

(define vector-index
  (lambda (s vos)
    (define helper
      (lambda (vos n)
        (if (zero? n)
            -1
            (if (equal? (vector-ref vos (- n 1)) s)
                (- n 1)
                (helper vos (- n 1))))))
    (helper vos (vector-length vos))))

(vector-index 'c '#(a b c d))
; 2

(define ribassoc
  (lambda (s los v fail-value)
    (let ([index (list-index s los)])
      (if (eq? index -1)
          fail-value
          (vector-ref v index)))))

(ribassoc 'b '(a b c) '#(1 2 3) 'fail)
; 2

(ribassoc 'c '(a b foo) '#(2 seqw bar) 'fail)
; fail

(ribassoc 'i '(a i o i) '#(fx (fz) () (fm fe)) 'fail)
; (fz)

(define filter-in
  (lambda (p lst)
    (if (null? lst)
        '()
        (if (p (car lst))
            (cons (car lst) (filter-in p (cdr lst)))
            (filter-in p (cdr lst))))))

(filter-in number? '(a 2 (1 3) b 7))
; (2 7)

(filter-in symbol? '(a (b c) 17 foo))
; (a foo)

(define product-sym-slst
  (lambda (s slst)
    (if (null? slst)
        '()
        (cons (list s (car slst)) (product-sym-slst s (cdr slst))))))

(define product
  (lambda (los1 los2)
    (if (null? los1)
        '()
        (append (product-sym-slst (car los1) los2) (product (cdr los1) los2)))))

(product '(a b c) '(x y))
; ((a x) (a y) (b x) (b y) (c x) (c y))

(define swapper-se
  (lambda (s1 s2 se)
        (cond
          ((equal? se s1) s2)
          ((equal? se s2) s1)
          (else se))))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (cons (swapper-se s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))
            (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))))))

(swapper 'a 'd '(a b c d))
; (d b c a)

(swapper 'x 'y '((x) y (z (x))))
; ((y) x (z (y)))

(define rotate-left
  (lambda (los)
    (if (null? los)
        '()
        (if (null? (cdr los))
            los
            (append (cdr los) (list (car los)))))))

(rotate-left '(a b c d))
; (b c d a)

(define get-full-but-last
  (lambda (lis)
    (if (null? lis)
        '()
        (if (null? (cdr lis))
            '()
            (cons (car lis) (get-full-but-last (cdr lis)))))))

(define get-last
  (lambda (los)
    (if (null? los)
        '()
        (if (null? (cdr los))
            (car los)
            (get-last (cdr los))))))

(define rotate-right
  (lambda (los)
    (if (null? los)
        '()
        (cons (get-last los) (get-full-but-last los)))))
    
(rotate-right '(a b c d))
; (d a b c)

(rotate-right '())
; ()

(rotate-right '(a))
; (a)

; Exercise 2.2.8

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '()) (down (cdr lst))))))

(down '(a b))
; ((a) (b))

(down '(a (more (complicated)) object))
; ((a) ((more (complicated))) (object))

(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (cons (car lst) (up (cdr lst)))
            (append (car lst) (up (cdr lst)))))))

(up '((1 2) (3 4)))
; (1 2 3 4)

(up '((x (y)) z))
; (x (y) z)

(define count-occurences-lst
  (lambda (s los)
    (if (null? los)
        0
        (if (list? (car los))
            (count-occurences s (car los))
            (if (equal? (car los) s)
                (+ 1 (count-occurences-lst s (cdr los)))
                (count-occurences-lst s (cdr los)))))))

(define count-occurences
  (lambda (s slst)
    (if (null? slst)
        0
        (if (symbol? (car slst))
            (if (eq? (car slst) s)
                (+ 1 (count-occurences s (cdr slst)))
                (count-occurences s (cdr slst)))
            (+ (count-occurences-lst s (car slst)) (count-occurences s (cdr slst)))))))


(count-occurences 'x '((f x) y (((x z) x))))
; 3

(count-occurences 'w '((f x) y (((x z) x))))
; 0

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

(flatten '(a b c))
; (a b c)

(flatten '((a b) c (((d)) e)))
; (a b c d e)

(flatten '(a b (() (c))))
; (a b c)


(define merge
  (lambda (lon1 lon2)
    (cond
      ((null? lon1) lon2)
      ((null? lon2) lon1)
      ((<= (car lon1) (car lon2)) (cons (car lon1) (merge (cdr lon1) lon2)))
      ((> (car lon1) (car lon2)) (cons (car lon2) (merge lon1 (cdr lon2))))
      (else '()))))

(merge '(1 2 4 5) '(2 4 8))
; (1 2 2 4 4 5 8)

(merge '(35 62 81 90 91) '(3 83 85 90))
; (3 35 62 81 83 85 90 90 91)

; Exercise 2.2.9

(define root
  (lambda (bst)
    (car bst)))

(define l-branch
  (lambda (bst)
    (cadr bst)))

(define r-branch
  (lambda (bst)
    (caddr bst)))

(define path
  (lambda (n bst)
    (if (null? bst)
        '()
        (cond
          ((equal? (root bst) n) '())
          ((< n (root bst)) (cons 'L (path n (l-branch bst))))
          ((> n (root bst)) (cons 'R (path n (r-branch bst))))))))

(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
; (R L L)


(define car&cdr0
  (lambda (s slst errvalue)
    (define helper-los
      (lambda (los)
        (if (null? los)
            '()
            (if (symbol? (car los))
                (if (eq? (car los) s)
                    (list 'cdr 'lst)
                    (list 'cdr (helper-los (cdr los))))
                (helper-lst los)))))
      

    (define helper-lst
      (lambda (lst)
        (if (null? lst)
             '()
             (if (null? (car lst))
                 '()
                 (if (list? (car lst))
                     (append (list 'car) (cons (helper-los (car lst)) (helper-lst (cdr lst))))
                     (if (equal? (car lst) s)
                         (list (list 'car 'lst))
                         (list (list 'car (helper-los (cdr lst))))))))))
         
    (cons 'lambda (cons (list 'lst) (helper-lst slst)))))

(car&cdr0 'a '(a b c) 'err)
; (lambda (lst) (car lst))

(car&cdr0 'c '(a b c) 'err)
; (lambda (lst) (car (cdr (cdr lst))))

(car&cdr0 'dog '(cst lion (fish dog)) 'fail)
; (lambda (lst) (car (cdr (car (cdr (cdr lst))))))


; I seem to have hit the wall on two things -

; (a) how to workout the `errValue` without a preemptive full lookup, before i start building the procedure.
; ```(car&cdr 'a '(f b c) 'err)
; (lambda (lst) (car (cdr (cdr ()))))   <--- this should be 'err```

; (b) how to abort constructing lookup into a unsuccessful sublist.
; ```(car&cdr 'dog '(cst lion (fish dog) pig) 'fail)
; (lambda (lst) (car (cdr (car (cdr (cdr lst)) (car ())))))    <--- the last (car()) shouldn't appear.```


; Improved version with three interesting techniques.
; a) or to backtrack
; b) accum to make it tail recursive
; c) accum-stack to backtrack from a sublist and continue with the rest of the list

(define car&cdr
  (lambda (s slst errvalue)
    
   (define pop-stack
      (lambda (lst)
        (cond
          ((null? lst) '())
          ((list? (car lst)) (list 'cdr (car (car lst))))
          (else (pop-stack (cdr lst))))))
    
    (define push-stack
      (lambda (lst)
        (cons lst '())))
 
    (define helper
      (lambda (lst sigil rest-list accum-stack accum)
        (cond
          ((null? lst) (if (null? rest-list)
                            '()
                           (helper rest-list sigil '() (pop-stack accum-stack) (pop-stack accum-stack)))) ; reset accumulator
          ((equal? sigil (car lst)) (list 'car accum))
          (else (or
                 (if (list? (car lst))
                   (helper (car lst) sigil (cdr lst) (push-stack accum-stack) (list 'car accum))
                   #f)
                 (helper (cdr lst) sigil rest-list (list 'cdr accum-stack) (list 'cdr accum)))))))
    
     (let ([result (helper slst s '() 'x 'x)])
       (if (list? result)
           (list 'lambda (list 'x) result)
           errvalue))))

(car&cdr 'a '(a b c) 'err)
; (lambda (x) (car x))

(car&cdr 'c '(a b c) 'err)
; (lambda (x) (car (cdr (cdr x))))

(car&cdr 'dog '(cst lion (fish dog)) 'fail)
; (lambda (x) (car (cdr (car (cdr (cdr x))))))

(car&cdr 'dog '(cst lion (fish dog) pig) 'fail)
; (lambda (x) (car (cdr (car (cdr (cdr x))))))

(car&cdr 'horse '(cst lion (fish) horse dog) 'fail)
; (lambda (x) (car (cdr (cdr (cdr x)))))

(define compose
  (lambda (p1)
    
    (define identity
      (lambda (identity) identity))
    
    (define compose-helper
      (lambda (lst)
        (if (null? lst)
            identity
            ((car lst) (list (compose-helper (cdr lst)))))))
        
    (if (null? p1)
        identity
        (list p1 (compose-helper '())))))
        

; Some helpers
(define identity
  (lambda (x) x))

(define any?
  (lambda (lst check-fn)
    (define helper
      (lambda (lst acc)
        (cond
          ((null? lst) #f)
          ((check-fn (car lst)) #t)
          (else (helper (cdr lst) acc)))))
    (helper lst '())))

(define reduce
  (lambda (lst fn acc)
    (cond
      ((null? lst) acc)
      (else (reduce (cdr lst) fn (fn (car lst) acc))))))

(define append-unique
  (lambda (lst1 lst2)
    (define collector
      (lambda (lst1 lst2 acc)
        (cond
          ((null? lst1) (append (reverse acc) lst2))
          ((null? lst2) (append lst1 acc))
          ((memq (car lst1) lst2)
           (collector (cdr lst1) lst2 acc))
          (else (collector (cdr lst1) lst2 (cons (car lst1) acc))))))
    (collector lst1 lst2 '())))


(define old-free-vars
  (lambda (l-exp)
    (define vars-part
      (lambda (exp)
        (car (cdr exp))))
    (define exp-part
      (lambda (exp)
         (car (cdr (cdr exp)))))
    (let ((e-part (exp-part l-exp)) (v-part (vars-part l-exp)))
      (define helper
        (lambda (exp acc)
          (cond
            ((null? exp) acc)
            ((any? v-part (lambda (x) (eq? (car exp) x))) (helper (cdr exp) acc))
            (else (helper (cdr exp) (cons (car exp) acc))))))
       (helper e-part '()))))

(old-free-vars '(lambda (x) (x y z)))
; (z y)

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eq? var exp))
      ((eq? (car exp) 'lambda)
       (and (not (eq? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      (else (any? exp (lambda (x) (occurs-free? var x)))))))

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
           (and (eq? (caadr exp) var)
            (occurs-free? var (caddr exp)))))
      (else (any? exp (lambda (x) (occurs-bound? var x)))))))

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
          ; a bug in the following line
          ; a) need to collect unique vars
          (else (flatten (apply append (map (lambda (x) (collector x vars)) exp)))))))
    (collector exp '())))

; Exercise 2.3.1
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

; Exercse 2.3.6
(define occurs-free-with-if?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eq? var exp))
      ((eq? (car exp) 'if)
       (or (occurs-free-with-if? var (car exp))
            (occurs-free-with-if? var (cadr exp))
            (occurs-free-with-if? var (caddr exp))))
      ((eq? (car exp) 'lambda)
       (and (not (eq? (caadr exp) var))
            (occurs-free-with-if? var (caddr exp))))
      (else (any? exp (lambda (x) (occurs-free? var x)))))))

(occurs-free-with-if? 'z '(if x (lambda (y) z) x))
; #t
(occurs-free-with-if? 'x '(if x (lambda (y) (y z))))
; #t
(occurs-free-with-if? 'y '(if x (lambda (y) (y z))))
; #f

(define find-position
  (lambda (v lst)
    (define helper
      (lambda (position lst)
        (cond
          ((null? lst) -1)
          ((eq? (car lst) v) position)
          (else (helper (+ position 1) (cdr lst))))))
    (helper 0 lst)))


; Exercise 2.3.10
(define lexical-address
  (lambda (exp)
    (define varref-helper
      (lambda (v d p)
        (list (list v ': d p))))
    (define exp-helper
      (lambda (plist exp depth-level)
         (cond
           ((null? exp) '())
           ((symbol? exp) (varref-helper exp depth-level (find-position exp plist)))
           ((eq? (car exp) 'lambda) (lambda-helper (append (cadr exp) plist) (caddr exp) (+ depth-level 1)))
           ((eq? (car exp) 'if) (if-helper (cadr exp) (caddr exp) (cadddr exp) depth-level plist))
           (else (append (exp-helper plist (car exp) depth-level) (exp-helper plist (cdr exp) depth-level))))))
    (define if-helper
      (lambda (pred then-exp else-exp depth-level plist)
        (append (map (lambda (x) (varref-helper x depth-level (find-position x plist))) pred)
                (map (lambda (x) (varref-helper x depth-level (find-position x plist))) then-exp)
                (map (lambda (x) (varref-helper x depth-level (find-position x plist))) else-exp))))
    (define lambda-helper
      (lambda (plist exp depth-level)
        (list 'lambda plist (exp-helper plist exp depth-level))))
    
    (exp-helper '(cons eq +) exp 0)))