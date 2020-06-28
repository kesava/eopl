#lang eopl

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
        (if (equal? (car los) s)
            (+ 1 (count-occurences-lst s (cdr los)))
            (count-occurences-lst s (cdr los))))))

(define count-occurences
  (lambda (s slst)
    (if (null? slst)
        0
        (if (symbol? (car slst))
            (if (eq? (car slst) s)
                (+ 1 (count-occurences s (cdr slst)))
                (count-occurences s (cdr slst)))
            (+ (count-occurences-lst s (car slst)) (count-occurences s (cdr slst)))))))