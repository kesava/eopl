#lang racket
(require racket/syntax)
(require (for-syntax racket/syntax))
(require macro-debugger/expand)
(require syntax/parse/define)
(require
  racket/stxparam
  (for-syntax syntax/parse))

(define-syntax (define-record stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       (with-syntax ([make-id (format-id #'id "make-~a" #'id)] [pred-id (format-id #'id "~a?" #'id)])
         #`(begin
             ; Define a constructor.
             (define (make-id fields ...)
               (apply vector (cons 'id  (list fields ...))))
             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id #'id "~a->~a" #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id "~a is not a ~a struct" v 'id))
                        (vector-ref v ix))))))]))

(define-syntax (variant-case stx)
  (syntax-case stx ()
    [(_ (a . d) clause ...)
     #'(let ([var (a . d)]) (variant-case var clause ...))]
    [(_ var)
     #'(error 'variant-case "no clause matches ~s" var)]
    [(_ var (else exp1 exp2 ...))
     #'(begin exp1 exp2 ...)]
    [(_ var (name (field ...) exp1 exp2 ...) clause ...)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [name->field (format-id #'name "~a->~a" #'name #'field)])
       #'(if (name? var)
                (let ([field (name->field var)] ...) exp1 exp2 ...)
                (variant-case var clause ...)))]))

(display (syntax->datum (expand-once #'(variant-case tree
                  (leaf (number) number)))))

(define-syntax (variant-case3 stx)
  (syntax-case stx ()
    [(_ (a . d) clause ...)
     #'(let ([var (a . d)]) (variant-case var clause ...))]
    [(_ var)
     #'(error 'variant-case "no clause matches ~s" var)]
    [(_ var (else exp1 exp2 ...))
     #'(begin exp1 exp2 ...)]
    [(_ var (name (field . fields) exp1 exp2 ...) clause ...)
     (displayln #'exp1)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [name-field-pairs-list (map
                                     (lambda (fld) (list fld
                                                         (list (format-id #'name "~a->~a" #'name fld) #'var)))
                                     (cons (syntax->datum #'field) (syntax->datum #'fields)))])
       #'(if (name? var)
             (let name-field-pairs-list exp1 exp2 ...)
             (variant-case var clause ...)))]))

(define-syntax variant-case2
  (syntax-rules (else)
    [(_ (a . d) clause ...)
     (let ([var (a . d)]) (variant-case var clause ...))]
    [(_ var) (error 'variant-case "no clause matches ~s" var)]
    [(_ var (else exp1 exp2 ...)) (begin exp1 exp2 ...)]
    [(_ var (name (field . fields) exp1 exp2 ...) clause ...)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [name-field-pairs-list (map
                                     (lambda (fld) (list fld
                                                         (list (format-id #'name "~a->~a" #'name fld) #'var)))
                                     (cons (syntax->datum #'field) (syntax->datum #'fields)))])
       #'(if (name? var)
             (let name-field-pairs-list)
             (variant-case var clause ...)))]))

(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]
      [(_ condition true-expr)
       #'(cond [condition true-expr]
               [else #f])]))

(define-syntax variant-case1
  (syntax-rules (else)
    [(_ (a . d) clause ...)
     (let ([var (a . d)]) (variant-case var clause ...))]
    [(_ var) (error 'variant-case "no clause matches ~s" var)]
    [(_ var (else exp1 exp2 ...)) (begin exp1 exp2 ...)]
    [(_ var (name (field . fields) exp1 . exp2) clause ...)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [name-field-pairs-list (map
                                     (lambda (fld) (list fld
                                                         (list (format-id #'name "~a->~a" #'name fld) #'var)))
                                     (cons (syntax->datum #'field) (syntax->datum #'fields)))]
                   [exps-list (cons #'exp1 #'exp2)])
       (if ((eval-syntax #'name?) var)
           (let ((pairs (syntax->datum #'name-field-pairs-list)))
             (let (
                   (vars (map (lambda (p) (car p)) pairs))
                   (exps (map (lambda (p)
                                (append
                                 (list (list 'lambda (list (syntax->datum #'var)) (cadr p)))
                                 (list (syntax->datum #'var))))
                              pairs)))
               (let ((exp (append (list 'lambda vars (car (syntax->datum #'exps-list))) exps)))
                 ;(displayln var)
                 ;(displayln vars)
                 ;(displayln exps)
                 ;(displayln exp)
                 ;(eval-syntax (datum->syntax #'var exp)))))
                 ((lambda (tree) (displayln tree) (displayln exp) (displayln (eval exp)) ((eval exp) tree)) var))))
             (variant-case var clause ...)))]))

(define-record interior (symbol left-tree right-tree))
(define-record leaf (number))

(define leaf-sum
  (lambda (tree)
    (variant-case tree
                  (leaf (number) number)
                  (interior (left-tree right-tree)
                            (+ (leaf-sum left-tree) (leaf-sum right-tree)))
                  (else (error "leaf-sum: Invalid tree" tree)))))

(define lfs
  (lambda (tree)
    ((lambda (number) number) ((lambda (tree) (leaf->number tree)) tree))))
(define rrt (lambda (tree)
              (variant-case tree
                            (leaf (number) number)
                            (interior (left-tree right-tree)
                                      (+ (leaf-sum left-tree) (leaf-sum right-tree))))))
                           ; (else (error "leaf-sum: Invalid tree" tree)))))

(define tree-1 (make-interior 'foo (make-interior 'bar (make-leaf 2) (make-leaf 3)) (make-leaf 4)))
(define tree-2 (make-leaf 2))

(define rtt
  (lambda (tree)
    (if (leaf? tree) (let ((number (leaf->number tree))) number)
         (if (interior? tree)
             (let ((left-tree (interior->left-tree tree)) (right-tree (interior->right-tree tree))) (+ (rtt left-tree) (rtt right-tree)))
             (variant-case tree (else (error "leaf-sum: Invalid tree" tree)))))))

(define-syntax (our-if stx)
  (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]))