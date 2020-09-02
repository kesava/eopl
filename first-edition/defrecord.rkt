#lang racket
(require racket/syntax)
(require (for-syntax racket/syntax))
(require macro-debugger/expand)

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

(define-syntax variant-case
  (syntax-rules (else)
    [(_ (a . d) clause ...)
     (let ([var (a . d)]) (variant-case var clause ...))]
    [(_ var) (error 'variant-case "no clause matches ~s" var)]
    [(_ var (else exp1 exp2 ...)) (begin exp1 exp2 ...)]
    [(_ var (name (field ...) exp1 exp2 ...) clause ...)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [name->field (format-id #'name "~a->~a" #'name #'field)] ...)
       (displayln #'name?)
       (displayln #'name->field)
       (displayln var)
       (syntax->datum #'(if (name? var)
                (let ([field (name->field var)] ...) exp1 exp2 ...)
                (variant-case var clause ...))))]))

;(display (syntax->datum (expand-once #'(variant-case tree
;                  (leaf (number) number)))))

(define-record interior (symbol left-tree right-tree))
(define-record leaf (number))

(define leaf-sum
  (lambda (tree)
    (variant-case tree
                  (leaf (number) number)
                  (interior (left-tree right-tree)
                            (+ (leaf-sum left-tree) (leaf-sum right-tree)))
                  (else (error "leaf-sum: Invalid tree" tree)))))
(define rrt (lambda (tree)
              (displayln tree)
              (variant-case tree
                            (leaf (number) number))))
                           ; (else (error "leaf-sum: Invalid tree" tree)))))

(define tt (lambda (tree)
             (if (leaf? tree) (let ((number (leaf->number tree))) number) (variant-case tree (interior (left-tree right-tree) (+ (leaf-sum left-tree) (leaf-sum right-tree))) (else (error "leaf-sum: Invalid tree" tree))))))
             ;(if (leaf? tree) (let ((number (leaf->number tree))) number) (variant-case tree))))

(define tree-1 (make-interior 'foo (make-interior 'bar 1 2) 3))
(define tree-2 (make-leaf 2))
