#lang racket
(require (for-syntax racket/syntax syntax/parse))
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

;(define-syntax (define-record stx)
;  (syntax-parse stx
;    [(_define-record name:id (field:id ...))
;     (with-syntax
;       ([make-name         (format-id #'name "make-~a" #'name)]
;        [(name->field ...) (for/list ([field (syntax->list #'(field ...))])
;                             (format-id #'name "~a->~a" #'name field))]
;        [(name-field ...)  (for/list ([field (syntax->list #'(field ...))])
;                             (format-id #'name "~a-~a" #'name field))])
;       (syntax/loc stx
;         (begin
;           ; Represent records as stanard structures.
;           (struct name (field ...)
;             #:transparent
;             #:constructor-name make-name)
;           ; Accessors
;           (define name->field name-field)
;           ...)))]))

(define-syntax (variant-case stx)
  (syntax-parse stx
    #:literals (else)
    ; Handle the general case (with `else`) first.
    [(_variant-case value-expr:expr
       [name:id (field:id ...) . body]       
       ...
       [else . else-body])
     (with-syntax
       ([(name? ...)             (for/list ([name (syntax->list #'(name ...))])
                                   (format-id #'name "~a?" name))]
        [((name->field ...) ...) (for/list ([name   (syntax->list #'(name ...))]
                                            [fields (syntax->list #'((field ...) ...))])
                                   (for/list ([field (syntax->list fields)])
                                     (format-id #'name "~a->~a" name field)))])       
       (syntax/loc stx
         (let ([value value-expr])
           (cond
             [(name? value) (let ([field (name->field value)] ...) . body)]
             ...
             [else . else-body]))))]
    ; The special case without `else` is equivalent to an else-clause
    ; that throws an error.
    [(_variant-case value-expr:expr
       [name:id (field:id ...) . body]       
       ...)
     ; 
     (syntax/loc stx
       (let ([value value-expr])
         (variant-case value
           [name (field ...) . body]       
           ...
           [else (error 'variant-case "no clause handled the value:\n ~a" value)])))]))

(provide (all-defined-out))

;(define-record leaf     (number))
;(define-record interior (symbol left-tree right-tree))
;
;;; (struct leaf     (number)                      #:constructor-name make-leaf)
;;; (struct interior (symbol left-tree right-tree) #:constructor-name make-interior)
;
;(define tree-1 (make-interior 'foo (make-interior 'bar (make-leaf 1) (make-leaf 2)) (make-leaf 3)))
;
;(define leaf-sum
;  (lambda (tree)
;    (variant-case tree
;      [leaf (number) number]
;      [interior (left-tree right-tree)
;                (+ (leaf-sum left-tree) (leaf-sum right-tree))]
;      [else (error 'leaf-sum "Invalid tree, got:\n ~a" tree)])))
;  
;(leaf-sum tree-1)
;(leaf-sum 'foo)
