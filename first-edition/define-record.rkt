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

(define-syntax variant-case
  (lambda (x)
    (syntax-case x (else)
      ((_ var) (syntax (error 'variant-case "no clause matches ~s" var)))
      ((_ var (else exp1 exp2 ...)) (syntax (begin exp1 exp2 ...)))
      ((_ exp clause ...)
       (not (identifier? (syntax exp)))
       (syntax (let ((var exp)) (_ var clause ...))))
      ((_ var (name (field ...) exp1 exp2 ...) clause ...)
       (let* ((implicit-identifier
              (lambda (original-context-id new-id-sym)
                (datum->syntax original-context-id new-id-sym)))

             (construct-name          ; From TR 356
              (lambda (template-id . args)
                (implicit-identifier
                 template-id
                 (string->symbol
                  (apply string-append
                         (map (lambda (x)
                                (if (string? x)
                                    x
                                    (symbol->string (syntax->datum x))))
                              args)))))))
         ;(display (syntax (field ...)))
         (with-syntax
             ((predicate (construct-name (syntax name) (syntax name) "?"))
              ((reader ...)
               (map (lambda (fld)
                      (construct-name (syntax name) (syntax name) "->" fld))
                    (syntax->list (syntax (field ...))))))
           (syntax
            (if (predicate var)
                (let ((field (reader var)) ...) exp1 exp2 ...)
                (variant-case var clause ...)))))))))
(provide (all-defined-out))

;(define-record interior (symbol left-tree right-tree))
;(define-record leaf (number))
;(define leaf-sum
;  (lambda (tree)
;    (variant-case tree
;                  (leaf (number) number)
;                  (interior (left-tree right-tree)
;                            (+ (leaf-sum left-tree) (leaf-sum right-tree)))
;                  (else (error "leaf-sum: Invalid tree" tree)))))
;(define tree-1 (make-interior 'foo (make-interior 'bar (make-leaf 2) (make-leaf 3)) (make-leaf 4)))
;(define tree-2 (make-leaf 2))