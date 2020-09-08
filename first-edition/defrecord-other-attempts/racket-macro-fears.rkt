#lang racket
(require racket/syntax)

 (define-syntax (hyphen-define/ok2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ([name (datum->syntax #'a
                                          (string->symbol (format "~a-~a0--"
                                                                  (syntax->datum #'a)
                                                                  (syntax->datum #'b))))])
         #'(define (name args ...)
             body0 body ...))]))

(define ll '(x y z))

(define make-pair
  (lambda (c d)
    (format "~a->~a" c d)))