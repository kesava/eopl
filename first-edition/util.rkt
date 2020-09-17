#lang eopl
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
  (extended-ff (sym symbol?) (val number-or-list?) (next-ff ff?))
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

(define the-empty-env (create-empty-ff))
(define extend-env extend-ff*)
(define apply-env apply-ff)
(define (exists-env? f symbol)
  (cases ff f
    (empty-ff () #f)
    (extended-ff (sym val next-ff)
                 (if (eq? sym symbol)
                     #t
                     (exists-env? next-ff symbol)))
    (extended-ff* (sym-list val-vector ff)
                  (let ((val (ribassoc symbol sym-list val-vector '*fail*)))
                    (if (eq? val '*fail*)
                        (exists-env? ff symbol)
                        #t)))
    (else #f)))

(define number-or-list?
  (lambda (datum)
    (or (number? datum) (list? datum))))

(provide (all-defined-out))