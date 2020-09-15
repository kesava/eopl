#lang eopl
(define cell-tag "cell")

(define make-cell
  (lambda (x)
    (vector cell-tag x)))

(define cell?
  (lambda (x)
    (and (vector? x)
         (= (vector-length x) 2)
         (eq? (vector-ref x 0) cell-tag))))

(define cell-ref
  (lambda (x)
    (if (cell? x)
        (vector-ref x 1)
        (eopl:error "Invalid argument to cell-ref:" x))))

(define cell-set!
  (lambda (x value)
    (if (cell? x)
        (vector-set! x 1 value)
        (eopl:error "Invalid argument to cell-set!: " x))))

(define cell-swap!
  (lambda (cell-1 cell-2)
    (let ((temp (cell-ref cell-1)))
      (cell-set! cell-1 (cell-ref cell-2))
      (cell-set! cell-2 temp))))

(provide (all-defined-out))