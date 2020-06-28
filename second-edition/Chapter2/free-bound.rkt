(#%require racket/trace)

(define (is-member? x lst)
  (if (member x lst)
      #t
      #f))

(define (_bound-vars exp vars acc)
   (if (list? exp) 
        (if (null? exp)
            acc
            (if (is-member? (car exp) vars)
                (_bound-vars (cdr exp) vars (cons (car exp) acc))
                (_bound-vars (cdr exp) vars acc)))))

(define (bound-vars l-exp)
  (if (eq? (car l-exp) 'lambda)
      (let ((vars (cadr l-exp)) (body (caddr l-exp)))
        (_bound-vars body vars '()))
      '()
    ))

(define (_free-vars exp vars acc)
   (if (list? exp) 
        (if (null? exp)
            acc
            (if (is-member? (car exp) vars)
                (_free-vars (cdr exp) vars acc)
                (_free-vars (cdr exp) vars (cons (car exp) acc))))))


(define (free-vars l-exp)
  (if (eq? (car l-exp) 'lambda)
      (let ((vars (cadr l-exp)) (body (caddr l-exp)))
        (_free-vars body vars '()))
      '()))

(bound-vars '(lambda (x) x))
(bound-vars '(lambda (x) (+ x 1)))
(bound-vars '(lambda (x y z) (+ x y 1)))

(free-vars '(lambda (x) x))
(free-vars '(lambda (x) (+ x 1)))
(free-vars '(lambda (x y z) (+ x y b)))

; (trace _free-vars)
; (_free-vars '(+ x 1) '(x y) '())