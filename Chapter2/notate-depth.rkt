(define notate-depth-1
  (lambda (list)
    (define notate-single
      (lambda (list depth)
        (cond
          ((or (null? list) (not (list? list))) (cons list (cons depth '())))
          (else (notate-single (car list) (+ depth 1))))))
    (define notate-internal
      (lambda (list notation)
        (cond
          ((null? list) notation)
          (else (cons (notate-single (car list) 0) (notate-internal (cdr list) '()))))))
    (notate-internal list '())))

;; > (notate-depth-2 '(a ((b)) (c) ((((((((d)))))))) (e) (f)))
;; ((a 0) (b 2) (c 1) (d 8) (e 1) (f 1))
;; > (notate-depth-2 '(a ((b)) (c) ((((((((d)))))))) (e (f))))
;; ((a 0) (b 2) (c 1) (d 8) ((e 0) ((f 1))))

(define notate-depth-2
  (lambda (list)
    (define notate-single
      (lambda (list depth)
        (cond
          ((or (null? list) (not (list? list))) (cons list (cons depth '())))
          ((null? (cdr list)) (notate-single (car list) (+ depth 1)))
          (else (cons (notate-single (car list) depth) (cons (notate-internal (cdr list) (cons (notate-single (car list) depth) '()) 0) '()))))))
    (define notate-internal
      (lambda (list notation common-depth)
        (cond
          ((null? list) notation)
          (else (cons (notate-single (car list) common-depth) (notate-internal (cdr list) '() common-depth))))))
    (notate-internal list '() 0)))