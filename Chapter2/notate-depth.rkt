;; > (notate-depth '(a ((b)) (c) ((((((((d)))))))) (e) (f)))
;; ((a 0) (b 2) (c 1) (d 8) (e 1) (f 1))
;; > (notate-depth '(a ((b)) (c) ((((((((d)))))))) (e (f))))
;; ((a 0) (b 2) (c 1) (d 8) ((e 0) ((f 1))))

(define notate-depth
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