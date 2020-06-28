(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '()) (down (cdr lst))))))

(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car (car lst)) (down (cdr lst))))))