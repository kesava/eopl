(define dupe
  (lambda (n x)
          (if (= n 0)
              '()
              (cons x (dupe (- n 1) x)))))



(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (cadr (car lst)) (car (car lst))) (invert (cdr lst))))))

(define list-index
  (lambda (item lst)
    (if (null? lst)
        -1
        (if (eq? (car lst) item)
            0
            (if (= (list-index item (cdr lst)) -1)
                -1
                (+ 1 (list-index item (cdr lst))))))))

(define filter-in
  (lambda (p lst)
    (if (null? lst)
        '()
        (if (eq? (p (car lst)) #t)
            (cons (car lst) (filter-in p (cdr lst)))
            (filter-in p (cdr lst))))))

