(define (rewrite-let exp)
  (if (eq? (car exp) 'let)
      (let ((vars-exprs (cadr exp)) (body (caddr exp)))
        (let ((vars (map car vars-exprs)) (exprs (map cadr vars-exprs)))
          (cons (cons 'lambda (cons vars (cons (rewrite-let body) '()))) exprs)))
      exp))

(rewrite-let '(let ((x (+ 2 2)) (y (+ 3 3)))
  (* x y)))

(rewrite-let '(let ((x 5))
    (let ((x 2)
          (y x))
      (list y x))))


(rewrite-let '(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x))))