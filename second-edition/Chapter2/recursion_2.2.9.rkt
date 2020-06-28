(define root
  (lambda (tree)
    (if (null? tree)
        0
        (if (null? (car tree))
            0
            (car tree)))))

(define left-branch
  (lambda (tree)
  (cadr tree)))

(define right-branch
  (lambda (tree)
  (caddr tree)))

(define treepath
  (lambda (tree item)
    (if (null? tree)
        '()
        (if (eq? (root tree) item)
            (cons 'root '())
            (if (< (root tree) item)
                (cons 'left (treepath (left-branch tree) item))
                (cons 'right (treepath (right-branch tree) item)))))))