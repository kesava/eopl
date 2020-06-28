(define (match-case-default lst check-against isodd  acc)
  (if (null? lst)
      acc
      (let ((condition-type (car (car lst))))
        (if (eq? condition-type 'case)
            (let ((value-for-case (cadr (car lst))) (statement (if isodd
                                                                    (cons (list '(if (eq? value-for-case check-against) value-fo-case)) acc)
                                                                    (cons value-for-case acc))))
              (match-case-default (cdr lst) check-against (not isodd) statement))
            (let ((statement (cons value-for-case acc)))
                (cons statement acc))))))
          
(define (switch->if exp)
  (if (eq? (car exp) 'switch)
      (let ((conditional-value (cadr exp)))
        (match-case-default (cddr exp) conditional-value #t '()))
      exp))

(switch->if '(switch (= 2 1) (case (+ 2 1) x) (case (- 2 1) y)))