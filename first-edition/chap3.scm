#lang eopl

; Exercise 3.1.1
(let ((x 5) (y 6) (z 7))
  (let ((x 13) (y (+ x y)) (z x))
    (- (+ x z) y)))
; 7

(let ((x 5) (y 6) (z 7))
  (+ (let ((z (+ x z))) (* z (+ x z)))
     (let ((z (* x y))) (+ z (* z (- z y))))))

; 954

(define extract-vars-exp
  (lambda (exp)
    (define helper
      (lambda (v-acc exp-acc lst)
        (cond
          ((null? lst) (list (reverse v-acc) (reverse exp-acc)))
          (else (helper (cons (car (car lst)) v-acc) (cons (cadr (car lst)) exp-acc) (cdr lst))))))
    (helper '() '() exp)))
          
(define let->application
  (lambda (let-exp)
    (if (eq? (car let-exp) 'let)
        (list (list 'lambda (car (extract-vars-exp (cadr let-exp))) (caddr let-exp)) (cadr (extract-vars-exp (cadr let-exp))))
        let-exp)))

(let->application '(let ((x 4) (y 3)) (let ((z 5)) (+ x (+ y z)))))
; ((lambda (x y) (let ((z 5)) (+ x (+ y z)))) (4 3))