#lang eopl
; Section 4.2
; Exercise 4.2.1 - use beta-reductions

; 1
; ((lambda (x) (x (y x)))
; z)
; => (z (y z))

; 2
; ((lambda (x) (x y))
;  (lambda (y) (x y)))
; => 
; ((lambda (z1) (z1 y))
;  (lambda (z2) (x z2)))
; =>
; ((lambda (z2) (x z2)) y)
; =>
; (x y)

; 3
; ((lambda (x)
;    (lambda (y) ((x y) z)))
;  (lambda (a) y))
; =>
; ((lambda (z1)
;    (lambda (z2) ((z1 z2) z)))
;  (lambda (a) y))
; =>
; ((lambda (z2) ((lambda (a) y) z2) z))
; =>
; ((lambda (a) y) z)
; =>
; y

; 4
; ((lambda (x)
;    (lambda (y)
;      ((lambda (x) (z x))
;       (lambda (y) (z y)))))
;  (lambda (y) y))
; =>
; (lambda (y)
;   ((lambda (z1) (z z1))
;    (lambda (z2) (z z2))))