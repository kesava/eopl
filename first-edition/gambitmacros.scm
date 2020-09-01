#lang r5rs
;;; MacGambit macros implementing records for the book:
;;;
;;; "Essentials of Programming Languages", Daniel P. Friedman,
;;;   Mitchell Wand and Christopher T. Haynes, MIT Press, 1992.
;;;
;;; (C) Copyright 1993 David McCusker

(define-macro (define-record rec-name rec-fields)
  `(let*
    ((sym string->symbol)
     (str symbol->string)
     (cat string-append)
     (vec-len (+ 1 (length ',rec-fields)))
     (name ',rec-name)
     (name-str (str ',rec-name))
     (make-name (sym (cat (str 'make-) name-str)) )
     (name? (sym (cat name-str "?")))
     (index 0))
    (eval
      `(define ,make-name
        (lambda values (apply vector ',name values))))
    (eval
      `(define ,name?
        (lambda (obj)
          (and (vector? obj)
            (= (vector-length obj) ,vec-len)
            (eq? (vector-ref obj 0) ',name)))))
    (for-each
      (lambda (f)
        (set! index (+ index 1))
        (let* ((name->field (sym (cat name-str "->" (str f))))
          (problem (cat (str name->field) ": bad record")))
          (eval
            `(define ,name->field
              (lambda (obj)
                (if (,name? obj)
                  (vector-ref obj ,index)
                  (error ,problem obj)))))))
      ',rec-fields)
    name))

(define every?
  (letrec
    ((all?
      (lambda (proc list)
        (if (pair? list)
          (if (proc (car list))
            (all? proc (cdr list))
            #f)
          (if (null? list) #t (error "every?: not a list" list))))))
    all?))

(define-macro (variant-case record-var . clauses)
  (let*
    ((sym string->symbol)
     (str symbol->string)
     (cat string-append)
     (exp (gensym))
     (type? (lambda (name) (list (sym (cat name "?")) exp)) )
     (good?
       (lambda (c)
         (and (pair? c)
           (or (eq? 'else (car c))
             (and (symbol? (car c))
               (pair? (cdr c))
               (list? (cadr c))
               (every? symbol? (cadr c)))))))
     (check-clause-syntax
       (lambda (c)
         (if (not (good? c))
           (error "variant-case: expected syntax (name field-list ...)" c))))
     (make-clause
       (lambda (c)
         (let*
           ((n (str (car c)))
            (n->f (lambda (f) (list f (list (sym (cat n "->" (str f))) exp)))))
           (if (eq? 'else (car c))
             c
             (list (type? n)
               (cons 'let (cons (map n->f (cadr c)) (cddr c)))))))))
    (for-each check-clause-syntax clauses)
    `(let ((,exp ,record-var))
         (cond ,@(map make-clause clauses)))))