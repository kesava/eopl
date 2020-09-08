#lang racket
(require "define-record.rkt")

;;; This file contains code from "Essentials of Programming Languages".
;;;
;;; Copyright (c) 1992, Massachusetts Institute of Technology

;;; This verison has been modified to add extra syntax for chapter 13,
;;; including those in the book, the Gustavus version of definerec, and
;;; the Gustavus addition, definetype.  Search for the string "chapter 13".
;;; These modifications are by Max Hailperin <max@gustavus.edu>.

;;; Beginning of figures for Appendix B

;;; Figure B.1 : page 465

(define-record leaf     (number))
(define-record interior (symbol left-tree right-tree))
(define-record define (var exp))
(define-record varref (var))
(define-record lit (datum))
(define-record app (rator rands))
(define-record if (test-exp then-exp else-exp))
(define-record let (decls body))
(define-record decl (var exp))
(define-record proc (formals body))
(define-record varassign (var exp))
(define-record letmutable (decls body))
(define-record begin (exp1 exp2))
(define-record letrecproc (procdecls body))
(define-record procdecl (var formals body))
(define-record letrec (decls body))
(define-record dynassign (var exp body))
(define-record letdynamic (decls body))

;;; Figure B.2 : page 466

;(define-record definearray (var dim-exp)) 
;(define-record letarray (arraydecls body))
;(define-record arrayref (array index))
;(define-record arrayassign (array index exp))
;(define-record letproc (procdecls body))
;(define-record local (decls body))
;(define-record keydecl (var exp))

;;; Figure B.2 : page 466 %%%%% This is for 3rd printing %%%%%%

(define-record definearray (var len-exp))
(define-record letarray (arraydecls body))
(define-record arrayref (array index))
(define-record arrayassign (array index exp))
(define-record letproc (procdecls body))
(define-record local (decls body))
(define-record keydecl (var exp))

;;; Figure B.3 : page 466

(define-record super-meth-app (name rands))
(define-record meth-app (name rands))
(define-record i-varref (var))
(define-record c-varref (var))
(define-record i-varassign (var exp))
(define-record c-varassign (var exp))
(define-record method (formals body))
(define-record new-simpleinst (class-exp)) ; corrected for second printing
(define-record new-simpleclass (c-vars i-vars methdecls init-exp))
(define-record new-class (parent-exp c-vars i-vars methdecls init-exp))
(define-record new-instance (class-exp parent-exp i-vars methdecls))

;;; Figure B.4 : page 466

(define-record abort (exp))
(define-record letcont (var body))
(define-record callcc (exp))
(define-record coroutine (exp))
(define-record wind (pre body post))

;;; Figure B.5 : page 466

(define-record sum (rands))

;; Added for chapter 13:

(define-record assert (type exp))
(define-record tcons (name types))
(define-record namedtype (name))
(define-record definetypeabbreviation (name type))
(define-record tuple (exps))
(define-record select (index exp))
(define-record definesumtype (name variants))
(define-record variant (name fields))
(define-record field (name type))

;; Added for Gustavus chapter 13 additions:

(define-record definerec (decls))
(define-record definetype (name rep-type decls))
(define-record case (exp clauses))
(define-record clause (name formals exp))
(define-record elseclause (exp))

;;; End of figures for Appendix B

;;; ****************************************************************

;;; Beginning of figures for Appendix D

;;; Figure D.1 : page 472, 473, 474

(define-record parser-answer (tree unparsed))
(define token-seq-head car)
(define token-seq-tail cdr)

(define check/drop
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action buffer (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define check/shift
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action (cons (token->data token) buffer)
                           (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define check/shift-class ; added for chapter 13
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action (cons (token->class token) buffer)
                           (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define goto-parser-state
  (lambda (state)
    (lambda (buffer token-seq)
      (let ((next-action (state (token-seq-head token-seq))))
        (next-action buffer token-seq)))))

(define reduce
  (lambda (prod-name)
    (lambda (buffer token-seq)
      (make-parser-answer
        (apply (get-constructor-from-name prod-name) (reverse buffer))
        token-seq))))

(define process-nt
  (lambda (state next-action)
    (lambda (buffer token-seq)
      (let ((next-answer ((goto-parser-state state) '() token-seq)))
        (next-action 
          (cons (parser-answer->tree next-answer) buffer)
          (parser-answer->unparsed next-answer))))))

(define emit-list
  (lambda ()
    (lambda (buffer token-seq)
      (make-parser-answer (reverse buffer) token-seq))))

(define character-string-parser
  (lambda (a-string)
    ;(stream-car  ; added by max@gac.edu 2000-03-14
     (parse-token-seq parse-form
                      (character-string-scanner a-string))))

(define parse-token-seq
  (lambda (start-state token-seq)
    (let ((answer (parse-once start-state token-seq)))
      (variant-case answer
        (parser-answer (tree unparsed)
          (if (eq? (token->class (token-seq-head unparsed)) 'end-marker)
              tree
              (error "Tokens left over:" unparsed)))))))

(define parse-once
  (lambda (start-state token-seq)
    ((goto-parser-state start-state) '() token-seq)))

(define parse-loop*
  (lambda (terminator separator action)
    (letrec
      ((more (lambda (token)
               (let ((class (token->class token)))
                 (cond
                   ((eq? class terminator) (emit-list))
                   ((eq? separator class)
                    (check/drop separator
                      (action (goto-parser-state more))))
                   (else (error "Invalid separator token:" token)))))))
      (lambda (token)
        (let ((class (token->class token)))
          (if (eq? class terminator)
              (emit-list)
              (action (goto-parser-state more))))))))

(define parse-loop+
  (lambda (terminator separator action)
    (letrec
      ((more (lambda (token)
               (let ((class (token->class token)))
                 (cond
                   ((eq? class terminator) (emit-list))
                   ((eq? separator class)
                    (check/drop separator
                      (action (goto-parser-state more))))
                   (else (error "Invalid separator token:" token)))))))
      (lambda (token)
        (action (goto-parser-state more))))))

(define parse-loop+-maybe-end  ; added for chapter 13
  (lambda (terminator separator action)
    (letrec
      ((more (lambda (token)
               (let ((class (token->class token)))
                 (cond
                   ((or (eq? class terminator)
                        (eq? class 'end-marker))
                    (emit-list))
                   ((eq? separator class)
                    (check/drop separator
                      (action (goto-parser-state more))))
                   (else (error "Invalid separator token:" token)))))))
      (lambda (token)
        (action (goto-parser-state more))))))

;;; Figure D.2 : page 474, 475

(define parse-form
  (lambda (token)
    ((case (token->class token)
       ((define) seen-define)
       ((definearray) seen-definearray)
       ;; Added for chapter 13:
       ((definetypeabbreviation) seen-definetypeabbreviation)
       ((definesumtype) seen-definesumtype)
       ;; Added for Gustavus chapter 13 additions:
       ((definerec) seen-definerec)
       ((definetype) seen-definetype)
       (else parse-exp))
     token)))

(define parse-exp
  (lambda (token)
    ((case (token->class token)
      ((number) seen-number)
      ((variable) seen-variable)
      ((lparen) seen-lparen)
      ((if) seen-if)
      ((let) seen-let)
      ((proc) seen-proc)
      ((begin) seen-begin)
      ((letmutable) seen-letmutable)
      ((letrecproc) seen-letrecproc)
      ((letrec) seen-letrec)
      ((letdynamic) seen-letdynamic)
      ((letarray) seen-letarray)
      ((letproc) seen-letproc)
      ((local) seen-local)
      ;((dollar-sign) seen-dollar-sign)
      ;((ampersand) seen-ampersand)
      ;((double-ampersand) seen-double-ampersand)
      ;((method) seen-method)
      ;((simpleclass) seen-simpleclass)      
      ;((simpleinstance) seen-simpleinstance)
      ;((class) seen-class) 
      ;((instance) seen-instance)
      ;((abort) seen-abort)
      ;((letcont) seen-letcont)
      ;((callcc) seen-callcc)
      ;((coroutine) seen-coroutine)
      ;((wind) seen-wind)
      ;((sum) seen-sum)
      ;; Added for chapter 13:
      ((true) seen-true)
      ((false) seen-false)
      ((assert) seen-assert)
      ((select) seen-select)
      ((langle) seen-langle)
      ;; Added for Gustavus chapter 13 additions:
      ((case) seen-case)
      (else (error "Invalid parse-exp token:" token)))
     token)))

(define parse-type  ; added for chapter 13
  (lambda (token)
    ((case (token->class token)
      ((int) seen-int)
      ((bool) seen-bool)
      ((product) seen-product)
      ((lparen) seen-type-lparen)
      ((variable) seen-typename)
      (else (error "Invalid parse-type token:" token)))
     token)))

(define parse/var
  (lambda (token)
    (case (token->class token)
      ((lparen) (seen-var&lparen token))
      ((assign-sym) (seen-var&assign-sym token))
      ((lbracket) (seen-var&lbracket token))
      (else (reduce 'varref)))))

;;;; Figure D.3 : page 476, 477

(define get-constructor-from-name
  (lambda (prod-name)
    (case prod-name
      ((chain) (lambda (x) x))
      ((lit) make-lit)      
      ((app) make-app)
      ((app/var) (lambda (var rands) (make-app (make-varref var) rands)))
      ((begin) (letrec ((loop (lambda (exps)
                                (if (null? (cdr exps))
                                    (car exps)
                                    (make-begin (car exps) (loop (cdr exps)))))))
                 loop))
      ((define) make-define)
      ((varref) make-varref)
      ((if) make-if)
      ((proc) make-proc)
      ((let) make-let)
      ((letmutable) make-letmutable)
      ((letrecproc) make-letrecproc)
      ((letrec) make-letrec)
      ((decl) make-decl)
      ((procdecl) make-procdecl)
      ((varassign) make-varassign)
      ((dynassign) make-dynassign)
      ((letdynamic) make-letdynamic)
      ((definearray) make-definearray)      
      ((arrayref) make-arrayref)      
      ((arrayref/var) (lambda (var index)
                        (make-arrayref (make-varref var) index)))
      ((arrayassign) make-arrayassign)
      ((arrayassign/var) (lambda (var index val)
                           (make-arrayassign (make-varref var) index val)))
      ((letarray) make-letarray)
      ((letproc) make-letproc)
      ((local) make-local)
      ((stuff-and-keydecls) append)      
      ((keydecl) make-keydecl)
      ((i-var) make-i-varref)
      ((c-var) make-c-varref)
      ((method) make-method)
      ((letcont) make-letcont)
      ((i-varassign) make-i-varassign)
      ((c-varassign) make-c-varassign)
      ((new-simpleinst) make-new-simpleinst)
      ((new-simpleclass) make-new-simpleclass)
      ((new-class) make-new-class)
      ((instance) make-new-instance)
      ((meth-app) (lambda (var rands)
                    (if (null? rands)
                        (error "Invalid method rands:" rands)
                        (let ((rand (car rands)))
                          (if (and (varref? rand)
                                   (eq? (varref->var rand) 'super))
                              (make-super-meth-app var (cdr rands))
                              (make-meth-app var rands))))))
      ((abort) make-abort)
      ((letcont) make-letcont)
      ((callcc) make-callcc)
      ((coroutine) make-coroutine)
      ((wind) make-wind)
      ((sum) make-sum)
      ;; Added for chapter 13:
      ((assert) make-assert)
      ((prim-type) (lambda (name) (make-tcons name '())))
      ((arrow-type) (lambda (domains range)
                      (make-tcons '->
                                  (cons range domains))))
      ((product-type) (lambda (types) (make-tcons 'product types)))
      ((definetypeabbreviation) make-definetypeabbreviation)
      ((definesumtype) make-definesumtype)
      ((select) (lambda (index exp)
                  (if (< index 0)
                      (error "illegal negative index in select:" index)
                      (make-select index exp))))
      ((tuple) make-tuple)
      ((variant) make-variant)
      ((field) make-field)
      ((typename) make-namedtype)
      ;; Added for Gustavus chapter 13 additions:
      ((definerec) make-definerec)
      ((definetype) make-definetype)
      ((case) make-case)
      ((clause) make-clause)
      ((elseclause) make-elseclause)
      (else (error "Bad production name:" prod-name)))))

;;; Figure D.4 : page 478, 479, 480, 481

(define seen-define
  (lambda (token)
    (check/drop 'define
      (check/shift 'variable
        (check/drop 'eqsign
          (process-nt parse-exp (reduce 'define)))))))

(define seen-definetypeabbreviation ; added for chapter 13
  (lambda (token)
    (check/drop 'definetypeabbreviation
      (check/shift 'variable
        (process-nt parse-type (reduce 'definetypeabbreviation))))))

(define seen-definerec ; added for Gustavus chapter 13 addition
  (lambda (token)
    (check/drop 'definerec
      (process-nt parse-decls-before-end
        (check/drop 'end
          (reduce 'definerec))))))

(define seen-definetype ; added for Gustavus chapter 13 addition
  (lambda (token)
    (check/drop 'definetype
      (check/shift 'variable
        (check/drop 'eqsign
          (process-nt parse-type
            (check/drop 'within
              (process-nt parse-decls-before-end
                (check/drop 'end
                  (reduce 'definetype))))))))))

(define seen-definesumtype ; added for chapter 13
  (lambda (token)
    (check/drop 'definesumtype
      (check/shift 'variable
        (process-nt parse-variant-list (reduce 'definesumtype))))))

(define seen-select ; added for chapter 13
  (lambda (token)
    (check/drop 'select
      (check/shift 'number
        (check/drop 'of
          (process-nt parse-exp (reduce 'select)))))))

(define seen-case ; added for Gustavus chapter 13 addition
  (lambda (token)
    (check/drop 'case
      (process-nt parse-exp
        (check/drop 'of
          (process-nt parse-clauses
            (check/drop 'end
              (reduce 'case))))))))

(define seen-number
  (lambda (token)
    (check/shift 'number (reduce 'lit))))

(define seen-true ; added for chapter 13
  (lambda (token)
    (check/shift-class 'true (reduce 'lit))))

(define seen-false ; added for chapter 13
  (lambda (token)
    (check/shift-class 'false (reduce 'lit))))

(define seen-int ; added for chapter 13
  (lambda (token)
    (check/shift-class 'int (reduce 'prim-type))))

(define seen-bool ; added for chapter 13
  (lambda (token)
    (check/shift-class 'bool (reduce 'prim-type))))

(define seen-assert ; added for chapter 13
  (lambda (token)
    (check/drop
     'assert
     (process-nt
      parse-type
      (check/drop
       'colon
       (process-nt
        parse-exp
        (reduce 'assert)))))))

(define seen-type-lparen ; added for chapter 13
  (lambda (token)
    (check/drop
     'lparen
     (check/drop
      'arrow
      (check/drop
       'lparen
       (process-nt
        parse-typelist
        (check/drop
         'rparen
         (process-nt
          parse-type
          (check/drop
           'rparen
           (reduce 'arrow-type))))))))))

(define seen-product ; added for chapter 13
  (lambda (token)
    (check/drop
     'product
     (check/drop
      'lparen
      (process-nt
       parse-typelist
       (check/drop
        'rparen
        (reduce 'product-type)))))))

(define seen-langle ; added for chapter 13
  (lambda (token)
    (check/drop
     'langle
     (process-nt
      parse-tuple-components
      (check/drop
       'rangle
       (reduce 'tuple))))))

(define parse-variant ; added for chapter 13
  (lambda (token)
    (check/shift
     'variable
     (check/drop
      'lparen
     (process-nt
      parse-field-list
      (check/drop
       'rparen
       (reduce 'variant)))))))

(define parse-field ; added for chapter 13
  (lambda (token)
    (check/shift
     'variable
     (check/drop
      'colon
     (process-nt
      parse-type
      (reduce 'field))))))

(define seen-typename ; added for chapter 13
  (lambda (token)
    (check/shift
     'variable
     (reduce 'typename))))

(define seen-variable
  (lambda (token)
    (check/shift 'variable (goto-parser-state parse/var))))

(define seen-lparen
  (lambda (token)
    (check/drop 'lparen
      (process-nt parse-exp
        (check/drop 'rparen
          (goto-parser-state parse-proc))))))

(define seen-if
  (lambda (token)
    (check/drop 'if
      (process-nt parse-exp
        (check/drop 'then
          (process-nt parse-exp
            (check/drop 'else
              (process-nt parse-exp (reduce 'if)))))))))

(define seen-proc
  (lambda (token)
    (check/drop 'proc
      (check/drop 'lparen
        (process-nt parse-varlist
          (check/drop 'rparen
            (process-nt parse-exp (reduce 'proc))))))))

(define seen-begin
  (lambda (token)
    (check/drop 'begin
      (process-nt parse-compound
        (check/drop 'end (reduce 'begin))))))

(define seen-let
  (lambda (token)
    (check/drop 'let
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'let)))))))

(define seen-letmutable
  (lambda (token)
    (check/drop 'letmutable
      (process-nt parse-decls
        (check/drop 'in
          (process-nt parse-exp (reduce 'letmutable)))))))

(define seen-letrec
  (lambda (token)
    (check/drop 'letrec
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'letrec)))))))

(define seen-letrecproc
  (lambda (token)
    (check/drop 'letrecproc
      (process-nt parse-procdecls
        (check/drop 'in (process-nt parse-exp (reduce 'letrecproc)))))))

(define seen-letdynamic
  (lambda (token)
    (check/drop 'letdynamic
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'letdynamic)))))))

(define seen-var&lparen
  (lambda (token)
    (check/drop 'lparen
      (process-nt parse-rands
        (check/drop 'rparen (reduce 'app/var))))))

(define seen-var&assign-sym
  (lambda (token)
    (check/drop 'assign-sym
      (process-nt parse-exp
        (goto-parser-state parse-assign-or-dynassign)))))

(define parse-proc
  (lambda (token)
    (case (token->class token)
      ((lparen)
       (check/drop 'lparen
         (process-nt parse-rands (check/drop 'rparen (reduce 'app)))))
      (else (reduce 'chain)))))

(define parse-assign-or-dynassign
  (lambda (token)
    (case (token->class token)
      ((during)
       (check/drop 'during (process-nt parse-exp (reduce 'dynassign))))
      (else (reduce 'varassign)))))

(define parse-varlist
  (parse-loop* 'rparen 'comma
    (lambda (action) (check/shift 'variable action))))

(define parse-typelist  ; added for chapter 13
  (parse-loop* 'rparen 'comma
    (lambda (action) (process-nt parse-type action))))

(define parse-variant-list  ; added for chapter 13
  (parse-loop+-maybe-end 'semicolon 'comma
    (lambda (action) (process-nt parse-variant action))))

(define parse-field-list  ; added for chapter 13
  (parse-loop* 'rparen 'comma
    (lambda (action) (process-nt parse-field action))))

(define parse-compound   ; corrected for second printing
  (parse-loop* 'end 'semicolon
    (lambda (action) (process-nt parse-exp action))))

(define parse-rands
  (parse-loop* 'rparen 'comma
    (lambda (action)
      (process-nt parse-exp action))))

(define parse-tuple-components ; added for chapter 13
  (parse-loop* 'rangle 'comma
    (lambda (action)
      (process-nt parse-exp action))))

(define parse-decls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-decl action))))

(define parse-decls-before-end ; added for Gustavus chapter 13 additions
  (parse-loop+ 'end 'semicolon
    (lambda (action) (process-nt parse-decl action))))

(define parse-clauses ; added for Gustavus chapter 13 additions
  (parse-loop+ 'end 'semicolon
    (lambda (action) (process-nt parse-clause action))))

(define parse-clause ; added for Gustavus chapter 13 additions
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'lparen
           (process-nt parse-varlist
             (check/drop 'rparen
               (check/drop 'arrow
                 (process-nt parse-exp (reduce 'clause))))))))
      ((else)
       (check/drop 'else
         (process-nt parse-exp (reduce 'elseclause))))
      (else (error "Invalid parse-clause token:" token)))))

(define parse-procdecls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-procdecl action))))

(define parse-decl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'eqsign
           (process-nt parse-exp (reduce 'decl)))))
      (else (error "Invalid parse-decl token:" token)))))

(define parse-procdecl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'lparen
           (process-nt parse-varlist
             (check/drop 'rparen
               (check/drop 'eqsign
                 (process-nt parse-exp (reduce 'procdecl))))))))
      (else (error "Invalid parse-procdecl token:" token)))))

;; Figure D.5 : page 482, 483

;(define parse-proc
;  (lambda (token)
;    (case (token->class token)
;      ((lparen)
;       (check/drop 'lparen
;         (process-nt parse-rands (check/drop 'rparen (reduce 'app)))))
;      ((lbracket)
;       (check/drop 'lbracket
;         (process-nt parse-exp
;           (check/drop 'rbracket
;             (goto-parser-state parse-assign-or-dynassign)))))
;      (else (reduce 'chain)))))

(define seen-var&lbracket
  (lambda (token)
    (check/drop 'lbracket
      (process-nt parse-exp
        (check/drop 'rbracket
          (goto-parser-state
            parse-arrayassign-or-arrayref/var))))))

(define seen-definearray
  (lambda (token)
    (check/drop 'definearray
      (check/shift 'variable
        (check/drop 'lbracket
          (process-nt parse-exp
            (check/drop 'rbracket (reduce 'definearray))))))))

(define seen-letarray
  (lambda (token)
    (check/drop 'letarray
      (process-nt parse-arraydecls
        (check/drop 'in (process-nt parse-exp (reduce 'letarray)))))))

(define seen-letproc
  (lambda (token)
    (check/drop 'letproc
      (process-nt parse-procdecls
        (check/drop 'in (process-nt parse-exp (reduce 'letproc)))))))

(define seen-local
  (lambda (token)
    (check/drop 'local
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'local)))))))

(define parse-arrayassign-or-arrayref
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'arrayassign))))
      (else (reduce 'arrayref)))))

(define parse-arrayassign-or-arrayref/var
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'arrayassign/var))))
      (else (reduce 'arrayref/var)))))

(define parse-arraydecls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-arraydecl action))))

(define parse-arraydecl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'lbracket
           (process-nt parse-exp
             (check/drop 'rbracket (reduce 'decl))))))
      (else (error "Invalid parse-arraydecl token:" token)))))

;;; End of figures for Appendix D

;;; ****************************************************************

;;; Beginning of figures for Appendix E

;;; Figure E.1 : page 489, 490

(define shift
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action (cons (char-seq-head char-seq) buffer)
        (char-seq-tail char-seq)))))

(define drop
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action buffer (char-seq-tail char-seq)))))

(define goto-scanner-state
  (lambda (state)
    (lambda (buffer char-seq)
      ((state (char-seq-head char-seq)) buffer char-seq))))

(define emit
  (lambda (cooker)
    (lambda (buffer char-seq)
      (make-scanner-answer (cooker (reverse buffer)) char-seq))))

(define-record token (class data))

(define-record scanner-answer (token unscanned))

(define char-seq-head car)
(define char-seq-tail cdr)

(define cook-punctuation
  (lambda (class)
    (lambda (buffer) (make-token class '*))))

(define cook-number
  (lambda (buffer)
    (make-token 'number (string->number (list->string buffer)))))

(define cook-identifier
  (lambda (buffer)
    (let ((symbol (string->symbol (list->string buffer))))
      (if (memq symbol keywords-list)
          (make-token symbol '*)
          (make-token 'variable symbol)))))

(define scan-once
  (lambda (start-state char-seq)
    ((goto-scanner-state start-state) '() char-seq)))

(define scan-char-seq
  (lambda (start-state char-seq)
    (let ((next-answer (scan-once start-state char-seq)))
      (variant-case next-answer
        (scanner-answer (token unscanned)                 
          (make-token-seq token
            (lambda ()
              (if (eq? (token->class token) 'end-marker)
                  '()
                  (scan-char-seq start-state unscanned)))))))))

(define character-string-scanner
  (lambda (a-string)
    (scan-char-seq scanner-start-state
                   ;(list->stream  ; Inserted by max@gac.edu 2000-03-14
                    (string->list (string-append a-string (string #\^))))))



;(define list->stream ; added by max@gac.edu 2000-03-14
;  (lambda (l)
;    (if (null? l)
;        the-empty-stream
;        (make-stream (car l)
;                     (lambda ()
;                       (list->stream (cdr l)))))))

(define make-token-seq
  (lambda (token thunk)
    (cons token (thunk))))

;;; Figure E.2 : page 491, 492

(define scanner-start-state
  (lambda (c)
    (cond
      ((char-whitespace? c)
       (drop (goto-scanner-state scanner-start-state)))      
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      ((char=? c #\%)
       (drop (goto-scanner-state scanner-comment-state)))
      ((char=? c #\()
       (drop (emit (cook-punctuation 'lparen))))
      ((char=? c #\))
       (drop (emit (cook-punctuation 'rparen))))
      ((char=? c #\[)
       (drop (emit (cook-punctuation 'lbracket))))
      ((char=? c #\])
       (drop (emit (cook-punctuation 'rbracket))))
      ((char=? c #\,)
       (drop (emit (cook-punctuation 'comma))))
      ((char=? c #\=)                                                  
       (drop (emit (cook-punctuation 'eqsign))))                       
      ((char=? c #\;)                                                  
       (drop (emit (cook-punctuation 'semicolon))))
      ((char=? c #\&)
       (drop (goto-scanner-state scanner-ampersand-state)))
      ((char=? c #\$)
       (drop (emit (cook-punctuation 'dollar-sign))))
      ((char=? c #\:)
       (drop (goto-scanner-state scanner-assign-state)))             
      ((or (char=? c #\+) (char=? c #\*))  ; #\- removed for chapter 13
       (shift (emit cook-identifier)))
      ((char=? c #\^)
       (emit (cook-punctuation 'end-marker)))      
      ;; Added for chapter 13:
      ((char=? c #\<)
       (drop (emit (cook-punctuation 'langle))))
      ((char=? c #\>)
       (drop (emit (cook-punctuation 'rangle))))
      ((char=? c #\-)
       (shift (goto-scanner-state scanner-minus-state)))
      (else (error "Invalid character to scan:" c)))))

(define scanner-identifier-state
  (lambda (c)
    (cond
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      (else (emit cook-identifier)))))

(define scanner-number-state
  (lambda (c)
    (cond
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      (else (emit cook-number)))))

(define scanner-comment-state
  (lambda (c)
    (cond
      ((char=? c #\newline)
       (drop (goto-scanner-state scanner-start-state)))
      (else (drop (goto-scanner-state scanner-comment-state))))))

(define scanner-assign-state
  (lambda (c)
    (cond
      ((char=? c #\=)
       (drop (emit (cook-punctuation 'assign-sym))))
      (else (emit (cook-punctuation 'colon))))))

(define scanner-ampersand-state
  (lambda (c)
    (cond
      ((char=? c #\&)
       (drop (emit (cook-punctuation 'double-ampersand))))
      (else (emit (cook-punctuation 'ampersand))))))

(define scanner-minus-state  ; added for chapter 13
  (lambda (c)
    (cond
      ((char=? c #\>)
       (drop (emit (cook-punctuation 'arrow))))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      (else (emit cook-identifier)))))

(define keywords-list
  '(if then else let in proc begin end letmutable letrecproc letrec
     during letdynamic letarray letproc local method define definearray
     simpleclass simpleinstance class instance abort letcont callcc
     coroutine wind unwind within sum
     ;; Added for chapter 13:
     true false int bool assert definetypeabbreviation definesumtype
     select of product
     ;; Added for Gustavus chapter 13 additions:
     definerec definetype case
     ))

;;; End of figures for Appendix E

;;; ****************************************************************

;;; Beginning of figures for Appendix F

;;; Figure F.1 : page 493

(define stream-car
  (lambda (s)
    (car (s))))

(define stream-cdr
  (lambda (s)
    (cdr (s))))

(define make-stream
  (lambda (car-val th)
    (lambda ()
      (cons car-val (let ((cdr-val #f))
                      (lambda ()
                        (if (pair? cdr-val)
                            cdr-val
                            (begin (set! cdr-val ((th))) cdr-val))))))))

(define the-null-stream
  (make-stream 'end-of-stream (lambda () the-null-stream)))

(define stream-null?
  (lambda (s)
    (eq? (stream-car s) 'end-of-stream)))
;
;;;; Figure F.2 : page 494
;
;(define stream-for-each
;  (lambda (proc stream)
;    (letrec
;      ((loop
;         (lambda (stream)
;           (if (not (stream-null? stream))
;               (begin
;                 (proc (stream-car stream))
;                 (loop (stream-cdr stream)))))))
;      (loop stream))))
;
;(define make-input-stream
;  (lambda ()
;    (let ((char (read-char)))
;      (if (eof-object? char)
;          the-null-stream
;          (make-stream char make-input-stream)))))
;
;;;; Figure F.3 : page 494
;
;;(define make-char-seq make-stream)
;;
;;(define char-seq-head stream-car)
;;
;;(define char-seq-tail stream-cdr)
;;
;;(define make-token-seq make-stream)
;;
;;(define token-seq-head stream-car)
;;
;;(define token-seq-tail stream-cdr)
;
;;;; Figure F.5 : page 496
;
;;(define parse-semicolon-terminated-form
;;  (lambda (token)
;;    (process-nt parse-form
;;      (goto-parser-state parse-terminated-by))))
;;
;;(define parse-terminated-by
;;  (lambda (token)
;;    (case (token->class token)
;;      ((semicolon)
;;       (check/drop 'semicolon (reduce 'chain)))
;;      ((end)
;;       (reduce 'chain))
;;      (else (error "Invalid terminator:" token)))))
;;
;;(define parse-token-seq
;;  (lambda (start-state token-seq)
;;    (let ((token (token-seq-head token-seq)))
;;      (if (or (eq? (token->class token) 'end)
;;              (eq? (token->class token) 'end-marker))
;;          the-null-stream
;;          (variant-case (parse-once start-state token-seq)
;;            (parser-answer (tree unparsed)
;;              (make-stream tree
;;                (lambda ()
;;                  (parse-token-seq start-state unparsed)))))))))
;
;;;; End of figures for Appendix F

(provide (all-defined-out))