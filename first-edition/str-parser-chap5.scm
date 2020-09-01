#lang eopl
(define-datatype exp exp?
  (def-exp
    (var symbol?)
    (exp exp?))
  (varref-exp
   (var exp?))
  (lit-exp
   (datum number?))
  (app-exp
   (rator exp?)
   (rands (list-of exp?)))
  (if-exp
   (test-exp exp?)
   (then-exp exp?)
   (else-exp exp?))
  (begin-exp
    (exp1 exp?)
    (exp2 exp?)))

(define keywords-list
  '(if then else begin))

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

(define-datatype token-type token-type?
  (token
   (class symbol?)
   (data symbol?)))

(define token->class
  (lambda (tok)
    (cases token-type tok
      (token (class data) class))))

(define unscanned?
  (lambda (str)
    (or (char? str) (string? str) (list? str))))

(define-datatype scanner-answer-type scanner-answer-type?
  (scanner-answer
   (token token-type?)
   (unscanned unscanned?)))


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
      (cases scanner-answer-type next-answer
        (scanner-answer (token unscanned)                 
          (make-token-seq token
            (lambda ()
              (if (eq? (token->class token) 'end-marker)
                  '()
                  (scan-char-seq start-state unscanned)))))))))

(define character-string-scanner
  (lambda (a-string)
    (scan-char-seq scanner-start-state
      (string->list (string-append a-string (string #\^))))))

(define make-token-seq
  (lambda (token thunk)
    (cons token (thunk))))

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
      ((or (char=? c #\+) (char=? c #\-) (char=? c #\*))
       (shift (emit cook-identifier)))
      ((char=? c #\^)
       (emit (cook-punctuation 'end-marker)))      
      (else (eopl:error "Invalid character to scan:" c)))))

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

(define character-string-parser
  (lambda (a-string)
    (parse-token-seq parse-form
      (character-string-scanner a-string))))


(define tree? pair?)

(define-datatype parser-answer-type parser-answer-type?
  (parser-answer
   (tree tree?)
   (unparsed list?)))

(define token-seq-head car)
(define token-seq-tail cdr)

(define parse-token-seq
  (lambda (start-state token-seq)
    (let ((answer (parse-once start-state token-seq)))
      (cases parser-answer-type answer
        (parser-answer (tree unparsed)
          (if (eq? (token->class (token-seq-head unparsed)) 'end-marker)
              tree
              (eopl:error "Tokens left over:" unparsed)))))))

(define parse-form
  (lambda (token)
    ((case (token->class token)
       ((define) seen-define)
       ((definearray) seen-definearray)
       (else parse-exp))
     token)))

(define parse-once
  (lambda (start-state token-seq)
    ((goto-parser-state start-state) '() token-seq)))