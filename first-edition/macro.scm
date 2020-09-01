#lang racket
(require racket/syntax)

(define-syntax (quoted-foo stx)
    #'"I am also foo, using #' instead of syntax")