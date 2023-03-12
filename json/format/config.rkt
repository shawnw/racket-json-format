#lang racket/base

(require racket/contract racket/unsafe/ops)

(provide
 (contract-out
  [pretty-print-json-ascii-only (parameter/c any/c boolean?)]
  [pretty-print-json-sort-keys (parameter/c any/c boolean?)]
  [pretty-print-json-indent (parameter/c (or/c exact-nonnegative-fixnum? 'tabs))]
  [pretty-print-json-colorize (parameter/c (or/c boolean? 'terminal))]
  ))

(define (exact-nonnegative-fixnum? x) (and (fixnum? x) (unsafe-fx>= x 0)))
(define (->boolean x) (if x #t #f))

(define pretty-print-json-ascii-only (make-parameter #f ->boolean))
(define pretty-print-json-sort-keys (make-parameter #f ->boolean))
(define pretty-print-json-indent (make-parameter 2))
(define pretty-print-json-colorize (make-parameter 'terminal))
