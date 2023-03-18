#lang racket/base

(require racket/contract racket/unsafe/ops soup-lib/parameter)

(provide
 (contract-out
  [pretty-print-json-ascii-only (parameter/c any/c boolean?)]
  [pretty-print-json-sort-keys (parameter/c any/c boolean?)]
  [pretty-print-json-indent (parameter/c (or/c exact-nonnegative-fixnum? 'tabs))]
  [pretty-print-json-colorize (parameter/c (or/c boolean? 'terminal))]
  ))

(define (exact-nonnegative-fixnum? x) (and (fixnum? x) (unsafe-fx>= x 0)))

(define-boolean-parameter pretty-print-json-ascii-only #f)
(define-boolean-parameter pretty-print-json-sort-keys #f)
(define-parameter pretty-print-json-indent 2)
(define-parameter pretty-print-json-colorize 'terminal)
