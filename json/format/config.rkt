#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [pretty-print-json-ascii-only (parameter/c any/c)]
  [pretty-print-json-sort-keys (parameter/c any/c)]
  [pretty-print-json-indent (parameter/c (or/c exact-nonnegative-integer? 'tabs))]
  [pretty-print-json-colorize (parameter/c (or/c boolean? 'terminal))]
  ))

(define pretty-print-json-ascii-only (make-parameter #f))
(define pretty-print-json-sort-keys (make-parameter #f))
(define pretty-print-json-indent (make-parameter 2))
(define pretty-print-json-colorize (make-parameter 'terminal))
