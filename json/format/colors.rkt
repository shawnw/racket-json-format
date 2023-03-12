#lang racket/base

; Internal module for parsing JQ_COLORS environment variable.

(require racket/dict racket/function racket/match racket/string racket/unsafe/ops "config.rkt")
(provide colorize? color-bytestr reset-color)

(define (make-color-map alist)
  (for/fold ([table (hasheqv)])
            ([(name num) (in-dict alist)])
    (hash-set* table name num num name)))

(define color-map
  (make-color-map
   '((default . 0) (bright . 1) (dim . 2) (underscore . 4) (blink . 5) (reverse . 7) (hidden . 8)
                   (black . 30) (red . 31) (green . 32) (yellow . 33) (blue . 34)
                   (magenta . 35) (cyan . 36) (white . 37))))

(struct color (style fg) #:transparent)
(define (make-color fg #:style [style 'default])
  (color (hash-ref color-map style) (hash-ref color-map fg)))

(define default-colors
  (hasheq
   'null (make-color 'black #:style 'bright)
   'true (make-color 'white)
   'false (make-color 'white)
   'number (make-color 'white)
   'string (make-color 'green)
   'array (make-color 'white #:style 'bright)
   'object (make-color 'white #:style 'bright)
   'field (make-color 'blue #:style 'bright)))

(define (parse-colors color-str)
  (define color-fields '(null true false number string array object))
  (for/fold ([table default-colors])
            ([elem (string-split color-str ":")]
             [field (in-list color-fields)])
    (match elem
      ((pregexp #px"^(\\d+);(\\d+)$" (list _ style fg))
       (let ([style (string->number style)]
             [fg (string->number fg)])
         (if (and (hash-has-key? color-map style)
                  (hash-has-key? color-map fg))
             (hash-update table field (color style fg))
             table)))
      (_ table))))

(define color-table
  (cond
    ((getenv "JQ_COLORS") => parse-colors)
    (else default-colors)))

(define cache (make-hash))
(define (color-bytestr type)
  (let ([c (hash-ref color-table type)])
    (hash-ref! cache c
               (thunk (unsafe-bytes->immutable-bytes!
                       (string->bytes/utf-8 (format "\e[~A;~Am" (color-style c) (color-fg c))))))))

(define (reset-color) #"\e[0m")

(define (colorize? port)
  (case (pretty-print-json-colorize)
    ((#t) #t)
    ((#f) #f)
    ((terminal) (terminal-port? port))))
