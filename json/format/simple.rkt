#lang racket/base

; Simple pure-Racket JSON formatter. Very similiar output to jq

(require racket/contract racket/function racket/port racket/symbol racket/unsafe/ops
         json soup-lib/json
         "config.rkt" "colors.rkt")

(provide
 (contract-out
  [jsexpr->pretty-json (-> jsexpr? string?)]
  [jsexpr->pretty-json/bytes (-> jsexpr? bytes?)]
  [format-json (-> string? string?)]
  [format-json/bytes (-> bytes? bytes?)]
  [pretty-print-jsexpr (->* (jsexpr?) (output-port?) void?)]
  [pretty-print-json (->* (string?) (output-port?) void?)]
  [pretty-print-json/bytes (->* (bytes?) (output-port?) void?)]))

; Like jsexpr->string but nicely formatted output
(define (jsexpr->pretty-json js)
  (call-with-output-string (curry pretty-print-jsexpr js)))

; Like jsexpr->bytes but nicely formatted output
(define (jsexpr->pretty-json/bytes js)
  (call-with-output-bytes (curry pretty-print-jsexpr js)))

; Reformat JSON text into nicely formatted JSON
(define (format-json json)
  (call-with-output-string (curry pretty-print-json json)))

; Reformat JSON bytestring into nicely formatted JSON
(define (format-json/bytes json)
  (call-with-output-bytes (curry pretty-print-json/bytes json)))

(define (pretty-print-json json [out-port (current-output-port)])
  (pretty-print-jsexpr (string->jsexpr json) out-port))

(define (pretty-print-json/bytes json [out-port (current-output-port)])
  (pretty-print-jsexpr (bytes->jsexpr json) out-port))

(define (pretty-print-jsexpr js [out-port (current-output-port)])
  (void (print-jsexpr js 0 out-port (colorize? out-port) (if (pretty-print-json-ascii-only) 'all 'control))))

(define spaces (unsafe-bytes->immutable-bytes! (make-bytes 32 (unsafe-char->integer #\space))))
(define tabs (unsafe-bytes->immutable-bytes! (make-bytes 32 (unsafe-char->integer #\tab))))

(define (write-n-bytes bstr n out-port)
  (if (unsafe-fx<= n (unsafe-bytes-length bstr))
      (write-bytes bstr out-port 0 n)
      (begin
        (write-bytes bstr out-port)
        (write-n-bytes bstr (unsafe-fx- n (unsafe-bytes-length bstr)) out-port))))

; Write the appropriate number of spaces or tabs.
(define (indent depth out-port)
  (let ([width (pretty-print-json-indent)])
    (if (eq? width 'tabs)
        (write-n-bytes tabs depth out-port)
        (write-n-bytes spaces (unsafe-fx* depth width) out-port))))

(define (print-jsexpr js depth out-port in-color? ascii?)
  (json-match #:unsafe js
    (object (print-object js depth out-port in-color? ascii?))
    (array (print-array js depth out-port in-color? ascii?))
    (else
     (when in-color?
       (write-bytes (color-bytestr (highlight-type js)) out-port))
     (write-json js out-port #:encode ascii?)
     (when in-color? (write-bytes (reset-color) out-port)))))

(define (print-object-element key val pos depth out-port in-color? ascii?)
  (when (unsafe-fx> pos 0)
    (write-char #\, out-port))
  (newline out-port)
  (when (unsafe-fx> depth 0)
    (indent depth out-port))
  (when in-color? (write-bytes (color-bytestr 'field) out-port))
  (write-json (symbol->immutable-string key) out-port #:encode ascii?)
  (when in-color? (write-bytes (reset-color) out-port))
  (write-bytes #" : " out-port)
  (print-jsexpr val depth out-port in-color? ascii?))

(define (print-object obj depth out-port in-color? ascii?)
  (cond
    ((= (hash-count obj) 0)
     (cond
       (in-color?
        (write-bytes (color-bytestr 'object) out-port)
        (write-bytes #"{}" out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-bytes #"{}" out-port))))
    (else
     (cond
       (in-color?
        (write-bytes (color-bytestr 'object) out-port)
        (write-char #\{ out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-char #\{ out-port)))
     (if (pretty-print-json-sort-keys)
         (for ([elem (hash->list obj #t)]
               [i (in-naturals)])
           (print-object-element (unsafe-car elem) (unsafe-cdr elem) i (unsafe-fx+ depth 1) out-port in-color? ascii?))
         (for ([(k v) (in-hash obj)]
               [i (in-naturals)])
           (print-object-element k v i (unsafe-fx+ depth 1) out-port in-color? ascii?)))
     (newline out-port)
     (when (unsafe-fx> depth 0)
       (indent depth out-port))
     (cond
       (in-color?
        (write-bytes (color-bytestr 'object) out-port)
        (write-char #\} out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-char #\} out-port))))))

(define (print-array lst depth out-port in-color? ascii?)
  (cond
    ((null? lst)
     (cond
       (in-color?
        (write-bytes (color-bytestr 'array) out-port)
        (write-bytes #"[]" out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-bytes #"[]" out-port))))
    (else
     (cond
       (in-color?
        (write-bytes (color-bytestr 'array) out-port)
        (write-char #\[ out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-char #\[ out-port)))
     (for ([elem (in-list lst)]
           [i (in-naturals)])
       (when (unsafe-fx> i 0)
         (write-char #\, out-port))
       (newline out-port)
       (indent (unsafe-fx+ depth 1) out-port)
       (print-jsexpr elem (unsafe-fx+ depth 1) out-port in-color? ascii?))
     (newline out-port)
     (when (unsafe-fx> depth 0)
       (indent depth out-port))
     (cond
       (in-color?
        (write-bytes (color-bytestr 'array) out-port)
        (write-char #\] out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-char #\] out-port))))))
