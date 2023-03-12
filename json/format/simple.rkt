#lang racket/base

; Simple pure-Racket JSON formatter. Very similiar output to jq

(require racket/contract racket/function racket/port racket/symbol racket/unsafe/ops
         json
         "config.rkt" "colors.rkt")

(provide
 (contract-out
  [jsexpr->pretty-json (-> jsexpr? string?)]
  [format-json (-> string? string?)]
  [pretty-print-jsexpr (->* (jsexpr?) (output-port?) void?)]
  [pretty-print-json (->* (string?) (output-port?) void?)]))

; Like jsexpr->string but nicely formatted output
(define (jsexpr->pretty-json js)
  (call-with-output-string (curry pretty-print-jsexpr js)))

; Reformat JSON text into nicely formatted JSON
(define (format-json json)
  (call-with-output-string (curry pretty-print-json json)))

(define (pretty-print-json json [out-port (current-output-port)])
  (pretty-print-jsexpr (string->jsexpr json) out-port))

(define (pretty-print-jsexpr js [out-port (current-output-port)])
  (void (print-jsexpr js 0 out-port (colorize? out-port))))

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

(define (print-jsexpr js depth out-port in-color?)
  (cond
    ((hash? js) (print-object js depth out-port in-color?))
    ((list? js) (print-array js depth out-port in-color?))
    (else
     (when in-color?
       (write-bytes (color-bytestr (cond ((string? js) 'string) ((number? js) 'number) ((eq? js #t) 'true) ((eq? js #f) 'false) ((eq? (json-null) js) 'null))) out-port))
     (write-json js out-port #:encode (if (pretty-print-json-ascii-only) 'all 'control))
     (when in-color? (write-bytes (reset-color) out-port)))))

(define (print-object-element key val pos depth out-port in-color?)
  (when (unsafe-fx> pos 0)
    (write-char #\, out-port))
  (newline out-port)
  (when (unsafe-fx> depth 0)
    (indent depth out-port))
  (when in-color? (write-bytes (color-bytestr 'field) out-port))
  (write-json (symbol->immutable-string key) out-port #:encode (if (pretty-print-json-ascii-only) 'all 'control))
  (when in-color? (write-bytes (reset-color) out-port))
  (write-bytes #" : " out-port)
  (print-jsexpr val depth out-port in-color?))

(define (print-object obj depth out-port in-color?)
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
           (print-object-element (unsafe-car elem) (unsafe-cdr elem) i (unsafe-fx+ depth 1) out-port in-color?))
         (for ([(k v) (in-hash obj)]
               [i (in-naturals)])
           (print-object-element k v i (unsafe-fx+ depth 1) out-port in-color?)))
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

(define (print-array lst depth out-port in-color?)
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
       (print-jsexpr elem (unsafe-fx+ depth 1) out-port in-color?))
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
