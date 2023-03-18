#lang racket/base

; Simple pure-Racket JSON formatter. Tries to use less vertical space than the others.

(require racket/bytes racket/contract racket/function racket/port racket/string racket/symbol racket/unsafe/ops
         json unicode-breaks soup-lib/parameter
         "config.rkt" "colors.rkt")

(define (exact-positive-fixnum? x)
  (and (fixnum? x) (unsafe-fx> x 0)))

(provide
 (contract-out
  [pretty-print-json-line-width (parameter/c exact-positive-fixnum?)]
  [pretty-print-json-tab-width (parameter/c exact-positive-fixnum?)]
  [jsexpr->pretty-json (-> jsexpr? string?)]
  [jsexpr->pretty-json/bytes (-> jsexpr? bytes?)]
  [format-json (-> string? string?)]
  [format-json/bytes (-> bytes? bytes?)]
  [pretty-print-jsexpr (->* (jsexpr?) (output-port?) void?)]
  [pretty-print-json (->* (string?) (output-port?) void?)]
  [pretty-print-json/bytes (->* (bytes?) (output-port?) void?)]))

(define-parameter pretty-print-json-line-width 80)
(define-parameter pretty-print-json-tab-width 8)

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
  (void (print-jsexpr js 0 0 out-port (colorize? out-port))))

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

(define (indent-width depth)
  (let ([width (pretty-print-json-indent)])
    (if (eq? width 'tabs)
        (unsafe-fx* depth (pretty-print-json-tab-width))
        (unsafe-fx* width depth))))

(define (print-jsexpr js depth width out-port in-color?)
  (cond
    ((hash? js) (print-object js depth width out-port in-color?))
    ((list? js) (print-array js depth width out-port in-color?))
    (else
     (when in-color?
       (write-bytes (color-bytestr (highlight-type js)) out-port))
     (write-json js out-port #:encode (if (pretty-print-json-ascii-only) 'all 'control))
     (when in-color? (write-bytes (reset-color) out-port)))))

(define (json-atom? js)
  (not (or (hash? js) (list? js))))

(define (char-display-width ch)
  (cond
    ((or (unsafe-char=? ch #\nul) (unsafe-char=? ch #\u200B)) 0)
    ((unsafe-char<=? #\u1160 ch #\u11FF) 0)
    ((unsafe-char=? ch #\u00AD) 1)
    ((let ([gc (char-general-category ch)])
       (or (eq? gc 'cc) (eq? gc 'mn) (eq? gc 'me) (eq? gc 'cf)))
     0)
    ((let ([eaw (char-east-asian-width-property ch)])
       (or (eq? eaw 'F) (eq? eaw 'W)))
     2)
    (else 1)))

(define (string-display-width str)
  (for/sum ([grapheme (in-graphemes str)])
    (char-display-width (unsafe-string-ref grapheme 0))))
  
(define (print-object-element key val pos depth width out-port in-color?)
  (let ([key (jsexpr->string (symbol->immutable-string key) #:encode (if (pretty-print-json-ascii-only) 'all 'control))])
    (when (> pos 0)
      (write-char #\, out-port))
    (newline out-port)
    (when (unsafe-fx> depth 0)
      (indent depth out-port))
    (when in-color? (write-bytes (color-bytestr 'field) out-port))
    (write-string key out-port)
    (when in-color? (write-bytes (reset-color) out-port))
    (write-bytes #" : " out-port)
    (print-jsexpr val depth (+ width (indent-width depth) (string-display-width key) 2) out-port in-color?)))

(define (print-object-simple obj depth width out-port in-color?)
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
        (print-object-element (unsafe-car elem) (unsafe-cdr elem) i (unsafe-fx+ depth 1) width out-port in-color?))
      (for ([(k v) (in-hash obj)]
            [i (in-naturals)])
        (print-object-element k v i (unsafe-fx+ depth 1) width out-port in-color?)))
  (newline out-port)
  (when (unsafe-fx> depth 0)
    (indent depth out-port))
  (cond
    (in-color?
     (write-bytes (color-bytestr 'object) out-port)
     (write-char #\} out-port)
     (write-bytes (reset-color) out-port))
    (else
     (write-char #\} out-port)))
  -1)

(define (object-display-width keys vals)
  (+ 4
     (* 4 (length keys))
     (foldl (lambda (k sum) (+ (string-display-width k) sum)) 0 keys)
     (foldl (lambda (v sum) (+ (string-display-width v) sum)) 0 vals)))

(define (print-object obj depth width out-port in-color?)
  (cond
    ((= (hash-count obj) 0)
     (cond
       (in-color?
        (write-bytes (color-bytestr 'object) out-port)
        (write-bytes #"{}" out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-bytes #"{}" out-port)))
     2)
    ((for/and ([val (in-hash-values obj)]) (json-atom? val))
     (let* ([encode (if (pretty-print-json-ascii-only) 'all 'control)]
            [keys (map (lambda (key) (jsexpr->string (symbol->immutable-string key) #:encode encode)) (hash-keys obj))]
            [vals (map (lambda (val) (jsexpr->string val #:encode encode)) (hash-values obj))]
            [display-width (object-display-width keys vals)])
       (cond
         ((<= (+ width display-width) (pretty-print-json-line-width))
          (cond
            (in-color?
             (write-bytes (color-bytestr 'object) out-port)
             (write-char #\{ out-port)
             (write-bytes (reset-color) out-port)
             (write-char #\space out-port))
            (else
             (write-bytes #"{ " out-port)))
          (if in-color?
              (write-bytes (bytes-join
                            (map (lambda (key val vstr)
                                   (bytes-append (color-bytestr 'field) (string->bytes/utf-8 key) (reset-color)
                                                 #": "
                                                 (color-bytestr (highlight-type val)) (string->bytes/utf-8 vstr) (reset-color)))
                                 keys (hash-values obj) vals)
                            #", ")
                           out-port)
              (write-string (string-join (map (lambda (k v) (string-append k ": " v)) keys vals) ", ") out-port))
          (write-char #\space out-port)
          (cond
            (in-color?
             (write-bytes (color-bytestr 'object) out-port)
             (write-char #\} out-port)
             (write-bytes (reset-color) out-port))
            (else
             (write-char #\} out-port)))
          display-width)
         (else
          (print-object-simple obj depth width out-port in-color?)))))
    (else
     (print-object-simple obj depth width out-port in-color?))))

(define (print-array-simple lst depth width out-port in-color?)
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
    (print-jsexpr elem (unsafe-fx+ depth 1) width out-port in-color?))
  (newline out-port)
  (when (unsafe-fx> depth 0)
    (indent depth out-port))
  (cond
    (in-color?
     (write-bytes (color-bytestr 'array) out-port)
     (write-char #\] out-port)
     (write-bytes (reset-color) out-port))
    (else
     (write-char #\] out-port)))
  -1)

(define (array-display-width los)
  (+ 4 (* 2 (length los)) (foldl (lambda (s sum) (+ (string-display-width s) sum)) 0 los)))

(define (print-array lst depth width out-port in-color?)
  (cond
    ((null? lst)
     (cond
       (in-color?
        (write-bytes (color-bytestr 'array) out-port)
        (write-bytes #"[]" out-port)
        (write-bytes (reset-color) out-port))
       (else
        (write-bytes #"[]" out-port)))
     2)
    ((andmap json-atom? lst)
     (let* ([encode (if (pretty-print-json-ascii-only) 'all 'control)]
            [exprs (map (lambda (expr) (jsexpr->string expr #:encode encode)) lst)]
            [display-width (array-display-width exprs)])
       (cond
         ((<= (+ width display-width) (pretty-print-json-line-width))
          (cond
            (in-color?
             (write-bytes (color-bytestr 'array) out-port)
             (write-char #\[ out-port)
             (write-bytes (reset-color) out-port)
             (write-char #\space out-port))
            (else
             (write-bytes #"[ " out-port)))
          (if in-color?
              (write-bytes (bytes-join
                            (map (lambda (str expr)
                                   (bytes-append (color-bytestr (highlight-type expr)) (string->bytes/utf-8 str) (reset-color)))
                                 exprs lst)
                            #", ")
                           out-port)
              (write-string (string-join exprs ", ") out-port))
          (write-char #\space out-port)
          (cond
            (in-color?
             (write-bytes (color-bytestr 'array) out-port)
             (write-char #\] out-port)
             (write-bytes (reset-color) out-port))
            (else
             (write-char #\] out-port)))
          display-width)
         (else
          (print-array-simple lst depth width out-port in-color?)))))
    (else
     (print-array-simple lst depth width out-port in-color?))))
