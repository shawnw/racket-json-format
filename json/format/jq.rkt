#lang racket/base

; Format JSON by piping it to a jq instance with a filter of . and reading its output.

(require racket/contract racket/function racket/port json
         "config.rkt"
         (for-syntax racket/base))

(provide
 (contract-out
  [jq-path (parameter/c (or/c path-string? #f))]
  [jq-filter (parameter/c string?)]
  [jsexpr-transform (-> jsexpr? string? jsexpr?)]
  [jsexpr->pretty-json (-> jsexpr? string?)]
  [jsexpr->pretty-json/bytes (-> jsexpr? bytes?)]
  [format-json (-> string? string?)]
  [format-json/bytes (-> bytes? bytes?)]
  [pretty-print-jsexpr (->* (jsexpr?) (output-port?) void?)]
  [pretty-print-json (->* (string?) (output-port?) void?)]
  [pretty-print-json/bytes (->* (bytes?) (output-port?) void?)]))

(define jq-path (make-parameter (find-executable-path "jq")))
(define jq-filter (make-parameter "."))

(define (jsexpr-transform js filter)
  (parameterize
      ([jq-filter filter]
       [pretty-print-json-colorize #f])
    (string->jsexpr (jsexpr->pretty-json js))))

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

(define (make-jq-args out-port)
  (append (if (pretty-print-json-ascii-only) '("--ascii-output") '())
          (if (pretty-print-json-sort-keys) '("--sort-keys") '())
          (if (eq? (pretty-print-json-indent) 'tabs) '("--tab") `("--indent" ,(number->string (pretty-print-json-indent))))
          (case (pretty-print-json-colorize)
            ((#t) '("--color-output"))
            ((#f) '("--monochrome-output"))
            ((terminal)
             (if (terminal-port? out-port) '("--color-output") '("--monochrome-output"))))
          `("--unbuffered" ,(jq-filter))))

(define-syntax define-pretty-printer
  (lambda (stx)
    (syntax-case stx ()
      ((_ name type)
       #`(define (name json [out-port (current-output-port)])
           ; Start a jq child process to format the JSON.
           (unless (jq-path)
             (raise (make-exn:fail:filesystem "jq executable not found" (current-continuation-marks))))
           (let*-values ([(jq-process jq-out jq-in jq-err)
                          (apply subprocess (if (file-stream-port? out-port) out-port #f) #f 'stdout (jq-path) (make-jq-args out-port))]
                         [(writer) ; One thread to write the JSON to jq and close the input pipe when done
                          (thread (thunk
                                   (#,(case (syntax-e #'type)
                                        ((jsexpr?) #'write-json)
                                        ((string?) #'write-string)
                                        ((bytes?)  #'write-bytes)
                                        (else (raise-syntax-error (syntax-e #'name) "called with unsupported data type" #'type)))
                                    json jq-in)
                                   (newline jq-in)
                                   (close-output-port jq-in)))]
                         [(reader)
                          ; Thread to read the formatted JSON from jq and send it to the output port.
                          ; If the output port is a type that subprocess can work with, it's used directly
                          ; and this isn't needed.
                          (if jq-out
                              (thread (thunk
                                       (copy-port jq-out out-port)
                                       (close-input-port jq-out)))
                              #f)])
             ; Wait for the threads to end and then for the jq subprocess to exit
             (thread-wait writer)
             (when reader (thread-wait reader))
             (subprocess-wait jq-process)))))))

(define-pretty-printer pretty-print-jsexpr jsexpr?)
(define-pretty-printer pretty-print-json string?)
(define-pretty-printer pretty-print-json/bytes bytes?)

