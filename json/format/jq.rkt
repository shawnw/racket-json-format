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
  [format-json (-> string? string?)]
  [pretty-print-jsexpr (->* (jsexpr?) (output-port?) void?)]
  [pretty-print-json (->* (string?) (output-port?) void?)]))

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

; Reformat JSON text into nicely formatted JSON
(define (format-json json)
  (call-with-output-string (curry pretty-print-json json)))

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
                          (apply subprocess #f #f 'stdout (jq-path) (make-jq-args out-port))]
                         [(writer) ; One thread to write the JSON to jq and close the input pipe when done
                          (thread (thunk
                                   (#,(if (eq? (syntax-e #'type) 'jsexpr?) #'write-json #'write-string) json jq-in)
                                   (newline jq-in)
                                   (close-output-port jq-in)))]
                         [(reader) ; Thread to read the formatted JSON from jq and send it to the output port
                          (thread (thunk
                                   (copy-port jq-out out-port)
                                   (close-input-port jq-out)))])
             ; Wait for the threads to end and then for the jq subprocess to exit
             (thread-wait writer)
             (thread-wait reader)
             (subprocess-wait jq-process)))))))

(define-pretty-printer pretty-print-jsexpr jsexpr?)
(define-pretty-printer pretty-print-json string?)
