#lang info
(define collection 'multi)
(define deps '("base" ("racket" #:version "8.4") "unicode-breaks"))
(define build-deps '("scribble-lib" "racket-doc"))
;(define scribblings '(("json/format/scribblings/json-format.scrbl" ())))
(define pkg-desc "JSON pretty printers")
(define version "0.0")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
