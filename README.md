json-format
===========

![Build Status](https://github.com/shawnw/racket-json-format/actions/workflows/ci.yml/badge.svg)

Modules for pretty-printing JSON values, for when the JSON module's
`jsexpr->string` isn't good enough - like for human consumption, etc.


Some usage examples:

    #lang racket/base

    (require json/format/config
             (prefix-in simple- json/format/simple)
             (prefix-in jq- json/format/jq))

    (define json #hasheq((zebra . 3.14) (emu . #t) (giraffe . #hasheq((z . "a") (b . "와플"))) (lion . (1 2 3))))

    (printf "Converting ~V:~%" json)
    (parameterize ([pretty-print-json-sort-keys #t]
                   [pretty-print-json-ascii-only #t])
      (displayln "Simple:")
      (simple-pretty-print-jsexpr json)
      (newline)
      (displayln "JQ:")
      (jq-pretty-print-jsexpr json)
      (displayln "Using jq to modify the object:")
      (simple-pretty-print-jsexpr (jq-jsexpr-transform json ".lion |= [ .[] | . + 1 ]"))
      (newline))
