#lang scribble/manual
@require[@for-label[json/format/jq
                    racket/base json]]

@title{Pretty Printing JSON}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

Want JSON that's easier for humans to read instead of the compact form produced by @code{write-json} and @code{jsexpr->string}?
Look no further!

@local-table-of-contents[#:style 'immediate-only]

@section{Available formatters}

@subsection{Simple formatting}

Pure racket, very similar to JQ output (Except for adding a space before colons in objects).

@defmodule[json/format/simple]

@defproc[(jsexpr->pretty-json [js jsexpr?]) string?]{

 Returns the given jsexpr as a pretty-printed string.

}

@defproc[(jsexpr->pretty-json/bytes [js jsexpr?]) bytes?]{

 Returns the given jsexpr as a pretty-printed byte string.

}

@defproc[(format-json [json string?]) string?]{

 Formats the given JSON value and returns it.

}

@defproc[(format-json/bytes [json bytes?]) bytes?]{

 Formats the given JSON value and returns it.

}

@defproc[(pretty-print-jsexpr [js jsexpr?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted jsexpr as JSON to the given port.

}

@defproc[(pretty-print-json [json string?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@defproc[(pretty-print-json/bytes [json bytes?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@subsection{Smart formatting}

Tries to use less vertical space by printing small arrays and objects on a single line. Experimental and not very well tested.

@defmodule[json/format/smart]

@defparam[pretty-print-json-line-width width exact-positive-integer? #:value 80]{

 The number of columns to use when deciding if an aggregate value can be printed on one line.

}

@defparam[pretty-print-json-tab-width width exact-positive-integer? #:value 8]{

 How many columns a tab takes up when calculating width.

}

@defproc[(jsexpr->pretty-json [js jsexpr?]) string?]{

 Returns the given jsexpr as a pretty-printed string.

}

@defproc[(jsexpr->pretty-json/bytes [js jsexpr?]) bytes?]{

 Returns the given jsexpr as a pretty-printed bytestring.

}

@defproc[(format-json [json string?]) string?]{

 Formats the given JSON value and returns it.

}

@defproc[(format-json/bytes [json bytes?]) bytes?]{

 Formats the given JSON value and returns it.

}

@defproc[(pretty-print-jsexpr [js jsexpr?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted jsexpr as JSON to the given port.

}

@defproc[(pretty-print-json [json string?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@defproc[(pretty-print-json/bytes [json bytes?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@subsection{JQ-powered formatting}

Uses an external @hyperlink["https://stedolan.github.io/jq/"]{jq} process to format JSON. Also allows you to run arbitrary jsexprs through JQ.

@defmodule[json/format/jq]

@defparam[jq-path path (or/c path-string? #f) #:value (find-executable-path "jq")]{

The path to the jq executable to use.

}

@defproc[(jsexpr-transform [js jsexpr?] [filter string?]) jsexpr?]{

 Return the result of running the JSON represented by @code{js} through the given jq filter program.

 Technically not formatting, but it uses the same framework, so why not include it?

}

@defproc[(jsexpr->pretty-json [js jsexpr?]) string?]{

 Returns the given jsexpr as a pretty-printed string.

}

@defproc[(jsexpr->pretty-json/bytes [js jsexpr?]) bytes?]{

 Returns the given jsexpr as a pretty-printed bytestring.

}

@defproc[(format-json [json string?]) string?]{

 Formats the given JSON value and returns it.

}

@defproc[(format-json/bytes [json bytes?]) bytes?]{

 Formats the given JSON value and returns it.

}

@defproc[(pretty-print-jsexpr [js jsexpr?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted jsexpr as JSON to the given port.

}

@defproc[(pretty-print-json [json string?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@defproc[(pretty-print-json/bytes [json bytes?] [out output-port? (current-output-port)]) void?]{

 Writes out the formatted JSON value to the given port.

}

@section{Controlling formatting style}

All of the above formatter modules use these parameters unless otherwise noted.

@defmodule[json/format/config]

@defboolparam[pretty-print-json-ascii-only ascii #:value #f]{

 If true, any non-ASCII codepoints in strings are escaped as @tt{\uXXXX} sequences.
                                                             
}

@defboolparam[pretty-print-json-sort-keys sort-keys #:value #f]{

 If true, objects are printed with keys in sorted order.

}

@defparam[pretty-print-json-indent width (or/c exact-nonnegative-integer? 'tabs) #:value 2]{

 Controls how many spaces to insert for each level of indentation (Or one tab per level if @code{'tabs}).

}

@defparam[pretty-print-json-colorize when (or/c boolean? 'terminal) #:value 'terminal]{

 Controls when to colorize the output - when the output port is a terminal, always if otherwise true, or never if false.

 Colors are customized as in the @hyperlink["https://stedolan.github.io/jq/manual/#Colors"]{same manner as jq}.

}

