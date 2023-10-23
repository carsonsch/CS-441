#lang racket
(require data/either)
(require data/monad)

(define program "")
(define program-pos 0)

; Returns a string "Accept" if the program is syntactically valid, other wise an error string
; explaining which line the error is on.
(define (parse path) 
    (set! program (file->string path))
    (set! program-pos 0)
        (from-failure "Accept" (parse-program))
)

; Note that `program-ends-correctly` ensures no text comes after the '$$' end of program value.
(define (parse-program)
    (displayln "parse-program")

    (do
        [parse-linelist]
        [match "\\$\\$"]
        [program-ends-correctly]
    )
)

; A linelist is a repetition is a repetition of zero or more lines.
; To detect if the next token is part of a line would be very long winded
; because we'd have to match a label, or any of the tokens starting a statement.
; It's more efficient to match against '$$' or 'endwhile', which are the only
; tokens that can end a linelist, and assume otherwise there must be another line.
(define (parse-linelist)
    (displayln "parse-linelist")

    (cond
        [(does-match? "\\$\\$") (success #t)]
        [(does-match? "endwhile[\r\t\n ]*;") (success #t)]
        [else (do
            [parse-line]
            [parse-linelist]
        )]
    )
)

(define (parse-line)
    (displayln "parse-line")

        (do
        [parse-label]
        [parse-stmt]
        [parse-linetail]
    )
)

; Parses for a label, and ensures that it isn't a reserved keyword.
(define (parse-label)
    (displayln "parse-label")
    (define label-regexp "[A-Za-z][A-Za-z0-9]*:")
    (define disallowed-labels '("if" "while" "read" "write" "goto" "gosub" "return" "break" "end"))
    ; Try to match, but ignore if it fails
    (cond
        [(does-match? label-regexp)
            (define label-lst (regexp-match "^[\n\r\t ]*([A-Za-z][A-Za-z0-9]*):" program program-pos))
            (define label (string-downcase (second label-lst))) ; Lowercase the label, we want to match even if the case is off
            (displayln (~a "This is the label: " label))
            (match label-regexp)
            (if (eqv? (member label disallowed-labels) #f)
                (success #t) ; Label doesn't match reserved keywords, we're good
                (syntax-error-failure (~a "Label '" label "' is a reserved keyword")) ; Matched reserved keyword
            )
        ]
        [else (success #t)] ; There isn't label here, match the epsilon production
    )
)

; A linetail is a repetition of zero or more statements.
; In order to tell if a statement follows, we have the stmt-tokens list
; containing the first token of every statement type
(define (parse-linetail)
    (displayln "parse-linetail")
    (define stmt-tokens '("if[\r\t\n ]*\\("
                          "while[\r\t\n ]*\\("
                          "read "
                          "write "
                          "goto "
                          "gosub "
                          "return[\r\t\n ]*;"
                          "break[\r\t\n ]*;"
                          "end[\r\t\n ]*;"
                          "[A-Za-z][A-Za-z0-9]*[\r\t\n ]*="))

    (cond
        [(ormap does-match? stmt-tokens) ; If true, we have another statement coming up
            (do
                [parse-stmt]
                [parse-linetail]
            )
        ]
        [else (success #t)] ; No more statements, apply epsilon production
    )
)

; Has to go through each possible statement type, and determine which function
; to call base off of the first token in the statement.
(define (parse-stmt)
    (displayln "parse-stmt")
    (cond
        [(does-match? "if") (parse-if)]
        [(does-match? "while") (parse-while)]
        [(does-match? "read") (parse-read)]
        [(does-match? "write") (parse-write)]
        [(does-match? "goto") (parse-goto)]
        [(does-match? "gosub") (parse-gosub)]
        [(does-match? "return[\r\t\n ]*;") (parse-return)]
        [(does-match? "break[\r\t\n ]*;") (parse-break)]
        [(does-match? "end[\r\t\n ]*;") (parse-end)]
        [(does-match? "[A-Za-z][A-Za-z0-9]*") (parse-assignment)]
        [else (syntax-error-failure "Malformed statement")] ; If we got here, this can't possibly be a statement
    )
)

(define (parse-assignment)
    (displayln "parse-assignment")
    (do
        [parse-id]
        [match "\\="]
        [parse-expr]
        [match ";"]
    )
)

(define (parse-if)
    (displayln "parse-if")

    (do
        [match "if"]
        [match "\\("]
        [parse-boolean]
        [match "\\)"]
        [parse-stmt]
        ; According to the grammar, this should be here
        ; but the sample files don't support this.
        [match ";"]
    )
)

(define (parse-while)
    (displayln "parse-while")

    (do
        [match "while"]
        [match "\\("]
        [parse-boolean]
        [match "\\)"]
        [parse-linelist]
        [match "endwhile"]
        [match ";"]
    )
)

(define (parse-read)
    (displayln "parse-read")

    (do
        [match "read"]
        [parse-id]
        [match ";"]
    )
)

(define (parse-write)
    (displayln "parse-write")

    (do
        [match "write"]
        [parse-id]
        [match ";"]
    )
)

(define (parse-goto)
    (displayln "parse-goto")

    (do
        [match "goto"]
        [parse-id]
        [match ";"]
    )
)

(define (parse-gosub)
    (displayln "parse-gosub")

    (do
        [match "gosub"]
        [parse-id]
        [match ";"]
    )
)

(define (parse-return)
    (displayln "parse-return")

    (do
        [match "return"]
        [match ";"]
    )
)

(define (parse-break)
    (displayln "parse-break")

    (do
        [match "break"]
        [match ";"]
    )
)

(define (parse-end)
    (displayln "parse-end")

    (do
        [match "end"]
        [match ";"]
    )
)

(define (parse-boolean)
    (displayln "parse-boolean")

    (cond
        [(does-match? "true") (match "true")]
        [(does-match? "false") (match "false")]
        [else (do
            [parse-expr]
            [parse-bool-op]
            [parse-expr]
        )]
    )
)

; Decides how to parse an expression, and which function should handle the case
(define (parse-expr)
    (displayln "parse-expr")

    (cond
        [(does-match? "\\(") (parse-expr-parenthesis)]
        [(does-match? "[A-Za-z]") (parse-expr-id)]
        [else (parse-expr-num)]
    )
)

; Parses an expression wrapper in parenthesis
(define (parse-expr-parenthesis)
    (displayln "parse-expr-parenthesis")

    (do 
        [match "\\("]
        [parse-expr]
        [match "\\)"]
    )
)

(define (parse-expr-id)
    (displayln "parse-expr-id")

    (do
        [parse-id]
        [parse-etail]
    )
)

(define (parse-expr-num)
    (displayln "parse-expr-num")

    (do
        [parse-num]
        [parse-etail]
    )
)

; Optionally matches a single operator
(define (parse-etail)
    (displayln "parse-etail")

    (define operators '(
        "\\+"
        "\\*"
        "\\/"
        "\\-"
    ))

    (if (ormap successful-match? operators)
        (parse-expr)
        (success #t)
    )
)

; Parses exactly one operator
(define (parse-bool-op)
    (displayln "parse-bool-op")

    (define operators '("<" ">" "<=" ">=" "<>" "="))
    
    (if (ormap successful-match? operators)
        (success #t)
        (syntax-error-failure "Invalid bool-op")
    )
)

(define (parse-id)
    (displayln "parse-id")

    (match "[A-Za-z][A-Za-z0-9]*")
)

(define (parse-num)
    (displayln "parse-num")

    (do
        [parse-numsign]
        [parse-digit-sequence]
    )
)

; Parses one or more digit
(define (parse-digit-sequence)
    (displayln "parse-digit-sequence")
    (match "[0-9]+")
)

; Matches + or - or epsilon
(define (parse-numsign)
    (displayln "parse-numsign")

    (define operators '("\\+" "\\-"))
    (ormap successful-match? operators)

    ; We don't care if they match, return success anyway
    (success #t)
)

; Matches the next token to the given regexp, and progresses the program-pos forward
(define (match regexp)
    (define whitespace-regexp (~a "^[\n\t\r ]*" regexp))
    (define matches (regexp-match-positions whitespace-regexp program program-pos))

    (displayln (~a "matching regex: '" regexp "'"))
    (displayln (~a "matching pos: " (substring program program-pos)))
    (displayln "")

    (cond
        [(eqv? matches #f)
            (displayln "match failed.")
            (syntax-error-failure)
        ]
        [else
            ; progress the program-pos so we can keep track of where in the program string we are
            (set! program-pos (cdr (first matches)))
            (success #t)
        ]        
    )
)

; Checks if the upcoming tokens match the given regexp
(define (does-match? regexp)
    (displayln (~a "Checking if regex matches: " regexp))
    (define whitespace-regexp (~a "^[\n\t\r ]*" regexp))
    (define matches (regexp-match-positions whitespace-regexp program program-pos))

    (pair? matches) ; a pair is returned only if there was a match of the regexp
)

(define (successful-match? regexp)
    (success? (match regexp))
)

; Generates a `failure` with an error message showing the current line number.
(define (syntax-error-failure [reason ""])
    (define prog-chars-so-far (take (string->list program) program-pos))
    (define current-line-no (length (indexes-of prog-chars-so-far #\newline)))

    (define line (list-ref (string-split program "\n") current-line-no))
    (displayln "")
    (displayln (~a "Offending line: " line))
    (failure (~a "Syntax error found on line " (add1 current-line-no) ". " reason))
)

; Validates that a program has no contents after the '$$' end of program marker.
(define (program-ends-correctly)
    (match "") ; Matches any remaining whitespace
    (if (< program-pos (string-length program))
        (syntax-error-failure "Program must not have any text after $$.")
        (success #t)
    )
)
