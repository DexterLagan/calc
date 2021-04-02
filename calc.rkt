#lang racket/base
(require racket/port)
(require racket/string)
(require racket/format)

;;; purpose

; An interactive calculator with support for variables.

;;; consts

(define *version* "1.0")

;;; defs

;; better displayln/string-append
(define echo
  (λ args
    (displayln (apply ~a args))))

;; generic about message
(define (about)
  (echo "Calc v" *version* " - interactive calculator with support for variables\n"
        "Type 'exit' or press ctrl-c to quit"))

;; define a global namespace to allow input-loop eval to understand our commands
(define-namespace-anchor a)
(define input-loop-ns (namespace-anchor->namespace a))

;; double string-replace
(define (string-replace2 s p1 p2 r1 r2)
  (string-replace (string-replace s p1 r1) p2 r2))

;; clean up entry from line-feeds and carriage returns
(define (clean-up command)
  (string-trim (string-replace2 command "\n" "\r" "" "")))

;; display a nice prompt with the current directory
(define (display-prompt)
  (display "> "))

;; import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))

  (upper-letter (:/ #\A #\Z))

  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))

(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define parser-errors
  (hash
   '((0 . #f))
   "missing lhs of eq"
   '((18 . #f) (6 . x) (0 . #f))
   "missing rhs of eq"
   '((12 . #f) (3 . 1) (0 . #f))
   "missing rhs of plus"
   '((3 . 1) (0 . #f))
   "missing left parenthesis"
   '((20 . 1) (8 . #f) (0 . #f))
   "missing right parenthesis"))

(define calcp
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error
    (lambda (tok-ok? tok-name tok-value #:stack se)
      (define pe
        (hash-ref parser-errors se #f))
      (if pe (error 'calc pe)
          (error
           'calc
           "Unexpected token: ~a~a\nparser-state = ~v"
           tok-name
           (if tok-value
               (format "(~a)" tok-value)
               "")
           se))))

   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))

   (grammar

    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])

    (exp [(NUM) $1]
         [(VAR) (hash-ref vars $1 (lambda () 0))]
         [(VAR = exp) (begin (hash-set! vars $1 $3)
                             $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (- $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))

;; run the calculator on the given input-port
(define (calc ip)
  (port-count-lines! ip)
  (let one-line ()
    (define result
      (calcp (lambda () (calcl ip))))
    (when result
      (printf "~a\n" result)
      (one-line))))

(module+ test
  (require rackunit
           racket/port)
  (define (run s)
    (with-output-to-string
      (λ ()
        (calc (open-input-string s)))))
  (define (ok s o)
    (check-equal? (run s) o))
  (define (no s xm)
    (with-handlers
        ([exn:fail?
          (λ (x)
            (check-regexp-match
             xm (exn-message x)))])
      (define o (run s))
      (check-true
       #f
       (format "expected error, got: ~v"
               o))))

  (ok "x=1\n(x + 2 * 3) - (1+2)*3"
      "1\n-2\n")
  (no "(x"
      "right parenthesis")
  (no "x)"
      "left parenthesis")
  (no "x+"
      "missing rhs of plus")
  (no "x="
      "missing rhs of eq")
  (no "=1"
      "missing lhs of eq"))

;; evaluates a string with the calculator
(define (run s)
  (display
   (with-output-to-string
     (λ ()
       (with-handlers
           ([exn:fail?
             (λ (x)
               (displayln (exn-message x)))])
         (calc (open-input-string s)))))))

;; input loop
(define (input-loop)
  (let/ec break
    (let loop ()
      ; display command prompt
      (display-prompt)                                                                         
      ; get input from user; trim and remove annoying enters and returns
      (define command
        (clean-up (read-line)))
      (cond [(string=? command "") (loop)]
            [(string-prefix? command "exit") (break)]
            [else (run command)])
      (loop))))

;;; main

(module+ main
  (about)
  (newline)
  (input-loop))


; EOF