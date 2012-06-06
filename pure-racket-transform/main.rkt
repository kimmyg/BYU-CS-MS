#lang racket

; cm-program->cm-parsed-program->(cm-parsed-value->cm-value,lc-parsed-program->(lc-program,lc-parsed-value->lc-value))

(define cm-program '(ccm))
(define cm-parsed-program (cm-parse cm-program))
(define cm-parsed-value (cm-eval cm-parsed-program '()))
(define cm-value (cm-emit cm-parsed-value))
(define lc-parsed-program (cm-transform cm-parsed-program))
(define lc-program (lc-emit lc-parsed-program))
(define lc-parsed-value (lc-eval lc-parsed-program))
(define lc-value (lc-emit lc-parsed-value))

(display "cm program")
(newline)
(display cm-program)
(newline)

(display "cm value")
(newline)
(display cm-value)
(newline)

(display "lc program")
(newline)
(display lc-program)
(newline)

(display "lc value")
(newline)
(display lc-value)
(newline)
