#lang racket

(require "emit.rkt")
(require "eval.rkt")
(require "parse.rkt")

(define random-var
  (λ ()
    (let ((i (random 3)))
      (cond
        ((= i 0) 'x)
        ((= i 1) 'y)
        (else    'z)))))

(define random-abs
  (λ ()
    `(λ (,(random-var)) ,(random-term))))

(define random-app
  (λ ()
    `(,(random-term) ,(random-term))))

(define (random-num)
  (random 10))

(define (random-term)
  (let ((i (random 4)))
    (cond
      ((= i 0) (random-var))
      ((= i 1) (random-abs))
      ((= i 2) (random-app))
      (else    (random-num)))))

(define test-lc-emit-parse
  (λ (n)
    (if (= n 0)
        (print "done")
        (let ((term (random-lc-term)))
          (if (equal? (emit (lc-parse term)) term)
              (test-lc-emit-parse (- n 1))
              (error "expected " term ", got" (lc-emit (lc-parse term))))))))

;(test-lc-emit-parse 2000)

(define test-lc-eval
  (λ (n)
    (if (= n 0)
        (print "done")
        (let ((term (random-lc-term)))
          (begin
            ;(display term)
            ;(newline)
            ;(display (lc-emit (lc-eval (lc-parse term))))
            (lc-eval (lc-parse term))
            ;(newline)
            (test-lc-eval (- n 1)))))))

;(test-lc-eval 100)

;(define program '((λ (x) (λ (y) (x y))) (x y)))

;(display program)
;(newline)
;(display (lc-parse program))
;(newline)
;(display (lc-emit (lc-eval (lc-parse program))))
;(newline)