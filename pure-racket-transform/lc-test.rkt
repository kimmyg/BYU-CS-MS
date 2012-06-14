;#lang racket
;(require racket/include)

;(include "lc.rkt")

(define random-lc-var
  (λ ()
    (let ((i (random 3)))
      (cond
        ((= i 0) 'x)
        ((= i 1) 'y)
        (else    'z)))))

(define random-lc-abs
  (λ ()
    `(λ (,(random-lc-var)) ,(random-lc-term))))

(define random-lc-app
  (λ ()
    `(,(random-lc-term) ,(random-lc-term))))

(define random-lc-term
  (λ ()
    (let ((i (random 3)))
      (cond
        ((= i 0) (random-lc-var))
        ((= i 1) (random-lc-abs))
        (else    (random-lc-app))))))

(define test-lc-emit-parse
  (λ (n)
    (if (= n 0)
        (print "done")
        (let ((term (random-lc-term)))
          (if (equal? (lc-emit (lc-parse term)) term)
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