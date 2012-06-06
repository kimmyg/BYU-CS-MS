#lang racket

;require lc.rkt

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

(define test-random-lc-term
  (λ (n)
    (if (= n 0)
        (print "done")
        (let ((term (random-lc-term)))
          (if (equal? (lc-emit (lc-parse term)) term)
              (test-random-lc-term (- n 1))
              (error "expected " term ", got" (lc-emit (lc-parse term))))))))

(test-random-lc-term 2000)
