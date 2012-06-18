#lang racket

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

(provide random-lc-term)
