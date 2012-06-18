#lang racket

(define lc-emit
  (λ (e)
    (if (list? e)
        (cond
          ((eq? (first e) 'var) (second e))
          ((eq? (first e) 'abs) `(λ (,(second e)) ,(lc-emit (third e))))
          ((eq? (first e) 'app) `(,(lc-emit (second e)) ,(lc-emit (third e))))
          (else (error "unrecognized tag " (first e))))
        (error "expected a list, got " e))))

(provide lc-emit)
