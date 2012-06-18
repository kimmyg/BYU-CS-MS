#lang racket

(define emit
  (λ (e)
    (if (list? e)
        (cond
          ((eq? (first e) 'var) (second e))
          ((eq? (first e) 'abs) `(λ (,(second e)) ,(emit (third e))))
          ((eq? (first e) 'app) `(,(emit (second e)) ,(emit (third e))))
          (else (error "unrecognized tag " (first e))))
        (error "expected a list, got " e))))

(provide emit)
