#lang racket

(define emit
  (λ (e)
    (if (list? e)
        (let ((tag (first e)))
          (cond
            ((eq? tag 'var) (second e))
            ((eq? tag 'abs) `(λ (,(second e)) ,(emit (third e))))
            ((eq? tag 'app) `(,(emit (second e)) ,(emit (third e))))
            ((eq? tag 'wcm) `(wcm ,(emit (second e)) ,(emit (third e))))
            ((eq? tag 'ccm) `(ccm))
            (else (error "unrecognized tag " (first e)))))
        (error "expected a list, got " e))))

(provide emit)
