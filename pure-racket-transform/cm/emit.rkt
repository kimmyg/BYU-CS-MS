#lang racket

(define (emit e)
  (if (list? e)
      (let ((tag (first e)))
        (cond
          ((eq? tag 'var) (second e))
          ((eq? tag 'abs) `(λ (,(second e)) ,(emit (third e))))
          ((eq? tag 'app) `(,(emit (second e)) ,(emit (third e))))
          ((eq? tag 'num) (second e))
          ((eq? tag 'cons) `(cons ,(emit (second e)) ,(emit (third e))))
          ((eq? tag 'nil) 'nil)
          ((eq? tag 'fst) `(fst ,(emit (second e))))
          ((eq? tag 'rst) `(rst ,(emit (second e))))
          ((eq? tag 'isnil?) `(isnil? ,(emit (second e))))
          ((eq? tag 'wcm) `(wcm ,(emit (second e)) ,(emit (third e))))
          ((eq? tag 'ccm) `(ccm))
          (else (error "unrecognized tag " (first e)))))
      (error "expected a list, got " e)))

(provide emit)
