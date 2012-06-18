#lang racket

(define cm-emit
  (λ (e)
    (if (list? e)
        (let ((tag (first e)))
          (cond
            ((eq? tag 'var) (second e))
            ((eq? tag 'abs) `(λ (,(second e)) ,(cm-emit (third e))))
            ((eq? tag 'app) `(,(cm-emit (second e)) ,(cm-emit (third e))))
            ((eq? tag 'wcm) `(wcm ,(cm-emit (second e)) ,(cm-emit (third e))))
            ((eq? tag 'ccm) `(ccm))
            (else (error "cm-emit unrecognized tag " (first e)))))
        (error "expected a list, got " e))))

(provide cm-emit)
