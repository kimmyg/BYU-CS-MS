#lang racket

(require "cm-parse.rkt"
         "lc-emit.rkt")

(define (calculate-index var nest depth)
  (if (empty? nest)
      depth
      (if (eq? var (first nest))
          depth
          (calculate-index var (rest nest) (+ depth 1)))))

(define (term->static-distance e)
  (let ((e (parse e)))
    (letrec ((term->static-distance-inner (λ (e nest)
                                            (let ((tag (first e)))
                                              (cond
                                                ((eq? tag 'var) `(var ,(calculate-index (second e) nest 0)))
                                                ((eq? tag 'abs) `(abs x ,(term->static-distance-inner (third e) (cons (second e) nest))))
                                                ((eq? tag 'app) `(app ,(term->static-distance-inner (second e) (cons #f nest)) ,(term->static-distance-inner (third e) (cons #f nest))))
                                                ((eq? tag 'num) e)
                                                (else (error "unrecognized tag" tag)))))))
      (emit (term->static-distance-inner e (list))))))

(define (term->first-alpha e)
  (let ((e (parse e)))
    (letrec ((term->first-alpha-inner (λ (e)
                                        (let ((tag (first e)))
                                          (cond
                                            ((eq? tag 'var) `(var ,(string->symbol (first (regexp-match #px"\\w" (symbol->string (second e)))))))
                                            ((eq? tag 'abs) `(abs ,(string->symbol (first (regexp-match #px"\\w" (symbol->string (second e))))) ,(term->first-alpha-inner (third e))))
                                            ((eq? tag 'app) `(app ,(term->first-alpha-inner (second e)) ,(term->first-alpha-inner (third e))))
                                            ((eq? tag 'num) e)
                                            (else (error "unrecognized tag" tag)))))))
      (emit (term->first-alpha-inner e)))))

(define (alpha-eq? e f)
  (equal? (term->static-distance e) (term->static-distance f)))

(provide term->static-distance
         term->first-alpha
         alpha-eq?)
