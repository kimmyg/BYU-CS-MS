#lang racket

(require "cm-parse.rkt")

(define (calculate-index var nest depth)
  (if (empty? nest)
      depth
      (if (eq? var (first nest))
          depth
          (calculate-index var (rest nest) (+ depth 1)))))

(define (term->static-distance e)
  (letrec ((term->static-distance-inner (λ (e nest)
                                          (let ((tag (first e)))
                                            (cond
                                              ((eq? tag 'var) `(var ,(calculate-index (second e) nest 0)))
                                              ((eq? tag 'abs) `(abs x ,(term->static-distance-inner (third e) (cons (second e) nest))))
                                              ((eq? tag 'app) `(app ,(term->static-distance-inner (second e) (cons #f nest)) ,(term->static-distance-inner (third e) (cons #f nest))))
                                              ((eq? tag 'num) `(num ,(second e)))
                                              (else (error "unrecognized tag" tag)))))))
    (term->static-distance-inner e (list))))

(define (alpha-eq? e f)
  (let ((e (parse e))
        (f (parse f)))
    (equal? (term->static-distance e) (term->static-distance f))))

(provide alpha-eq?)


