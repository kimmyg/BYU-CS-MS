#lang racket

(define (calculate-index var nest depth)
  (if (empty? nest)
      depth
      (if (eq? var (first nest))
          depth
          (calculate-index var (rest nest) (+ depth 1)))))

(define (term->static-distance e [nest (list)])
  (match e
    [(list λ (list x1) e1)
     `(λ ,(term->static-distance e1 (cons x1 nest)))]
    [(list e1 e2)
     `(,(term->static-distance e1 (cons #f nest)) ,(term->static-distance e2 (cons #f nest)))]
    [(? number? n1)
     n1]
    [x1
     (calculate-index x1 nest 0)]))

(define (term->first-alpha e)
  e)

(define (alpha-eq? e f)
  (equal? (term->static-distance e) (term->static-distance f)))

(provide term->static-distance
         term->first-alpha
         alpha-eq?)
