#lang racket

(define (lc-length e)
  (match e
    [(list 'λ (list x1) e1)
     (+ 1 (lc-length e1))]
    [(list e1 e2)
     (+ (lc-length e1) (lc-length e2))]
    [x1
     1]))

(define (lc->tex e [i 0])
  (match e
    [(list 'λ (list x1) e1)
     (begin
       (printf "\\lambda ~a." x1)
       (lc->tex e1 i))]
    [(list e1 e2)
     (if (and (> (lc-length e1) 3) (> (lc-length e2) 3))
         (begin
           (printf "(&")
           (lc->tex e1 (+ i 1))
           (printf "\\\\")
           (newline)
           (for ([j (+ i 1)])
             (printf "&"))
           (lc->tex e2 (+ i 1))
           (printf ")"))
         (begin
           (printf "(")
           (lc->tex e1 (+ i 1))
           (printf "\\,")
           (lc->tex e2 (+ i 1))
           (printf ")")))]
    [x1
     (printf "~a" x1)]))

(provide lc->tex)
     