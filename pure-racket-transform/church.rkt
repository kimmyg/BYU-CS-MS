#lang racket

(define TRUE
  '(λ (x)
    (λ (y)
      x)))

(define FALSE
  '(λ (x)
    (λ (y)
      y)))

(define IF
  '(λ (b)
    (λ (m)
      (λ (n)
        ((b m) n)))))

(define ZERO
  '(λ (f)
    (λ (x) x)))

(define ISZERO
  `(λ (n)
    ((n (λ (x) ,FALSE)) ,TRUE)))

(define SUCC
  '(λ (n)
    (λ (f)
      (λ (x)
        (f ((n f) x))))))

(define PRED
  '(λ (n)
    (λ (f)
      (λ (x)
        (((n (λ (g) (λ (h) (h (g f))))) (λ (u) x)) (λ (u) u))))))

(define ADD
  '(λ (m)
    (λ (n)
      (λ (f)
        (λ (x)
          (m f ((n f) x)))))))

(define TIMES
  '(λ (m)
    (λ (n)
      (λ (f)
        (m (n f))))))

(define PAIR
  '(λ (x)
    (λ (y)
      (λ (z)
        ((z x) y)))))

(define FST
  `(λ (p)
    (p ,TRUE)))

(define SND
  `(λ (p)
    (p ,FALSE)))

(define NIL `((,PAIR ,TRUE) ,TRUE))

(define ISNIL FST)

(define CONS
  `(λ (h)
    (λ (t)
      ((,PAIR ,FALSE) ((,PAIR h) t)))))

(define HEAD
  `(λ (z)
    (,FST (,SND z))))

(define TAIL
  `(λ (z)
    (,SND (,SND z))))
