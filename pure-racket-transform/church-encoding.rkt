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

(define T
  '(λ (f)
    ((λ (x) (f (x x)))
     (λ (x) (f (λ (y) ((x x) y)))))))

(define FACT
  `(,T (λ (f)
       (λ (n)
         ((((,IF (,ISZERO n)) (λ () (,SUCC ,ZERO))) (λ () (wcm n ((,TIMES n) (f (,PRED n)))))))))))

(define c-to-b
  (λ (b)
    ((b true) false)))

(define c-to-n
  (λ (n)
    ((n add1) 0)))

(define c-to-l
  (λ (l)
    (if (c-to-b (ISNIL l))
        '()
        (cons (HEAD l) (c-to-l (TAIL l))))))

(define cm-transform-var
  (λ (x)
    `(λ (y)
       ,x)))

(define cm-transform-num
  (λ (x)
    `(λ (y)
       ,x)))

(define cm-transform-abs
  (λ (abs)
    `(λ ,(second abs)
       (λ (y)
         (,(cm-transform (third abs)) (,CONS ,FALSE (,SND y)))))))

(define cm-transform-app
  (λ (app)
    `(λ (y)
       ((,(cm-transform (first app)) (,(cm-transform (second app)) (,CONS ,FALSE (,SND y)))) (,CONS ,FALSE (,SND y))))))

(define cm-transform-wcm
  (λ (wcm)
    `(λ (y) (,(cm-transform (third wcm)) (,CONS ,TRUE (,CONS ,(second wcm) (,IF (,FST y)
                                                                         (,SND (,SND y))
                                                                         (,SND y))))))))

(define cm-transform-ccm
  (λ (ccm)
    `(λ (y)
       (,SND y))))

(define cm-transform
  (λ (term)
    (cond
      ((symbol? term) (cm-transform-var term))
      ((number? term) (cm-transform-num term))
      ((list? term) (if (symbol? (first term))
                        (cond
                          ((eq? (first term) 'λ) (cm-transform-abs term))
                          ((eq? (first term) 'wcm) (cm-transform-wcm term))
                          ((eq? (first term) 'ccm) (cm-transform-ccm term))
                          (else (first term)))
                          ;(else (error "unrecognized symbol" (first term))))
                        (cm-transform-app term)))
      (else (error "not a supported term:" term)))))

(define PROGRAM `(,FACT (,SUCC ,ZERO)))

;PROGRAM
(cm-transform PROGRAM)

;(parameterize ([current-namespace (make-base-namespace)])
;        ((eval (cm-transform program)) (cons #f '()))))))