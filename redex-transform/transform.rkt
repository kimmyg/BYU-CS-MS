#lang racket
;; direct style
(define (c e)
    (let ([flag (gensym 'f)]
          [marks (gensym 'm)]
          [a (gensym 'a)]
          [b (gensym 'b)])
      (match e
        [(list 'ccm)
         `(lambda (,flag)
            (lambda (,marks)
              ,marks))]
        [(list 'wcm mark-expr body-expr)
         `(lambda (flag)
            (lambda (marks)
              ((,(c body-expr) (lambda (x) (lambda (y) x)))
               (((lambda (,mark-value) (lambda (,rest-marks) ,(c-hat `(lambda (z) ,(c `((z ,mark-value) ,rest-marks))))))
                 ((,(c mark-expr) (lambda (x) (lambda (y) y))) ,marks))
                ((flag ,(c-hat `((lambda (p) (p (lambda (x) (lambda (y) y)))) ,marks))) ,marks)))))]
        [(list 'lambda (list x0) e0)
         `(lambda (,flag)
            (lambda (,marks)
              (lambda (,x0)
                ,(c e0))))]
        [(list rator-expr rand-expr)
         `(lambda (,flag)
            (lambda (,marks)
              (((((,(c rator-expr) (lambda (x) (lambda (y) y))) ,marks)
                 ((,(c rand-expr) (lambda (x) (lambda (y) y))) ,marks))
                ,flag)
               ,marks)))]
        ['error
         'error]
        [x0
         `(lambda (flag) (lambda (marks) ,x0))])))

(define (c-hat e)
    (let ([f (gensym 'f)]
          [m (gensym 'm)])
      `((,(c e) (lambda (x) (lambda (y) y))) (lambda (x) ,(c '(lambda (y) y))))))

;; continuation-passing style
(define (c-cps e)
  (let ([kont (gensym 'k)]
        [flag (gensym 'f)]
        [marks (gensym 'm)]
        [rator-value (gensym 't)]
        [rand-value (gensym 'n)]
        [rest-marks (gensym 'r)]
        [a (gensym 'a)]
        [b (gensym 'b)]
        [f (gensym 'f)])
    (match e
      [(list 'ccm)
       `(lambda (,kont)
          (lambda (,flag)
            (lambda (,marks)
              (,kont ,marks))))]
      [(list 'wcm mark-expr body-expr)
       `(lambda (,kont)
          (lambda (,flag)
            (lambda (,marks)
              (((,(c-cps mark-expr)
                 (lambda (,mark-value)
                   ((lambda (,rest-marks) 
                      (((,(c-cps body-expr)
                         ,kont)
                        (lambda (x) (lambda (y) x)))
                       ,(c-hat `(lambda (z) ((z ,mark-value) ,rest-marks)))))
                    ((flag ,(c-hat `((lambda (p) (p (lambda (x) (lambda (y) y)))) ,marks))) ,marks))))
                (lambda (x) (lambda (y) y)))
               ,marks))))]
      [(list 'lambda (list x0) e0)
       `(lambda (,kont)
          (lambda (,flag)
            (lambda (,marks)
              (kont (lambda (,x0) ,(c-cps e0))))))]
      [(list rator-expr rand-expr)
       `(lambda (,kont)
          (lambda (,flag)
            (lambda (,marks)
              (((,(c-cps rator-expr)
                 (lambda (,rator-value)
                   (((,(c-cps rand-expr)
                      (lambda (,rand-value)
                        ((((,rator-value ,rand-value) ,kont) ,flag) ,marks)))
                     (lambda (x) (lambda (y) y)))
                    ,marks)))
                (lambda (x) (lambda (y) y)))
               ,marks))))]
      ['error
       'error]
      [x0
       `(lambda (,kont)
          (lambda (,flag)
            (lambda (,marks)
              (,kont ,x0))))])))

(define (c-hat-cps e)
  (let ([k (gensym 'k)]
        [f (gensym 'f)]
        [m (gensym 'm)])
    `(((,(c-cps e) (lambda (x) x)) (lambda (x) (lambda (y) y))) (lambda (x) ,(c-cps '(lambda (y) y))))))

(provide c-hat
         c-hat-cps)