#lang racket

(require "../lc/emit.rkt")
(require "../cm/parse.rkt")

(define (transform-var var)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k 
          (abs ,m 
               (app 
                (var ,k) 
                ,var)))))

(define (transform-abs abs)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k 
          (abs ,m 
               (app 
                (var ,k) 
                (abs 
                 ,(second abs) 
                 ,(transform-inner (third abs))))))))

(define (transform-app app)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e))
        (f (gensym 'f)))
    `(abs ,k 
          (abs ,m 
               (app 
                (app 
                 ,(transform-inner (second app)) 
                 (abs ,e 
                     (app 
                      (app 
                       ,(transform-inner (third app)) 
                       (abs ,f 
                            (app 
                             (app
                              (app 
                               (var ,e) 
                               (var ,f)) 
                              (var ,k))
                             (var ,m))))
                      (var ,m))))
                (var ,m))))))

(define TRUE  '(abs x (abs y (var x))))
(define FALSE '(abs x (abs y (var y))))
(define IF    '(abs b (abs t (abs f (app (app (var b) (var t)) (var f))))))
(define PAIR  '(abs a (abs b (abs z (app (app (var z) (var a)) (var b))))))
(define FST   `(abs p (app (var p) ,TRUE)))
(define SND   `(abs p (app (var p) ,FALSE)))
(define CONS  PAIR)
(define HEAD  TRUE)
(define TAIL  FALSE)
(define NIL   FALSE)

(define (transform-wcm wcm)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k
          (abs ,m
               (app
                (app
                 ,(transform-inner (second wcm))
                 (abs n (app 
                         (app 
                          ,(transform-inner (third wcm)) 
                          (var ,k))
                         (app 
                          (app ,PAIR ,TRUE) 
                          (app 
                           (app 
                            ,CONS 
                            (var n)) 
                           (app 
                            (app 
                             (app 
                              ,IF 
                              (app ,FST (var ,m))) 
                             (app ,TAIL (app ,SND (var ,m)))) (app ,SND (var ,m))))))))
                (var ,m))))))
                                                                       

(define (transform-ccm ccm)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k 
          (abs ,m 
               (app 
                ,SND 
                (var ,m))))))

; takes cm terms to lc terms
(define (transform-inner term)
  (if (list? term)
      (let ((tag (first term)))
        (cond
          ((eq? tag 'var) (transform-var term))
          ((eq? tag 'abs) (transform-abs term))
          ((eq? tag 'app) (transform-app term))
          ((eq? tag 'wcm) (transform-wcm term))
          ((eq? tag 'ccm) (transform-ccm term))
          (else (error "unrecognized tag" tag))))
      (error "expected list, got" term)))

(define (transform term)
  (emit (transform-inner (parse term))))

(provide transform)