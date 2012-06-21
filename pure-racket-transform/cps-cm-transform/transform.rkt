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
                      (cons ,FALSE (rst (var ,m))))))
                (cons ,FALSE (rst (var ,m))))))))

(define (transform-num num)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) ,num)))))

(define (transform-cons kons)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e))
        (f (gensym 'f)))
    `(abs ,k
          (abs ,m
               (app
                (app
                 ,(transform-inner (second kons))
                 (abs ,e
                      (app
                       (app
                        ,(transform-inner (third kons))
                        (abs ,f
                             (app
                              (var ,k)
                              (cons (var ,e) (var ,f)))))
                       (var ,m))))
                (cons ,FALSE (rst (var ,m))))))))

(define (transform-nil nil)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) ,nil)))))

(define (transform-fst fst)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e)))
    `(abs ,k
          (abs ,m
               (app
                (var ,k)
                (app
                 (app
                  ,(transform-inner (second fst))
                  (abs ,e
                       (app
                        (var ,k)
                        (fst (var ,e)))))
                 (cons ,FALSE (rst (var ,m)))))))))
                  
(define (transform-rst rst)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e)))
    `(abs ,k
          (abs ,m
               (app
                (var ,k)
                (app
                 (app
                  ,(transform-inner (second rst))
                  (abs ,e
                       (app
                        (var ,k)
                        (rst (var ,e)))))
                 (cons ,FALSE (rst (var ,m)))))))))

(define (transform-isnil? isnil?)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e)))
    `(abs ,k
          (abs ,m
               (app
                (var ,k)
                (app
                 (app
                  ,(transform-inner (second isnil?))
                  (abs ,e
                       (app
                        (var ,k)
                        (isnil? (var ,e)))))
                 (cons ,FALSE (rst (var ,m)))))))))

(define (transform-wcm wcm)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e)))
    `(abs ,k
          (abs ,m
               (app
                (app
                 ,(transform-inner (second wcm))
                 (abs ,e
                      (app
                       (app
                        ,(transform-inner (third wcm))
                        (var ,k))
                       (cons
                          ,TRUE
                          (cons
                           (var ,e)
                           (rst
                            (app
                             (app
                              (app
                               ,IF
                               (fst (var ,m)))
                              (rst (var ,m)))
                             (var ,m))))))))
                 (cons ,FALSE (rst (var ,m))))))))

(define (transform-ccm ccm)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k 
          (abs ,m 
               (app
                (var ,k)
                (rst (var ,m)))))))

; takes cm terms to lc terms
(define (transform-inner term)
  (if (list? term)
      (let ((tag (first term)))
        (cond
          ((eq? tag 'var) (transform-var term))
          ((eq? tag 'abs) (transform-abs term))
          ((eq? tag 'app) (transform-app term))
          ((eq? tag 'num) (transform-num term))
          ((eq? tag 'cons) (transform-cons term))
          ((eq? tag 'nil) (transform-nil term))
          ((eq? tag 'fst) (transform-fst term))
          ((eq? tag 'rst) (transform-rst term))
          ((eq? tag 'isnil?) (transform-isnil? term))
          ((eq? tag 'app) (transform-app term))
          ((eq? tag 'wcm) (transform-wcm term))
          ((eq? tag 'ccm) (transform-ccm term))
          (else (error "unrecognized tag" tag))))
      (error "expected list, got" term)))

(define (transform term)
  (emit (transform-inner (parse term))));(emit `(app (app ,(transform-inner (parse term)) (abs x (var x))) (abs x (abs y (var y))))))

(provide transform)