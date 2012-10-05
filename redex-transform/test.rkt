#lang racket
(require redex)

(require racket/pretty
         "cm.rkt"
         "lc.rkt"
         "transform.rkt"
         "alpha.rkt")

(define (transform-test program)
  (let* ((value (first (apply-reduction-relation* λcm-rr program)))
         (value1 (first (apply-reduction-relation* λv-rr (transform-c value))))
         (value2 (first (apply-reduction-relation* λv-rr (transform-c program)))))
    (if (alpha-eq? value1 value2)
        (if #f
            (begin
              (display value)
              (newline)
              (display (term->first-alpha value1))
              (newline)
              (display (term->first-alpha value2))
              (newline)
              #t)
            #t)
        (begin
          (display value)
          (newline)
          (display (term->first-alpha value1))
          (newline)
          (display (term->first-alpha value2))
          (newline)
          #f))))

(define (trace program)
  (begin
    (traces λcm-rr program)
    (traces λv-rr (transform-c program))))

(define (random-cm-term l [bv (list)])
  (if (> l 0)
      (cond
        ((= l 1)
         (if (empty? bv)
             '(ccm)
             (let ((c (random 7)))
               (cond
                 ((< c 4)
                  (list-ref bv (random (length bv))))
                 ((< c 6)
                  (random 10))
                 (else
                  '(ccm))))))
        (else
         (let ((c (random 8)))
           (cond
             ((< c 4)
              (let ((x (gensym 'x)))
                `(λ (,x) ,(random-cm-term (- l 1) (cons x bv)))))
             ((< c 6)
              (let ((d (+ (random (- l 1)) 1)))
                `(,(random-cm-term d bv) ,(random-cm-term (- l d) bv))))
             (else
              (let ((d (+ (random (- l 1)) 1)))
                `(wcm ,(random-cm-term d bv) ,(random-cm-term (- l d) bv))))))))
      #f))

(define (cm-term-length t)
  (match t
    [(list 'ccm)
     1]
    [(list 'wcm e_1 e_2)
     (+ (cm-term-length e_1) (cm-term-length e_2))]
    [(list e_1 e_2)
     (+ (cm-term-length e_1) (cm-term-length e_2))]
    [(list 'λ (list x_1) e_1)
     (+ 1 (cm-term-length e_1))]
    [x
     1]))

(transform-test '1)
(transform-test '(λ (x) x))
(transform-test '(ccm))
(transform-test '((λ (x) x) (ccm)))
(transform-test '(wcm 0 (ccm)))
(transform-test '(wcm (ccm) (ccm)))
(transform-test '(wcm (wcm (ccm) (ccm)) (λ (x) x)))
(transform-test '(wcm (wcm (ccm) (ccm)) b))
;(transform-test '(wcm (λ (a) (λ (b) b)) (ccm)))
(transform-test '(wcm (λ (x) x) (ccm)))
(transform-test '(wcm (λ (x) (λ (y) x)) (ccm)))
(transform-test '(wcm 0 (wcm 1 (ccm))))
;(transform-test 'F)
(transform-test '(wcm (ccm) 3))
(transform-test '((ccm) 2))
(transform-test '(wcm ((ccm) 1) (ccm)))
(transform-test '(wcm 0 ((ccm) (ccm))))
(transform-test '(wcm 1 ((ccm) (λ (x) (λ (y) x)))))
;(transform-test '(a b))
(transform-test '(wcm (ccm) (ccm)))
(transform-test '((wcm (ccm) (ccm)) 1))
;(transform-test '(Y (wcm (ccm) (ccm))))
<<<<<<< HEAD
;(transform-test '(error (ccm)))
;(transform-test '(wcm 1 (wcm (ccm) (ccm))))
;(transform-test '(wcm 0 ((λ (x) (wcm x (ccm))) 1)))
;(trace '(wcm (λ (x759754) (λ (x759755) ((ccm) x759754))) ((ccm) ((ccm) (ccm)))))

;(trace '(wcm 1 ((ccm) (λ (x) (λ (y) x)))))

;(trace `(wcm (λ (x) (ccm)) (((ccm) (λ (x) (λ (y) x))) (λ (x) (x)))))
#;(traces λv-rr `(((((,(transform '(λ (z) ((z (λ (a) a)) (λ (b) b))))
             (λ (z) z)) (λ (z) z)) (λ (x) (λ (f) (λ (m) (λ (y) (λ (f) (λ (m) x))))))) (λ (q1) q1)) (λ (q2) q2)))



=======
(transform-test '(error (ccm)))
(transform-test '(wcm 1 (wcm (ccm) (ccm))))
(transform-test '(wcm 0 ((λ (x) (wcm x (ccm))) 1)))
(transform-test '(wcm 0 ((λ (x) x) (wcm 1 (ccm)))))

(define p '(wcm 0 (ccm)))
;(trace '(x (λ (y) y)))
>>>>>>> 9bfdfd1758eb73be65d40a16164483f9002bb043


(define (apply-reduction-relation/n rr e n [i 0])
  (if (= i n)
      (list e)
      (apply append (map (λ (t) (apply-reduction-relation/n rr t n (+ i 1))) (apply-reduction-relation rr e))))) 

#;(transform-test '(wcm 0
                        ((λ (ignored)
                           ((λ (x) x)
                            (wcm 1
                                 (ccm))))
                         (λ (x) x))))

#;(transform-test '(wcm (λ (f) (λ (z) z))
                        ((λ (ignored)
                           ((λ (x) x)
                            (wcm (λ (f) (λ (z) (f z)))
                                 (ccm))))
                         (λ (x) x))))

;
;(transform-test '(λ (u) u))
;(trace (random-cm-term 10))
;(trace '((ccm) 2))

(define (the-important-property-holds program)
  (let* ((value1 (first (apply-reduction-relation* λv-rr (transform-c (first (apply-reduction-relation* λcm-rr program))))))
         (value2 (first (apply-reduction-relation* λv-rr (transform-c program)))))
    (alpha-eq? value1 value2)))

(define (prepare-cm-term term)
  (let ((t (random-cm-term (cm-term-length term))))
    (begin
      (display t)
      (newline)
      t)))

(define (check generate test #:attempts [n 1000])
  (let ((result (do
                    ([i 0 (+ i 1)]
                     [t (generate 1) (generate (+ i 2))])
                  ((or (= i n) (not (test t)))
                   (if (= i n)
                       #f
                       t))
                  (display t)
                  (newline))))
    result))

;(check random-cm-term the-important-property-holds #:attempts 10)


;(redex-check λcm e (the-important-property-holds (term e)) #:attempts 1000 #:prepare prepare-cm-term)

