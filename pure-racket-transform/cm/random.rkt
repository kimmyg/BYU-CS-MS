#lang racket

(define random-cm-var
  (λ ()
    (let ((i (random 3)))
      (cond
        ((= i 0) 'x)
        ((= i 1) 'y)
        (else    'z)))))

(define random-cm-abs
  (λ ([n 0])
    (if (= n 0)
        `(λ (,(random-cm-var)) ,(random-cm-term))
        `(λ (,(random-cm-var)) ,(random-cm-term (- n 1))))))
    
(define random-cm-app
  (λ ([n 0])
    (if (= n 0)
        `(,(random-cm-term) ,(random-cm-term))
        (let ((i (+ (random (- n 1)) 1)))
          `(,(random-cm-term i) ,(random-cm-term (- n i)))))))

(define random-cm-wcm
  (λ ([n 0])
    (if (= n 0)
        `(wcm ,(random-cm-term) ,(random-cm-term))
        (let ((i (+ (random (- n 2)) 1)))
          `(wcm ,(random-cm-term i) ,(random-cm-term (- (- n 1) i)))))))

(define random-cm-ccm
  (λ ()
    `(ccm)))

(define random-cm-term
  (λ ([n 0])
    (if (= n 0)
      (let ((i (random 5)))
        (cond
          ((= i 0) (random-cm-var))
          ((= i 1) (random-cm-abs))
          ((= i 2) (random-cm-app))
          ((= i 3) (random-cm-wcm))
          (else    (random-cm-ccm))))
      (cond
        ((= n 1)
         (let ((i (random 2)))
           (if (= i 0)
               (random-cm-var)
               (random-cm-ccm))))
        ((= n 2)
         (let ((i (random 2)))
           (if (= i 0)
               (random-cm-abs n)
               (random-cm-app n))))
        (else
         (let ((i (random 3)))
           (cond
             ((= i 0) (random-cm-abs n))
             ((= i 1) (random-cm-app n))
             (else    (random-cm-wcm n)))))))))

(define cm-var-of-length
  (λ (n)
    (if (= n 1)
        '(x y z)
        '())))

(define cm-abs-of-length
  (λ (n)
    (if (= n 0)
        '()
        (append*
         (for/list ([var (cm-var-of-length 1)])
           (for/list ([term (cm-terms-of-length (- n 1))])
             `(λ (,var) ,term)))))))

(define cm-app-of-length
  (λ (n)
    (if (= n 0)
        '()
        (append*
         (for/list ([i n])
           (append*
            (for/list ([rator (cm-terms-of-length i)])
             (for/list ([rand (cm-terms-of-length (- n i))])
               `(,rator ,rand)))))))))

(define cm-wcm-of-length
  (λ (n)
    (if (= n 0)
        '()
        (let ((m (- n 1)))
          (append*
           (for/list ([i m])
             (append*
              (for/list ([mark (cm-terms-of-length i)])
                (for/list ([body (cm-terms-of-length (- m i))])
                  `(wcm ,mark ,body))))))))))

(define cm-ccm-of-length
  (λ (n)
    (if (= n 1)
        '((ccm))
        '())))

(define cm-terms-of-length
  (λ (n)
    (append (cm-var-of-length n) (cm-abs-of-length n) (cm-app-of-length n) (cm-wcm-of-length n) (cm-ccm-of-length n))))

(provide random-cm-term)
(provide cm-terms-of-length)
