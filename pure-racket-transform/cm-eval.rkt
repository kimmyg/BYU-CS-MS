#lang racket

(require "cm-emit.rkt")
(require "cm-parse.rkt")

(define cm-rename-var
  (λ (var x y)
    (if (eq? (second var) x)
        `(var ,y)
        var)))

(define cm-rename-abs
  (λ (abs x y)
    (if (eq? (second abs) x)
        abs
        `(abs ,(second abs) ,(cm-rename (third abs) x y)))))

(define cm-rename-app
  (λ (app x y)
    `(app ,(cm-rename (second app) x y) ,(cm-rename (third app) x y))))

(define cm-rename-wcm
  (λ (wcm x y)
    `(wcm ,(cm-rename (second wcm) x y) ,(cm-rename (third wcm) x y))))

(define cm-rename-ccm
  (λ (ccm x y)
    ccm))

; change x to y in e
(define cm-rename
  (λ (e x y)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (cm-rename-var e x y))
        ((eq? tag 'abs) (cm-rename-abs e x y))
        ((eq? tag 'app) (cm-rename-app e x y))
        ((eq? tag 'wcm) (cm-rename-wcm e x y))
        ((eq? tag 'ccm) (cm-rename-ccm e x y))
        (else (error "unrecognized tag " e))))))

(define cm-occurs-free-in-var
  (λ (var x)
    (eq? (second var) x)))

(define cm-occurs-free-in-abs
  (λ (abs x)
    (if (eq? (second abs) x)
        #f
        (cm-occurs-free-in (third abs) x))))

(define cm-occurs-free-in-app
  (λ (app x)
    (or (cm-occurs-free-in (second app) x) (cm-occurs-free-in (third app) x))))

(define cm-occurs-free-in-wcm
  (λ (wcm x)
    (or (cm-occurs-free-in (second wcm) x) (cm-occurs-free-in (third wcm) x))))

(define cm-occurs-free-in-ccm
  (λ (ccm x)
    #f))

(define cm-occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (cm-occurs-free-in-var e x))
        ((eq? tag 'abs) (cm-occurs-free-in-abs e x))
        ((eq? tag 'app) (cm-occurs-free-in-app e x))
        ((eq? tag 'wcm) (cm-occurs-free-in-wcm e x))
        ((eq? tag 'ccm) (cm-occurs-free-in-ccm e x))
        (else (error "unrecognized tag " e))))))

(define cm-substitute-var
  (λ (e x f)
    (if (eq? (second e) x)
        f
        e)))

(define cm-substitute-abs
  (λ (e x f)
    (if (eq? (second e) x)
        e
        (if (cm-occurs-free-in f (second e))
            `(abs ,(second e) ,(cm-substitute (third e) x (cm-rename f (second e) (gensym 'x))))
            `(abs ,(second e) ,(cm-substitute (third e) x f))))))

(define cm-substitute-app
  (λ (app x f)
    `(app ,(cm-substitute (second app) x f) ,(cm-substitute (third app) x f))))

(define cm-substitute-wcm
  (λ (wcm x f)
    `(wcm ,(cm-substitute (second wcm) x f) ,(cm-substitute (third wcm) x f))))

(define cm-substitute-ccm
  (λ (ccm x f)
    ccm))

(define cm-substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (cm-substitute-var e x f))
        ((eq? tag 'abs) (cm-substitute-abs e x f))
        ((eq? tag 'app) (cm-substitute-app e x f))
        ((eq? tag 'wcm) (cm-substitute-wcm e x f))
        ((eq? tag 'ccm) (cm-substitute-ccm e x f))
        (else (error "cm-substitute unrecognized tag " tag))))))

(define cm-eval-var
  (λ (var k)
    var))

(define cm-eval-abs
  (λ (abs k)
    `(abs ,(second abs) ,(cm-eval-inner (third abs) k))))

(define cm-eval-app
  (λ (app k)
    (let ((rator (cm-eval-inner (second app) k))
          (rand  (cm-eval-inner (third  app) k)))
      (if (eq? (first rator) 'abs)
          (cm-eval-inner (cm-substitute (third rator) (second rator) rand) k)
          `(app ,rator ,rand)))))

(define cm-eval-wcm
  (λ (wcm k)
    (if (eq? (first (third wcm)) 'wcm)
        (cm-eval-inner (third wcm) k)
        (let ((z (gensym 'x)))
          (cm-eval-inner (third wcm) `(abs ,z (app (app (var ,z) ,(cm-eval-inner (second wcm) k)) ,k)))))))

(define cm-eval-ccm
  (λ (ccm k)
    k))

(define cm-eval-inner
  (λ (e k)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (cm-eval-var e k))
        ((eq? tag 'abs) (cm-eval-abs e k))
        ((eq? tag 'app) (cm-eval-app e k))
        ((eq? tag 'wcm) (cm-eval-wcm e k))
        ((eq? tag 'ccm) (cm-eval-ccm e k))
        (else (error "unrecognized tag " tag))))))

(define cm-eval
  (λ (e)
    (cm-emit (cm-eval-inner (cm-parse e) '(abs x (abs y (var y)))))))

(provide cm-eval)