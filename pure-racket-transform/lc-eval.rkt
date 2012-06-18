#lang racket

(define lc-rename-var
  (λ (var x y)
    (if (eq? (second var) x)
        `(var ,y)
        var)))

(define lc-rename-abs
  (λ (abs x y)
    (if (eq? (second abs) x)
        abs
        `(abs ,(second abs) ,(lc-rename (third abs) x y)))))

(define lc-rename-app
  (λ (app x y)
    `(app ,(lc-rename (second app) x y) ,(lc-rename (third app) x y))))

; change x to y in e
(define lc-rename
  (λ (e x y)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-rename-var e x y))
        ((eq? tag 'abs) (lc-rename-abs e x y))
        ((eq? tag 'app) (lc-rename-app e x y))
        (else (error "unrecognized tag " e))))))

(define lc-occurs-free-in-var
  (λ (var x)
    (eq? (second var) x)))

(define lc-occurs-free-in-abs
  (λ (abs x)
    (if (eq? (second abs) x)
        #f
        (lc-occurs-free-in (third abs) x))))

(define lc-occurs-free-in-app
  (λ (app x)
    (or (lc-occurs-free-in (second app) x) (lc-occurs-free-in (third app) x))))

(define lc-occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-occurs-free-in-var e x))
        ((eq? tag 'abs) (lc-occurs-free-in-abs e x))
        ((eq? tag 'app) (lc-occurs-free-in-app e x))
        (else (error "unrecognized tag " e))))))

(define lc-substitute-var
  (λ (var x f)
    (if (eq? (second var) x)
        f
        var)))

(define lc-substitute-abs
  (λ (abs x f)
    (if (eq? (second abs) x)
        abs
        (if (lc-occurs-free-in f (second abs))
            `(abs ,(second abs) ,(lc-substitute (third abs) x (lc-rename f (second abs) (fresh-variable))))
            `(abs ,(second abs) ,(lc-substitute (third abs) x f))))))

(define lc-substitute-app
  (λ (app x f)
    `(app ,(lc-substitute (second app) x f) ,(lc-substitute (third app) x f))))

(define lc-substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-substitute-var e x f))
        ((eq? tag 'abs) (lc-substitute-abs e x f))
        ((eq? tag 'app) (lc-substitute-app e x f))
        (else (error "unrecognized tag " tag))))))

(define lc-eval-var
  (λ (var)
    var))

(define lc-eval-abs
  (λ (abs)
    `(abs ,(second abs) ,(lc-eval-inner (third abs)))))

(define lc-eval-app
  (λ (app)
    (let ((rator (lc-eval-inner (second app)))
          (rand (lc-eval-inner (third app))))
      (if (eq? (first rator) 'abs)
          (lc-eval-inner (lc-substitute (third rator) (second rator) rand))
          `(app ,rator ,rand)))))
        
(define lc-eval-inner
  (λ (e)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-eval-var e))
        ((eq? tag 'abs) (lc-eval-abs e))
        ((eq? tag 'app) (lc-eval-app e))
        (else (error "unrecognized tag " tag))))))

(define lc-eval
  (λ (e)
    (lc-emit (lc-eval-inner (lc-parse e)))))

(provide lc-eval)
