#lang racket

(require "emit.rkt")
(require "parse.rkt")

(define rename-var
  (λ (var x y)
    (if (eq? (second var) x)
        `(var ,y)
        var)))

(define rename-abs
  (λ (abs x y)
    (if (eq? (second abs) x)
        abs
        `(abs ,(second abs) ,(rename (third abs) x y)))))

(define rename-app
  (λ (app x y)
    `(app ,(rename (second app) x y) ,(rename (third app) x y))))

; change x to y in e
(define rename
  (λ (e x y)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (rename-var e x y))
        ((eq? tag 'abs) (rename-abs e x y))
        ((eq? tag 'app) (rename-app e x y))
        (else (error "unrecognized tag " e))))))

(define occurs-free-in-var
  (λ (var x)
    (eq? (second var) x)))

(define occurs-free-in-abs
  (λ (abs x)
    (if (eq? (second abs) x)
        #f
        (occurs-free-in (third abs) x))))

(define occurs-free-in-app
  (λ (app x)
    (or (occurs-free-in (second app) x) (occurs-free-in (third app) x))))

(define occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (occurs-free-in-var e x))
        ((eq? tag 'abs) (occurs-free-in-abs e x))
        ((eq? tag 'app) (occurs-free-in-app e x))
        (else (error "unrecognized tag " e))))))

(define substitute-var
  (λ (var x f)
    (if (eq? (second var) x)
        f
        var)))

(define substitute-abs
  (λ (abs x f)
    (if (eq? (second abs) x)
        abs
        (if (occurs-free-in f (second abs))
            `(abs ,(second abs) ,(substitute (third abs) x (rename f (second abs) (gensym 'x))))
            `(abs ,(second abs) ,(substitute (third abs) x f))))))

(define substitute-app
  (λ (app x f)
    `(app ,(substitute (second app) x f) ,(substitute (third app) x f))))

(define substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (substitute-var e x f))
        ((eq? tag 'abs) (substitute-abs e x f))
        ((eq? tag 'app) (substitute-app e x f))
        (else (error "unrecognized tag " tag))))))

(define eval-var
  (λ (var)
    var))

(define eval-abs
  (λ (abs)
    `(abs ,(second abs) ,(eval-inner (third abs)))))

(define eval-app
  (λ (app)
    (let ((rator (eval-inner (second app)))
          (rand (eval-inner (third app))))
      (if (eq? (first rator) 'abs)
          (eval-inner (substitute (third rator) (second rator) rand))
          `(app ,rator ,rand)))))
        
(define eval-inner
  (λ (e)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (eval-var e))
        ((eq? tag 'abs) (eval-abs e))
        ((eq? tag 'app) (eval-app e))
        (else (error "unrecognized tag " tag))))))

(define eval
  (λ (e)
    (emit (eval-inner (parse e)))))

(provide eval)
