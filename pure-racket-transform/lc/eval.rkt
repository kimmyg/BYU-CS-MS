#lang racket

(require "emit.rkt")
(require "parse.rkt")

(include "eval-base.rkt")

; change x to y in e
(define (rename e x y)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (rename-var e x y))
      ((eq? tag 'abs) (rename-abs e x y))
      ((eq? tag 'app) (rename-app e x y))
      ((eq? tag 'num) (rename-num e x y))
      ((eq? tag 'cons) (rename-cons e x y))
      ((eq? tag 'nil) (rename-nil e x y))
      ((eq? tag 'fst) (rename-fst e x y))
      ((eq? tag 'rst) (rename-rst e x y))
      ((eq? tag 'isnil?) (rename-isnil? e x y))
      (else (error "unrecognized tag " e)))))

(define (occurs-free-in e x)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (occurs-free-in-var e x))
      ((eq? tag 'abs) (occurs-free-in-abs e x))
      ((eq? tag 'app) (occurs-free-in-app e x))
      ((eq? tag 'num) (occurs-free-in-num e x))
      ((eq? tag 'cons) (occurs-free-in-cons e x))
      ((eq? tag 'nil) (occurs-free-in-nil e x))
      ((eq? tag 'fst) (occurs-free-in-fst e x))
      ((eq? tag 'rst) (occurs-free-in-rst e x))
      ((eq? tag 'isnil?) (occurs-free-in-isnil? e x))
      (else (error "unrecognized tag " e)))))

(define (substitute e x f)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (substitute-var e x f))
      ((eq? tag 'abs) (substitute-abs e x f))
      ((eq? tag 'app) (substitute-app e x f))
      ((eq? tag 'num) (substitute-num e x f))
      ((eq? tag 'cons) (substitute-cons e x f))
      ((eq? tag 'nil) (substitute-nil e x f))
      ((eq? tag 'fst) (substitute-fst e x f))
      ((eq? tag 'rst) (substitute-rst e x f))
      ((eq? tag 'isnil?) (substitute-isnil? e x f))
      (else (error "unrecognized tag " tag)))))

; eval

(define (eval-var var)
  var)

(define (eval-abs abs)
  abs)

(define (eval-app app)
  (let ((rator (eval-inner (second app)))
        (rand (eval-inner (third app))))
    (if (eq? (first rator) 'abs)
        (eval-inner (substitute (third rator) (second rator) rand))
        `(app ,rator ,rand))))

(define (eval-num num)
  num)

(define (eval-cons cons)
  `(cons ,(eval-inner (second cons)) ,(eval-inner (third cons))))

(define (eval-nil nil)
  nil)

(define (eval-fst fst)
  (let ((arg (eval-inner (second fst))))
    (if (eq? (first arg) 'cons)
        (second arg)
        (error "attempting to take fst of " arg))))

(define (eval-rst rst)
  (let ((arg (eval-inner (second rst))))
    (if (eq? (first arg) 'cons)
        (third arg)
        (error "attempting to take rst of " arg))))

(define (eval-isnil? isnil?)
  (let ((arg (eval-inner (second isnil?))))
    (let ((tag (first arg)))
      (cond
        ((eq? tag 'nil) '(abs x (abs y (var x))))
        ((eq? tag 'cons) '(abs x (abs y (var y))))
        (else (error "attempting isnil? on " arg))))))

(define (eval-inner e)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (eval-var e))
      ((eq? tag 'abs) (eval-abs e))
      ((eq? tag 'app) (eval-app e))
      ((eq? tag 'num) (eval-num e))
      ((eq? tag 'cons) (eval-cons e))
      ((eq? tag 'nil) (eval-nil e))
      ((eq? tag 'fst) (eval-fst e))
      ((eq? tag 'rst) (eval-rst e))
      ((eq? tag 'isnil?) (eval-isnil? e))
      (else (error "unrecognized tag " tag)))))

(define (eval e)
  (emit (eval-inner (parse e))))

(provide eval)
