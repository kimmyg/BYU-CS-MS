#lang racket
(require redex)

(require "cm.rkt"
         "lc.rkt"
         "transform.rkt"
         "alpha.rkt")

(define (transform-test program)
  (let* ((value (first (apply-reduction-relation* λcm-rr program)))
         (value1 (transform value))
         (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
    (if (alpha-eq? value1 value2)
        #t
        (begin
          (display value)
          (newline)
          (display value1)
          (newline)
          (display value2)
          (newline)
          #f))))

(transform-test '(wcm (λ (x) x) (ccm)))

#|
pass in λp.((p λx.λy.y) ,(transform-abs 'λx.λy.y)

\p.((p E) F)

\k.\m.(k \p.T[((p E) F)])
\k.\m.(k \p.T[((p E) F)])

 (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))
\k.\m.(\k.(k (m \x.\y.y))) \snd_m.((T[E] (\e.((T[F] (\f.(((e f) k) m)) <marks>))) <marks>))

(abs ,k (abs ,m (app (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) (abs ,snd_m (app (app T[(p E)] (abs ,e (app (app T[F] (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))
(abs k (abs m (app (var k) (abs p (abs ,k (abs ,m (app (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) (abs ,snd_m (app (app (abs ,k (abs ,m (app (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) (abs ,snd_m (app (app (abs k (abs m (app (var k) (var p)))) (abs ,e (app (app (var ,tail_m) (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))))))



|#