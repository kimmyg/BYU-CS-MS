(define fresh-variable
  (let ((seed 0))
    (λ ()
      (let ((x (string->symbol (string-append "x" (number->string seed)))))
        (begin
          (set! seed (+ seed 1))
          x)))))
