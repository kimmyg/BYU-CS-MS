#lang slideshow

; a2ps

(require slideshow/code)
(require (planet jaymccarthy/slideshow-latex:1:1))

;(putenv "TEXINPUTS" "/usr/racket/collects/slatex")
;(latex-path (find-executable-path "slatex"))
;(add-preamble "\\usepackage{slatex}")

(define (underline p)
  (refocus
   (vc-append p (colorize (linewidth 4 (hline (pict-width p) 0)) "red"))
   p))

(define (frame/blue pict)
  (frame pict #:color "blue" #:line-width 4))

(define (vc-append/gap . picts)
  (apply vc-append gap-size picts))

(define (select-and-collect f xs)
  (if (empty? xs)
      empty
      (let ([x (first xs)]
            [xs (rest xs)])
        (cons (cons (f x) xs)
              (map (λ (xs)
                     (cons x xs))
                   (select-and-collect f xs))))))

(define (underline-each picts)
  (select-and-collect underline picts))

(define (select-them-inner before-and-after? combiner f . xs)
  (let ([ys (select-and-collect f xs)])
    (map (λ (xs)
           (apply combiner xs))
         (if before-and-after? (cons xs (append ys (list xs))) ys))))

(define (select-them before-and-after? combiner f . xs)
  (map list (apply select-them-inner before-and-after? combiner f xs)))

(define (underline-them display-first? combiner . items)
  (apply select-them display-first? combiner underline items))

(define (pin-arrows-lines arrow-size pict locs [prefix empty])
  (if (empty? locs)
      pict
      (match-let ([(list src find-src dest find-dest) (first locs)])
        (let ([result-pict (pin-arrows-line arrow-size pict (append prefix src) find-src (append prefix dest) find-dest)])
          (pin-arrows-lines arrow-size result-pict (rest locs) (cons pict prefix))))))

(define (item2 . args)
  (inset (apply item args) 16 4))
;  (inset (apply hc-append (map (λ (arg)
;                                 (cond
;                                   [(string? arg) (t arg)]
;                                   [else arg]))
;                               args)) 16 4))

;(define-syntax (slide/reduction

(define c-diagram ($ "\\begin{array}{ccc}p & \\rightarrow_{cm}^{*} & v\\\\\\downarrow_\\mathcal{C} & & \\downarrow_\\mathcal{C}\\\\\\mathcal{C}[p] & \\rightarrow_{v}^{*} & \\mathcal{C}[v]\\end{array}"))

(define (t-s t s)
  (text t (current-main-font) s))

(define (emphasize l inner r)
  (hc-append (colorize (tt l) "gray")
             (colorize (tt inner) "black")
             (colorize (tt r) "gray")))

#;(slide
   (let ([s1 (emphasize "" "(fac 2)" "")]
         [s2 (emphasize "" "(if0 2 1 (wcm 2 (* 2 (fac (- 2 1))))" "")]
         [s3 (emphasize "" "(wcm 2 (* 2 (fac (- 2 1))))" "")]
         [s4 (emphasize "(wcm 2 " "(* 2 (fac (- 2 1)))" ")")]
         [s5 (emphasize "(wcm 2 (* 2 " "(fac (- 2 1))" "))")]
         [s6 (emphasize "(wcm 2 (* 2 " "(if0 1 1 (wcm 1 (* 1 (fac 0))" "))")])
     (vc-append s1 s2 s3)))



(define (strike pict)
  (refocus (cc-superimpose pict (linewidth 4 (hline (pict-width pict) (pict-height pict)))) pict))

(slide
 (t "A CPS-like Transformation of Continuation Marks")
 (t-s "Kimball Germane" (* (current-font-size) 3/4))
 (t-s "with Jay McCarthy advising" (* (current-font-size) 3/4)))

(slide
 #:title "Thesis"
 (para "A CPS-like transformation can transform the" ($ "\\lambda") "-calculus"
       "with continuation marks into the plain" ($ "\\lambda") "-calculus in a"
       "meaning-preserving way."))

; a Turing machine is a machine model of computation
; we fix the program and feed input to the machine
; computation is performed as the machine goes from state to state
; the lambda-calculus is a Turing-complete system of logic
; akin to algebra. program and data are combined into a term
; computation is performed as the term reduces and evolves
(slide
 #:title (para "What is the " ($ "\\lambda") "-calculus?")
 (t-s "A language model of computation" 48))

(slide
 #:title (para ($ "\\lambda") "-calculus")
 (para "Terms " ($ "e") " take the form of")
 ; terms in this language are defined inductively
 'alts
 (select-them #t vc-append/gap frame/blue
              (item2 "variables " ($ "x"))
              ; these correspond to mathematical variables
              ; in other words, we don't think of assigning values to variables
              ; but rather substituting variables with values
              (item2 "abstractions " ($ "\\lambda x.e"))
              ; ...where e is itself a lambda calculus term
              ; this is the first of two ways we see the inductive structure of lambda calculus terms
              ; these correspond to functions: x is the parameter of the function and e is the body of
              ; the function
              (item2 "applications " ($ "(e_0\\,e_1)"))
              ; ...where e_0 and e_1 are lambda calculus terms
              ; this form corresponds to application; e_0 represents the function and e_1
              ; represents the argument
              ))

(slide
 #:title (para ($ "\\lambda") "-calculus evaluation")
 ; evaluation of the lambda calculus is defined recursively
 ; which parallels its inductive definition
 (para "Evaluation of a term " ($ "e") "depends on its form")
 'alts
 (select-them #t vc-append/gap frame/blue
              (item2 "variables " ($ "x") " are substituted with their binding in the environment")
              ; lone variables are unbound and we consider unbound variables an error
              (item2 "abstractions " ($ "\\lambda x.e") " evaluate to closures")
              (item2 "applications " ($ "(e_0\\,e_1)") " are evaluated by recursively evaluating " ($ "e_0")
                     " then " ($ "e_1") " and then substituting")))

(slide
 #:title (para ($ "\\lambda") "-calculus substitution")
 (para "When a term of the form " ($ "(\\lambda x.e' v)") " is encountered in evaluation,"
       "it is" (it "reduced") "by the rule " ($ "(\\lambda x.e' v)\\rightarrow e'[x\\leftarrow v]") ".")
 )

(slide
 #:title (para "Example " ($ "\\lambda") "-calculus evaluation")
 (vl-append
  (emphasize "" "((λx.λy.y ((λx.λy.x 1) 2)) 3)" "")
  (emphasize "(" "(λx.λy.y ((λx.λy.x 1) 2))" " 3)")
  (emphasize "((" "λx.λy.y" " ((λx.λy.x 1) 2)) 3)")
  (emphasize "((λx.λy.y " "((λx.λy.x 1) 2)" ") 3)")
  (emphasize "((λx.λy.y (" "(λx.λy.x 1)" " 2)) 3)")
  (emphasize "((λx.λy.y ((" "λx.λy.x" " 1) 2)) 3)")
  (emphasize "((λx.λy.y ((λx.λy.x " "1" ") 2)) 3)")
  (emphasize "((λx.λy.y (" "(λx.λy.x 1)" " 2)) 3)")
  (emphasize "((λx.λy.y (" "λy.1" " 2)) 3)")
  (emphasize "((λx.λy.y (λy.1 " "2" ")) 3)")
  (emphasize "((λx.λy.y " "(λy.1 2)" ") 3)")
  (emphasize "((λx.λy.y " "1" ") 3)")
  (emphasize "(" "(λx.λy.y 1)" " 3)")
  (emphasize "(" "λy.y" " 3)")
  ;(tt "...")
  (emphasize "(λy.y " "3" ")")
  (emphasize "" "(λy.y 3)" "")
  (emphasize "" "3" "")))

#;(slide
   #:title (para ($ "\\lambda") "-calculus evaluation contexts")
   ; there is a formal definition for these contexts
   ; the recursive evaluation strategy gives rise to an inductive definition
   (para "An evaluation context " ($ "E") " takes the form of")
   'alts
   (select-them #t vc-append/gap frame/blue
                (item2 "a hole " ($ "\\bullet") " to be filled with a value or more context")
                (item2 ($ "(E\\,e)"))
                ; the evaluation of the function or operator
                (item2 ($ "(v\\,E)"))
                ; the evaluation of the argument or operand
                ))





(slide
 (t "A quick refresher on continuation marks"))

(slide
 (para "Continuation marks allow you to:")
 'alts
 (select-them #t vc-append/gap frame/blue
              (item2 "mark the continuation with " ($ "(\\mathrm{wcm}\\,e_0\\,e_1)"))
              (item2 "obtain the current marks with " ($ "(\\mathrm{ccm})"))))

#;(slide
   (t "Suppose")
   (tt "(fac n)")
   (t "is defined as")
   'alts
   (list (list (emphasize "" "(if0 n (begin (print (ccm)) 1) (wcm n (* n (fac (- n 1))))" ""))
         (list (emphasize "(if0 n " "(begin (print (ccm)) 1)" " (wcm n (* n (fac (- n 1))))"))
         (list (emphasize "(if0 n (begin (print (ccm)) 1) " "(wcm n (* n (fac (- n 1)))" ")"))
         (list (emphasize "" "(if0 n (begin (print (ccm)) 1) (wcm n (* n (fac (- n 1))))" ""))))

#;(slide
   'alts
   (select-them #f vc-append frame/blue
                (emphasize "" "(fac 2)" "")
                (emphasize "" "(if0 2 ... (wcm 2 (* 2 (fac (- 2 1))))" "")
                (emphasize "(if0 " "2" " ... (wcm 2 (* 2 (fac (- 2 1)))))")
                (emphasize "" "(wcm 2 (* 2 (fac (- 2 1))))" "")
                (emphasize "(wcm " "2" " (* 2 (fac (- 2 1))))")
                (emphasize "(wcm 2 " "(* 2 (fac (- 2 1)))" ")")
                (emphasize "(wcm 2 (* " "2" " (fac (- 2 1))))")
                (emphasize "(wcm 2 (* 2 " "(fac (- 2 1))" "))")
                (emphasize "(wcm 2 (* 2 (" "fac" " (- 2 1))))")
                (emphasize "(wcm 2 (* 2 (fac " "(- 2 1)" ")))")
                (emphasize "(wcm 2 (* 2 (fac (- " "2" " 1))))")
                (emphasize "(wcm 2 (* 2 (fac (- 2 " "1" "))))")
                (emphasize "(wcm 2 (* 2 (fac " "1" ")))")
                (emphasize "(wcm 2 (* 2 " "(fac 1)" "))")
                (emphasize "(wcm 2 (* 2 " "(if0 1 ... (wcm 1 (* 1 (fac (- 1 1)))))" "))")
                (emphasize "(wcm 2 (* 2 (if0 " "1" " ... (wcm 1 (* 1 (fac (- 1 1)))))))")
                (emphasize "(wcm 2 (* 2 " "(wcm 1 (* 1 (fac (- 1 1))))" "))")
                (emphasize "(wcm 2 (* 2 (wcm " "1" " (* 1 (fac (- 1 1))))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 " "(* 1 (fac (- 1 1)))" ")))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* " "1" " (fac (- 1 1))))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(fac (- 1 1))" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (" "fac" " (- 1 1))))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (fac " "(- 1 1)" ")))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (fac (- " "1" " 1))))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (fac (- 1 " "1" "))))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (fac " "(- 1 1)" ")))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (fac " "0" ")))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(fac 0)" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(if0 0 (begin (print (ccm)) 1) ...)" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (if0 " "0" " (begin (print (ccm)) 1) ...)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(begin (print (ccm)) 1)" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin " "(print (ccm))" " 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin (" "print" " (ccm)) 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin (print " "(ccm)" ") 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin (print " "(cons 2 (cons 1 nil))" ") 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin " "(print (cons 2 (cons 1 nil)))" " 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 (begin " "<void>" " 1)))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(begin <void> 1)" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "(begin 1)" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 (* 1 " "1" "))))")
                (emphasize "(wcm 2 (* 2 (wcm 1 " "(* 1 1)" ")))")
                (emphasize "(wcm 2 (* 2 (wcm 1 " "1" ")))")
                (emphasize "(wcm 2 (* 2 " "(wcm 1 1)" "))")
                (emphasize "(wcm 2 (* 2 " "1" "))")
                (emphasize "(wcm 2 " "(* 2 1)" ")")
                (emphasize "(wcm 2 " "2" ")")
                (emphasize "" "(wcm 2 2)" "")
                (emphasize "" "2" "")))

#;(slide
   (text "Suppose we have a module in our system with a function")
   (code (explode-death-star)))

;(slide/reduction '(fac 2))

; review single-argument lambda calculus briefly
; \x.x is like function(x) { return x; }
; if f is \x.x, (f y) is like f(y)
; review continuation-passing style
; introduce lambda cm

#;(slide
   #:title "Languages"
   ; the first thing we must do is formally define the languages we are working with
   'alts
   (select-them #t vc-append/gap frame/blue
                (inset (vl-append gap-size
                                  ($ "\\lambda_{v}")
                                  (para ($ "e = x\\,|\\,\\lambda x.e\\,|\\,(e\\,e)"))
                                  (para ($ "E = (E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet"))
                                  (para "Reduction rules")
                                  (para ($ "(\\lambda x.e'\\,v)\\rightarrow e'[x\\leftarrow v]")))
                       16)
                ; the first language is the call-by-value lambda calculus we just discussed
                ; e defines terms in the language and E defines evaluation contexts 
                (inset (vl-append gap-size
                                  ($ "\\lambda_{cm}")
                                  (para ($ "e = x\\,|\\,\\lambda x.e\\,|\\,(e\\,e)\\,|\\,(\\mathrm{wcm}\\,e\\,e)\\,|\\,(\\mathrm{ccm})"))
                                  (para ($ "E = (\\mathrm{wcm}\\,v\\,F)\\,|\\,F")
                                        ($ "F = (\\mathrm{wcm}\\,E\\,e)\\,|\\,(E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet")))
                       16)
                ; the second language is the call-by-value lambda calculus with continuation 
                ; marks. see the two additional forms that terms can take, wcm and ccm
                ; contexts E are defined in such a way that two (wcm v F) contexts cannot be 
                ; directly nested within one another
                ))

#;(slide
   (t-s "Languages" 64))

(slide
 #:title "Thesis"
 (para "A CPS-like transformation can transform the "
       (underline (hc-append ($ "\\lambda") (t "-calculus with continuation marks")))
       "into the" (underline (t "plain")) 
       (underline (hc-append ($ "\\lambda") (t "-calculus")))
       "in a meaning-preserving way."))

(slide
 #:title (para ($ "\\lambda_{v}"))
 (para "Terms")
 (para ($ "e = x\\,|\\,\\lambda x.e\\,|\\,(e\\,e)"))
 (para "Contexts")
 (para ($ "E = (E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet")
       (ghost ($ "F = (\\mathrm{wcm}\\,E\\,e)\\,|\\,(E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet")))
 (para "Reduction rules " ($ "\\rightarrow_{v}"))
 (para ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]")
       (ghost ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]"))
       (ghost ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']")) (blank 64 0) ;hack
       (ghost ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]"))))

(slide
 #:title (para ($ "\\lambda_{cm}"))
 (para "Terms")
 (para ($ "e =") (cellophane ($ "x\\,|\\,\\lambda x.e\\,|\\,(e\\,e)") 0.75) ($ "|\\,(\\mathrm{wcm}\\,e\\,e)\\,|\\,(\\mathrm{ccm})"))
 (para "Contexts")
 (para ($ "E = (\\mathrm{wcm}\\,v\\,F)\\,|\\,F") (blank 256 0) ;hack
       ($ "F = (\\mathrm{wcm}\\,E\\,e)") (cellophane ($ "|\\,(E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet") 0.75))
 (para "Reduction rules " ($ "\\rightarrow_{cm}"))
 (para (cellophane ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 0.75)
       ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]")
       ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") (blank 64 0) ;hack
       ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]")))

(slide
 #:title (para "A transformation " ($ "\\mathcal{C}"))
 ($ "\\mathcal{C}:\\lambda_{cm}\\rightarrow\\lambda_{v}")
 (t "and should satisfy")
 c-diagram)


(slide
 #:title (para "What makes this difficult?")
 (para "Terms in " ($ "\\lambda_{v}") " have access to their environment")
 'alts
 (list (list (para "Terms in " ($ "\\lambda_{cm}") " have access to their environment"))
       (list (para "Terms in " ($ "\\lambda_{cm}") " have access to their environment and context")))
 'next
 (blank 0 32)
 (para "Intuition")
 (para "Pass contextual information explicitly to terms as arguments, introducing"
       "it into the environment."))

(slide
 (para "What information do we pass?")
 'next
 (para "Anything the original " ($ "\\lambda_{cm}") " program could observe")
 ; this is our guiding principle
 'next
 (item "the list of current continuation marks " ($ "\\chi(E)"))
 ; certainly the continuation marks. these are observable in the original program with (ccm)
 )

(slide
 ($ "\\mathcal{C}[\\lambda x.e]=\\lambda \\mathit{marks}.\\lambda x.\\mathcal{C}[e]")
 ($ "\\mathcal{C}[(e\\,f)]=((e\\,\\mathit{marks})\\,(f\\,\\mathit{marks}))\\,\\mathit{marks})")
 'next
 (t "mark-passing style"))
 
 ;#:title "Mark-passing style"
 ;(t "Transform")
 ;($ "\\lambda x.\\dots\\mapsto\\lambda\\mathit{marks}.\\lambda x.\\dots")
 ;(t "and")
 ;($ "(f\\,x)\\mapsto((f\\,\\mathit{marks})\\,x)"))

(slide
 (t "Mark-passing style accounts for")
 'alts
 (list (list (para (frame/blue (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4))
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (frame/blue (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4))
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset (strike ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]")) 16 4)
                   (frame/blue (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4)) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset (strike ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]")) 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (frame/blue (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4))))))
 

(slide
 #:title "Tail position"
 (para "Marks placed in tail position annotate the newest part"
       "of the continuation, overwriting previous marks (if they"
       "exist).")
 ; this makes tail position observable, albeit indirectly
 )

(slide
 #:title "What information do we pass?"
 (para "Anything the original " ($ "\\lambda_{cm}") " program could observe:")
 (item "the list of current continuation marks " ($ "\\chi(E)"))
 'alts
 (list (list (item "???"))
       (list (item "a flag reflecting tail position " ($ "\\xi(E)"))))
 )

; now we would like to verify
; proof is expensive
; so we test
; testing is like the naive approach to solving the halting problem

#;(slide
   #:title (para "Definition of " ($ "\\mathcal{C}"))
   )


#;(slide
   #:title (para ($ "\\mathcal{C}[(\\mathit{rator_expr}\\,\\mathit{rand_expr})]"))
   (code (λ (flags)
           (λ (marks)
             (((((C[rator_expr] FALSE) marks)
                ((C[rand_expr] FALSE) marks))
               flags)
              marks)))))

#;(slide
   #:title (para ($ "\\mathcal{C}[(\\mathrm{wcm}\\,\\mathit{mark_expr}\\,\\mathit{body_expr})]"))
   (code (λ (flag)
           (λ (marks)
             ((C[body_expr] TRUE)
              (((λ (mark-value) (λ (rest-marks) C[((CONS mark-value) rest-marks)]))
                ((C[mark-expr] FALSE) marks))
               ((flag C[(SND marks)]) marks))))))
   )


; so we provide to each redex contextual information--anything that the corresponding 
; cm program could observe
; took the approach of mark-passing style
; naive mark-passing is not enough
; observe: (fac 2) -> (list 1 2), (fac-tr 2) -> (list 1 2)
; this approach does not preserve correct tail-call behavior
; this aspect of the continuation is observable, so we must pass it to the term
; fortunately, this isn't a lot of information: we just need a flag
; these two pieces of information--a flag and a list of marks--capture all contextual information
; now, the tranformation is fairly straightforward:
; variables are abstracted:
; x -> \f m.x
; values are independent of their context and so ignore that information:
; \x.e -> \f m.\x.C[e]
; application:
; evaluate the operator with contextual information
; evaluate the operand with contextual information
; does evaluation of either of these occur in the body of a wcm?
; no, let the flags reflect that
; what is the list of marks at this point?
; whatever it was before evaluation of this application began
; pass the marks unaltered
; note: this list is persistent (in a data structures sense)

(slide
 #:title "Testing with Redex"
 (para "Process")
 'alts
 (map list
      (map (λ (pict) (hc-append pict c-diagram))
           (parameterize ([current-para-width (- (current-para-width) (pict-width c-diagram))])
             (select-them-inner #t vc-append/gap frame/blue
                                (inset (para "1. generate a random " ($ "\\lambda_{cm}") " program") 16 4)
                                (inset (para "2. reduce according to " ($ "\\rightarrow_{cm}") " and transform the result") 16 4)
                                (inset (para "3. transform the program and reduce according to " ($ "\\rightarrow_{v}")) 16 4)
                                (inset (para "4. compare the results") 16 4)))))
 'next
 (t "Repeat 10,000 times")
 )

; after that's done, you end up with something like this:
; show all the terms

; once a transform withstood 10,000 tests, we considered it for...

(slide
 (t-s "Proof" 64))

; we prove that the continuation-passing style transformation preserves meaning

(slide
 c-diagram
 (t "unfolds to")
 ($ "p\\rightarrow^{*}_{cm} v\\Rightarrow\\mathcal{C}[p]\\rightarrow^{*}_{v}\\mathcal{C}[v]"))

; there's a problem with this:
; the transformation abstracts each term so it won't reduce

; change this to 
#;(slide
 ($ "\\mathcal{T}[(e\\,f)]=\\lambda k.(\\mathcal{T}[e]\\,\\lambda e'.(\\mathcal{T}[f]\\,\\lambda f'.((e'\\,f')\\,k)))"))

; we address this problem with our proof strategy

(let ([cmb ($ "E[e]")]
      [cma ($ "E'[e']")]
      [vb ($ "\\mathcal{C}[E[e]]")]
      [va ($ "\\mathcal{C}[E'[e']]")])
  (let ([cm-rr (hc-append 8 cmb ($ "\\rightarrow_{cm}") cma)]
        [v-rr (hc-append 8 vb ($ "\\rightarrow_{v}^{*}") va)])
    (slide
     #:title "Proof strategy"
     (t "Show simulation")
     (linewidth 8 (pin-arrows-lines 24
                                    (vc-append cm-rr (blank 0 128) v-rr)
                                    (list (list (list cm-rr cmb) cb-find (list v-rr vb) ct-find)
                                          (list (list cm-rr cma) cb-find (list v-rr va) ct-find)))))))


(slide
 (para ($ "\\mathcal{C}") " is defined over terms " ($ "e"))
 'next
 (para "Extend " ($ "\\mathcal{C}") " over")
 (item "contexts " ($ "E"))
 (item "context-expression pairs " ($ "(E,e)") " (which we write as " ($ "E[e]") ")")
 'next
 (blank 0 16)
 ($ "\\mathcal{C}[E[e]]=(((\\mathcal{C}[e]\\,\\mathcal{C}[E])\\,\\xi(E))\\,\\mathcal{C}[\\chi(E)])")
 'next
 ($ "\\mathcal{C}[\\bullet[p]]=(((\\mathcal{C}[p]\\,\\mathcal{C}[\\bullet])\\,\\xi(\\bullet))\\,\\mathcal{C}[\\chi(\\bullet)])"))

(slide
 (para "We must simulate")
 (item "context manipulation")
 (item "reduction rules"))

(slide
 #:title "Context manipulation"
 'alts
 (select-them #t vc-append/gap frame/blue
              (item2 ($ "E[(e_0\\,e_1)]\\rightarrow E[(\\bullet\\,e_1)][e_0]"))
              (item2 ($ "E[(\\bullet\\,e_1)][v_0]\\rightarrow E[(v_0\\,\\bullet)][e_1]")))
 (item2 "etc."))

(slide
 #:title "Reduction rules"
 (para ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]"))
 'next
 (item "substitution lemma " ($ "\\mathcal{C}[e[x\\leftarrow v]]=\\mathcal{C}[e][x\\leftarrow\\mathcal{C}[v]]"))
 'next
 (para ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]"))
 'next
 (item "use " ($ "\\xi(E)") " to know correct mark behavior")
 'next
 (para ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']"))
 'next
 (item "by construction of " ($ "\\mathcal{C}[E[e]]"))
 'next
 (para ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]"))
 'next
 (item ($ "\\mathcal{C}[(\\mathrm{ccm})]=\\lambda k.\\lambda f.\\lambda m.(k\\,m)")))

; a2ps --output=proof.ps --pages=41-44 -1 dissertation.dvi
; a2ps --output=appendex.ps --pages=49-72 -1 dissertation.dvi
; after leaving out conclusion/bibliography in TeX dvi generation
; a2ps --output=proof-appendix.ps --pages=41-72 --rows=4 --columns=7 --major=row dissertation.dvi

(slide
 (rb-superimpose (scale (rotate (bitmap "proof-appendix.png") (* 3 (/ pi 2))) 19/64)
                 (inset (t-s "QED" 48) 0 0 20 75)))

(slide
 #:title (para (strike (t "Thesis")) (t "Fact"))
 (para "A CPS-like transformation can transform the" ($ "\\lambda") "-calculus"
       "with continuation marks into the plain" ($ "\\lambda") "-calculus in a"
       "meaning-preserving way."))

(slide
 #:title "Conclusion"
 (para "Now we can correctly and easily add continuation marks to a higher-order"
       "language such as")
 (item "JavaScript")
 'next
 (item "Ruby")
 'next
 (item "Python (sort of)")
 'next
 (item "Haskell")
 'next
 (item "Lisp")
 'next
 (item "Perl")
 'next
 (item "etc."))

#;(slide
 (t "run factorial"))






; now we can easily add continuation marks to a higher-order language, like JavaScript
