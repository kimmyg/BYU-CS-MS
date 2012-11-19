#lang slideshow

; a2ps

(require slideshow/code)
(require (planet jaymccarthy/slideshow-latex:1:1))


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

(define (select-them-inner before? after? combiner f . xs)
  (let ([ys (select-and-collect f xs)])
    (map (λ (xs)
           (apply combiner xs))
         (let ([tail (if after? (append ys (list xs)) ys)])
           (if before? (cons xs tail) tail)))))

(define (select-them before? after? combiner f . xs)
  (map list (apply select-them-inner before? after? combiner f xs)))

(define (underline-them display-first? combiner . items)
  (apply select-them display-first? display-first? combiner underline items))

(define (pin-arrows-lines arrow-size pict locs [prefix empty])
  (if (empty? locs)
      pict
      (match-let ([(list src find-src dest find-dest) (first locs)])
        (let ([result-pict (pin-arrows-line arrow-size pict (append prefix src) find-src (append prefix dest) find-dest)])
          (pin-arrows-lines arrow-size result-pict (rest locs) (cons pict prefix))))))

(define (item2 . args)
  (inset (apply item args) 16 4))


(define c-diagram ($ "\\begin{array}{ccc}p & \\rightarrow_{cm}^{*} & v\\\\\\downarrow_\\mathcal{C} & & \\downarrow_\\mathcal{C}\\\\\\mathcal{C}[p] & \\rightarrow_{v}^{*} & \\mathcal{C}[v]\\end{array}"))

(define (t-s t s)
  (text t (current-main-font) s))

(define (emphasize l inner r)
  (hc-append (colorize (tt l) "gray")
             (colorize (tt inner) "black")
             (colorize (tt r) "gray")))

(define (strike pict)
  (refocus (cc-superimpose pict (linewidth 4 (hline (pict-width pict) (pict-height pict)))) pict))

(slide
 (t "A CPS-like Transformation of Continuation Marks")
 (t-s "Kimball Germane" (* (current-font-size) 3/4))
 (t-s "with Jay McCarthy advising" (* (current-font-size) 3/4)))

; to begin, I will talk about the lambda-calculus

(slide
 #:title "Thesis"
 (para "A CPS-like transformation can transform the" ($ "\\lambda") "-calculus"
       "with continuation marks into the plain" ($ "\\lambda") "-calculus in a"
       "meaning-preserving way."))

; the lambda-calculus is a Turing-complete system of logic that provides a 
; a language model of computation. whereas Turing-machines fix programs and
; operate on data fed into the machine, the lambda-calculus combines code and 
; data into a language term. Turing machine computation is effected by 
; machine state transitions. lambda-calculus computation is effected by 
; language term reduction.

(slide
 #:title (para "The " ($ "\\lambda") "-calculus")
 (t-s "A language model of computation" 48))

; terms in this language are defined inductively

; variables x correspond to mathematical variables in that we don't think of assigning 
; values to variables but rather substituting variables with values

; abstractions \x.e (where e is itself a lambda calculus term) correspond to functions: 
; x is the parameter and e is the body of the function

; applications--the juxtaposition of terms e_0 and e_1-- correspond to function application.
; e_0 represents the function or operator and e_1 represents the argument or operand.

(slide
 #:title (para ($ "\\lambda") "-calculus")
 (para "Terms " ($ "e") " take the form of")
 'alts
 (select-them #t #f vc-append/gap frame/blue
              (item2 "variables " ($ "x"))
              (item2 "abstractions " ($ "\\lambda x.e"))
              
              (item2 "applications " ($ "(e_0\\,e_1)"))
              
              ))

; evaluation of a lambda calculus term is defined recursively

; variables are substituted with their value in the environment

; abstractions evaluate to themselves and capture the values of their free variables 
; from the environment. these values are called closures.

; applications are evaluated by recursively evaluating the e_0 then e_1 and then substituting

(slide
 #:title (para ($ "\\lambda") "-calculus evaluation")
 (para "Evaluation of a term " ($ "e") "depends on its form")
 'alts
 (select-them #t #f vc-append/gap frame/blue
              (item2 "variables " ($ "x") " are substituted with their value in the environment")
              (item2 "abstractions " ($ "\\lambda x.e") " evaluate to closures")
              (item2 "applications " ($ "(e_0\\,e_1)") " are evaluated recursively before substitution")))

(slide
 #:title (para ($ "\\lambda") "-calculus substitution")
 (para "When a term of the form " ($ "(\\lambda x.e' v)") " is encountered in evaluation,"
       "it is" (it "reduced") "by the rule " ($ "(\\lambda x.e' v)\\rightarrow e'[x\\leftarrow v]") ".")
 )

; here is the evaluatation of a nested lambda-calculus term
; like nested arithmetic expressions, subexpressions are evaluated first
; as we proceed deeper into subexpressions, we accumulate computation
; to perform once we have evaluated the current subexpression--this is shown in gray. this is called
; the evaluation context or continuation of the subexpression.

; once we have an abstraction and a value--in this case, a number--we apply the 
; abstraction to the value. this replaces occurrences of the bound variable with the 
; value.

; evaluation proceeds until a value is obtained in an empty context
; this is roughly the extent of evaluation in the lambda-calculus

; fix alignment

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
  (emphasize "    ((λx.λy.y (" "λy.1" " 2)) 3)")
  (emphasize "    ((λx.λy.y (λy.1 " "2" ")) 3)")
  (emphasize "    ((λx.λy.y " "(λy.1 2)" ") 3)")
  (emphasize "        ((λx.λy.y " "1" ") 3)")
  (emphasize "        (" "(λx.λy.y 1)" " 3)")
  (emphasize "            (" "λy.y" " 3)")
  (emphasize "            (λy.y " "3" ")")
  (emphasize "            " "(λy.y 3)" "")
  (emphasize "                " "3" "")))



; continuation marks enhance it

; --

; --

(slide
 (para "Continuation marks allow you to:")
 'alts
 (select-them #t #f vc-append/gap frame/blue
              (item2 "mark the continuation with " ($ "(\\mathrm{wcm}\\,e_0\\,e_1)"))
              (item2 "obtain the current marks with " ($ "(\\mathrm{ccm})"))))

; my thesis is about a transformation from one language to another, specifically, the lambda-
; calculus with continuation marks and the plain lambda-calculus. a formal transformation 
; requires a formal definition of these languages.

(slide
 #:title "Thesis"
 (para "A CPS-like transformation can transform the "
       (underline (hc-append ($ "\\lambda") (t "-calculus with continuation marks")))
       "into the" (underline (t "plain")) 
       (underline (hc-append ($ "\\lambda") (t "-calculus")))
       "in a meaning-preserving way."))

; lambda_v is the plain lambda calculus and is what we have focused on so far. the terms 
; have the familiar form. contexts are defined recursively by the given grammar. the 
; first term represents evaluation of the operator of an application; the second term 
; represents evaluation of the operand. the third, a hole, denote an empty context.
; lambda_v has only one reduction rule, beta reduction or substitution, which defines
; a reduction relation on lambda_v terms denoted by arrow_v.

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

; lambda_cm, the lambda calculus with continuation marks, is an extension of lambda_v. it has
; two additional terms, wcm and ccm, as seen earlier. additional context forms are added which 
; accommodate wcm forms. we add another category to impose a structural constraint duscussed later .

; the substitution rule is inherited from lambda_v. the second rule states that wcm terms take 
; on the value of the body. the third rule states that ccm terms take on the value of chi(E), 
; a function of the context; the chi function conceptually walks the context accumulating a 
; list of the continuation marks. the last rule states that when one wcm form is nested 
; directly within another, the outer one is discarded. the additional contextual category 
; ensures that valid evaluation contexts cannot contain directly nested wcm forms. these 
; four rules define a reduction relation on lambda_cm terms denoted by arrow_cm.

(slide
 #:title (para ($ "\\lambda_{cm}"))
 (para "Terms")
 (para ($ "e =") (cellophane ($ "x\\,|\\,\\lambda x.e\\,|\\,(e\\,e)") 0.75) ($ "|\\,(\\mathrm{wcm}\\,e\\,e)\\,|\\,(\\mathrm{ccm})"))
 (para "Contexts")
 (para ($ "E = (\\mathrm{wcm}\\,v\\,F)\\,|\\,F") (blank 256 0) ;hack
       ($ "F = (\\mathrm{wcm}\\,E\\,e)\\,|") (cellophane ($ "(E\\,e)\\,|\\,(v\\,E)\\,|\\,\\bullet") 0.75))
 (para "Reduction rules " ($ "\\rightarrow_{cm}"))
 (para (cellophane ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 0.75)
       ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") (blank 64 0) ;hack
       ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]")
       ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]"))
       )

; with formal definitions of the source language, lambda_cm, and target language, lambda_v, 
; we can define a transformation.

(slide
 (t-s "Transformation" 64))

; our transformation from lambda_cm terms to lambda_v terms must preserve meaning.
; that is, if a lambda_cm term reduces in a finite number of steps to a lambda_cm value, 
; the transformed term should reduce in a finite number of steps to a corresponding value.
; the diagram captures this property. in other words, the transformation commutes with 
; evaluation.

(slide
 (para "Our transformation " ($ "\\mathcal{C}"))
 ($ "\\mathcal{C}:\\lambda_{cm}\\rightarrow\\lambda_{v}")
 (para "should satisfy")
 c-diagram)

; a transformation must address the essential differences between lambda_v and lambda_cm

; terms in lambda_v have access to their environment or, in other words, the values of 
; free variables within the term.

; terms in lambda_cm have access to their environment...

; emphasize only

; give example re machine compilation

; and context, via the ccm form.

; the only way we can do it 
; this provides some guidance on what the transformation should do: pass contextual 
; information to terms as arguments, thereby introducing it into their environment.

(slide
 #:title "Essential differences"
 'next
 (para "Terms in " ($ "\\lambda_{v}") " have access to their environment")
 'alts
 (list (list (para "Terms in " ($ "\\lambda_{cm}") " have access to their environment"))
       (list (para "Terms in " ($ "\\lambda_{cm}") " have access to their environment and context")))
 'next
 (blank 0 32)
 (para "Intuition")
 (para "Pass contextual information explicitly to terms as arguments, thereby"
       "introducing it into their environment."))

; we must determine what contextual information is essential to preserve meaning.

; for this purpose, our guiding principle is to pass any information the original 
; program could observe. this ensures we have at least the necessary information 
; to preserve meaning.

; this certainly includes the list of current continuation marks defined by chi(E)

(slide
 (para "What information do we pass?")
 'next
 (para "Anything the original " ($ "\\lambda_{cm}") " program could observe")
 'next
 (item "the list of current continuation marks " ($ "\\chi(E)"))
 )

; we effect this information passing by adding a formal parameter to every function and 
; argument to every call site representing the list of current continuation marks. the 
; payoff of this invasive approach is a succinct definition of ccm.
; this mark-passing style approach is common and is similar in nature to continuation-
; passing style, store-passing style, and security-passing style

(slide
 #:title "Mark-passing style"
 ($ "\\mathcal{C}[\\lambda x.e]=\\lambda\\mathit{marks}.\\lambda x.\\mathcal{C}[e]")
 ($ "\\mathcal{C}[(e\\,f)]=((e\\,\\mathit{marks})\\,(f\\,\\mathit{marks}))\\,\\mathit{marks})")
 ($ "c(ccm)=\\lambda\\mathit{marks}.marks"))
 

; the mark-passing style approach must respect each reduction rule to preserve meaning

; each additional formal parameter introduced by this approach is met with an argument, so 
; it preserves the substitution rule

; marks don't appear in the tail position (get returned) of the term

; the approach is designed to explicitly preserve this rule, which it does simply and directly

; this is controversial but necessary (peeps might want v and v')
; stack traces in Racket
; tail calls not preserved  

; this rule, which defines the collapse of an outer wcm form under a certain condition, is 
; /not/ preserved by this approach. this rule concerns the space safety of the lambda_cm
; language. without it, programs that require constant space in lambda_cm can require 
; unbounded space in lambda_v.

(slide
 #:title (para ($ "\\chi(E)") " passing accounts for")
 'alts
 (list (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4)))
       (list (para (frame/blue (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4))
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (frame/blue (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4)) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (frame/blue (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4))
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (frame/blue (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4))))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (inset (strike ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]")) 16 4)))))
       
; this rule captures the behavior of marks placed in tail position: a mark so placed 
; overwrites the previous mark there, if it exists. this makes tail position observable, 
; albeit indirectly.

(slide
 #:title "Tail position"
 (para "Marks placed in tail position annotate the newest part"
       "of the continuation, overwriting previous marks (if they"
       "exist).")
 )

; it seems that there is another piece of essential information...

; a flag which encodes tail position information which we define over contexts E as xi.
; as a mnemonic device, note that xi looks like a curly pig tail and is used to denote 
; the tail position information

(slide
 #:title "What information do we pass?"
 (para "Anything the original " ($ "\\lambda_{cm}") " program could observe:")
 (item "the list of current continuation marks " ($ "\\chi(E)"))
 'alts
 (list (list (item "???"))
       (list (item "a flag reflecting tail position " ($ "\\xi(E)"))))
 )

; the addition of tail position information does not affect the first three rules 
; (significantly)...

; and allows us to preserve the fourth rule.

(slide
 #:title (para ($ "\\chi(E)") " and " ($ "\\xi(E)") " passing account for")
 'alts
 (list (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (inset (strike ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]")) 16 4)))
       (list (para (inset ($ "E[(\\lambda x.e'\\,v)]\\rightarrow E[e'[x\\leftarrow v]]") 16 4)
                   (inset ($ "E[(\\mathrm{wcm}\\,v\\,v')]\\rightarrow E[v']") 16 4) (blank 64 0) ;hack
                   (inset ($ "E[(\\mathrm{ccm})]\\rightarrow E[\\chi(E)]") 16 4)
                   (frame/blue (inset ($ "E[(\\mathrm{wcm}\\,v\\,(\\mathrm{wcm}\\,v'\\,e))]\\rightarrow E[(\\mathrm{wcm}\\,v'\\,e)]") 16 4))))))

; with the four rules preserved in theory, we can formally verify that the transformation 
; preserves meaning. true verification, formal proof, is an expensive process and to 
; prevent undue expense, we would like to have some confidence that our transformation is 
; correct before we embark on a proof. randomized testing is a good way to do this.

(slide
 (t-s "Verification" 64))

; redex is a domain-specific language for exploring language semantics which we used to 
; perform automated randomized testing. the testing process proceeds like this:

; first, we generate a random lambda_cm program. redex produces this automatically from the 
; grammar of the language.

; second, we reduce the program and transform the result. this process follows the path 
; along the top and right sides of the diagram.

; third, we transform the program and reduce the result. this process follows the path 
; along the left and bottom sides of the diagram.

; fourth, we ensure that they ended up at the same place. if they didn't, we have a 
; counterexample and know the transformation is not correct. if they did, we repeat the 
; process, gradually increasing the size of the programs.

; our experience was that, while the approach of the transformation was straightforward, 
; it had many technicalities that were refined in the testing process. it would have 
; been expensive and difficult to discover these subtleties in the proving process.

; be specific, add 10,000 times, mention Jay's paper

(slide
 #:title "Testing with Redex"
 (para "Process")
 'alts
 (map list
      (map (λ (pict) (hc-append pict c-diagram))
           (parameterize ([current-para-width (- (current-para-width) (pict-width c-diagram))])
             (select-them-inner #t #t vc-append/gap frame/blue
                                (inset (para "1. generate a random " ($ "\\lambda_{cm}") " program") 16 4)
                                (inset (para "2. reduce according to " ($ "\\rightarrow_{cm}") " and transform the result") 16 4)
                                (inset (para "3. transform the program and reduce according to " ($ "\\rightarrow_{v}")) 16 4)
                                (inset (para "4. compare the results") 16 4)))))
 )

; with some confidence in the transformation, we can prove the meaning-preservation property.
; we unfold the commutativity diagram to obtain our main theorem of correctness.
; <read theorem statement>

(slide
 c-diagram
 (t "unfolds to")
 ($ "p\\rightarrow^{*}_{cm} v\\Rightarrow\\mathcal{C}[p]\\rightarrow^{*}_{v}\\mathcal{C}[v]"))

; however, a transformation over terms alone does not allow us to reason about the process 
; of evaluation--only the end result. it would be helpful to be able to reason about each 
; step of evaluation.

; say more

(let ([cmb ($ "E[e]")]
      [cma ($ "E'[e']")]
      [vb ($ "\\mathcal{C}[E[e]]")]
      [va ($ "\\mathcal{C}[E'[e']]")])
  (let ([cm-rr (hc-append 8 cmb ($ "\\rightarrow_{cm}") cma)]
        [v-rr (hc-append 8 vb ($ "\\rightarrow_{v}^{*}") va)])
    (slide
     (vc-append cm-rr (blank 0 128) (ghost v-rr)))))

; so, we extend C over expressions and context-expression pairs

; this second definition is simple, and utilizes all the essential terms

(slide
 (para "Extend " ($ "\\mathcal{C}") " over")
 (item "contexts " ($ "E"))
 (item "context-expression pairs " ($ "E[e]"))
 'next
 (blank 0 16)
 ($ "\\mathcal{C}[E[e]]=(((\\mathcal{C}[e]\\,\\mathcal{C}[E])\\,\\xi(E))\\,\\mathcal{C}[\\chi(E)])"))

; by extending C as we have, we can put terms (unclear) in correspondence. this allows us to show 
; that the transformed terms simulate the original terms by taking some finite number 
; of evaluation steps for each step of the original term.

(let ([cmb ($ "E[e]")]
      [cma ($ "E'[e']")]
      [vb ($ "\\mathcal{C}[E[e]]")]
      [va ($ "\\mathcal{C}[E'[e']]")])
  (let ([cm-rr (hc-append 8 cmb ($ "\\rightarrow_{cm}") cma)]
        [v-rr (hc-append 8 vb ($ "\\rightarrow_{v}^{*}") va)])
    (slide
     (linewidth 8 (pin-arrows-lines 24
                                    (vc-append cm-rr (blank 0 128) v-rr)
                                    (list (list (list cm-rr cmb) cb-find (list v-rr vb) ct-find)
                                          (list (list cm-rr cma) cb-find (list v-rr va) ct-find)))))))

; there are two classes of reduction steps that we must simulate: context manipulation and 
; bonafide reduction rules.

(slide
 (para "We must simulate")
 (item "context manipulation")
 (item "reduction rules"))

; context manipulation 

(slide
 #:title "Context manipulation"
 'alts
 (select-them #t #t vc-append/gap frame/blue
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
