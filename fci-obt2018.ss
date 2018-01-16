#lang at-exp slideshow
(require slideshow
         slideshow/code
         slideshow/code-pict
         slideshow/text
         scheme/gui/base
         slideshow/pict
         pict/color)

#|
Climbing Up the Semantic Tower - at Runtime
First-class Implementations

presentation at Off the Beaten Track, 2018-01-13

For development, try:
  slideshow --comment-on-slide fci-obt2018.ss

NB: Quit with Alt-Q

On demo day, try:
  slideshow --preview --comment --monitor 1 fci-obt2018.ss
On stumpwm, you can go to Frame 1 (fnext, C-t o),
pull the presentation window (pull, C-t C-6 or such),
then go back to Frame 0 (C-t o),
pull the preview (C-t C-5 or such) and comment (C-t C-7 or such),
and use split view (C-t s, which can later be cancelled with C-t Q).

Output slides for this document may be found at:
  http://fare.tunes.org/files/cs/fci-obt2018.pdf

This document is based on my PhD thesis:
http://fare.tunes.org/files/tmp/phd/thesis.pdf

This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(set-margin! 0)

;;; Definitions

(define (color-named name) (send the-color-database find-color name))
(define *blue* (color-named "blue"))
(define *red* (color-named "red"))
(define ~ @t{     })
(define (title-style x) (text x (cons 'bold 'default) 34))
(define (url x) (colorize (tt x) *blue*))
(define (L . x) x)
(define (xlide #:title (title "") . body)
  (apply slide #:title (title-style title) body))

;; Convert one digit to a roman numeral, given strings for one unit, five units and ten units.
(define (roman-numeral<-digit digit i v x)
  (case digit
    [(0) ""]
    [(1) i]
    [(2) (string-append i i)]
    [(3) (string-append i i i)]
    [(4) (string-append i v)]
    [(5) v]
    [(6) (string-append v i)]
    [(7) (string-append v i i)]
    [(8) (string-append v i i i)]
    [(9) (string-append i x)]
    [else (error "incorrect digit ~a" digit)]))

(define (roman-numeral<-integer n)
  ;; NB: works for integer from 1 to 3999
  (when (or (not (integer? n)) (< n 1) (> n 3999))
    (error "I cannot convert ~s to a roman numeral" n))
  (let* ((units (modulo n 10))
         (n/10 (/ (- n units) 10))
         (tens (modulo n/10 10))
         (n/100 (/ (- n/10 tens) 10))
         (hundreds (modulo n/100 10))
         (thousands (/ (- n/100 hundreds) 10)))
    (string-append
     (roman-numeral<-digit thousands "M" "" "")
     (roman-numeral<-digit hundreds "C" "D" "M")
     (roman-numeral<-digit tens "X" "L" "C")
     (roman-numeral<-digit units "I" "V" "X"))))

(define (LaTeX-name x)
  "LaTeX name for a symbol"
  (call-with-output-string
   (λ (out)
     (call-with-input-string
      (symbol->string x)
      (λ (in)
	(letrec
	    ((loop
	      (λ (upcase-next?)
		(let ((c (peek-char in)))
		  (cond
		   ((eof-object? c))
		   ((char-alphabetic? c)
		    (read-char in)
		    (display ((if upcase-next? char-upcase char-downcase) c) out)
		    (loop #f))
		   ((char-numeric? c)
		    (display #\x out)
		    (display (roman-numeral<-integer
			      (string->number
			       (bytes->string/latin-1
				(first
				 (regexp-match #rx"^[0-9]+" in)))))
			     out)
		    (loop #f))
		   (else
		    (read-char in)
		    (loop #t)))))))
	  (loop #f)))))))

(define-syntax (fig stx)
  (syntax-case stx ()
    [(_ id)
     #`(figure '#,(syntax->datum #'id))]
    [(_ id args...)
     #`(figure '#,(syntax->datum #'id) args...)]))

(define (figure name . args)
  (let ((b (bitmap (string-append "resources/pic/fig-" (LaTeX-name name) ".png"))))
    (scale b (* 0.333333333333333 (if (empty? args) 1 (first args))))))


;;; Slides ;;;

(xlide
 #:title "Climbing Up the Semantic Tower — at Runtime"
 ~
 @bt{First Class Implementations}
 ~ ~
 (para #:align 'center @t{François-René Rideau,} @it{Metafore Hyperpost})
 ~ ~
 @t{Off the Beaten Track 2018, 2018-01-13}
 @url{https://github.com/fare/climbing}
 ~
 @para{@it{Presented at BostonHaskell (2016), rejected at SNAPL 2017}}
 (comment "\
Hi, I am François-René Rideau, and today I'm here to tell you about

First-class Implementations.
"))

#;(xlide
 #:title "This Talk"
 @para{At the Ecole Normale, Patrick Cousot was teaching}
 @para[#:align 'center]{@it{Abstract Interpretation}}
 ~
 @para{All I was interested in was the opposite direction:}
 @para[#:align 'center]{@it{Concrete Implementation}})

(xlide
 #:title "The Basic Intuition"
 @para[#:align 'center]{"Good programmers can and must mentally zoom in and out
of levels of abstraction"}
 @comment{understanding none possibly contradicts the other ones}
 ~
 @para{What if not just mentally but programmatically?}
 @comment{Then we could reconcile semantics and reflection,
               topics usually considered mutually exclusive}
   @para{- semantics: formally reason about these levels}
   @para{- reflection: navigate and transform them @it{at runtime}}
 ~
 @comment{Amplification: Achieve more without being as good})

(xlide
 #:title "Plan"
 @para[@para{Formalizing Implementations} ~]
 ~
 @para[@para{First-Class Implementations} ~]
 ~
 @para[@para{Principled Reflection} ~]
 ~
 @para[@para{Reflective Architecture} ~])

(xlide
 #:title "The Take Home Points"
 @;Implementation is co-(Abstract Interpretation)}
 @para{@para{Formalizing Implementations: @it{Categories!}}
 @para{@it{Observability: Neglected key concept --- safe points}}}
 ~
 @; First-class: the opposite of magic}
 @; First-class safe points (= PCLSRing!)}
 @para{@para{First-Class Implementations @it{via Protocol Extraction}}
 @para{@it{Explore the Semantic Tower --- at runtime!}}}
 ~
 @para{@para{Principled Reflection: @it{Migration}}
 @para{@it{Natural Transformations generalize Instrumentation}}}
 ~
 @para{@para{Reflective Architecture: @it{3D Towers}}
 @para{@it{Social Implications: Platforms, not Applications}}})

(xlide
 (title-style "Implementations, informally"))

(xlide
 #:title "You want a program"
 @fig[simple-tower-1])

(xlide
 #:title "You have a PC"
 @fig[simple-tower-2])

(xlide
 #:title "You write an implementation"
 @fig[simple-tower-3])

(xlide
 #:title "In the best possible language"
 @fig[simple-tower-4]
 @comment{But that's not what the machine provides})

(xlide
 #:title "The language itself has an implementation"
 @fig[simple-tower-5])

(xlide
 #:title "Specific dialects, implementations, versions..."
 @fig[simple-tower-6])

(xlide
 #:title "Compiling is hard, use an IR..."
 @fig[simple-tower-7])

(xlide
 #:title "Programming is hard, use a DSL..."
 @fig[simple-tower-8])

(xlide
 #:title "What do you mean, x86?"
 @fig[simple-tower-9])

(xlide
 #:title "There is no bottom!"
 @fig[simple-tower-10])

(xlide
 #:title "Always finer divisions"
 @fig[simple-tower-10])

(xlide
 #:title "Implementations, informally"
 @para[#:align 'center]{@it{Relate} computations}
 @para[#:align 'center]{Not just two, @it{towers} of them}
 ~ ~
 @para[#:align 'center]{@it{Reason} about implementations?}
 @para[#:align 'center]{In a composable way}
 ~
 )

(xlide
 #:title "Formalization Challenges"
 @para{First, formalize computations}
 @para{- Few implementations are formalized}
 @para{- Those that are use incompatible formalisms}
 ~
 ;;'next
 @para{Then, unify these formalisms}
 @para{- What suitable relation between computations?}
 @para{- What composable properties for relations?})

(xlide
 #:title "Unify Existing Semantic Models"
 @para{Operational Semantics (Small Step)}
 @para{Operational Semantics (Big Step)}
 @para{Labeled Transition Systems}
 @para{Term Rewriting, Rewrite Logic}
 @para{Modal Logic, Hoare Logic, Refinement}
 @para{Partial Order}
 @para{Abstract State Machines}
 @para{...}
 @para{even Denotational Semantics})

(xlide
 (title-style "Formalizing Implementations")
 (comment "\
First, let's formalize what it means to be an implementation.

And for a good starting point, let's pick something familiar, Abstract Interpretation.
"))

(xlide
 #:title "Category Theory"
 @para{A category: nodes, arrows, composition structure}
 @para{Mathheads: node=object arrow=(homo)morphism}
 @para{Higher: categories of arrows, of categories}
 ~
 ;;'next
 @para{Simple universal core, unlimited abstraction}
 @para{Theorems "for free", Types}
 @para{No low-level punning as in Set Theory})

(xlide
 #:title "Categories"
 @fig[functor-in-category]
 @fig[ar-ka]
 'next
 @fig[ar-ki]
 @fig[ar-kp]
 (comment "
 points and arrows.

 under: category it's in
 over: name of the function,
   or abstraction thereof (observable effect)
"))

(xlide
 #:title "Computation as Categories"
 @para{Nodes: states of the computation}
 @para{Arrows: transitions between states, traces}
 ~
 @fig[operational-semantics]
 ~
 @para{Figure conventions:}
 @para{- Computation time goes left to right}
 @para{- Abstraction goes from bottom to top}
 @para{- (Effect) Label above, (sub)category below}
 (comment "
 points and arrows.

 x, y, z: states
 S: category or subset thereof
 S_e: subset of category with some effects
 f: an effect (or an abstract interpretation thereof)})

 phase space, trajectory
"))

(xlide
 #:title "(Abstract) Interpretation"
 @fig[implementation-1]
 (comment "\
Abstract Interpretation is basically a functor \
from a concrete computing system C to an abstract computing system A. \
Functor basically means that it preserves the structure of computation. \
For the sake of abstract interpretation, this structure is usually described \
in terms of denotational semantics.

In practice, C is a computing system for humans to program in; \
it can express arbitrary computations. \
A is an \"abstraction\" of it, mapping each program to a \"type\" \
that summarizes some of its properties. \
Indeed, any abstract interpretation is isomorphic to a static type system, \
though not necessarily with a type system you'd use to annotate programs by hand. \
The abstract interpretation is necessarily forgetful, losing information \
and blurring some distinctions considered irrelevant or too hard to compute \
so that it may be computable in finite time and yield useful analyses \
based on which to analyse and transform a program.

Notice I the arrow is dashed. \
That's because I haven't specified yet exactly what it means \
and it might not be an actual homomorphism in the category being considered. \
For instance, if you consider a type analysis of \
a programming language with a dynamic semantics defined without types, \
the analysis might be functorial on the _subset_ of well-typed programs, \
but not when you consider the larger set of programs including those that have static type errors. \
In other words, it's a _partial function_.

Concrete Implementation is something completely different.
 "))

(xlide
 #:title "(Concrete) Implementation"
 @fig[implementation-1]
 (comment "\
It's actually the very same diagram. \
But looking at it in a very different way, with very different applications.

Here, the _abstract_ system is usually the computing system for humans to program in; \
the concrete system is a lower-level system such as a virtual machine, \
or whatever the CPU architecture and operating system provide.

A concrete implementation is also written as an arrow from C to A, and not the other way around, \
because it is still the direction that is deterministic and functorial: \
for every abstract computation state a, there may be many potential corresponding \
concrete computation states c; \
but for every concrete computation state c, \
there is (at most) one abstraction computation state a that corresponds to it.

And there again, it's a dashed line, because this functor is also a _partial function_, \
defined only on a subset of the states in C. \
Not all states of the concrete machine make sense in terms of the abstract machine.
"))

(xlide
 #:title "Concrete Implementation vs Abstract Interpretation"
 @para{Dynamic (Runtime) vs Static (Compile-time)}
 @para{Operational Semantics vs Denotational Semantics}
 ~
 @para{Downward (concrete) vs Upward (abstract)}
 @para{Co-functorial vs Functorial}
 @para{Noisy vs lossy}
 @para{Non-deterministic vs deterministic}
 (comment "\
There are two main differences between \
Concrete Implementation vs Abstract Interpretation, \
each with many aspects.

First, Concrete Implementation is interested in capturing \
the complete dynamic semantics of the programming language at runtime, \
not just an approximate static semantics. \
For that, we will focus on the Operational Semantics of programs, \
including such runtime data as control stacks and data heaps, \
maybe even addresses and pointer values; \
we won't be satisfied with type approximations or Denotational Semantics.

Second, even though these arrows obey the same rules as for Abstract Interpretation, \
we're typically exploring them downward, the direction opposite to where the arrow is pointing, \
to where it's functorial, to where \
it's deterministic, simple, simplifying things and losing information. \
Instead, we're going to add a lot of noisy information that matters to implementations \
but not to abstract semantics.
"))

(xlide
 #:title "Partiality" ;; slide added on 2017-09-08
 @fig[partiality]
 (comment "
In this diagram, a computation in a C-like language is implemented by computation in a stack machine language. The abstract computation outputs a string, then another one. The abstract operational semantics is defined in terms of a continuation k and the effect of an output string. The concrete program directly writes the concatenation of the two strings. The concrete operational semantics is defined in terms of a data stack s, a continuation k, and the effect of an output string. The abstract state where one substring only is output is not implemented. The concrete state where a string is pushed to the stack is not represented. Yet the concrete computation reaches an end state which can be interpreted as having had all the specified effects of the abstract computation.
"))

(xlide
 #:title "Partial Functions (1)"
 @fig[implementation-1]
 (comment "
most computation states and transitions
are intermediate states with no direct meaning
in the abstract computation

the proverbial non-atomic transfer of resources between two owners;

only @emph{observable} concrete states can be interpreted
as having an abstract @emph{meaning}.

Partiality allows discrete computations to be implemented with
continuous computations, infinite ones with finite ones,
the non-deterministic with the deterministic, etc., and vice-versa.
"))

(xlide
 #:title "Partial Functions (2)"
 @fig[implementation-2]
 (comment "
In general the nodes of a computation encode dynamic execution state such as
registers, bindings, mutable store, call stacks,
messages sent and received, etc.
An implementation is injective;
it must distinguish the states it implements, and cannot lose that information,
though it can add implementation details and drop alternatives not taken.
Conversely, interpretations may lose information,
introduce approximations, or add alternatives --- and indeed include
static analysis, type systems and abstract interpretation, that do.
"))

(xlide
 #:title "Partial Functions (3)"
 @fig[implementation-3]
 (comment "
"))

(xlide
 #:title "Deduction"
 @fig[implementation-4]
 ~
 ~
 (comment "
"))

(xlide
 #:title "Observable State"
 @fig[observable-state-1]
 'next
 ~
 @it{o = c}
 (comment "
"))

(xlide
 (title-style "Properties of Implementations")
 (comment "\
"))

(xlide
 #:title "Soundness"
 @fig[soundness]
 (comment "
"))

(xlide
 #:title "Totality"
 @fig[totality]
 (comment "
"))

(xlide
 #:title "Completeness"
 @fig[completeness]
 (comment "
"))

(xlide
 #:title "Advance Preservation"
 @fig[advance-preservation]
 (comment "
"))

(xlide
 #:title "Liveness"
 @fig[liveness]
 (comment "
"))

(xlide
 #:title "Strong Liveness"
 @fig[strong-liveness]
 (comment "
"))

(xlide
 #:title "Composability"
 @fig[implementation-composition]
 (comment "
"))

(xlide
 #:title "Composability"
 @fig[decomposing]
 (comment "
Haskell: deforestation of nano-pass compiler implementations!
"))

(xlide
 #:title "Composability"
 @fig[composing-up]
 (comment "
"))

(xlide
 #:title "Composability"
 @fig[composing-down]
 (comment "
"))

(xlide
 #:title "Observability (aka PCLSRing)"
 @fig[observability]
 ~
 (comment "
"))

(xlide
 #:title "Observability (aka PCLSRing)"
 @fig[observability-2]
 @para[#:align 'center]{... not composable!}
 (comment "
"))

(xlide
 #:title "Observability + Completeness"
 @fig[observability-3]
 @para[#:align 'center]{Composable!}
 (comment "
"))


(xlide
 (title-style "II. First-class Implementations")
 (comment "\
Now that we have identified what we mean by 'Implementation', \
and the need for some of its properties to be exposed to programmers, \
let's propose a simple protocol based on this notion and its properties.
"))

(xlide
 (title-style "II.1 Protocol Extraction"))

(xlide
 #:title "Protocol: Categories (in Agda)"
 ;; ⇢ s ⤑ s ⇢ s ⟿ s ⇝ s ⇾ s ⟶ s → s ⇒ s ⟹ s ⇨ s ➡ s ⮕ s 🡒 s ⇛ s ⇀ s
 @code[
record Category ... : Set ... where ...
\ \ field
\ \ \ \ Obj : Set ...
\ \ \ \ _⇒_ : Rel Obj ...
\ \ \ \ id  : ∀ {A} → (A ⇒ A)
\ \ \ \ _∘_ : ∀ {A B C} → (B ⇒ C) → (A ⇒ B) → (A ⇒ C)
\ \ ...
]
 @para[#:align 'left]{Showing fields with computational content}
 @para[#:align 'left]{Many more fields for logical specification})

(xlide
 #:title "Protocol: Operational Semantics"
 @code[
record OpSem ... (C : Category ...) : Set ... where
\  open Category
\  field
\  \  run :: s ⤏ Arr s
\  \  done :: s ⟶ Bool]
 ~
 @para[#:align 'left]{Usual functions: @tt{⟶}}
 @para[#:align 'left]{Effectful functions: @tt{⤏} (non-det)}
 @comment{partial non-deterministic functions, a.k.a relations, but with a functional notation})

(xlide
 #:title "Protocol: Operational Semantics"
 @code[
record OpSem ... (C : Category ...) : Set ... where
\  open Category
\  field
\  \  run :: s ⤏ Arr s
\  \  done :: s ⟶ Bool
\  \  eval :: s ⤏ Arr s
\  \  advance :: s ⤏ Arr s]
~ ~)

(xlide
 #:title "Protocol: Implementation"
 @code[
    class Impl a c where
    \ \ interpret :: c ⤏ a
    \ \ interpretArr :: (Arr c) ⤏ (Arr a)]
 ~
 @para{So far, a (partial) functor from @tt{c} to @tt{a}}
 @para{Arr = pirate sound = functorial map})

(xlide
 #:title "Protocol: Totality"
 @fig[totality]
 ~
 @code[implement :: a ⤏ c])

(xlide
 #:title "Protocol: Completeness"
 @fig[completeness]
 ~
 @code[implementArr :: c ⟶ (Arr a) ⤏ (Arr c)]
 (comment "
"))

(xlide
 #:title "Protocol: Liveness"
 @fig[liveness]
 ~
 @code[advanceInterpretation :: c ⤏ Arr c]
 (comment "
"))

(xlide
 #:title "Protocol: Observability (PCLSRing)"
 @fig[observability]
 ~
 @code[safePoint :: c ⤏ Arr c]
 @code[safeArrow :: Arr c ⤏ Arr c]
 (comment "
In some cases, could return c.
"))

(xlide
 #:title "Reified vs Reflected Evaluation"
 @para{Reified:}
 @code[\  eval :: s ⤏ Arr s]
 @para{  Only effect is non-determinism}
 ~
 ~
 @para{Reflected:}
 @code[\  eval! :: s ⤏ s \  \ ]
 @para{  Arbitrary side-effects}
 (comment "XXXXXXX
...
"))

(xlide
 #:title "Runnable vs Observable Protocols"
 @para{Reflection:}
 @code[\  perform :: s ⤏ m
\  performArr :: (Arr s) ⟶ m ⤏ m \  \  \  ]
 @para{first-class semantics runnable as machine state}
 ~
 @para{Reification:}
 @code[\  simulate :: m ⤏ s
       \  simulateArr :: m ⟶ (m ⤏ m) ⤏ Arr s]
 @para{machine state observable as first-class semantics}
 (comment "XXXXXXX
...
"))

(xlide
 #:title "Lifting Reflection and Reification Protocols"
 @para{If you can @tt{implement} @tt{a} with @tt{c}:}
 @code[
a.perform an =
\  c.perform (implement an)
a.performArr aa state =
\  c.performArr
\  \  (implementArr (c.simulate state) aa)
\  \  state
\ 
a.simulate state =
\  interpret (c.simulate m)
a.simulateArr state change =
\  interpretArr
\  \  (safeArrow
\  \  \  (c.simulateArr state change))])

(xlide
 #:title "Lifting Evaluation Protocols"
 @para{If the implementation is live, observable:}
 @code[
a.run an =
\  interpretArr
\  \  (safePoint (c.run (implement an)))
a.advance an =
\  interpretArr
\  \  (advanceInterpretation (implement an))]
 (comment "XXXXXXX
...
"))


(xlide
 (title-style "II.2 The Semantic Tower"))

(xlide
 #:title "Compilation (1)"
 @fig[compilation-1]
 ~
 ~
 @code[implement :: (Impl a c) ⇒ a ⤏ c]
 (comment "
"))

(xlide
 #:title "Compilation (2)"
 @fig[compilation-2]
 ~
 @code[
interpret :: (Impl a s) ⇒ s ⤏ a
implement :: (Impl a c) ⇒ a ⤏ c]
 (comment "
"))

(xlide
 #:title "Compilation (3)"
 @fig[compilation-3]
 @code[
u :: OpSem -- specify up to what rewrites
interpret :: (Impl u s) ⇒ s ⤏ u
implement :: (Impl u c) ⇒ u ⤏ c]
 (comment "
"))

(xlide
 #:title "Static Type Systems"
 @fig[abstract-interpretation]
 ~
 @para{Subject reduction: @it{T} contains no exomorphisms}
 (comment "
"))

(xlide
 #:title "Aspect-Oriented Programming (1)"
 @fig[aop-1]
 ~
 ~
 (comment "
"))

(xlide
 #:title "Aspect-Oriented Programming (2)"
 @fig[aop-2]
 'next
 ~
 @para{Constraint Logic Meta-programming!}
 (comment "
"))

(xlide
 #:title "Semantic Tower"
 @fig[semantic-tower]
 (comment "
"))

(xlide
 #:title "The Tower is not Linear"
 @fig[semantic-tower-2 .75]
 (comment "
"))

(xlide
 #:title "Refactoring"
 @fig[refactoring]
 (comment "
"))

(xlide
 #:title "Developing"
 @fig[developing]
 (comment "R vs D ???? XXXX
"))

(xlide
 (title-style "III. Principled Reflection")
 (comment "\
What do the previous primitives enable us to easily do that was otherwise difficult?
"))

(xlide
 (title-style "III.1 Migration"))

(xlide
 #:title "Migration"
 @fig[migration]
 (comment "
"))

(xlide
 #:title "When your hammer is Migration..."
 @para{Process Migration}
 @para{Garbage Collection}
 @para{Zero Copy Routing}
 @para{Dynamic Configuration}
 @para{JIT Compilation}
 @para{etc.}
 (comment "
"))

(xlide
 #:title "Requirement: Full Abstraction"
 @para{Computations have a clear opaque bottom:}
 @para{1- It's perfectly clear what the bottom is}
 @para{2- The bottom is totally opaque}
 ~
 @para{Indeed, what's below can change at runtime!}
 @para{Alternatively, include what's "below"}
 @para{The language or system must explicitly support that}
 (comment "
"))

(xlide
 #:title "Migration (Optimized)"
 @fig[migration-opt]
 (comment "
"))

(xlide
 #:title "Migration (Implemented)"
 @fig[migration-opt-2]
 (comment "
"))

(xlide
 #:title "Migration (Factored out)"
 @fig[migration-opt-3]
 (comment "
"))

(xlide
 #:title "Fruitful change in Perspective"
 @para{Correctness}
 @para{Dynamism}
 @para{Retroactivity}
 @para{Composability}
 @para{Predictable Cost-Reduction}
 (comment "
"))

(xlide
 #:title "Migration Tower"
 @fig[migration-tower]
 (comment "
"))

(xlide
 #:title "Migration Control"
 @para{Internal: automatic change in representation}
 ~
 @para{External: parameters under user control}
 ~
 @para{One man's internal is another man's external…}
 ~
 @para{Need an Architecture for migration control}
 (comment "
"))

#|
(xlide
 #:title "Optimistic Evaluation"
 @fig[optimistic-evaluation]
 (comment "
"))
|#

(xlide
 (title-style "III.2 Natural Transformations of Implementations"))

(xlide
 #:title "Instrumentation"
 @para{Tracing, Logging, Stepping, Profiling}
 @para{Omniscient debugging, Comparative Debugging}
 @para{Code and Data Coverage}
 @para{Resource Accounting, Access Control}
 @para{Parallelization, Optimistic Evaluation}
 @para{Orthogonal persistence}
 @para{Virtualization}
 @para{Optimizations}
 (comment "
"))

(xlide
 #:title "Natural Transformation"
 @para{Twist: @it{dual} of nat. transf. on @it{dual} of (partial) funct.}
 ~
 @para{Automatic Instrumentation}
 @para{Universal transformations}
 @para{Composable transformations}
 @para{Amenable to formal reasoning}
 ~
 @para{Open problem, but promiseful approach}
 (comment "
Example: system with one node and infinite loop
The trace counts the number of times we went through the loop,
and distinguishes nodes by count; but they all map to the same node.
"))

(xlide
 (title-style "IV. Reflective Architecture")
 (comment "\
Assuming that we have a system with the previously-mentioned primitives, \
how may we better factor our software systems?
"))

(xlide
 (title-style "IV.1 Runtime Architecture"))

(xlide
 #:title "Runtime Architecture"
 @para{Development Platform (Emacs, IDE, ...)}
 ~
 @para{User Interface Shell}
 ~
 @para{Operating System}
 ~
 @para{Distributed and Virtualized Application Management})

(xlide
 #:title "Every Program has a Semantic Tower"
 @para{Semantics on top + Turtles all the way to the bottom}
 ~
 @para{Top specified by User, bottom controlled by System}
 ~
 @para{For the PLs your build, those you use}
 ~
 @para{Static or dynamic control})

(xlide
 #:title "Every Tower has its Controller"
 @para{Runtime Meta-program, Shared (or not)}
 ~
 @para{Virtualization: control effects, connect I/O}
 ~
 @para{Reflective Tower of Meta-programs}
 (comment "Controlling with ptrace / with a virtual machine monitor")
 ~
 @para{New meta dimension: Puppeteers all the way back!}
 (comment "There again, users specify the 'top' of that dimension"))

(xlide
 #:title "Implicit I/O"
 @code[Input :: tag -> IO indata
       Output :: tag -> outdata -> IO ()]
 ~
 @para{Handled by controller}
 (comment "
Arbitrary from the point of view of the program.

Gets 'Interesting' with dependent types!")
 (comment "
Security: Can leak information through side channel attack. \
We call that Logging.

Weird protocols could be used to do input and output. \
Or then again, not so weird protocols, if you want.
")
 ~
 @para{Virtualization of effects at language level}
 (comment "
See extensible effects, etc.")
 ~
 @para{Dynamically reconfigurable})

(xlide
 (title-style "IV.2 Architectural Benefits"))

(xlide
 #:title "Performance: Dynamic Global Optimization"
 @para{When configuration changes, migrate}
 ~
 @para{Optimize the current configuration}
 ~
 @para{Minimize encoding, Zero copy}
 ~
 @para{Skip unobserved computations}
 (comment "\
   Turn eager into lazy: Haskellers should love.

   Reminds me of this story of a man who uploads himself into a computer, then \
   disconnects the simulation from the rest of the world..."))

(xlide
 #:title "Simplicity: Separate program and metaprogram"
 @para{Example: File selector, UI, etc.}
 ~
 @para{Evolve, Distribute, Share, Configure separately}
 ~
 @para{Separate Capabilities, Semantics}
 ~
 @para{Robustness, Security: Smaller Attack Surface}
 (comment "
stream rather than filename, etc... choose your abstraction!
"))

(xlide
 #:title "Not Just a Library"
 (comment "
The semantics of a library is imported as part of that of the program. \
Not so with the semantics of a metaprogram, that remains resolutely separate. \
A program can run with very different metaprograms, \
but only with nearly identical libraries. \
Note that if you adjust the program specification to include lower layers of implementation, \
then what was a metaprogram that only intercepts some I/O while respecting scope \
becomes a library; but implementation metaprograms in general can do more than that.

A metaprogram can be modified at runtime, whereas the semantics of a library is fixed \
at compile-time (for static libraries) or load-time (for dynamic libraries).

A metaprogram can see all about the program, \
but an implementation metaprogram can change nothing about it, \
only refine, and control I/O. \
A library can only see what is in its scope, and arguments provided, \
but can otherwise do pretty much anything with them.

A library follows the control flow of the caller. \
An implementation metaprogram can change the control flow of the caller \
— within semantic constraints; \
and it can instrument instructions that don't explicitly call it \
— e.g. log every binding or modification to every variable, for Omniscient Debugging.
")
 @para{Semantic separation vs inclusion}
 ~
 @para{Bound at Runtime vs Fixed at Compile-/Load- time}
 ~
 @para{Different scopes and capabilities}
 ~
 @para{Different control flow})

(xlide
 #:title "Different Social Architecture"
 @para{New dimension of modularity}
 ~
 @para{Deliver components, not applications}
 ~
 @para{No more fixed bottom, fine-grained virtualization}
 ~
 @para{Orthogonally address ``Non-functional requirements''}
 ~
 @para{Pay aspect specialists for components}
 ~
 @para{More like Emacs libraries and browser plugins})

(xlide
 (title-style "Conclusion"))

(xlide
 #:title "Related Works and Opportunities"
 @para{Formal Methods for proving program correctness}
 ~
 @para{Open Implementation, AOP...}
 ~
 @para{Many hacks for GC, Migration, Persistence...}
 ~
 @para{Virtualization, distribution...})

(xlide
 #:title "Common Theme"
 @para{Programming in the Large, not in the Small}
 ~
 @para{Software Architecture that Scales}
 ~
 @para{Semantics matter}
 ~
 @para{Dimensions of Modularity beyond the usual})

(xlide
 #:title "The Take Home Points (redux)"
 @;Implementation is co-(Abstract Interpretation)}
 @para{@para{Formalizing Implementations: Categories!}
 @para{Observability: Neglected key concept --- safe points}}
 ~
 @; First-class: the opposite of magic}
 @; First-class safe points (= PCLSRing!)}
 @para{@para{First-Class Implementations via Protocol Extraction}
 @para{Explore the Semantic Tower --- at runtime!}}
 ~
 @para{@para{Principled Reflection: Migration}
 @para{Natural Transformations generalize Instrumentation}}
 ~
 @para{@para{Reflective Architecture: 3D Towers}
 @para{Social Implications: Platforms, not Applications}})

(xlide
 #:title "Challenge"
 @para{Put First-class Implementations in your platform}
 ~
 @para{Platform: PL, IDE, OS, Shell, Distributed System}
 ~
 @para{Factor your software into meta-levels}
 ~
 @para[#:align 'center]{Enjoy simplification, robustness, security})

(xlide
 #:title "The Meta-Story"
 @para{My contribution is mostly not technical.}
 @para{It is more ambitious:}
 'next
 ~
 @para[#:align 'center]{@it{A change of point of view about computing}}
 ~
 'next
 @para[#:align 'center]{Thank you!}
 ~
 @para{My thesis: @it{The Semantics of Reflective Systems}}
 @url{https://j.mp/FarePhD}
 ~
 @para{My blog: @it{Houyhnhnm Computing}}
 @url{https://ngnghm.github.io/})

