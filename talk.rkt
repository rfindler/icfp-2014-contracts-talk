#lang racket
(require "intro.rkt"
         "counts/time-vs-contracts.rkt"
         "util.rkt"
         "dep.rkt"
         "redex-typeset.rkt"
         "equations.rkt"
         "boundary.rkt"
         "heap-slides.rkt"
         "random-generation.rkt"
         "title.rkt"
         "faces.rkt"
         "boundaries-not-projections.rkt"
         "takeaways.rkt"
         slideshow
         slideshow/play)

(title-slide)

(begin
  (subtitle "Pragmatics:"
            "Contracts are"
            "infectious")
  
  (define first-sequence (make-pip-sequence 0 0 #f #f))
  (play-n (wrap/first-argument-always-1 first-sequence))
  
  (define pip5050 (make-pip-sequence 50 50 #t #f))
  (slide (pip5050 1 1 0 0 0 0 0 0))
  (play-n
   (λ (a b c)
     (pip5050 1 1 1 1 1 a b c)))
  
  (bad-call)
  
  (define pip-false (make-pip-sequence #f #f 'error #f))
  (slide (pip-false 1 1 1 1 1 0 0 0))
  
  (define pip-false/contract (make-pip-sequence #f #f 'error 'type #:red-contract? #t))
  (play-n (λ (n1 n2 n3) (pip-false/contract n1 n2 n3 0 0 0 0 0)))
  
  (violation-with-type-based-contract)
  
  (time-vs-contracts))

(begin
  (subtitle "Interlude:"
            "Contracts are"
            "not Types")
  
  (define pip-neg (make-pip-sequence -50 -75 #t 'type))
  (slide (pip-neg 1 1 0 0 0 0 0 0))
  (play-n
   (λ (a b c)
     (pip-neg 1 1 1 1 1 a b c)))
  
  (slide original-contract)
  (play-n numeric-contract->dependent-version)
  
  (violation-with-dependent-based-contract)
  
  (introduce-dc-function)
  (motivate-dc-contract)
  (staged-dc-contract)
  (slide restores-state-afer-call?))

(begin
  (subtitle "Semantics:"
            "Dependency"
            "& Effects")
    
  (boundaries-not-projections)
  (introduce-calculus)
  (first-order-flow)
  (base-rule)
  (higher-order-flow)
  (play-n function-rule-part1)
  (function-in-boundary)
  (play-n function-rule-part2))

(begin
  (subtitle "Applications:"
            "Types, Testing,"
            "Analysis, &"
            "More")
  
  (introduce-randomness)
  (show-heap-bugs)
  (big-type/us-them)
  (slide (scale-to-fit provide-lines-pict client-w client-h))
  (applications))

(recap)
(final-thought)

(printf "slides built (in ~a seconds)\n" 
        (~r (/ (current-process-milliseconds) 1000) #:precision 2))
