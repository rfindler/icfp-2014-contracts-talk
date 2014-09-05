#lang racket
(require "util.rkt" 
         "redex-model.rkt"
         redex
         slideshow
         slideshow/play)
(provide introduce-calculus)

(literal-style '(bold . modern))
(paren-style '(bold . modern))
(default-style 'modern) ;; just to space things out a bit more
;(non-terminal-style '(italic . modern))

(define rewrite-typeof
  (match-lambda
    [(list _ _ Γ e τ _)
     (list "" Γ " ⊢ " e " : " τ)]))
(define rewrite-extend
  (match-lambda
    [(list _ _ Γ x τ _)
     (list "" Γ ", " x ":" τ)]))
(define rewrite-lookup
  (match-lambda
    [(list _ _ Γ x _)
     (list "" Γ "(" x ")")]))
(define rewrite-different
  (match-lambda
    [(list _ _ a b _)
     (list "" a " ≠ " b "")]))
(define rewrite-subst
  (match-lambda
    [(list _ _ e qq _)
     (match (lw-e qq)
       [(list _ x v _)
        (list "" e "{" x ":=" v "}")])]))
(define rewrite-domof
  (match-lambda
    [(list _ _ a _)
     (list "dom(" a ") ")]))
(define rewrite-mon
  (match-lambda
    [(list op m τ a b c e cl)
     (define ap (extract-var a))
     (define bp (extract-var b))
     (list op 
           
           (hbl-append
            ((current-text) "mon" (literal-style) (default-font-size))
            (render-label (format "~a,~a" ap bp))
            ((current-text) " " (literal-style) (default-font-size)))
           
           c e cl)]))

(define rewrite-->i
  (match-lambda
    [(list op arr c dom rng cl)
     (define cp (extract-var c))
     (list op 
           
           (hbl-append
            ((current-text) "->i" (literal-style) (default-font-size))
            (render-label (format "~a" cp))
            ((current-text) " " (literal-style) (default-font-size)))
           
           dom rng cl)]))

(define (render-label str)
  (define normal ((current-text) str (current-main-font) (default-font-size)))
  (define g (ghost normal))
  (refocus (cb-superimpose (vc-append normal (blank 0 6)) g) g))

(define (extract-var x)
  (define s (lw-e x))
  (unless (symbol? s) 
    (error 'extract-var 
           "mon appears to be used with something other than a variable in the label position, got ~s"
           s))
  (symbol->string s))

(define all-type-rules '())

(define (group-type-rules rules) 
  (set! all-type-rules rules)
  (apply vc-append 20 
         (hc-append 30 (list-ref rules 0) (list-ref rules 1))
         (cddr rules)))

(define background
  (with-compound-rewriters 
   (['typeof rewrite-typeof]
    ['extend rewrite-extend]
    ['lookup rewrite-lookup]
    ['different rewrite-different]
    ['subst rewrite-subst]
    ['mon rewrite-mon]
    ['->i rewrite-->i]
    ['domof rewrite-domof])
   (ht-append 100
              (vc-append 100
                         (language->pict
                          contracts
                          #:nts 
                          (remove* '(x a b c) (language-nts contracts)))
                         (ht-append 
                          40
                          (language->pict
                           L
                           #:nts '(v E))
                          (reduction-relation->pict red)))
              (parameterize ([relation-clauses-combine group-type-rules])
                (judgment-form->pict typeof)))))

(define mon-rule (list-ref all-type-rules 8))
(define prd-rule (list-ref all-type-rules 11))
(define arr-rule (list-ref all-type-rules 12))
(define dep-rule (list-ref all-type-rules 13))

(define (white-bkg p)
  (refocus (cc-superimpose
            (colorize (filled-rectangle (+ (pict-width p) 20)
                                        (+ (pict-height p) 20)
                                        #:draw-border? #f)
                      "white")
            p)
           p))

(define scale-factor (/ (- 1024 margin margin)
                        (pict-width (cc-superimpose arr-rule 
                                                    dep-rule
                                                    mon-rule
                                                    prd-rule))))
(define mon-dest (scale (ghost (launder mon-rule)) scale-factor))
(define arr-dest (scale (ghost (launder arr-rule)) scale-factor))
(define dep-dest (scale (ghost (launder dep-rule)) scale-factor))
(define prd-dest (scale (ghost (launder prd-rule)) scale-factor))

(define scaled-bkg
  (scale-to-fit
   background
   (- 1024 margin margin)
   (- 768 margin margin)))
(define frozen-bkg (freeze scaled-bkg 0 0 0 0))
(define base 
  (cc-superimpose
   (ghost scaled-bkg)
   mon-dest
   arr-dest
   dep-dest
   prd-dest))
   
(define (bring-out-and-put-back rule dest n1 _n2)
  (define n2 (fast-end _n2))
  (refocus (cond
             [(or (and (= n1 0) (= n2 0))
                  (and (= n1 1) (= n2 1)))
              (ghost base)]
             [(zero? n2)
              (slide-pict
               (ghost base)
               (add-white (scale rule (+ 1 (* n1 (- scale-factor 1)))) n1)
               rule 
               dest
               n1)]
             [else
              (slide-pict
               (ghost base)
               (add-white (scale rule (+ 1 (* (- 1 n2) (- scale-factor 1)))) 
                          (- 1 n2))
               dest
               rule
               n2)])
           base))

(define (bring-out-and-dont-put-back rule dest n1)
  (refocus (cond
             [(= n1 0)
              (ghost base)]
             [else
              (slide-pict
               (ghost base)
               (add-white (scale rule (+ 1 (* n1 (- scale-factor 1)))) n1)
               rule 
               dest
               n1)])
           base))

(define (add-white p n)
  (refocus
   (cc-superimpose (cellophane 
                    (colorize (filled-rounded-rectangle
                               (+ (pict-width p) 20)
                               (+ (pict-height p) 20)
                               (+ (* n 8) 8)
                               #:draw-border? #f)
                              "gray")
                    n)
                   p)
   p))

(define (introduce-calculus)
  (define proc
    (λ (n1 n23 n45 n67)
      (define-values (n2 n3) (split-phase n23))
      (define-values (n4 n5) (split-phase n45))
      (define-values (n6 n7) (split-phase n67))
      (cc-superimpose
       frozen-bkg
       (bring-out-and-put-back prd-rule prd-dest n1 n2)
       (bring-out-and-put-back arr-rule arr-dest n3 n4)
       (bring-out-and-put-back mon-rule mon-dest n5 n6)
       (bring-out-and-dont-put-back dep-rule dep-dest n7))))
  (play-n proc
          #:steps
          (for/list ([i (in-range (procedure-arity proc))])
            (cond
              [(= i 0)
               10]
              [else 
               30]))))

(module+ slideshow (introduce-calculus))
