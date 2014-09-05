#lang racket
(require slideshow
         "util.rkt"
         "code-combine.rkt"
         "faces.rkt"
         slideshow/code
         slideshow/play)

(define (mon-based-on-code #:rev? [rev? #f] #:exp exp #:ctc ctc 
                           #:pos [pos (if rev? "b" "a")]
                           #:neg [neg (if rev? "a" "b")])
  (define exponent (lift-up-label (hbl-append (t pos) (t ",") (t neg)) #t))
  (code (mon #,exponent #,exp #,ctc)))

(define (lift-up-label what bonus-back?)
  (define lift-amount 10)
  (define (lf p n) 
    (define p2 (ghost (launder p)))
    (refocus (cbl-superimpose (vl-append p (blank 0 n)) p2) p2))
  (inset (lf what lift-amount) 
         (- (+ (pict-width (tt " ")) (if bonus-back? 6 0)))
         0 0 0))

(define (from-string s)
  (cond
    [(pict? s) s]
    [(string? s) (t s)]
    [else (tt (format "~s" s))]))

(define check-rule-ns (make-base-namespace))
(parameterize ([current-namespace check-rule-ns])
  (namespace-require 'racket)
  (eval '(define-syntax (mon stx)
           (syntax-case stx ()
             [(_ a b exp val)
              (begin
                (unless (identifier? #'a) 
                  (raise-syntax-error 
                   'mon
                   "expected a label for the first argument"
                   stx #'a))
                (unless (identifier? #'b) 
                  (raise-syntax-error 
                   'mon
                   "expected a label for the second argument"
                   stx #'b))
                #'(begin exp val))])))
  (eval '(define-syntax (blame stx)
           (syntax-case stx ()
             [(_ id)
              (identifier? #'id)
              #'(void)])))
  (eval '(define-syntax (-> stx)
           (syntax-case stx ()
             [(_ a b)
              #'(void)])))
  (eval '(define-syntax (->i stx)
           (syntax-case stx ()
             [(_ a b) #'(void)] ;; first, no label; still ok
             [(_ l a b)
              (identifier? #'l)
              #'(void)]))))

(define (check-a-rule cp)
  (parameterize ([current-namespace check-rule-ns])
    (define all (read-all cp))
    (unless (= 3 (length all))
      (error 'check-a-rule "expected three things, got ~s" all))
    (unless (equal? (syntax->datum (list-ref all 1)) '=)
      (error 'check-a-rule "expected equal as the second thing, got ~s" 
             (syntax->datum (list-ref all 1))))
    (expand (list-ref all 0))
    (expand (list-ref all 1)))
  (void))

(define (base-rule)
  (define rule
    (vl-append/p
     (hbl-append/p (tt/p "(mon ") 
                   (exponent "a" "b")
                   (tt/p " v pred?)"))
     (tt/p "=")
     (tt/p "(if (pred? v) v (blame a))")))
  (check-a-rule rule)
  (slide (get-pict rule)))

(define exponent
  (let ()
    (define (exponent/private pct str)
      (define pict (lift-up-label pct #t))
      (inject pict str))
    (define exponent
      (case-lambda
        [(a) (exponent/private (t a)
                               a)]
        [(a b) (exponent a b #f)]
        [(a b red?) (exponent/private 
                     (hbl-append (t a) (t ",") 
                                 (if red? 
                                     (colorize (t b) "red")
                                     (t b)))
                     (format " ~a ~a " a b))]))
    exponent))

(define (function-rule #:greenberg? greenberg? n1 n2 n3 n4 n5 n6 n7 n8-n9)
  (define red-labels? (zero? n1))
  (define-values (n8 n9) (split-phase n8-n9))
  (define picky-before
    (hbl-append/p
     (tt/p "(mon ")
     (exponent "b" (if (zero? n9) "a" "c") (not (zero? n9)))
     (tt/p " ")))
  (define picky-after
    (tt/p " dom)"))
  (define rule
    (vl-append/p
     (hbl-append/p 
      (tt/p "(mon ") (exponent "a" "b") 
      (hbl-append/p (tt/p "(λ (") (tt/p "x") (tt/p ") ") (tt/p "e") (tt/p ") "))
      (lbl-superimpose/p
       (ghost/p (tt/p "(->i dom drng))"))
       (hbl-append/p
        (tt/p "(->")
        (hide-width (cellophane/p (tt/p "i") n2) n1)
        (hide-width (cellophane/p 
                     (hbl-append/p (tt/p " ") (colorize/p (exponent "c") "red"))
                     n9)
                    n8)
        (tt/p " dom ")
        (hide-width (cellophane/p (tt/p "d") n2) n1)
        (tt/p "rng))"))))
     (tt/p "=")
     (vl-append/p
      (hbl-append/p (tt/p "(λ (x)"))
      (hide-height (cellophane/p 
                    (hbl-append/p
                     (tt/p "  (let ([rng (drng ")
                     (lbl-superimpose/p
                      
                      ;; this is a spacer for this line so things grow only
                      ;; rightwards
                      (ghost/p (hbl-append/p picky-before
                                             (tt/p "x")
                                             picky-after
                                             (tt/p ")])")))
                      
                      (hbl-append/p
                       (cellophane/p (hide-width picky-before n5) n6)
                       (tt/p "x")
                       (cellophane/p (hide-width picky-after n5) n6)
                       (tt/p ")])"))))
                    n4) n3)
      (htl-append/p (hide-width (tt/p "  ") n3)
                    (tt/p "  (let ([x (mon ")
                    (if red-labels?
                        (colorize/p (exponent "b" "a") "red")
                        (exponent "b" "a"))
                    (tt/p " x dom)])"))
      (hbl-append/p (hide-width (tt/p "  ") n3)
                    (tt/p "    (mon ")
                    (if red-labels?
                        (colorize/p (exponent "a" "b") "red")
                        (exponent "a" "b"))
                    (tt/p " e rng)))")
                    (cellophane/p (tt/p ")") n4))
      
      ;; this line is here only to make the slide that introduces
      ;; the let look better (it only slides things down, not both up and down)
      (hide-height (ghost/p (tt/p "x")) (- 1 n3)))))
  
  (check-a-rule rule)
  (cc-superimpose
   (rt-superimpose (blank client-w client-h)
                   (cellophane (tt "[2002]")
                               (* n4 (- 1 n5)))
                   (cellophane (ht-append
                                20
                                (vr-append
                                 (t "Greenberg et al" )
                                 (t "[2010]"))
                                mgree)
                               (* n7 (if greenberg? 1 0))))
   (vc-append
    (blank 0 40)
    (get-pict rule))))


(define function-rule-part2-count 1)
(define function-rule-part1-count (- (procedure-arity function-rule) function-rule-part2-count))

(define function-rule-part1
  (procedure-reduce-arity
   (λ args (apply function-rule
                  #:greenberg? #t
                  (append args
                          (build-list function-rule-part2-count (λ (x) 0)))))
   function-rule-part1-count))

(define function-rule-part2
  (procedure-reduce-arity
   (λ args
     (apply function-rule
            #:greenberg? #f
            (append (build-list function-rule-part1-count (λ (x) 1))
                    args)))
   function-rule-part2-count))

(define mgree (get-bitmap 'mgree))

(define (hide-width p n)
  (inset/p p 0 0 (* (- 1 n) (pict-width (get-pict p)) -1) 0))
(define (hide-height p n)
  (inset/p p 0 (* (- 1 n) (pict-height (get-pict p)) -1) 0 0))

(define (dependent-function-rule1)
  (slide
   (vl-append (mon-based-on-code #:exp (code (λ (x) e)) #:ctc (code (->i dom drng)))
              (t  "=")
              (code (λ (x)
                      (let ([rng (drng x)]
                            [x #,(mon-based-on-code #:exp (code x)
                                      #:ctc (code dom) 
                                      #:rev? #t)])
                        #,(mon-based-on-code #:exp (code e)
                               #:ctc (code rng))))))))

(define (dependent-function-rule2)
  (slide
   (vl-append (mon-based-on-code #:exp (code (λ (x) e)) #:ctc (code (->i dom drng)))
              (t  "=")
              (code (λ (x)
                      (let* ([x #,(mon-based-on-code #:exp (code x)
                                       #:ctc (code dom) 
                                       #:rev? #t)]
                             [rng (drng x)])
                        #,(mon-based-on-code #:exp (code e)
                               #:ctc (code rng))))))))

(define (dependent-function-rule3)
  (slide
   (vl-append (mon-based-on-code #:exp (code (λ (x) e)) 
                   #:ctc (code (->i #,(lift-up-label (t "c") #f) dom drng)))
              (t  "=")
              (code (λ (x)
                      (let ([rng (drng #,(mon-based-on-code #:exp (code x)
                                              #:ctc (code dom) 
                                              #:pos "c"
                                              #:neg "a"))]
                            [x #,(mon-based-on-code #:exp (code x)
                                      #:ctc (code dom) 
                                      #:rev? #t)])
                        #,(mon-based-on-code #:exp (code e)
                               #:ctc (code rng))))))))

(provide base-rule
         function-rule
         function-rule-part1
         function-rule-part2)

(module+ main 
  (play-n function-rule-part1)
  (play-n function-rule-part2))
