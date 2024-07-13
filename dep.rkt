#lang racket
(require slideshow/fullscreen
         slideshow/play
         slideshow/code
         slideshow/balloon
         (prefix-in unsafe: pict/private/main)
         "intro.rkt"
         "util.rkt"
         "code-combine.rkt")

(provide staged-dc-contract
         restores-state-afer-call?
         original-contract
         numeric-contract->dependent-version
         introduce-dc-function
         motivate-dc-contract)

(define original-contract
  (make-sure-contract
   (vl-append/p
    (tt/p "(-> pict?")
    (tt/p "    real?")
    (tt/p "    real?")
    (tt/p "    boolean?)"))))

(define (numeric-contract->dependent-version n1 n2 n3 n4 n5 n6)
  (define (hide p1 p2 n1 n2)
    (define δ (- (pict-width (get-pict p2))
                 (pict-width (get-pict p1))))
    (inset/p (lbl-superimpose/p (cellophane/p p1 (- 1 n2))
                                (cellophane/p p2 n2))
             0 0 
             (* -1 (- 1 n1) δ)
             0))
  (define (hide1 p) (hide (blank/p) p n1 n2))
  (define (hide2 p) (hide (blank/p) p n3 n4))
  (define (hide3 p1 p2) (hide p1 p2 n5 n6))
  (make-sure-contract
   (vl-append/p
    (hbl-append/p (tt/p "(->") 
                  (hide1 (tt/p "i")) 
                  (tt/p " ") 
                  (hide1 (tt/p "([p ")) 
                  (tt/p "pict?") 
                  (hide1 (tt/p "]")))
    (hbl-append/p (tt/p "    ") 
                  (hide1 (tt/p "  [x "))
                  (hide2 (tt/p "(p) "))
                  (hide3 (tt/p "(>=/c 0)") (tt/p "(real-in 0 (p-w p))"))
                  (hide1 (tt/p "]")))
    (hbl-append/p (tt/p "    ")
                  (hide1 (tt/p "  [y "))
                  (hide2 (tt/p "(p) "))
                  (hide3 (tt/p "(>=/c 0)") (tt/p "(real-in 0 (p-h p))"))
                  (hide1 (tt/p "])")))
    (hbl-append/p (tt/p "    ")
                  (hide1 (tt/p "[res "))
                  (tt/p "boolean?")
                  (hide1 (tt/p "]"))
                  (tt/p ")")))))

(define dependent+function-contract
  (make-sure-contract
   (vl-append/p
    (tt/p "(->i ([f (-> dc<%> real? real?")
    (tt/p "             void?)]")
    (tt/p "      [w real?]")
    (tt/p "      [h real?])")
    (tt/p "     [result pict?])"))
   #:extras (list '(define dc<%> any/c))))

(define dependent+function-contract/pre
  (make-sure-contract
   (vl-append/p
    (tt/p "(->i ([f (-> dc<%> real? real?")
    (tt/p "             void?)]")
    (tt/p "      [w real?]")
    (tt/p "      [h real?])")
    (tt/p "     #:pre (f)")
    (tt/p "     (restores-state-after-call? f)")
    (tt/p "     [result pict?])"))
   #:extras (list '(define dc<%> any/c))))

(define restores-state-afer-call?
  (make-sure/defn
   (vl-append/p
    (tt/p "(define (restores-state-after-call? f)")
    (tt/p "  (define a-dc (make-bitmap-backed-dc))")
    (tt/p "  (randomize-state a-dc)")
    (tt/p "  (define before (get-dc-state a-dc))")
    (tt/p "  (f a-dc 0 0)")
    (tt/p "  (equal? before (get-dc-state a-dc)))"))))


(define (height n p)
  (define amt (- (* (pict-height (get-pict p)) (- 1 n) 1/2)))
  (inset/p p 0 amt 0 amt))
(define (width-right n oth-max-width p)
  (define amt (- (* (- (pict-width (get-pict p)) oth-max-width) (- 1 n))))
  (inset/p p 0 0 amt 0))

(define (dc-animation n1 n2 n3 n4 n5 n6)
  (define colorize-n (if (zero? n6) (* n1 (- 1 n2)) n6))
  (define parens-first-line (tt/p "             dx dy 200 120)"))
  (define parens-second-line (tt/p "       (send dc set-brush oldb)"))
  (define move-paren-width (- (pict-width (get-pict parens-second-line))
                              (pict-width (get-pict parens-first-line))))
  (define move-paren-height (+ (pict-height (get-pict parens-first-line))
                               (pict-ascent (get-pict parens-first-line))))
  (vl-append/p
   (cellophane/p (tt/p "(colorize") colorize-n)
   (tt/p " (dc (λ (dc dx dy)")
   (cellophane/p (height n2 (tt/p "       (define oldb (send dc get-brush))")) n3)
   (cellophane/p (height n4 (tt/p "       (send dc set-brush \"red\" 'solid)")) n5)
   (tt/p "       (send dc draw-ellipse")
   (hbl-append/p parens-first-line)
   (hbl-append/p (cellophane/p (height n2 parens-second-line) n3)
                 (let ([paren (inset/p (tt/p ")") 0 0 0 
                                       (* n2 -1 (pict-descent (tt ")"))))]
                       [blankness (inset/p (tt/p " ")
                                           0
                                           (- (* 1/2 n2 (pict-height (tt " ")))))])
                   (inset/p 
                    (refocus/p (vl-append/p paren blankness) blankness)
                    (* (- 1 (fast-end n2)) move-paren-width -1)
                    0
                    0
                    0)))
   (tt/p "     200 ")
   (tt/p "     120)")
   (cellophane/p (tt/p " \"blue\")") colorize-n)))

(define (introduce-dc-function)
  (play-n
   #:aspect 'fullscreen
   (let ([bkg
          (ghost
           (launder
            (get-pict
             (apply
              dc-animation
              (build-list (procedure-arity dc-animation)
                          (λ (n) 1))))))])
     (define ns (make-pict-namespace))
     (parameterize ([current-namespace ns])
       (namespace-require 'racket/class))
     (procedure-reduce-arity
      (λ args
        (define code-p (apply dc-animation args))
        (define p (parameterize ([current-namespace ns])
                    (eval (read-it code-p))))
        (vc-append (lt-superimpose bkg (get-pict code-p))
                   (colorize (linestyle 'transparent p) "black")))
      (procedure-arity dc-animation)))))

(define-syntax-rule 
  (both e ...)
  (both/proc (λ () (code e ...)) (let () e ...)))
(define (both/proc the-code-thunk the-pict)
  (define the-code (color-code "black" the-code-thunk))
  (values the-code the-pict))

(define (motivate-dc-contract)
  (define-values (the-code the-pict)
    (both
     (define eps
       (dc
        (λ (dc dx dy)
          (send dc draw-ellipse
                dx dy 200 100))
        200 100))
     (define red
       (unsafe:dc
        (λ (dc dx dy)
          (send dc set-brush
                "red" 'solid)
          (send dc draw-ellipse
                dx dy 200 100))
        200 100))
     (colorize
      (vc-append 20 eps red eps)
      "black")))
  (play-n
   #:aspect 'fullscreen
   (λ (n1 n2)
     (define tan-border-size 10)
     (define line-size (pict-height (code x)))
     (define gap-size (- (pict-height (code x
                                            x))
                         (* 2 line-size)))
     (define highlighted-code
       (pin-under
        the-code
        (+ (- tan-border-size))
        (+ (* n1 line-size 6) (* n1 gap-size 6)
           (* n2 line-size 8) (* n2 gap-size 8))
        (colorize
         (filled-rectangle (+ (pict-width the-code) tan-border-size tan-border-size)
                           (+ (* line-size 6)
                              (* gap-size 5)
                              (* line-size 2 n1)
                              (* gap-size 2 n1)
                              (* line-size -5 n2)
                              (* gap-size -5 n2))
                           #:draw-border? #f)
         "NavajoWhite")))
     (hc-append 120
                (scale highlighted-code (/ 700 (pict-height highlighted-code)))
                (linestyle 'transparent the-pict)))))

(define (staged-dc-contract)
  (define balloon
    (pip-wrap-balloon
     (vl-append 
      (hbl-append (t "Wrapping ") (tt "f") (t ": ") (colorize (t "bad idea!") "red"))
      (ht-append
       (blank 20 0)
       (vl-append
        (t "• checking happens too late")
        (t "• too expensive"))))
     'sw
     -20 30))
  (play-n
   #:aspect 'fullscreen
   (λ (n1 n2 n3)
     (define f-tt (tt/p "f"))
     (define lines-before
       (list 
        (hbl-append/p (tt/p "(->i ([") f-tt (tt/p " (-> dc<%> real? real?"))
        (tt/p "             void?)]")
        (tt/p "      [w real?]")
        (tt/p "      [h real?])")))
     (define lines-after
       (list 
        (tt/p "     [result pict?])")))
     (define max-width (apply max (map pict-width/p (append lines-before lines-after))))
     (define lines-during
       (list
        (cellophane/p (height n2 (tt/p "     #:pre (f)")) n3)
        (cellophane/p (width-right 
                       n2
                       max-width
                       (height n2 (tt/p "     (restores-state-after-call? f)")))
                      n3)))
     (define combined
       (make-sure-contract
        (apply vl-append/p
               (append lines-before
                       lines-during
                       lines-after))
        #:extras (list '(define dc<%> any/c))))
     (pin-over combined
               (get-pict f-tt)
               rt-find
               (cellophane balloon (* (fast-start n1) (- 1 (fast-start n2))))))))

(module+ slideshow
  
  (introduce-dc-function))
