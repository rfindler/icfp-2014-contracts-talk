#lang racket
(require slideshow
         slideshow/play
         slideshow/code
         pict
         rackunit
         racket/gui/base
         "color.rkt"
         "code-combine.rkt"
         "util.rkt"
         "render-bytes.rkt")

(provide make-pip-sequence 
         bad-call
         violation-with-type-based-contract
         violation-with-dependent-based-contract)

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ e)
     (with-syntax ([line (syntax-line stx)]
                   [file (syntax-source stx)])
       #'(assert/proc e line 'file))]))

(define (assert/proc v line file)
  (unless v
    (error 'assert "~a:~a assertion failure" line file)))

(get-current-code-font-size current-font-size)

(define-syntax-rule 
  (b-code exps ...)
  (color-code "black" (λ () (code exps ...))))
(define-syntax-rule 
  (w-code exps ...)
  (color-code "white" (λ () (code exps ...))))

(define ns (make-pict-namespace))

(define-syntax-rule
  (render-and-define (exps ...) more-args ...)
  (render-and-define/proc (b-code exps ...) 
                          '(exps ...)
                          more-args ...))

(define (run-as-module pc modname)
  (run-it
   (vl-append/p
    (tt/p (format "(begin (module ~a racket (require pict)" modname))
    pc
    (tt/p (format ")(require '~a))" modname)))))

(define (render-as-module main module-body -l -t -r -b modname)
  (define rounded-circle-size 40)
  (define background
    (filled-rounded-rectangle
     (+ (pict-width module-body) -l -r 40)
     (+ (pict-height module-body) -t -b 40)
     rounded-circle-size
     #:draw-border? #f))
  (define border
    (colorize (linewidth 
               10
               (rounded-rectangle (pict-width background) 
                                  (pict-height background)
                                  rounded-circle-size))
              blue-boundary-color))
  (define title (t (format "~a" modname)))
  (pin-over
   (pin-under main module-body cc-find
              (let ([b (blank)])
                (refocus (cc-superimpose (colorize background "black")
                                         b)
                         b)))
   background
   lt-find
   (refocus (vl-append
             5
             (hc-append (blank 40 0)
                        (cc-superimpose
                         (colorize (filled-rectangle (+ 30 (pict-width title))
                                                     (pict-height title))
                                   blue-boundary-color)
                         title))
             border)
            border)))

(define (zoom-into-module module-name total p n)
  (define-values (x y) (lt-find total p))
  (define l (* n x))
  (define t (* n y))
  (define r (* n (- (pict-width total) (+ x (pict-width p)))))
  (define b (* n (- (pict-height total) (+ y (pict-height p)))))
  (define pi (ghost (launder p)))
  (refocus (render-as-module 
            total
            p
            l t r b
            module-name)
           p))

(define (run-it pc #:err-ok? [err-ok? #f])
  (define result
    (with-handlers ([exn:fail? values])
      (parameterize ([current-namespace ns]
                     [compile-enforce-module-constants #f])
        (define exp (read-it pc))
        (eval exp))))
  (unless err-ok?
    (when (exn:fail? result)
      (raise result)))
  result)

(define (make-pip-sequence x-argument-value y-argument-value expected-result contract 
                           #:red-contract? [red-contract? #f])
  (define x-argument (tt/p (format "~a" x-argument-value)))
  (define y-argument (tt/p (format "~a" y-argument-value)))
  (define the-cloud-code (tt/p "(cloud 100 100)"))
  (define the-cloud-argument (inset (cloud 100 100) 0 0 0 -40))
  (define the-cloud-argument/2 (launder the-cloud-argument))
  
  (define x-argument-number (if (number? x-argument-value) x-argument-value 0))
  (define y-argument-number (if (number? y-argument-value) y-argument-value 0))
  
  (define red-dot (inset (inset (colorize (disk 30 #:draw-border? #f) "red") -15)
                         (+ 5 x-argument-number)
                         (+ 5 y-argument-number)
                         0 0))
  
  (define red-dot-cloud
    (freeze* (inset (lt-superimpose red-dot (cloud 100 100))
                    0 0 0 -40)
             (+ 5 (max 0 (- x-argument-number)) 15)
             (+ 5 (max 0 (- y-argument-number)) 15)
             15
             (+ 40 15)))
  
  (define-values (red-dot-cloud-bytes cloud-bytes)
    (render-bytes (pict->argb-pixels red-dot-cloud)
                  (pict->argb-pixels the-cloud-argument)
                  24))
  
  (define zero-arguments (hbl-append/p x-argument (tt/p " ") y-argument))
  (define cloud-zero-zero-argument-destination
    (hbl-append/p (launder/p the-cloud-argument)
                  (tt/p " ")
                  zero-arguments))
  (define cloud-argument-destination (launder/p (ghost/p the-cloud-argument)))
  (define red-dot-cloud-argument-destination (cloud 100 100))
  (define p-dot-reference 
    (let ([p (tt/p "p-dot")])
      (refocus/p (cb-superimpose/p (ghost/p red-dot-cloud-argument-destination) p)
                 p)))
  
  (define the-contract-line-one
    (tt/p "(-> pict? real? real?"))
  (define the-contract-line-two
    (tt/p "    boolean?)"))
  
  (define the-contract
    (vl-append (get-pict the-contract-line-one)
               (get-pict the-contract-line-two)))
  
  (define the-provide
    (cond
      [(not contract)
       (vl-append/p
        (tt/p "(provide point-in?)")
        (tt/p ""))]
      [(equal? contract 'type)
       (define line-two-prefix (tt/p " [point-in? "))
       (vl-append/p
        (tt/p "(provide/contract")
        (hbl-append/p line-two-prefix 
                      (colorize/p the-contract-line-one 
                                  (if red-contract? "red" "white")))
        (hbl-append/p (ghost/p line-two-prefix)
                      (colorize/p the-contract-line-two (if red-contract? "red" "white"))
                      (tt/p "])")))]
      [(equal? contract 'dependent)
       (tt/p 
        (format 
         "~s"
         `(provide/contract 
           [point-in? 
            (->i ([p pict?] 
                  [w (p) (real-in 0 (pict-width p))] 
                  [h (p) (real-in 0 (pict-height p))])
                 [res boolean?])])))]
      [else (error 'make-pip-sequence "unknown contract ~s" contract)]))
  
  (define (pip-content provide-n
                       substitute-n
                       pin-over-n
                       replace-p-dot-n
                       convert-bytes-n)
    (define pxy-arguments
      (let ([p (cellophane/p (tt/p "p x y") (- 1 substitute-n))])
        (refocus/p (rbl-superimpose/p p (ghost/p cloud-zero-zero-argument-destination))
                   p)))
    (define p-argument2 
      (let ([p (cellophane/p (tt/p "p") (- 1 substitute-n))])
        (refocus/p (ct-superimpose/p p cloud-argument-destination)
                   p)))
    
    (define main
      (vl-append/p
       (cellophane/p the-provide provide-n)
       (vl-append/p
        (inset/p (vl-append/p
                  (cellophane/p (tt/p "(define (point-in? p x y)") (- 1 substitute-n))
                  (cellophane/p (tt/p "  (define p-dot") (- 1 replace-p-dot-n)))
                 0 (- (* substitute-n 40)) 0 0)
        (blank/p 0 (* substitute-n 30))
        (hbl-append/p (cellophane/p (tt/p "    (pin-under ") (- 1 pin-over-n))
                      (blank/p (* (+ 30
                                     (pict-width/p x-argument)
                                     (pict-width/p y-argument))
                                  substitute-n)
                               0)
                      pxy-arguments
                      (cellophane/p (tt/p " (disk 1))") (- 1 pin-over-n))
                      (cellophane/p (tt/p ")") (- 1 replace-p-dot-n)))
        (tt/p "  (equal?")
        (lbl-superimpose/p
         (cellophane/p (hbl-append/p (tt/p "   (pict->argb-pixels ")
                                     (cellophane/p p-dot-reference (- 1 replace-p-dot-n))
                                     (tt/p ")"))
                       (- 1 convert-bytes-n))
         (cellophane/p (tt/p (format "   ~a)" cloud-bytes)) convert-bytes-n))
        (lbl-superimpose/p
         (cellophane/p
          (hbl-append/p (tt/p "   (pict->argb-pixels ")
                        (blank/p (* 20 substitute-n) 0)
                        p-argument2
                        (blank/p (* 40 substitute-n) 0)
                        (tt/p "))")
                        (cellophane/p (tt/p ")") (- 1 substitute-n)))
          (- 1 convert-bytes-n))
         (cellophane/p (tt/p (format "   ~a))" red-dot-cloud-bytes)) convert-bytes-n)))))
    main)
  (define pip-module-name 'point-in-module)
  
  (let ([main (pip-content 1 0 0 0 0)])
    (run-as-module main pip-module-name))
  
  (define (test1/pc cloud-appear-n call-disappear-n substitute-n pin-over-n replace-p-dot-n)
    (define hp-space
      (* (- (pict-width/p the-cloud-code)
            (pict-width the-cloud-argument))
         (- 1 cloud-appear-n)))
    (define outside-space
      (* (- (pict-width/p the-cloud-code)
            (pict-width the-cloud-argument))
         cloud-appear-n))
    (define cloud-zero-zero-arguments
      (cond
        [(zero? cloud-appear-n)
         (hbl-append/p the-cloud-code
                       (tt/p " ")
                       zero-arguments)]
        [else
         (define cloud+maybe-red
           (lt-superimpose 
            (cellophane red-dot pin-over-n)
            the-cloud-argument/2))
         (hbl-append/p (if (zero? replace-p-dot-n)
                           cloud+maybe-red
                           (ghost/p cloud+maybe-red))
                       (blank/p hp-space 0)
                       (tt/p " ")
                       (cellophane/p zero-arguments (- 1 pin-over-n)))]))
    (define main
      (hbl-append/p (cellophane/p (tt/p "(point-in? ") 
                                  call-disappear-n)
                    (blank/p 0 (pict-height the-cloud-argument))
                    cloud-zero-zero-arguments
                    (cellophane/p (tt/p ")") call-disappear-n)
                    (if (zero? cloud-appear-n)
                        (blank/p 0 0)
                        (blank/p outside-space 0))))
    (values (if (zero? substitute-n)
                main
                (ghost/p main))
            cloud-zero-zero-arguments))
  
  (let-values ([(main cloud-zero-zero-arguments)
                (test1/pc 0 1 0 0 0)])
    (cond
      [(equal? expected-result 'error)
       (check-pred exn? (run-it main #:err-ok? #t))]
      [else
       (check-equal? (run-it main) expected-result)]))
  
  (define (intro prequel-contract-n
                 test-appear-n
                 test-reduce-n
                 enter-module-n
                 substitute-n
                 pin-over-n
                 replace-p-dot-n
                 convert-bytes-n)
    (define g (inexact->exact (floor (* enter-module-n 255))))
    (define content-main
      (pip-content (- 1 enter-module-n)
                   substitute-n 
                   pin-over-n
                   replace-p-dot-n
                   convert-bytes-n))
    (define-values (test1-main cloud-zero-zero-arguments)
      (test1/pc test-reduce-n (- 1 substitute-n) substitute-n pin-over-n replace-p-dot-n))
    (define pict-content (get-pict content-main))
    (define module-body+test
      (cc-superimpose
       (blank 1024 768)
       (vl-append
        40 
        (colorize pict-content "white")
        (colorize
         (cellophane (get-pict test1-main) test-appear-n)
         (make-object color% g g g)))))
    (define module-zoomed
      (inset (zoom-into-module pip-module-name
                               module-body+test
                               pict-content
                               enter-module-n)
             (- margin)))
    (define new-cloud-zero-zero (launder (get-pict cloud-zero-zero-arguments)))
    (define arguments-slid
      (slide-pict 
       (slide-pict
        module-zoomed
        (if (zero? substitute-n) 
            (blank) 
            (colorize new-cloud-zero-zero "white"))
        (get-pict cloud-zero-zero-arguments)
        (get-pict cloud-zero-zero-argument-destination)
        substitute-n)
       (if (zero? substitute-n) 
           (blank) 
           (cellophane (freeze* (launder the-cloud-argument) 0 0 0 40) (- 1 convert-bytes-n)))
       the-cloud-argument/2
       (get-pict cloud-argument-destination)
       substitute-n))
    (define p-dot-slid
      (slide-pict
       arguments-slid
       (if (zero? replace-p-dot-n) 
           (blank) 
           (cellophane red-dot-cloud (- 1 convert-bytes-n)))
       new-cloud-zero-zero
       red-dot-cloud-argument-destination
       replace-p-dot-n))
    (define the-contract-start (ghost (launder (get-pict the-contract-line-one))))
    (define provide-placed
      (cc-superimpose (cellophane p-dot-slid prequel-contract-n)
                      the-contract-start))
    (slide-pict
     (cc-superimpose provide-placed
                     (inset
                      (cellophane (colorize (filled-rectangle 1024 768) "black")
                                  (- 1 prequel-contract-n))
                      (- margin)))
     (if (= 1 prequel-contract-n)
         (blank)
         (colorize the-contract "white"))
     the-contract-start
     (get-pict the-contract-line-one)
     prequel-contract-n))
  
  intro)

(define (bad-call)
  (define call (tt/p (format "~s" '(point-in? (cloud 100 100) #f #f))))
  (define result (run-it call #:err-ok? #t))
  (define lines (contract-error-msg->strings result))
  (define fade-to 1/4)
  (play-n
   (λ (n1 n2)
     (define passed-it? #f)
     (scale-to-fit
      (vl-append
       (hbl-append (tt "> ") (get-pict call))
       (cellophane 
        (apply 
         vl-append
         (for/list ([l (in-list lines)])
           (colorize
            (cond
              [(regexp-match #rx"blaming" l)
               (define m (regexp-match #rx"(^ *)([^ ].*$)" l))
               (define before (list-ref m 1))
               (define after (list-ref m 2))
               (define p (tt after))
               (define to-be-bigger (scale p (+ 1 (* .9 n2)) (+ 1 (* 4 n2))))
               (define prefix (blank (pict-width (tt before)) 0))
               (define bigger
                 (cc-superimpose 
                  (cellophane (colorize (filled-rectangle (pict-width to-be-bigger)
                                                          (pict-height to-be-bigger)
                                                          #:draw-border? #f)
                                        "white")
                              n2)
                  to-be-bigger))
               (set! passed-it? #t)
               (hbl-append prefix
                           (inset bigger
                                  0 ;(/ (- (pict-width bigger) (pict-width p)) -2)
                                  (/ (- (pict-height to-be-bigger) (pict-height p)) -2)))]
              [else 
               (define line
                 (cellophane 
                  (tt l)
                  (+ fade-to (* (- 1 n2) (- 1 fade-to)))))
               (if passed-it?
                   (cellophane line (- 1 n2))
                   line)])
            "red")))
         n1))
      (- 1024 margin margin)
      (- 768 margin margin)))))

(define (violation-with-type-based-contract)
  (make-pip-sequence 0 0 #f #f)
  (define bad-call-with-type-level-contract (make-pip-sequence #f #f 'error 'type))
  (define call (tt/p (format "~s" '(point-in? (cloud 100 100) #f #f))))
  (define result (run-it call #:err-ok? #t))
  (define strs (contract-error-msg->strings result))
  (slide
   (scale-to-fit (colorize (apply vl-append (map tt strs)) "red")
                 (- 1024 margin margin)
                 (- 768 margin margin))))

(define (violation-with-dependent-based-contract)
  (make-pip-sequence 0 0 #f #f)
  (define bad-call-with-type-level-contract (make-pip-sequence #f #f 'error 'dependent))
  (define call (tt/p (format "~s" '(point-in? (cloud 100 100) -50 -75))))
  (define result (run-it call #:err-ok? #t))
  (define strs (contract-error-msg->strings result))
  (slide
   (vl-append
    (hbl-append (tt "> ") (get-pict call))
    (scale-to-fit (colorize (apply vl-append (map tt strs)) "red")
                  (- 1024 margin margin)
                  (- 768 margin margin)))))

(define (contract-error-msg->strings exn)
  (for/list ([x (in-list (regexp-split #rx"\n" (exn-message exn)))]
             #:unless (regexp-match #rx"^ *at: " x))
    x))
  
(module+ main
  (play-n (make-pip-sequence 0 0 #f 'type)))
