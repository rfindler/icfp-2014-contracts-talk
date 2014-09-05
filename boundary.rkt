#lang racket
(require slideshow 
         slideshow/code
         slideshow/play
         racket/draw
         "faces.rkt"
         "function-machine.rkt"
         "util.rkt"
         "color.rkt")

(provide send-value-in send-value-out bad-value-in-boundary
         value-all-the-way-out
         boundary-line-thickness
         first-order-flow
         higher-order-flow
         function-in-boundary
         introduce-randomness
         applications
         boundary-pict)

(define (boundary ctc
                  value-entering-boundary
                  #:bad-value? [bad-value? #f]
                  #:going? [going? #f]
                  #:% [% 0]
                  #:circle-background [circle-background 0])
  (define circle-size (+ 100
                         (max (pict-width value-entering-boundary) 
                              (pict-height value-entering-boundary))))
  (define-values (left right bkg line boundary-color) (make-bkg-and-line bad-value?))
  (define ctc-space 10)
  (define amt-to-clip (- (* (- 1 %) (+ boundary-line-thickness circle-size))))
  (define half-of-center-circle
    (clip
     (inset
      (inset
       (cc-superimpose
        (colorize (disk circle-size) (if going? "black" "white"))
        (colorize (linewidth boundary-line-thickness (circle circle-size))
                  boundary-color))
       boundary-line-thickness)
      (if going? amt-to-clip 0)
      0 
      (if going? 0 amt-to-clip)
      0)))
  (define center-circle
    (refocus ((if going? rc-superimpose lc-superimpose)
              half-of-center-circle
              (inset (cellophane 
                      (colorize 
                       (disk (+ circle-size boundary-line-thickness) #:draw-border? #f)
                       boundary-color)
                      circle-background)
                     (/ boundary-line-thickness 2)))
             half-of-center-circle))
  
  (define ctc+bkg
    (if ctc
        (pin-over (colorize (filled-rectangle
                             (+ (pict-width ctc) ctc-space ctc-space)
                             (+ (pict-height ctc) ctc-space ctc-space)
                             #:draw-border? #f)
                            boundary-color)
                  ctc-space
                  ctc-space
                  ctc)
        (blank)))
  
  (cc-superimpose
   bkg
   (inset 
    (refocus (ht-append 
              (refocus ((if going? lc-superimpose rc-superimpose)
                        line
                        center-circle)
                       line)
              ctc+bkg)
             line)
    0
    (- margin))))

(define (make-bkg-and-line bad-value?)
  (define boundary-color (if bad-value? red-boundary-color blue-boundary-color))
  (define left (colorize (filled-rectangle 512 768 #:draw-border? #f) "black"))
  (define right (colorize (filled-rectangle 512 768 #:draw-border? #f) "white"))
  (define bkg (hc-append left right))
  (define line (colorize (filled-rectangle boundary-line-thickness 768 #:draw-border? #f)
                         boundary-color))
  (values left right (inset bkg (- margin)) line boundary-color))

(define (send-value-in ctc value)
  (λ (n)
    (define-values (n1 n2) (split n))
    (pin-over/%
     (boundary ctc
               value
               #:% (/ n1 2)
               #:circle-background n2)
     (interpolate 5/6 1/2 n1)
     1/2
     value)))

(define (send-value-out ctc value)
  (λ (n)
    (define-values (n1 n2) (split n))
    (pin-over/%
     (boundary ctc
               value
               #:going? #t
               #:% (/ (- 1 n2) 2)
               #:circle-background (- 1 n1))
     (interpolate 1/2 1/6 n2)
     1/2
     value)))

(define (value-all-the-way-out ctc value)
  ((send-value-out ctc value) 1))

(define (bad-value-in-boundary ctc value)
  (pin-over/%
   (boundary ctc value
             #:bad-value? #t
             #:going? #t
             #:% 1/2
             #:circle-background 1)
   1/2
   1/2
   value))

(define two (scale/improve-new-text (bt "2") 5))
(define four (scale/improve-new-text (bt "4") 5))

(define less-than-three (colorize (tt "(<=/c 3)") "white"))
(define ho-contract (colorize (vl-append (tt "(-> (<=/c 3)") 
                                         (tt "    (<=/c 3))")) 
                              "white"))

(define (first-order-flow)
  (play-n (send-value-in less-than-three two))
  (play-n (send-value-out less-than-three (colorize two "white")))
  (play-n (send-value-in less-than-three four))
  (slide (bad-value-in-boundary less-than-three four)))

(define (higher-order-flow)
  (play-n (send-value-in ho-contract function-machine))
  (play-n (send-value-out ho-contract 
                          (function-machine-with-white-insides-and-boundaries 
                           (blank)
                           (blank)))))

(define blue-to-blue-scale
  (/ boundary-line-thickness 
     function-machine-blue-boundary-thickness))

(define (function-in-boundary)
  (define the-number-start (blank))
  (define the-number-end (blank))
  (define the-machine 
    (lt-superimpose (launder function-machine)
                    the-number-end))
  (define background
    (pin-over/%
     (boundary ho-contract
               function-machine
               #:% 1/2
               #:circle-background 1)
     1/2 1/2
     the-machine))     
  (play-n
   #:steps (list 10 40)
   (λ (n1 n2)
     (define the-number
       (blue-boundary-around
        (scale/improve-new-text
         (colorize (bt "4") "forestgreen")
         (+ (* (- 1 (/ n2 2)) 3 n1) .01))))
     
     
     (define-values (ex ey) (lt-find background the-machine))
     ;; this magic number should probably be computed 
     ;; in terms of the various funnel constants, but I've lost
     ;; the context now so just guestimate by eye
     (set! ey (+ ey 70))
     
     ;; extra 4 to account for the pen width (too much, of course)
     (set! ex (- ex (/ (pict-width the-number) 1) 4))
     
     (define-values (sx sy) (values (/ (pict-width background) 2)
                                    (+ (/ (pict-height background) 2) 200)))
     
     (define dist 100)

     (define points '())
     
     (define-values (cx cy)
       (let* ([start-v (make-rectangular (- sx ex) (- sy ey))]
              [mid-v (* 1/2 start-v)]
              [dest-v (+ mid-v (make-polar dist (- (angle start-v) (/ pi 2))))]
              [dest (+ dest-v (make-rectangular ex ey))])
         (values (real-part dest) (imag-part dest))))
     
     (define radius (magnitude (make-rectangular (- cx ex) (- cy ey))))
     
     (define start-θ (angle (make-rectangular (- sx cx) (- sy cy))))
     (define end-θ (+ (angle (make-rectangular (- ex cx) (- ey cy))) (* 2 pi)))
     (define θ (+ start-θ (* n2 (- end-θ start-θ))))
     (define x (+ cx (* radius (cos θ))))
     (define y (+ cy (* radius (sin θ))))
     
     (pin-over
      (pin-over/%
       background
       1/2 4/5
       the-number-start)
      (- x (/ (pict-width the-number) 2))
      (- y (/ (pict-height the-number) 2))
      the-number))))

(define (add-dots p dots)
  (define dot-size 10)
  (for/fold ([p p]) ([dot (in-list dots)])
    (pin-over
     p
     (- (real-part dot) (/ dot-size 2))
     (- (imag-part dot) (/ dot-size 2))
     (colorize (disk dot-size) "red"))))

(define (blue-boundary-around p)
  (define size (* (max (pict-width p) (pict-height p)) 1.1))
  (refocus (cc-superimpose
            (colorize (disk size #:draw-border? #f) blue-boundary-color)
            p)
           p))


;                                 
;                                 
;                                 
;                                 
;                                 
;                                 
;   ;;;;;  ;;; ;;  ;;; ;;   ;;;;  
;  ;;;;;;; ;;;;;;; ;;;;;;; ;;; ;; 
;  ;;  ;;; ;;; ;;; ;;; ;;; ;;;    
;    ;;;;; ;;; ;;; ;;; ;;;  ;;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;;    ;;; 
;  ;;; ;;; ;;;;;;; ;;;;;;; ;; ;;; 
;   ;;;;;; ;;; ;;  ;;; ;;   ;;;;  
;          ;;;     ;;;            
;          ;;;     ;;;            
;                                 
;                                 



(define (introduce-randomness)
  (define-values (left right bkg line boundary-color) (make-bkg-and-line #f))
  (define bkg+line (cc-superimpose bkg (inset line 0 (- margin))))
  (play-n
   (λ (n1 n2)
     (parameterize ([current-font-size 60])
       (pin-center 
        (pin-center 
         bkg+line
         (colorize (cc-superimpose 
                    (cellophane (t "Component B") (- 1 n1))
                    (cellophane (t "Randomness") n2)) 
                   "black")
         right)
        (colorize (t "Component A")
                  "white")
        left)))))

(define (pin-center main new-pict loc)
  (define-values (cx cy) (cc-find main loc))
  (pin-over
   main
   (- cx (/ (pict-width new-pict) 2))
   (- cy (/ (pict-height new-pict) 2))
   new-pict))


(define tr-team (show-team 'sam 'eric 'asumu 'vincent))
(define analysis-team (show-team 'phil 'david 'sam))
(define amal-team (show-team 'amal))

(define (applications)
  (typed-racket)
  (static-analysis)
  (multi-language))

(define (typed-racket)
  (an-application "Gradual Typing" 3
                  '("Typed" "Racket")
                  '("Racket")
                  tr-team
                  #t))
(define (static-analysis)
  (an-application "Static Analysis" 3
                  '("Havoc")
                  '("Racket")
                  analysis-team
                  #f))
(define (multi-language)
  (an-application "Fully Abstract Compilers" 2 
                  '("call/cc," "set!, ...")
                  '("STLC")
                  amal-team
                  #f))

(define (an-application what what-scale left-words right-words team fade-in?)
  (define-values (left right bkg line boundary-color) (make-bkg-and-line #f))
  (define bkg+line (cc-superimpose bkg (inset line 0 (- margin))))
  (define (the-proc n1 n2)
    (ct-superimpose
     (cb-superimpose
      (pin-center
       (pin-center
        bkg+line
        (scale/improve-new-text
         (colorize (cellophane (vl-append (apply vl-append (map t left-words))
                                          (blank 0 100))
                               n2)
                   "black")
         2)
        right)
       (colorize 
        (scale/improve-new-text
         (cellophane (vl-append (apply vl-append (map t right-words))
                                (blank 0 100))
                     n2)
         2)
        "white")
       left)
      (vc-append
       (cc-superimpose
        (colorize (scale/improve-new-text
                   (cellophane (t what) (* n1 (- 1 n2)))
                   what-scale)
                  "blue")
        (cellophane team n2))
       (blank 0 50)))))
  (if fade-in?
      (play-n the-proc)
      (play-n (λ (n1) (the-proc 1 n1)))))

(define (boundary-pict)
  (define-values (left right bkg line boundary-color) (make-bkg-and-line #f))
  (cc-superimpose bkg (inset line (- margin))))

(module+ main
  (applications))
