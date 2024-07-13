#lang racket
(require slideshow/fullscreen
         slideshow/play
         plot
         "faces.rkt"
         "util.rkt"
         "counts/time-vs-contracts.rkt"
         "boundary.rkt"
         "boundaries-not-projections.rkt"
         "faces.rkt")

(provide recap
         final-thought)

(define four-quadrants
  (list (inset (scale-to-fit 
                (vc-append -16
                           (t "Contracts") 
                           (t "⊄") 
                           (inset (t "Types") 0 -6 0 0))
                client-w client-h)
               100)
        (vc-append (scale/improve-new-text (t "Contracts are infectious") 2)
                   (freeze* (parameterize ([plot-background "gray"])
                              (make-plot #:width 1000))))
        (inset (not-projections) 100)
        (vc-append -80
                   (scale/improve-new-text (t "Boundaries matter") 2)
                   (inset (boundary-pict) 100))))

(define same-size-four-quadrants
  (let ([w (apply max (map pict-width four-quadrants))]
        [h (apply max (map pict-height four-quadrants))])
    (for/list ([p (in-list four-quadrants)])
      (cc-superimpose p (blank w h)))))

(define base
  (table 2 same-size-four-quadrants cc-superimpose cc-superimpose 0 0))

(define the-lines
  (let ([w (pict-width base)]
        [h (pict-height base)])
    (cc-superimpose 
     (ghost (launder base))
     (dc (λ (dc dx dy) (send dc draw-line dx dy (+ dx w) dy)) w 1)
     (dc (λ (dc dx dy) (send dc draw-line dx dy dx (+ dy h))) 1 h))))

(define (go sub n1 n2)
  (define final-scale 1.8)
  (define final-spot (ghost (launder (scale sub final-scale))))
  (define main (ghost (cc-superimpose base final-spot)))
  (cond
    [(zero? n2)
     (launder
      (slide-pict 
       main
       (scale sub (+ (* n1 (- final-scale 1)) 1))
       sub
       final-spot
       n1))]
    [else
     (launder
      (slide-pict 
       main
       (scale sub (+ (* (- 1 n2) (- final-scale 1)) 1))
       final-spot
       sub
       n2))]))

(define (fade p me-out me-in . ns)
  (define cello-amt (apply compute-cello me-out me-in ns))
  (cellophane p cello-amt))

(define (compute-cello me-out me-in . ns)
  (cond
    [(andmap zero? (list* me-out me-in ns)) 1]
    [(or (< 0 me-in 1)
         (< 0 me-out 1))
     1]
    [else
     (let loop ([ns ns])
       (cond
         [(null? ns) (if (= me-out 1) 1 0)]
         [(< 0 (car ns) 1) (- 1 (car ns))]
         [(< 0 (cadr ns) 1) (cadr ns)]
         [(and (= (car ns) 1)
               (= (cadr ns) 0))
          0]
         [(and (= (cadr ns) 1)
               (or (and (pair? (cddr ns))
                        (= (caddr ns) 0))
                   (null? (cddr ns))))
          1]
         [else (loop (cddr ns))]))]))

(define (find-lines-cello . ns)
  (let loop ([ns ns]
             [prev #f])
    (cond
      [(null? ns) 1]
      [(< 0 (car ns) 1) (- 1 (car ns))]
      [(< 0 (cadr ns) 1) (cadr ns)]
      [(and (equal? prev 1) (= (car ns) 0)) 1]
      [(and (= (car ns) 1) (= (cadr ns) 0)) 0]
      [else (loop (cddr ns) (cadr ns))])))
         

(define (recap)
  (play-n
   #:aspect 'fullscreen
   #:steps '(10 10 20 20 20 10)
   (λ (n1 n1b n2-n3 n4-n5 n6-n7 n8)
     (define-values (n2 n3) (split n2-n3))
     (define-values (n4 n5) (split n4-n5))
     (define-values (n6 n7) (split n6-n7))
     (define pieces 
       (list (fade (go (list-ref same-size-four-quadrants 0) n1 n2)
                   n1 n2 n3 n4 n5 n6 n7 n8)
             (fade (go (list-ref same-size-four-quadrants 1) n3 n4)
                   n3 n4 n1 n2 n5 n6 n7 n8)
             (fade (go (list-ref same-size-four-quadrants 2) n5 n6)
                   n5 n6 n1 n2 n3 n4 n7 n8)
             (fade (go (list-ref same-size-four-quadrants 3) n7 n8)
                   n7 n8 n1 n2 n3 n4 n5 n6)))
     (define top-most 
       (cond
         [(not (zero? n7)) 3]
         [(not (zero? n5)) 2]
         [(not (zero? n3)) 1]
         [else 0]))
     (define (snoc x l) (append l (list x)))
     (set! pieces (snoc (list-ref pieces top-most)
                        (remove (list-ref pieces top-most) pieces)))
     (define lines-cello (find-lines-cello n1 n2 n3 n4 n5 n6 n7 n8))
     (lb-superimpose
      (cc-superimpose
       (inset (colorize (filled-rectangle 1024 768 #:draw-border? #f) "gray") (- margin))
       (scale-to-fit
        (apply
         cc-superimpose
         (cellophane the-lines lines-cello)
         (ghost base)
         pieces)
        client-w client-h))
      (cellophane 
       (hc-append (blank 50 0)
                  (vl-append 
                   affine-team+words
                   (blank 0 50)))
       (* n1b (- 1 n2)))))))

(define vertical-words
  (colorize
   (text "Jesse Tov & Riccardo Pucella [ESOP 2010]"
         (current-main-font)
         (current-font-size)
         (/ pi -2))
   "blue"))

(define affine-team (show-team #:orientation 'vertical 'jesse 'riccardo))

(define affine-team+words
  (ht-append 4 
             affine-team
             (scale vertical-words
                    (/ (pict-height affine-team)
                       (pict-height vertical-words)))))

(define (final-thought)
  (slide
   (vc-append
    (vl-append
     20
     (t "ICFP knows how to prove specifications.")
     (blank)
     (t "Contract infectiousness is our opportunity")
     (t "to share that knowledge with the world."))
    (blank 0 100)
    (t "加油!"))))

(module+ main (recap))
