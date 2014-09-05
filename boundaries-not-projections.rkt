#lang racket
(require slideshow 
         slideshow/play
         "util.rkt" "faces.rkt")

(provide boundaries-not-projections
         not-projections)

  (define proj-word (scale/improve-new-text (bt "Projections") 4))
  (define boun-word (scale/improve-new-text (bt "Boundaries") 4))
  (define s (* (pict-width proj-word) 2/3))
  (define r (/ s 2))
  (define lw 80)
  (define (rb p) (linewidth lw (colorize p "red")))
  (define (proj-vs-boun proj boun)
    (cc-superimpose
     (proj (rb (circle s)))
     (proj
      (rb
       (dc (λ (dc dx dy)
             (send dc draw-line
                   (+ dx r (* (- r (/ lw 2)) (cos (* pi 1/4))))
                   (+ dy r (* (- r (/ lw 2)) (sin (* pi 1/4))))
                   (+ dx r (* (- r (/ lw 2)) (cos (* pi 5/4))))
                   (+ dy r (* (- r (/ lw 2)) (sin (* pi 5/4))))))
           s s)))
     (refocus (cc-superimpose
               (boun (colorize (scale/improve-new-text (bt "✓") 20) "forestgreen"))
               (vc-append (blank 0 100) (rc-superimpose
                                         (proj proj-word)
                                         (boun boun-word))))
              boun-word)))
  
  (define christos (get-bitmap 'christos))

(define (mk-proc n1)
  (lt-superimpose (cellophane (ht-append 20
                                         (scale christos 2/3)
                                         (vl-append (t "Christos")
                                                    (t "Dimoulas")))
                              n1)
                  (proj-vs-boun ghost values)))

(define (boundaries-not-projections) 
  (slide (proj-vs-boun values ghost))
  (play-n mk-proc))

(define (not-projections) (proj-vs-boun values ghost))
