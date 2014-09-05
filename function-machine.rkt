#lang racket
(require "color.rkt"
         racket/gui/base
         slideshow)

(provide function-machine
         function-machine-with-white-insides-and-boundaries
         input-side-boundary
         function-machine-blue-boundary-thickness
         funnel-height)

(define before-funnel-dist 20)
(define after-funnel-dist 60)
(define funnel-height 30)
(define funnel-width 60)
(define box-height 60)
(define scale-factor 2)
(define function-machine-blue-boundary-thickness
  (* boundary-line-thickness 3/4 1/2))

(define (make-function-machine-steps function-machine-blue-boundary-thickness)
  (define x 0)
  (define y 0)
  (define pth (list (list 'move 0 0)))
  
  (define max-x 0)
  (define max-y 0)
  (define min-x 0)
  (define min-y 0)
  
  (define funnel-offset #f)
  
  (define θ 90)
  
  (define (move dist [draw? #t])
    (define α (degrees->radians θ))
    (set! x (+ x (* (cos α) dist)))
    (set! y (+ y (- (* (sin α) dist))))
    (set! max-x (max x max-x))
    (set! max-y (max y max-y))
    (set! min-x (min x min-x))
    (set! min-y (min y min-y))
    (set! pth (cons (list (if draw? 'draw 'move) x y) pth)))
  (define (turn θ′) (set! θ (+ θ θ′)))
  
  (define (draw-top/bottom-edge)
    (move before-funnel-dist)
    (turn -120)
    (move funnel-height)
    (set! funnel-offset y) ;; we want the 2nd one, so just clobber away
    (turn 120)
    (move funnel-width #f)
    (turn -60)
    (turn 180)
    (move funnel-height)
    (turn -180)
    (turn 60)
    (move after-funnel-dist))
  
  (draw-top/bottom-edge)
  (turn 90)
  (move box-height)
  (turn 90)
  (draw-top/bottom-edge)
  (turn 90)
  (move box-height)
  
  (values (for/list ([ent (in-list pth)])
            (define-values (move/draw x y) (apply values ent))
            (list move/draw 
                  (* (- x min-x) scale-factor)
                  (* (- y min-y) scale-factor)))
          (* (- max-x min-x) scale-factor)
          (* (- max-y min-y) scale-factor)
          (* (- funnel-offset min-y) scale-factor)))

(define-values (function-machine-steps
                function-machine-width
                function-machine-height
                funnel-offset)
  (make-function-machine-steps function-machine-blue-boundary-thickness))
    
(define function-machine-path (new dc-path%))
(send function-machine-path move-to 0 0)
(for ([x (in-list function-machine-steps)])
  (send function-machine-path line-to (list-ref x 1) (list-ref x 2)))
(send function-machine-path close)

(define (make-painted-function-machine inner-color)
  (dc (λ (dc dx dy)
        (define pen (send dc get-pen))
        (define brush (send dc get-brush))
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush inner-color 'solid)
        (send dc draw-path function-machine-path dx dy)
        (send dc set-pen "darkgray" 12 'solid)
        (send dc set-brush "gray" 'transparent)
        (for ([prev (in-list function-machine-steps)]
              [this (in-list (cdr function-machine-steps))])
          (define-values (m/d px py) (apply values prev))
          (define-values (_ tx ty) (apply values this))
          (when (equal? m/d 'draw)
            (send dc draw-line 
                  (+ px dx) (+ py dy)
                  (+ tx dx) (+ ty dy))))
        (send dc set-pen pen)
        (send dc set-brush brush))
      function-machine-width
      function-machine-height))

(define function-machine (make-painted-function-machine "black"))
(define painted-function-machine (make-painted-function-machine "white"))

(define function-machine-blue-line
  (dc
   (λ (dc dx dy)
     (define pen (send dc get-pen))
     (send dc set-pen blue-boundary-color
           (* 2 function-machine-blue-boundary-thickness)
           'solid)
     (send dc draw-line dx dy dx (+ dy (* scale-factor funnel-width)))
     (send dc set-pen pen))
   0 (* scale-factor funnel-width)))

(define input-side-boundary
  (launder (inset function-machine-blue-line 0 funnel-offset 0 0)))

(define (function-machine-with-white-insides-and-boundaries dom-ctc rng-ctc)
  (define dom-side (cc-superimpose input-side-boundary))
  (define rng-side (launder input-side-boundary))
  (define main
    (ht-append
     dom-side
     (lb-superimpose
      (hb-append (launder (ghost painted-function-machine))
                 (inset rng-side 0 0 0 funnel-offset))
      painted-function-machine)))
  (define-values (dx dy) (cc-find main dom-side))
  (define-values (rx ry) (cc-find main rng-side))
  (pin-over (pin-over main
                      dx dy
                      dom-ctc)
            rx ry rng-ctc))

(module+ slideshow
  
  (define (upside-down-tt str)
    (text str `(bold . modern) (current-font-size) (degrees->radians 180)))
  
  (slide function-machine)
  
  (slide 
   (cc-superimpose
    (inset (filled-rectangle 1024 768) (- margin))
    (function-machine-with-white-insides-and-boundaries
     (scale/improve-new-text (tt "(<=/c 3)") 1/4)
     (scale/improve-new-text (upside-down-tt "(<=/c 3)") 1/4)))))
