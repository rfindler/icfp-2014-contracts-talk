#lang racket
(require "color.rkt"
         slideshow /fullscreen
         slideshow/code
         slideshow/play
         racket/draw)

(provide send-value-in send-value-out bad-value-in-boundary)

(define (boundary ctc
                  value-entering-boundary
                  #:bad-value? [bad-value? #f]
                  #:going? [going? #f]
                  #:% [% 0]
                  #:circle-background [circle-background 0])
  (define boundary-color (if bad-value? red-boundary-color blue-boundary-color))
  (define line-size 500)
  (define circle-size (+ 100
                         (max (pict-width value-entering-boundary) 
                              (pict-height value-entering-boundary))))
  (define bkg (hc-append (colorize (filled-rectangle 512 768 #:draw-border? #f) "black")
                         (colorize (filled-rectangle 512 768 #:draw-border? #f) "white")))
  (define line (colorize (filled-rectangle line-size 768 #:draw-border? #f)
                         boundary-color))
  (define ctc-space 10)
  (define amt-to-clip (- (* (- 1 %) (+ line-size circle-size))))
  (define half-of-center-circle
    (clip
     (inset
      (inset
       (cc-superimpose
        (colorize (disk circle-size) (if going? "black" "white"))
        (colorize (linewidth line-size (circle circle-size))
                  boundary-color))
       line-size)
      (if going? amt-to-clip 0)
      0 
      (if going? 0 amt-to-clip)
      0)))
  (define center-circle
    (refocus ((if going? rc-superimpose lc-superimpose)
              half-of-center-circle
              (inset (cellophane 
                      (colorize 
                       (disk (+ circle-size line-size) #:draw-border? #f)
                       boundary-color)
                      circle-background)
                     (/ line-size 2)))
             half-of-center-circle))
  
  (define ctc+bkg
    (pin-over (colorize (filled-rectangle
                         (+ (pict-width ctc) ctc-space ctc-space)
                         (+ (pict-height ctc) ctc-space ctc-space)
                         #:draw-border? #f)
                        boundary-color)
              ctc-space
              ctc-space
              ctc))
  (cc-superimpose
   (inset bkg (- margin))
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

(define (interpolate-color c1 c2 %)
  (define c-start (if (string? c1) (send the-color-database find-color c1) c1))
  (define c-end (if (string? c2) (send the-color-database find-color c2) c2))
  (make-object color% 
    (inexact->exact (round (interpolate (send c-start red) (send c-end red) %)))
    (inexact->exact (round (interpolate (send c-start green) (send c-end green) %)))
    (inexact->exact (round (interpolate (send c-start blue) (send c-end blue) %)))))

(define (split n) 
  (cond
    [(<= n .5) (values (* n 2) 0)]
    [else (values 1 (* (- n .5) 2))]))

(define (pin-over/% m dx dy p)
  (pin-over m 
            (- (* dx (pict-width m)) (/ (pict-width p) 2))
            (- (* dy (pict-height m)) (/ (pict-height p) 2))
            p))

(define (interpolate start stop n)
  (+ start (* (- stop start) n)))

(define (send-value-in ctc value)
  (λ (n)
    (define-values (n1 n2) (split n))
    (pin-over/%
     (boundary ctc
               value
               #:% (/ n1 2)
               #:circle-background n2)
     (interpolate 3/4 1/2 n1)
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
     (interpolate 1/2 1/4 n2)
     1/2
     value)))

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
