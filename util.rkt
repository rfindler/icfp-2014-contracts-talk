#lang racket
(require slideshow racket/gui/base
         slideshow/code)

(current-font-size 40)
(current-page-number-color (send the-color-database find-color "darkgray"))
(current-keyword-list '("mon" "blame" "->" "->d"))

(provide interpolate-color interpolate split pin-over/% freeze*
         wrap/first-argument-always-1 subtitle
         color-code
         (contract-out
          [make-pict-namespace (-> namespace?)]))

(define (make-pict-namespace)
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'pict ns)
  (parameterize ([current-namespace ns])
    (namespace-require 'pict))
  ns)

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

(define freeze*
  (case-lambda
    [(p l t r b)
     (define insetted (inset p l t r b))
     (define bmp (make-bitmap (inexact->exact (ceiling (pict-width insetted)))
                              (inexact->exact (ceiling (pict-height insetted)))))
     (define bdc (make-object bitmap-dc% bmp))
     (send bdc set-smoothing 'aligned)
     (draw-pict insetted bdc 0 0)
     (send bdc set-bitmap #f)
     (inset (bitmap bmp) (- l) (- t) (- r) (- b))]
    [(p) (freeze* p 0 0 0 0)]))


(define (wrap/first-argument-always-1 f)
  (procedure-reduce-arity
   (λ args (apply f 1 args))
   (- (procedure-arity f) 1)))


(define (subtitle a b1 . bs)
  (slide
   (scale/improve-new-text
    (vl-append
     (scale/improve-new-text (t a) 4/5)
     (apply
      vl-append 
      (if (regexp-match? #rx"[ypq]" b1) -4 -10)
      (bt b1)
      (map bt bs)))
    3)))

(define (color-code color thunk)
  (parameterize ((current-comment-color color)
                 (current-keyword-color color)
                 (current-id-color color)
                 (current-literal-color color)
                 (current-const-color color)
                 (current-base-color color))
    (thunk)))
