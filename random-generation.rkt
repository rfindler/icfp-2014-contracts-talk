#lang racket
(require slideshow slideshow/play "util.rkt")
(provide big-type/us-them)

(define (arr)
  (parameterize ([current-main-font "Tex Gyre Bonum"])
    (t "→")))

(define (add-link from to up? n p)
  (cc-superimpose
   p
   (cellophane
    (linewidth
     10
     (launder
      (pin-arrows-line 30 
                       (ghost p)
                       from 
                       (if up? ct-find cb-find)
                       to 
                       (if up? ct-find cb-find)
                       #:start-angle (if up? (* pi 1/2) (* pi -1/2))
                       #:end-angle (if up? (* pi -1/2) (* pi 1/2))
                       #:start-pull .4
                       #:end-pull .4)))
    n)))

(define (add-label txt dist n p)
  (define args (list p (cellophane (scale/improve-new-text (t txt) 2) n)))
  (when (negative? dist)
    (set! dist (- dist))
    (set! args (reverse args)))
  (refocus (apply vc-append dist args) p))
  
(define-syntax-rule
  (scale-em e ...)
  (list (scale/improve-new-text e 2.5) ...))

(define arr-type-pieces
  (scale-em
   (t "(")
   (t "A")
   (t " ")
   (arr)
   (t " ")
   (t "B")
   (t ") ")
   (arr)
   (t " (")
   (t "C")
   (t " ")
   (arr)
   (t " ")
   (t "D")
   (t ")")))

(define A (list-ref arr-type-pieces 1))
(define B (list-ref arr-type-pieces 5))
(define C (list-ref arr-type-pieces 9))
(define D (list-ref arr-type-pieces 13))

(define whole-type (apply hbl-append arr-type-pieces))

(define (big-type/us-them)
  (play-n
   (λ (n1 n2)
     (add-label
      "we provide" 100 n1
      (add-label
       "we get to use" -50 n2
       (add-link 
        A D #t n2
        (add-link 
         B C #f n1
         whole-type)))))))

(module+ main (big-type/us-them))