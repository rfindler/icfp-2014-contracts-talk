#lang racket
(require racket/runtime-path slideshow "util.rkt")
(provide get-bitmap
         (contract-out
          [show-team (->* () 
                          (#:orientation (or/c 'horizontal 'vertical))
                          #:rest (listof symbol?)
                          pict?)]))

(define-runtime-path sam.jpg "sam.jpg")
(define-runtime-path asumu.jpg "asumu.jpg")
(define-runtime-path eric.jpg "eric.jpg")
(define-runtime-path vincent.jpg "vincent.jpg")
(define-runtime-path david.jpg "david.jpg")
(define-runtime-path phil.jpg "phil.jpg")
(define-runtime-path amal.jpg "amal.jpg")
(define-runtime-path mgree.png "mgree.jpg")
(define-runtime-path christos.jpg "christos.jpg")
(define-runtime-path jesse.jpg "jesse.jpg")
(define-runtime-path riccardo.jpg "riccardo.jpg")

(define all-bitmaps
  (hash 'sam (bitmap sam.jpg)
        'asumu (bitmap asumu.jpg)
        'eric (bitmap eric.jpg)
        'vincent (bitmap vincent.jpg)
        'david (bitmap david.jpg)
        'phil (bitmap phil.jpg)
        'amal (bitmap amal.jpg)
        'mgree (bitmap mgree.png)
        'christos (bitmap christos.jpg)
        'jesse (bitmap jesse.jpg)
        'riccardo (bitmap riccardo.jpg)))

(define all-names
  (hash 'sam "Sam Tobin-Hochstadt"
        'asumu "Asumu Takikawa"
        'eric "Eric Dobson"
        'vincent "Vincent St-Amour"
        'david "David Van Horn"
        'phil "Phúc Nguyễn"
        'amal "Amal Ahmed"
        'mgree "Michael Greenberg"
        'christos "Christos Dimoulas"
        'jesse "Jesse Tov"
        'riccardo "Riccardo Pucella"))

(define (get-bitmap name)
  (hash-ref all-bitmaps name (λ () (error 'get-bitmap "unknown person ~s" name))))
(define (get-name name)
  (hash-ref all-names name (λ () (error 'get-bitmap "unknown person ~s" name))))

(define (size-up-and-align . picts)
  (define height (apply min
                        (for/list ([(k v) (in-hash all-bitmaps)])
                          (pict-height v))))
  (apply hc-append
         10
         (map (λ (x) (scale x (/ height (pict-height x))))
              picts)))

(define (size-up-and-align-vertically . picts)
  (define width (apply min
                        (for/list ([(k v) (in-hash all-bitmaps)])
                          (pict-width v))))
  (apply vc-append
         10
         (map (λ (x) (scale x (/ width (pict-width x))))
              picts)))

(define (show-team #:blue? [blue? #t] #:orientation [orientation 'horizontal] . names)
  (cond
    [(equal? orientation 'horizontal)
     (above-and-size
      (apply 
       size-up-and-align
       (map get-bitmap names))
      (colorize 
       (t (apply string-append
                 (add-between (map get-name names) ", ")))
       (if blue? "blue" "black")))]
    [else
     (apply 
      size-up-and-align-vertically
      (map get-bitmap names))]))

(define (above-and-size p1 p2)
  (vc-append p1 (scale p2 (/ (pict-width p1) (pict-width p2)))))
