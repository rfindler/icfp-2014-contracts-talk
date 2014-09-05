#lang racket
(require pict)
(provide render-bytes)

(define (render-bytes b1 b2 n)
  (define bs1 (format "~s" b1))
  (define bs2 (format "~s" b2))
  (define chop-it
    (cond
      [(or (equal? bs1 bs2)
           (not (equal? (substring bs1 0 (- n 4))
                        (substring bs2 0 (- n 4)))))
       (λ (s) (string-append (substring s 0 (- n 4)) "...\""))]
      [else
       (define difference-start
         (for/or ([i (in-range (min (string-length bs1) (string-length bs2)))])
           (and (not (equal? (string-ref bs1 i)
                             (string-ref bs2 i)))
                i)))
       (cond
         [(and difference-start 
               (<= (+ difference-start n) (string-length bs1)))
          (λ (s)
            (string-append "#\"..."
                           (substring s
                                      difference-start 
                                      (+ difference-start n -9))
                           "...\""))]
         [else
          (error
           'render-bytes
           (string-append
            "don't handle this case yet (when one is a substring of the other or"
            " when the difference is near the end)"))])]))
  (values (chop-it bs1)
          (chop-it bs2)))

(require rackunit)
(check-equal?
 (call-with-values (λ () (render-bytes (make-bytes 100 255)
                                       (make-bytes 100 255)
                                       20))
                   list)
 (list "#\"\\377\\377\\377\\3...\"" 
       "#\"\\377\\377\\377\\3...\""))
(check-equal?
 (call-with-values (λ () (render-bytes (make-bytes 100 255)
                                       (make-bytes 100 254)
                                       20))
                   list)
 (list "#\"\\377\\377\\377\\3...\"" 
       "#\"\\376\\376\\376\\3...\""))
(check-equal?
 (call-with-values (λ () 
                     (define b1 (make-bytes 100 255))
                     (bytes-set! b1 50 0)
                     (bytes-set! b1 51 0)
                     (render-bytes (make-bytes 100 255)
                                   b1
                                   20))
                   list)
 (list "#\"...377\\377\\377...\""
       "#\"...0\\0\\377\\377...\""))
