#lang racket/base
(require pict pict/tree-layout
         racket/match racket/contract)

(struct binary-heap (size vec) #:mutable #:transparent)
(define (valid-heap? heap)
  (match heap
    [(binary-heap size vec)
     (let loop ([i 0]
                [parent -inf.0])
       (cond
         [(< i size)
          (define this (vector-ref vec i))
          (and (<= parent this)
               (loop (left-child i) this)
               (loop (right-child i) this))]
         [else #t]))]))
(define heap/c (and/c binary-heap? valid-heap?))

(provide
 (contract-out
  
  [new-heap
   (-> heap/c)]
  
  [insert!
   (->i ([h heap/c] 
         [i integer?]) 
        [result void?]
        #:post (h) (valid-heap? h))]
  
  [delete!
   (->i ([h heap/c])
        [res (or/c integer? #f)]
        #:post (h) (valid-heap? h))]))

(define (new-heap) (binary-heap 0 (make-vector 1 #f)))

(define (insert! heap nv)
  (match heap
    [(binary-heap size vec)
     (unless (< size (vector-length vec))
       (define new-vec
         (make-vector (* (vector-length vec) 2) #f))
       (vector-copy! new-vec 0 vec)
       (set-binary-heap-vec! heap new-vec)
       (set! vec new-vec))
     (vector-set! vec size nv)
     (set-binary-heap-size! heap (+ size 1))
     (let loop ([i size] [iv nv])
       (unless (= i 0) 
         (define p (parent i))
         (define pv (vector-ref vec p))
         (when (< iv pv)
           (vector-set! vec p iv)
           (vector-set! vec i pv)
           (loop p iv))))]))

(define (delete! heap)
  (match heap
    [(binary-heap size vec)
     (cond
       [(= size 0) #f]
       [else
        (define ans (vector-ref vec 0))
        (vector-set! vec 0 (vector-ref vec (- size 1)))
        (set-binary-heap-size! heap (- size 1))
        (let ([size (- size 1)])
          (let loop ([i 0])
            (when (< i size)
              (define v (vector-ref vec i))
              (define li (left-child i))
              (define ri (right-child i))
              (when (< li size)
                (define-values (smaller-child-index smaller-child-val)
                  (cond
                    [(< ri size)
                     (define l (vector-ref vec li))
                     (define r (vector-ref vec ri))
                     (if (<= l r)
                         (values li l)
                         (values ri r))]
                    [else (values li (vector-ref vec li))]))
                (when (< smaller-child-val v)
                  (vector-set! vec smaller-child-index v)
                  (vector-set! vec i smaller-child-val)
                  (loop smaller-child-index))))))
        ans])]))

(define (left-child i) (+ (* i 2) 1))
(define (right-child i) (+ (* i 2) 2))
(define (parent i) (quotient (- i 1) 2))
