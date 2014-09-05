#lang racket
(require plot racket/runtime-path)

(define-runtime-path committers-to-contracts.rktd "committers-to-contracts.rktd")
  
(define info (call-with-input-file committers-to-contracts.rktd read))

(plot
 (points
  (for/list ([x (in-list info)]
             #:unless (member (list-ref x 0)
                              '("Matthew Flatt"
                                "Robby Findler")))
    (vector (list-ref x 1)
            (list-ref x 2))))
 #:x-min -2
 #:y-min -10
 #:x-label "δ contracts"
 #:y-label "number of commits")
