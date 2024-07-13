#lang racket
(require racket/runtime-path
         slideshow
         slideshow/play
         racket/gui/base
         plot
         "../util.rkt")

(provide time-vs-contracts
         make-plot)

(define histogram-x-ticks-locations '(-128 -64 -16 16 64 128))

(define dummy-parameter (make-parameter #f))

(define-syntax (if-imported stx)
  (syntax-case stx ()
    [(_ id1 id2)
     (if (pair? (identifier-binding #'id1))
         #'id1
         #'id2)]))

(define maybe-plot-font-face (if-imported plot-font-face dummy-parameter))

(define-runtime-path commits+sums.rktd "commits+sums+seconds.rktd")

(define data (call-with-input-file commits+sums.rktd read))

(define non-zero-deltas
  (filter
   (compose not zero?)
   (for/list ([i (in-list data)]
              [j (in-list (cdr data))])
     (define count-i (list-ref i 0))
     (define count-j (list-ref j 0))
     (- count-j count-i))))

(define contracts-in-average-change
  (~r (/ (apply + non-zero-deltas) (length non-zero-deltas))
      #:precision 1))

(define (ellipse1)
  (inset (colorize (linewidth 12 (ellipse 50 150)) "red")
         150 380 0 0))

(define (ellipse2)
  (inset (colorize (linewidth 12 (ellipse 50 150)) "red")
         227 310 0 0))

(define (ellipse3)
  (inset (colorize (linewidth 12 (ellipse 70 180)) "red")
         417 200 0 0))

(define (time-vs-contracts)
  (define the-plot (make-plot))
  (play-n
   #:aspect 'widescreen
   (λ (n1 n2 n3)
     (rb-superimpose 
      (lt-superimpose 
       the-plot
       (cellophane (ellipse1)
                   (* n1 (- 1 (fast-start n2))))
       (cellophane (ellipse2)
                   (* n2 (- 1 (fast-start n3)))))
      (cellophane 
       (inset (if (zero? n3) (blank) histogram-bitmap)
              0 0 40 85)
       n3)))))

(define (make-plot #:width [width 900] #:height [height (* width 600/900)])
  (font-config
   (λ ()
     (parameterize ([plot-x-ticks no-ticks])
       (plot-pict
        #:y-label "Number of Contracts"
        #:x-label "Time (one unit of time is one commit; 10,000 commits total)"
        #:width width
        #:height (ceiling height)
        (list
         (let ([extra-x-space 120]
               [extra-y-space 20])
           (invisible-rect (- extra-x-space) ;; why?
                           (+ (length data) extra-x-space)
                           (- (apply min (map first data)) extra-y-space)
                           (+ (apply max (map first data)) extra-y-space)))
         (parameterize ([line-width 6])
           (lines
            #:color "black"
            (for/list ([contract-count+commit+date (in-list data)]
                       [i (in-naturals)])
              (vector i (first contract-count+commit+date)))))
         (mk-ticks)))))))

(define (mk-ticks)
  (define last-one (- (length data) 1))
  (define guess-at-minimum-gap-size 1100)
  (define previous-month/year #f)
  (define previous-i #f)
  
  (x-ticks
   (filter
    values
    (for/list ([contract-count+commit+seconds (in-list data)]
               [i (in-naturals)])
      (define date (seconds->date (list-ref contract-count+commit+seconds 2)))
      (define month (date-month date))
      (define year (date-year date))
      (cond
        [(or (= i last-one)
             (and (not (equal? (list month year) previous-month/year))
                  (or (not previous-i) 
                      (< (+ previous-i guess-at-minimum-gap-size) i))))
         (set! previous-month/year (list month year))
         (set! previous-i i)
         (tick i #t (format-date month year))]
        [else #f])))))
   

(define (format-date month year)
  (define mon (case month
                [(1) "Jan"]
                [(2) "Feb"]
                [(3) "Mar"]
                [(4) "Apr"]
                [(5) "May"]
                [(6) "Jun"]
                [(7) "Jul"]
                [(8) "Aug"]
                [(9) "Sep"]
                [(10) "Oct"]
                [(11) "Nov"]
                [(12) "Dec"]))
  (format "~a’~a" mon (modulo year 1000)))

(define (histogram)
  (define ht (make-hash))
  (define smallest 0)
  (define largest 0)
  (for ([this-contract-count+commit+date (in-list data)]
        [next-contract-count+commit+date (in-list (cdr data))])
    (define δ
      (- (first next-contract-count+commit+date)
         (first this-contract-count+commit+date)))
    (set! largest (max δ largest))
    (set! smallest (min δ smallest))
    (unless (zero? δ)
      (hash-set! ht δ (+ 1 (hash-ref ht δ 0)))))
  (for ([x (in-range smallest (+ largest 1))])
    (hash-set! ht x (hash-ref ht x 0)))
  (define hist-data (cons (list 0 0) (hash-map ht list)))
  (font-config 
   (λ ()
     (plot-pict #:width 420
                #:height 360
                #:x-label #f
                #:y-label #f
                (list
                 (discrete-histogram
                  #:add-ticks? #f
                  (sort hist-data < #:key first))
                 (x-ticks 
                  (for/list ([x (in-list histogram-x-ticks-locations)])
                    (tick (+ x (- smallest)) #t (format "~s" x)))))))))


(define (font-config thunk)
  (parameterize ([plot-font-size 18]
                 [plot-font-family slideshow-family]
                 [maybe-plot-font-face slideshow-face])
    (thunk)))
    
(define-values (slideshow-face slideshow-family)
  (let loop ([mf (current-main-font)])
    (match mf
      [(cons (? string? face) (? symbol? family))
       (values face family)]
      [(? string?)
       (values mf 'default)]
      [(? symbol?)
       (values #f mf)]
      [(cons (? symbol?) stuff)
       (loop stuff)])))

(define histogram-bitmap (freeze* (histogram) 0 0 0 0))

(module+ slideshow
  #;
  (slide 
   (lt-superimpose 
    (mk-plot)
    (ellipse1)
    (ellipse2)
    (ellipse3)))
  
  (time-vs-contracts))
