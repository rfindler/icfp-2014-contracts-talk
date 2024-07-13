#lang racket
(require "heap.rkt"
         "util.rkt"
         racket/runtime-path
         slideshow/fullscreen
         slideshow/code
         slideshow/play)

(provide show-heap-bugs
         provide-lines-pict)

(define (show-heap-bugs)
  (show-heap-bugs/contracts-in-out)
  (show-heap-bugs/first-bubble)
  (for ([p (in-list (bug-info-result-picts (car bug-infos)))])
     (slide (scale-to-fit p client-w client-h)))
  (show-heap-bugs/slide-one-bubble (car spots) (cadr spots))
  
  (for ([p (in-list (bug-info-result-picts (cadr bug-infos)))])
    (slide (scale-to-fit p client-w client-h)))
  
  (show-heap-bugs/all-but-first-two-bubbles)
  
  (define (even-out l)
    (cond
      [(even? (length l)) l]
      [else (append l (list (blank)))]))
  
  (slide (scale-to-fit 
          (table
           2
           (even-out
            (map (λ (p) (inset p 20 0))
                 (apply
                  append
                  (for/list ([bug-info (in-list (cddr bug-infos))])
                    (bug-info-result-picts bug-info)))))
           cc-superimpose cc-superimpose
           40 40)
          client-w client-h))
  
  (slide
   (scale-to-fit
    (pin-under
     background-with-spots
     first-lt-pict
     cc-find
     red-bubble)
    client-w client-h)))

(define (bubble during)
  (define dot (blank))
  (launder
   (refocus (cc-superimpose 
             dot
             (colorize
              (disk (+ 80 (max (pict-width during) (pict-height during)))
                    #:draw-border? #f)
              "red"))
            dot)))

(define-runtime-path heap.rkt "heap.rkt")

(define (for/lts f)
  (define exp
    (call-with-input-file heap.rkt
      (λ (port)
        (port-count-lines! port)
        (parameterize ([read-accept-reader #t])
          (read-syntax heap.rkt port)))))
  (define (cmp? x y) 
    (define sx (syntax-e x))
    (define sy (syntax-e y))
    (and (symbol? sx) (symbol? sy) (equal? sx sy)))
  (let loop ([exp exp])
    (syntax-case* exp (define valid-heap? visualize < <=) cmp?
      [(define (valid-heap? . whatever) . whatever2)
       (syntax->datum exp)]
      [(define (visualize . whatever) . whatever2)
       (syntax->datum exp)]
      [<
       (f exp)]
      [<=
       (f exp)]
      [(x ...)
       (for/list ([exp (in-list (syntax->list exp))])
         (loop exp))]
      [_ (syntax->datum exp)])))

(define lt-count
  (let ([n 0])
    (for/lts
     (λ (x) (set! n (+ n 1))))
    n))

(define columns 2)
(define (pull-in-file changed-lines #:break-up-lines? [break-up-lines? #t])
  (define lt-picts '())
  (define lines
    (call-with-input-file heap.rkt
      (λ (port)
        (for/list ([x (in-lines port)]
                   [i (in-naturals 1)])
          (cond
            [(member i changed-lines)
             (cond
               [break-up-lines?
                (define-values (main during) (break-up-line x))
                (set! lt-picts (cons during lt-picts))
                main]
               [else
                (set! lt-picts (cons (tt x) lt-picts))
                (car lt-picts)])]
            [else
             (tt x)])))))
  (define col-height (ceiling (/ (length lines) columns)))
  (define entire-program
    (let loop ([lines lines])
      (cond
        [(<= (length lines) col-height)
         (apply vl-append lines)]
        [else
         (ht-append
          (apply vl-append (take lines col-height))
          (loop (drop lines col-height)))])))
  (values entire-program
          (reverse lt-picts)))

(define (break-up-line line)
  (define m (regexp-match #rx"^(.*)(<[=]?)(.*)$" line))
  (unless m (error 'put-red-circle-behind-last-less-than 
                   "didn't find < or <= in ~s" line))
  (define-values (before during after) (apply values (map tt (cdr m))))
  (define main (hbl-append before during after))
  (values main during))

(struct bug-info (changed-program lt-pict result-picts))
(define (build-bug-infos)
  (for/list ([which-mutation (in-range lt-count)])
    (define which-lt 0)
    (define changed-line #f)
    (define mutant-program
      (for/lts (λ (stx)
                 (begin0
                   (cond
                     [(= which-lt which-mutation)
                      (set! changed-line (syntax-line stx))
                      (match (syntax->datum stx)
                        [`< >]
                        [`<= >=])]
                     [else stx])
                   (set! which-lt (+ which-lt 1))))))
    
    (define-values (exp exp-pict)
      (exp-and-pict (contract-exercise new-heap insert! delete!)))
    
    (define trials 0)
    (define result
      (parameterize ([current-namespace (make-pict-namespace)])
        (eval mutant-program)
        (eval '(require 'heap))
        (namespace-require 'racket/contract)
        (with-handlers ([exn:fail? exn-message])
          (for ([x (in-range 100)])
            (set! trials (+ x 1))
            (eval exp))
          (set! trials #f))))
    
    (printf "mut ~a: ~a\n"
            which-mutation
            (if trials
                (format "found in ~s trial~a"
                        trials
                        (if (= 1 trials) "" "s"))
                (format "NEVER FOUND")))
    
    (define-values (changed-program lt-picts) (pull-in-file (list changed-line)))
    (define linewidth 45)
    (define result-pict
      (cond
        [(string? result)
         (colorize
          (apply vl-append (map (λ (x) (tt (chop-to linewidth x)))
                                (regexp-split #rx"\n" result)))
          "red")]
        [else
         (vl-append
          (blank 0 20)
          (scale/improve-new-text (t "... nothing happens") 1.5)
          (tt (build-string linewidth (λ (x) #\space))))]))
    
    (define prompt+expression 
      (htl-append (tt "> ") exp-pict))
    
    (define result-with-caption 
      (vl-append
       prompt+expression 
       result-pict))
    
    (bug-info changed-program
              (car lt-picts)
              (if (zero? which-mutation)
                  (list 
                   (lt-superimpose
                    (blank client-w client-h)
                    prompt+expression)
                   (lt-superimpose
                    (blank client-w client-h)
                    result-with-caption))
                  (list result-with-caption)))))

(define-values (provide-start-line provide-end-line)
  (call-with-input-file heap.rkt
    (λ (port)
      (for/fold ([start #f][end #f]) ([line (in-lines port)][i (in-naturals 1)])
        (cond
          [(and (not start) (regexp-match #rx"provide" line))
           (values i #f)]
          [(and start (not end) (equal? "" line))
           (values start (- i 1))]
          [else 
           (values start end)])))))
(unless provide-end-line
  (error "didn't find blank line following provide"))

(define (chop-to len str)
  (cond
    [(<= (string-length str) len)
     str]
    [else
     (string-append (substring str 0 (- len 3)) "...")]))

(define-values (original-program provide-lines) 
  (pull-in-file 
   (for/list ([x (in-range provide-start-line
                           (+ provide-end-line 1))])
     x)
   #:break-up-lines? #f))

(define bug-infos (build-bug-infos))

(define provide-lines-pict
  (launder
   (let ([p (apply vl-append provide-lines)])
     (refocus (cc-superimpose 
               (colorize (filled-rectangle
                          (+ (pict-width p) 20)
                          (+ (pict-height p) 20)
                          #:draw-border? #t)
                         "white")
               p)
              p))))

(define provide-lines-dest-scale 3)
(define provide-lines-pict-dest
  (launder (ghost (scale provide-lines-pict provide-lines-dest-scale))))

(define background
  (cc-superimpose
   original-program
   (ghost (apply cc-superimpose (map bug-info-changed-program bug-infos)))
   provide-lines-pict-dest))

(define red-bubble (bubble (apply cc-superimpose (map bug-info-lt-pict bug-infos))))
(define first-lt-pict (bug-info-lt-pict (car bug-infos)))
  
(define-values (rev-spots background-with-spots)
  (for/fold ([spots '()] [background background]) ([bug-info (in-list bug-infos)])
    (define spot (blank))
    (values (cons spot spots)
            (pin-under background
                       (bug-info-lt-pict bug-info)
                       cc-find
                       spot))))
(define spots (reverse rev-spots))

(define (show-heap-bugs/contracts-in-out)
  (play-n
   #:aspect 'fullscreen
   (λ (contracts-out contracts-in)
     (scale-to-fit
      (if (= 0 contracts-in)
          (slide-pict
           (cc-superimpose
            background
            (cellophane (colorize (filled-rectangle
                                   (pict-width background)
                                   (pict-height background)
                                   #:draw-border? #f)
                                  "white")
                        (* .8 contracts-out)))
           (scale provide-lines-pict (+ (* contracts-out 
                                           (- provide-lines-dest-scale 1))
                                        1))
           (car provide-lines)
           provide-lines-pict-dest
           contracts-out)
          (slide-pict
           (cc-superimpose 
            background
            (cellophane (colorize (filled-rectangle
                                   (pict-width background)
                                   (pict-height background)
                                   #:draw-border? #f)
                                  "white")
                        (* .8 (- 1 contracts-in))))
           (scale provide-lines-pict (+ (* (- 1 contracts-in)
                                           (- provide-lines-dest-scale 1))
                                        1))
           provide-lines-pict-dest
           (car provide-lines)
          contracts-in))
      client-w client-h))))

(define (show-heap-bugs/first-bubble)
  (play-n
   #:aspect 'fullscreen
   #:skip-first? #t
   (λ (red-bubble-appears)
     (scale-to-fit
      (pin-under
       background-with-spots
       first-lt-pict
       cc-find
       (cellophane red-bubble red-bubble-appears))
      client-w client-h))))
  
(define (show-heap-bugs/all-but-first-two-bubbles)
  (play-n
   #:aspect 'fullscreen
   (λ (red-bubble-appears)
     (scale-to-fit
      (for/fold ([p background-with-spots]) ([bug-info (in-list (cddr bug-infos))])
        (define lt-pict (bug-info-lt-pict bug-info))
        (pin-under
         p
         lt-pict
         cc-find
         (cellophane red-bubble red-bubble-appears)))
      client-w client-h))))

(define (show-heap-bugs/slide-one-bubble last-spot this-spot)
  (play-n
   #:aspect 'fullscreen
   (λ (n)
     (scale-to-fit
      (slide-one-spot background-with-spots red-bubble last-spot this-spot n)
      client-w client-h))))

(define (slide-one-spot main to-slide last-spot this-spot n)
  (cc-superimpose 
   (launder (slide-pict (ghost main)
                        to-slide 
                        last-spot
                        this-spot
                        n))
   main))

(define-syntax-rule 
  (exp-and-pict e)
  (values `e (color-code "black" (λ () (code e)))))

(define-syntax-rule
  (quote-and-thunk e)
  (values (color-code "black" (λ () (code e)))
          (λ () e)))

(module+ slideshow
  (show-heap-bugs))
