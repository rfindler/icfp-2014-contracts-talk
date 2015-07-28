#lang racket
(require pict slideshow "util.rkt")


(provide
 (contract-out
  [ghost/p (-> (or/c pict? cp?) cp?)]
  [launder/p (-> (or/c pict? cp?) cp?)]
  [tt/p (-> string? cp?)]
  [nt/p (-> string? cp?)]
  [refocus/p (-> (or/c cp? pict?) (or/c cp? pict?) cp?)]
  [pict-width/p (-> cp? real?)]
  [pict-height/p (-> cp? real?)]
  [get-pict (-> cp? pict?)]
  [read-it (-> cp? (or/c eof-object? syntax?))]
  [read-all (-> cp? (listof syntax?))]
  [make-sure-contract (->* (cp?) (#:extras (listof any/c)) pict?)]
  [make-sure/defn (->* (cp?) (#:extras (listof any/c)) pict?)]
  [frame/p (-> cp? cp?)]
  [inject (-> pict? string? cp?)]
  [colorize/p (-> (or/c pict? cp?) string? cp?)]))

(provide blank/p
         inset/p
         cellophane/p
         freeze/p)


(struct cp (string-tree pict cello))

(define (inject p str) (cp str p 1))

(define-for-syntax (add/p id)
  (datum->syntax 
   id
   (string->symbol (format "~a/p" (syntax-e id)))))

(define-syntax (do-append/p stx)
  (syntax-case stx ()
    [(_ x-append extra)
     (with-syntax ([name (add/p #'x-append)])
       #'(begin
           (define name (make-append/p/proc x-append extra 'name))
           (provide
            (contract-out
             [name superimpose-append/p/c]))))]))

(define superimpose-append/p/c
  (->* ((or/c cp? pict?)) #:rest (listof (or/c cp? pict?)) cp?))

(define (make-append/p/proc x-append extra name)
  (define (result arg1 . args)
    (define cps (map (λ (x) (coerce-to-cp name x)) (cons arg1 args)))
    (define (combine2 a b) 
      (match a
        [(cp string-tree-a pict-a cello-a)
         (match b
           [(cp string-tree-b pict-b cello-b)
            (cp (if (and string-tree-a string-tree-b)
                    (cons string-tree-a string-tree-b)
                    (or string-tree-a string-tree-b))
                (x-append pict-a pict-b)
                (max cello-a cello-b))])]))
    (let loop ([cps cps])
      (cond
        [(null? (cdr cps)) (car cps)]
        [else (combine2 (car cps) (loop (cdr cps)))])))
  (procedure-rename result name))

(define (append-the-ports . ports)
  (define real-ports (filter values ports))
  (cond
    [(null? real-ports) #f]
    [else (apply input-port-append #t real-ports)]))

(do-append/p vl-append "\n")
(do-append/p vr-append "\n")
(do-append/p vc-append "\n")
(do-append/p hb-append "")
(do-append/p ht-append "")
(do-append/p hc-append "")
(do-append/p hbl-append "")
(do-append/p htl-append "")


(define (ghost/p a-cp)
  (match (coerce-to-cp 'ghost/p a-cp)
    [(cp string-tree pict cello)
     (cp #f (ghost pict) 0)]))

(define (launder/p a-cp)
  (match (coerce-to-cp 'launder/p a-cp)
    [(cp string-tree pict cello)
     (cp string-tree (launder pict) cello)]))

(define-syntax (do-superimpose/p stx)
  (syntax-case stx () 
    [(_ x-superimpose)
     (with-syntax ([name (add/p #'x-superimpose)])
       #'(begin
           (define name (make-superimpose/p/proc x-superimpose 'name))
           (provide (contract-out [name superimpose-append/p/c]))))]))

(define (make-superimpose/p/proc x-superimpose name)
  (define (result . args)
    (define cps (map (λ (x) (coerce-to-cp name x)) args))
    (define with-string-trees
      (sort (filter cp-string-tree cps)
            >
            #:key cp-cello))
    (define with-ones (filter (λ (x) (= 1 (cp-cello x))) with-string-trees))
    (cond
      [((length with-ones) . >= . 2)
       (error name 
              "expected only one argument to have content, got ~a"
              (length with-ones))]
      [else
       (define picts (map cp-pict cps))
       (define new-pict (apply x-superimpose picts))
       (cond
         [(null? with-string-trees) (cp #f new-pict 1)]
         [else 
          (define fst (car with-string-trees))
          (cp (cp-string-tree fst) new-pict (cp-cello fst))])]))
  (procedure-rename result name))

(do-superimpose/p lt-superimpose)
(do-superimpose/p lc-superimpose)
(do-superimpose/p lb-superimpose)
(do-superimpose/p lbl-superimpose)
(do-superimpose/p ltl-superimpose)
(do-superimpose/p ct-superimpose)
(do-superimpose/p cc-superimpose)
(do-superimpose/p cb-superimpose)
(do-superimpose/p cbl-superimpose)
(do-superimpose/p ctl-superimpose)
(do-superimpose/p rt-superimpose)
(do-superimpose/p rc-superimpose)
(do-superimpose/p rb-superimpose)
(do-superimpose/p rbl-superimpose)
(do-superimpose/p rtl-superimpose)


(define (tt/p str) (cp str (tt str) 1))
(define (nt/p str) (cp str (text str '(italic . roman) (current-font-size)) 1))

(define (coerce-to-cp who p)
  (cond
    ;; picts become literals in the ports
    [(pict? p) (cp p p 1)]
    [(cp? p) p]
    [else (error who "expected a cp or a pict, got ~e" p)]))


(define (read-all cp)
  (define t (cp-string-tree cp))
  (define-values (in out) (make-pipe-with-specials))
  (cond
    [t
     (thread
      (λ () 
        (let loop ([t t])
          (cond
            [(pair? t) 
             (loop (car t))
             (loop (cdr t))]
            [(string? t)
             (write-string t out)]
            [(pict? t)
             (write-special t out)]
            [else (error 'read-it "unknown thing: ~s" t)]))
        (close-output-port out)))]
    [else
     (close-output-port out)])
  (port-count-lines! in)
  (let loop ()
    (define ans (read-syntax 'cp in))
    (cond
      [(eof-object? ans) '()]
      [else (cons ans (loop))])))

(define (read-it cp)
  (define anses (read-all cp))
  (cond
    [(null? anses)
     eof]
    [(null? (cdr anses))
     (car anses)]
    [else
     (error 'read-it "found more than one expression: ~s" anses)]))

(define (get-pict cp)
  (cp-pict cp))

(define (blank/p . args)
  (cp #f (apply blank args) 0))


(define (pict-width/p a-cp) (pict-width (cp-pict a-cp)))
(define (pict-height/p a-cp) (pict-height (cp-pict a-cp)))

(define (inset/p a-cp . args) 
  (match (coerce-to-cp 'inset/p a-cp)
    [(cp string-tree pict cello)
     (cp string-tree (apply inset pict args) cello)]))

(define (frame/p a-cp)
  (match (coerce-to-cp 'frame/p a-cp)
    [(cp string-tree pict cello)
     (cp string-tree (frame pict) cello)]))

(define (cellophane/p a-cp n)
  (match (coerce-to-cp 'cellophane/p a-cp)
    [(cp string-tree pict cello)
     (define new-n (* cello n))
     (cp (if (zero? new-n) #f string-tree)
         (cellophane pict n)
         new-n)]))

(define (refocus/p p1 p2)
  (match (coerce-to-cp 'refocus/p p1)
    [(cp string-tree1 pict1 cello-1)
     (match (coerce-to-cp 'refocus/p p2)
       [(cp string-tree2 pict2 cello2)
        (cp string-tree1 (refocus pict1 pict2) cello-1)])]))

(define (colorize/p p c)
  (match (coerce-to-cp 'colorize/p p)
    [(cp string-tree pict cello)
     (cp string-tree (colorize pict c) cello)]))
  
(define (freeze/p a-cp l t r b)
  (match (coerce-to-cp 'freeze/p a-cp)
    [(cp string-tree pict cello)
     (cp string-tree (freeze* pict l t r b) cello)]))

(define (make-sure-contract a-cp #:extras [extras '()])
  (make-sure-ok a-cp 'contract? #:extras extras)
  (get-pict a-cp))

(define (make-sure/defn a-cp #:extras [extras '()])
  (define ns (make-pict-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/contract)
    (for ([extra (in-list extras)])
      (eval extra))
    (eval (read-it a-cp)))
  (get-pict a-cp))

(define (make-sure-ok a-cp ok? #:extras [extras '()])
  (define ns (make-pict-namespace))
  (define-values (is-contract? val)
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/contract)
      (eval '(define p-w pict-width))
      (eval '(define p-h pict-height))
      (for ([extra (in-list extras)])
        (eval extra))
      (define expr (read-it a-cp))
      (with-handlers ([exn:fail?
                       (λ (exn)
                         (eprintf "error while evaluating:\n")
                         (pretty-print (syntax->datum expr) (current-error-port))
                         (raise exn))])
        (eval `(let ([c ,expr])
                 (values (,ok? c) c))))))
  (unless is-contract?
    (error 'make-sure-contract "expected a contract, got ~s" val)))
