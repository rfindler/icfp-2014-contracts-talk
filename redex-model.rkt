#lang racket
(require redex 
         slideshow/fullscreen
         slideshow/play
         "util.rkt")

(provide contracts L red typeof)

(define-language contracts
  (e ::=
     (mon τ a b e e)
     pred?
     (-> e e)
     (->i c e e)
     (any/c τ)
     k
     (if e e e)
     (+ e e)
     (e e)
     (λ (x τ) e)
     x
     (blame τ a))
  (τ ::= (-> τ τ) ι (ctc τ))
  (ι ::= N B)
  (k ::= number boolean)
  
  (x ::= variable-not-otherwise-mentioned)
  (a b c ::= (variable-prefix :))
  (pred? ::= 
         even? odd?
         positive? negative?))

(define-extended-language L contracts
  (v ::= 
     number
     boolean
     (λ (x τ) ... e)
     pred?
     (-> v v)
     (->i c v v)
     (any/c τ))
  (E ::= 
     hole
     (E e) (v E)
     (+ E e) (+ v E)
     (mon τ a b E e) 
     (mon τ a b v E)
     (-> E e) (-> v E)
     (->i c E e) 
     (->i c v E)
     (if E e e))
  (Γ ::= · (x τ Γ)))

(define-judgment-form L
  #:mode (typeof I I O)
  
  [-------------------
   (typeof Γ number N)]
  
  [--------------------
   (typeof Γ boolean B)]
  
  [(typeof Γ e_1 N) (typeof Γ e_2 N)
   ---------------------------------
   (typeof Γ (+ e_1 e_2) N)]
  
  [(typeof Γ e_1 B) (typeof Γ e_2 τ) (typeof Γ e_3 τ)
   --------------------------------------------------
   (typeof Γ (if e_1 e_2 e_3) τ)]
  
  [(where τ (lookup Γ x))
   ----------------------
   (typeof Γ x τ)]
  
  [(typeof Γ e_1 (-> τ_2 τ)) (typeof Γ e_2 τ_2)
   --------------------------------------------
   (typeof Γ (e_1 e_2) τ)]
  
  [(typeof Γ e_1 (ctc ι)) (typeof Γ e_2 ι)
   ---------------------------------------
   (typeof Γ (e_1 e_2) B)]
  
  [(typeof (extend Γ x_1 τ_1) e τ_2)
   ----------------------------------------
   (typeof Γ (λ (x_1 τ_1) e) (-> τ_1 τ_2))]
    
  [(typeof Γ e_1 (ctc τ))  
   (typeof Γ e_2 τ)
   --------------------------------
   (typeof Γ (mon τ a b e_1 e_2) τ)]
  
  [------------------------
   (typeof Γ (blame τ a) τ)]
  
  [----------------------------
   (typeof Γ (any/c τ) (ctc τ))]
  
  [(where ι (domof pred?))
   ------------------------
   (typeof Γ pred? (ctc ι))]
  
  [(typeof Γ e_1 (ctc τ_1))
   (typeof Γ e_2 (ctc τ_2))
   ------------------------------------------
   (typeof Γ (-> e_1 e_2) (ctc (-> τ_1 τ_2)))]
  
  [(typeof Γ e_1 (ctc τ_1)) 
   (typeof Γ e_2 (-> τ_1 (ctc τ_2)))
   ---------------------------------------------
   (typeof Γ (->i c e_1 e_2) (ctc (-> τ_1 τ_2)))])

(define-metafunction L
  lookup : Γ x -> τ or #f
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)]
  [(lookup · x) #f])

(define-metafunction L
  domof : pred? -> ι
  [(domof pred?) N])

(define-metafunction L
  extend : Γ x τ -> Γ
  [(extend Γ x τ) (x τ Γ)])

(require redex/tut-subst)
(define red 
  (reduction-relation
   L
   (--> (in-hole E ((λ (x τ) e) v))
        (in-hole E (subst e (x v))))
   (--> (in-hole E (pred? v))
        (in-hole E (δ pred? v)))
   (--> (in-hole E ((any/c ι) v))
        (in-hole E #t))
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E (Σ number_1 number_2)))
   (--> (in-hole E (if #f e_1 e_2)) (in-hole E e_2))
   (--> (in-hole E (if v e_1 e_2)) (in-hole E e_1)
        (side-condition (term (different v #f))))
   (--> (in-hole E (blame τ a)) (blame τ a)
        (side-condition (term (different E hole))))
   
   (--> (in-hole E (mon τ a b pred? v))
        (in-hole E (if (pred? v) v (blame τ a))))
   (--> (in-hole E (mon τ a b (any/c τ) v))
        (in-hole E v))
   (--> (in-hole E (mon (-> τ_1 τ_2) a b (-> v_1 v_2) v_3))
        (in-hole E (λ (x τ_1) 
                     (mon τ_2 a b
                          v_2
                          (v_3 (mon τ_1 b a v_1 x))))))
   (--> (in-hole E (mon (-> τ_1 τ_2) a b (->i c v_1 v_2) v_3))
        (in-hole E (λ (x τ_1)
                     (mon τ_2 a b
                          (v_2 (mon τ_1 b c v_1 x))
                          (v_3 (mon τ_1 b a v_1 x))))))))

(define ns (make-base-namespace))
(define-metafunction L
  [(δ pred? v)
   ,(parameterize ([current-namespace ns])
      (eval (term (pred? v))))])
(define-metafunction L
  [(Σ number ...) ,(apply + (term (number ...)))])
(define-metafunction L
  [(subst e (x v) ...)
   ,(subst/proc x? (term (x ...)) (term (v ...)) (term e))])
(define pre-x? (redex-match? L x))

;; technically, progress-and-preservation won't hold because of problems
;; with variables. Specifically, ((λ (:G N) (blame N :G)) 0) should be
;; disallowed in the syntax becuase variables shouldn't begin with colons,
;; but it is allowed. Random checking isn't likely to pick variables beginning
;; with colons, however, so this is mostly unnoticed. And adding a side-condition
;; to x would mean that define-judgment-form won't be able to generate random 
;; well-typed terms
(define (x? x) (and (pre-x? x) (not (regexp-match #rx"^:" (symbol->string x)))))
(define-metafunction L
  [(different any_1 any_1) #f]
  [(different any_1 any_2) #t])

(define (type-and-reduce t)
  (define init-τs (judgment-holds (typeof · ,t τ) τ))
  (cond
    [(or (null? init-τs) (not init-τs)) 'ill-typed]
    [(pair? (cdr init-τs)) 'multiple-types]
    [else
     (let loop ([t t])
       (define nexts (apply-reduction-relation red t))
       (cond
         [(null? nexts) t]
         [(null? (cdr nexts))
          (define next (car nexts))
          (define next-τs (judgment-holds (typeof · ,next τ) τ))
          (cond
            [(equal? next-τs init-τs) 
             (loop next)]
            [else (list 'intermediate-state-illtyped next)])]
         [else
          'multiple-types]))]))

(test-->> red #:cycles-ok (term (((λ (x N) x) (λ (x N) (x x))) (λ (x N) (x x)))))
(test-equal (type-and-reduce (term (even? 1))) (term #f))
(test-equal (type-and-reduce (term (positive? 1))) (term #t))
(test-equal (type-and-reduce (term (+ 1 (mon N :a :b positive? 1)))) (term 2))
(test-equal (type-and-reduce (term (+ 1 (mon N :a :b even? 1)))) (term (blame N :a)))
(test-equal (type-and-reduce (term ((λ (x (-> N N)) (x 3)) (λ (x N) (+ x 1))))) (term 4))
(test-equal (type-and-reduce (term ((λ (x (ctc N)) (x 3)) positive?))) (term #t))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b (-> positive? even?) (λ (x N) x)) 2)))
            (term 2))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b (-> positive? even?) (λ (x N) x)) 1)))
            (term (blame N :a)))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b (-> positive? even?) (λ (x N) x)) -1)))
            (term (blame N :b)))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (->i :c positive? (λ (x N) (if (even? x) 
                                                                        positive?
                                                                        negative?)))
                                         (λ (x N) x))
                                    2)))
            (term 2))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (->i :c positive? (λ (x N) (if (even? x) 
                                                                        positive?
                                                                        negative?)))
                                         (λ (x N) x))
                                    3)))
            (term (blame N :a)))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (->i :c positive? (λ (x N) (if (even? x) 
                                                                        positive?
                                                                        negative?)))
                                         (λ (x N) x))
                                    -2)))
            (term (blame N :b)))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (->i :c negative? (λ (x N) (if (even? x) 
                                                                        positive?
                                                                        negative?)))
                                         (λ (x N) x))
                                    -2)))
            (term (blame N :a)))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (any/c (-> N N))
                                         (λ (x N) x))
                                    (mon N :c :d (any/c N) 1))))
            (term 1))
(test-equal (type-and-reduce (term ((mon (-> N N) :a :b
                                         (-> (any/c N) (any/c N))
                                         (λ (x N) x))
                                    (mon N :c :d (any/c N) 1))))
            (term 1))

(test-equal (type-and-reduce (term ((mon (-> (-> N B) N) :a :b
                                         (->i :c
                                              (-> positive? (any/c B))
                                              (λ (f (-> N B)) (if (f -1) even? positive?)))
                                         (λ (f (-> N B)) 3))
                                    (λ (x N) #true))))
            (term (blame N :c)))

(define is-blame? (redex-match? L (blame τ a)))
(define v? (redex-match? L v))

(define do-print (make-parameter #f))
(define (progress-and-preservation orig-e)
  (define τs (judgment-holds (typeof · ,orig-e τ) τ))
  (cond
    [(null? τs) #t]
    [(pair? (cdr τs))
     (when (do-print) (eprintf "more than one type: ~s\n" τs))
     #f]
    [else
     (let loop ([e orig-e])
       (cond
         [(is-blame? e) #t]
         [else
          (define τ2s (judgment-holds (typeof · ,e τ) τ))
          (cond
            [(equal? τs τ2s)
             (define nexts (apply-reduction-relation red e))
             (cond
               [(null? nexts) 
                (cond
                  [(v? e) #t]
                  [else 
                   (when (do-print) (eprintf "irreducible non-value, non-blame ~s\n" e))
                   #f])]
               [(pair? (cdr nexts)) 
                (when (do-print) (eprintf "ambiguous rewrite: ~s => ~s\n" e nexts))
                #f]
               [else (loop (car nexts))])]
            [else
             (when (do-print)
               (eprintf "ill-typed ~s ~s, but reachable from ~s\n"
                        τ2s
                        e orig-e))
             #f])]))]))

(module+ main
  (redex-check L e (progress-and-preservation (term e)))
  
  (redex-check L #:satisfying (typeof · e τ) 
               (progress-and-preservation (term e))))
