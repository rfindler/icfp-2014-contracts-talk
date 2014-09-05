#lang racket
(require racket/runtime-path
         racket/date)

(define LOG.rktd "/Users/robby/Desktop/x/LOG.rktd")

(require plot)

;; used to explore the data, looking for strange commits
(define (find-diffs)
  (define previous-count #f)
  (define previous-commit #f)
  (define previous-hash #f)
  (process-log
   (λ (commit hash)
     (define count
       (for/sum ([(k v) (in-hash hash)])
         v))
     (when previous-count
       (unless (<= (abs (- previous-count count)) 50)
         (printf "~s ~s\n" previous-commit previous-count)
         (printf "~s ~s\n" commit count)
         #;
         (begin
           (for ([(k previous-count) (in-hash previous-hash)])
             (define this-count (hash-ref hash k #f))
             (unless (equal? previous-count this-count)
               (printf "~s => ~s  ~a\n" previous-count this-count k)))
           (for ([(k count) (in-hash hash)])
             (define previous-count (hash-ref previous-hash k #f))
             (unless previous-count
               (printf "~s => ~s  ~a\n" previous-count count k))))
         (printf "\n")))
     (set! previous-commit commit)
     (set! previous-count count)
     (set! previous-hash hash))))

;; this is like find-diffs, but uses the result of simplify-and-save-log
;; instead of using the full log.
(define (find-diffs/fast)
  (define all-of-them (call-with-input-file commits+sums+seconds.rktd read))
  (for ([commit+sum+seconds (in-list (cdr all-of-them))]
        [previous-commit+sum+seconds (in-list all-of-them)])
    (define count (list-ref commit+sum+seconds 0))
    (define previous-count (list-ref previous-commit+sum+seconds 0))
    (when (or ((abs (- previous-count count)) . >= . 60)
              (and (< count previous-count)
                   (not (<= (abs (- previous-count count)) 3))))
      (define commit (list-ref commit+sum+seconds 1))
      (define previous-commit (list-ref previous-commit+sum+seconds 1))
      (printf "~s ~s\n" previous-commit previous-count)
      (printf "~s ~s\n" commit count)
      (printf "diff ~a on ~a\n"
              (- count previous-count)
              (date->string (seconds->date (list-ref commit+sum+seconds 2))))
      (printf "\n"))))


(define-runtime-path committers-to-contracts.rktd "committers-to-contracts.rktd")
(define (rewrite-committers-to-contracts.rktd)
  (define all-of-them (call-with-input-file commits+sums+seconds.rktd read))
  (define committers->contracts (make-hash))
  (define committers->commit-counts (make-hash))
  (define committers->contract-delta (make-hash))
  (for ([commit+sum+seconds (in-list (cdr all-of-them))]
        [previous-commit+sum+seconds (in-list all-of-them)]
        [i (in-naturals)])
    (unless (zero? i) (when (zero? (modulo i 1000)) (printf "~a ...\n" i)))
    (define delta (- (list-ref commit+sum+seconds 0)
                     (list-ref previous-commit+sum+seconds 0)))
    (define the-hash (list-ref commit+sum+seconds 1))
    (define sp (open-output-string))
    (parameterize ([current-output-port sp]
                   [current-input-port (open-input-string "")]
                   [current-directory git-directory])
      (system (format "git log -n 1 --format=format:%an ~a" the-hash)))
    (define who (get-output-string sp))
    (define committer-table (hash-ref committers->contracts who (λ () (make-hash))))
    (hash-set! committer-table delta (list-ref commit+sum+seconds 1))
    (hash-set! committers->contracts who committer-table)
    (hash-set! committers->commit-counts
               who 
               (+ 1 (hash-ref committers->commit-counts who 0)))
    (hash-set! committers->contract-delta
               who 
               (+ delta (hash-ref committers->contract-delta who 0))))
  (define result
    (sort (hash-map committers->contracts 
                    (λ (who committer-table)
                      (list who
                            (hash-ref committers->contract-delta who)
                            (hash-ref committers->commit-counts who)
                            (sort (hash-map committer-table list)
                                  <
                                  #:key car))))
          <
          #:key cadr))
  (call-with-output-file committers-to-contracts.rktd
    (λ (port)
      (display 
       ";; (listof (list string[name] number[total-δ-contracts] number[total-commits]\n"
       port)
      (display
       ";;               (listof (list number <info>))[example commit for each different δ]))\n"
       port)
      (pretty-write result port))
    #:exists 'truncate))

;; simple plotting, based on the saved, actual data
(define (plot-data)
  (define sums (map first (compute-sums+commits)))
  (plot (lines
         (for/list ([v (in-list sums)]
                    [i (in-naturals)])
           (vector i v)))
        #:x-label "commits ago (time)" 
        #:y-label "number of contracts"))

(define (compute-sums+commits [show-progress? #f])
  (define sums+commits '())
  (process-log
   (λ (commit hash i)
     (when show-progress?
       (unless (zero? i) (when (zero? (modulo i 1000)) (printf "~a ...\n" i))))
     (set! sums+commits
           (cons (list (for/sum ([(k v) (in-hash hash)]
                                 #:unless (skip-file? k))
                         v)
                       commit)
                 sums+commits))))
  sums+commits)

(define (skip-file? filename)
  (define segments (map path->string (explode-path filename)))
  
  ;; some copied code in redex
  (or (and (member "redex-examples" segments)
           (member "benchmark" segments)
           (not (member "redex-benchmark" segments))
           ;; the files in the benchmark that end with this regexp are copies of other
           ;; files (well, until the time in the git history when they moved into
           ;; the redex-benchmark pkg (hence the line above))
           (regexp-match #rx"-[0-9]+[.]rkt" (last segments)))
      
      ;; the contract test suite
      (and (member "racket-pkgs" segments)
           (member "racket-test" segments)
           (member "contract" segments))))

;; save just the contract counts and the times to make building plots faster
(define-runtime-path commits+sums+seconds.rktd "commits+sums+seconds.rktd")
(define git-directory "/Users/robby/git/exp/plt")
(define (simplify-and-save-log)
  (define commits+sums (compute-sums+commits #t))
  (define commits+sums+seconds 
    (for/list ([commit+sum (in-list commits+sums)]
               [i (in-naturals)])
      (unless (zero? i) (when (zero? (modulo i 1000)) (printf "~a ...\n" i)))
      (define sum (list-ref commit+sum 0))
      (define commit-hash (list-ref commit+sum 1))
      (define the-hash (list-ref (regexp-match #rx"commit ([^ ]*)\n" commit-hash) 1))
      (define sp (open-output-string))
      (parameterize ([current-output-port sp]
                     [current-input-port (open-input-string "")]
                     [current-directory git-directory])
        (system (format "git log -n 1 --format=format:%ci ~a" the-hash)))
      (define date-string (get-output-string sp))
      (define-values (year month day hour min seconds)
        (apply 
         values
         (map
          string->number
          (cdr
           (regexp-match #rx"([0-9]*)-([0-9]*)-([0-9]*) ([0-9]*):([0-9]*):([0-9]*)" 
                         date-string)))))
      
      ;; move time forward a little bit for commits that
      ;; are during that non-existent time in 2010 
      ;; when we lept forward in central time's adjustment...
      (when (equal? '(2 14 3 2010)
                    (list hour day month year))
        (set! hour 3))
      
      (define the-seconds (find-seconds seconds min hour day month year))
      (list sum the-hash the-seconds)))
  (call-with-output-file commits+sums+seconds.rktd
    (λ (port)
      (fprintf 
       port
       ";; (listof (list/c nat[contract count] string[commit hash] number[seconds of commit]\n")
      (pretty-write commits+sums+seconds port))
    #:exists 'truncate))

;; generic log processing function
(define (process-log f)
  (call-with-input-file LOG.rktd
    (λ (port)
      (let loop ([previous-commit #f]
                 [previous-hash #f]
                 [i 0])
        (define commit (read port))
        (define hash (read port))
        (unless (eof-object? hash)
          (cond
            [(procedure-arity-includes? f 3)
             (f commit hash i)]
            [else 
             (f commit hash)])
          (loop commit hash (+ i 1)))))))
