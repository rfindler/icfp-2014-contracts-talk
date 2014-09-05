#lang racket
(require racket/gui/base) ;; make sure GUI is loaded so snipclasses load

(define the-git-dir "/Users/robby/x/plt")
(define logfile "/Users/robby/x/LOG.rktd")
(current-directory the-git-dir)

(define (get-counts)
  
  (define (main)
    (for ([file (in-directory the-git-dir
                              #;
                              (simplify-path
                               (build-path (collection-file-path "id-table.rkt" "syntax")
                                           'up)))])
      (define pth (explode-path file))
      (unless (member "typed-racket-test" (map path->string pth))
        (when (regexp-match #rx"[.]rkt$" (path->string file))
          (set! current-file file)
          ;(count-a-contract 0) ;; make the logs a lot smaller by not doing this
          (define p (open-input-file file))
          (dynamic-wind
           void
           (λ () (process-file p))
           (λ () (close-input-port p)))))))
  
  (define unknowns (make-hash))
  (define counts (make-hash))
  (define current-file #f)
  (define (count-a-contract k)
    (define key (path->string current-file))
    (hash-set! counts key (+ k (hash-ref counts key 0))))
  
  (define (process-file port)
    (port-count-lines! port)
    (define exp 
      (with-handlers ([exn:fail? (λ (x) 
                                   ;(eprintf "~a:\n" file)
                                   ;((error-display-handler) (exn-message x) x)
                                   #f
                                   )])
        (parameterize ([read-accept-reader #t])
           (read port))))
    (process-sexp port exp))
  
  (define (process-sexps port)
    (let loop ()
      (define exp (read port))
      (cond
        [(eof-object? exp) (void)]
        [else 
         (process-sexp port exp)
         (loop)])))
  
  (define (process-sexp port sexp)
    (define prefixes '())
    (define (matches? sym)
      (λ (x)
        (or (equal? sym x)
            (and (symbol? x)
                 (for/or ([prefix (in-list prefixes)])
                   (equal? (string->symbol (string-append prefix (symbol->string sym)))
                           x))))))    
    
    (let loop ([sexp sexp])
      (match sexp
        [`(prefix-in ,prefix . ,whatever)
         (when (symbol? prefix)
           (set! prefixes (cons (symbol->string prefix) prefixes)))]
        [`(,(or (? (matches? 'provide/contract))
                (? (matches? 'provide/cond-contract))
                (? (matches? 'contracted)))
           [,f ,ctc ...] ...)
         ;; contracted does not current appear to be used outside of a signature
         (count-a-contract (length f))]
        [`(,(? (matches? 'cond-contracted)) ,id ,ctc)
         (count-a-contract 1)]
        [`(,(? (matches? 'contract-out)) [,f ,ctc ...] ...)
         (count-a-contract (length f))]
        [`(recontract-out ,x ...)
         (count-a-contract (length x))]
        [`(,(or (? (matches? 'define/contract))
                (? (matches? 'define/cond-contract)))
           . ,whatever)
         (count-a-contract 1)]
        [`(,(? (matches? 'with-contract)) ,whatever ...)
         (count-a-contract 1)]
        [`(,(or (? (matches? 'def-contract))
                (? (matches? 'define-struct/contract))
                (? (matches? 'define-struct/cond-contract)))
           . ,whatever)
         ;; decided to count this a single contract, protecting the entire struct
         ;; possibly this is the wrong way to look at this tho, not sure.
         ;;
         ;; def-contract is a macro used in TR that expands into define-struct/contract
         (count-a-contract 1)]
        [(? list?) (for-each loop sexp)]
        [(? symbol?)
         (when (regexp-match #rx"contract" (symbol->string sexp))
           (unless (hash-ref unknowns sexp #f)
             (hash-set! unknowns sexp #t)
             (define-values (line col pos) (port-next-location port))
             '(eprintf "aha! ~s ~a:~a:~a\n" 
                       sexp 
                       current-file
                       line col)))]
        [_ (void)])))
  (main)
  counts)

;; after 12305, things get strange with merge commits and maybe other things
(for ([x (in-range 12305)])
  (printf "~a\n" x)
  (system (format "git checkout master~~~a" x))
  (define sp (open-output-string))
  (parameterize ([current-output-port sp])
    (system "git log -n 1 | head -1"))
  (call-with-output-file logfile
    (λ (port)
      (write (get-output-string sp) port)
      (newline port))
    #:exists 'append)
  (define hash (get-counts))
  (call-with-output-file logfile
    (λ (port)
      (write hash port)
      (newline port))
    #:exists 'append))
