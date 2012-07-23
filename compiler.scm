#lang racket

(require racket/match)
(require racket/mpair)
 
(struct til-anon-lambda (param-count body) #:transparent)
(struct til-lambda (name param-count body) #:transparent #:mutable)
(struct til-inline (body)                  #:transparent)

(define (til-anon-lambda->til-lambda name tal)
  (til-lambda name 
              (til-anon-lambda-param-count tal)
              (til-anon-lambda-body        tal)))

(define til_params
  (hash 
   'til_lambdify 1 
   'til_push     1))

(define (range x)
  (define (range-helper h l)
    (if (= h l) 
        '()
        (cons l (range-helper h (+ l 1)))))
  (range-helper x 0))
 
(define (tdisplay label x)
  (display label) (display ": ") (display x) (newline) x)

(define (assmap f x)
  (map (λ (x) (list (car x) (f (cadr x)))) x))

(define (til-lambda-map-body f x)
  (map (λ (x) (struct-copy til-lambda x [body (f (til-lambda-body x))])) x))

(define (mapfoldl f acc l)
  (define (mapfoldl-helper f acc l l2)
    (if (empty? l) 
        (cons acc (reverse l2))
        (let ([result (f (car l) acc)])
          (mapfoldl-helper f (car result) (cdr l) (cons (cdr result) l2)))))
  (mapfoldl-helper f acc l '()))
        

(define (compile-primitive p bindings)
  (match p
    ['+              '(til_push_fake_frame til_lambdify til_add)]
    ['-              '(til_push_fake_frame til_lambdify til_sub)]
    ['*              '(til_push_fake_frame til_lambdify til_mult)]
    ['/              '(til_push_fake_frame til_lambdify til_div)]
    ['=              '(til_push_fake_frame til_lambdify til_eq)]
    ['sqrt           '(til_push_fake_frame til_lambdify til_sqrt)]
    ['display-int    '(til_push_fake_frame til_lambdify til_display_int)]
    ['?              '(til_push_fake_frame til_lambdify til_if)]
    [_ #f]))

(define (get-binding b bindings)
  (define (get-binding-helper b bindings l)
    (if (empty? bindings)
        #f
        (match (assq b (car bindings))
          [#f               (get-binding-helper b (cdr bindings) l)]
          [(list b address) (cons l address)])))
  (get-binding-helper b bindings 0))

(define n2s number->string)

(define (compile-binding v bindings)
  (define b (get-binding v bindings))
  (and b `( (til_prm ,(car b) ,(cdr b)) )))
  
(define (fail-f x)
  (if x x (error "failed on false")))

(define (compile-apply f params bindings)
  (let ([compile-code (λ (code) (compile-code code bindings))])
    (append 
     (apply append (map compile-code (reverse params)))
     (append (compile-code f) '(til_call)))))


(define (compile-simple simple bindings)
  (cond
    [(number? simple) (list 'til_push simple)]
    [(symbol? simple) (fail-f (or (compile-primitive simple bindings)
                                  (compile-binding simple bindings)))]
    [else             (error "compile-simple cond error")]))
    
(define (compile-λ params body bindings)
  (let ([new-bindings (cons
                       (map list params (range (length params)))
                       bindings)])
    (list '(til_push_frame 0) 'til_lambdify (til-anon-lambda (length params)
                                                             (append
                                                              (compile-code body new-bindings) 
                                                              `(til_popret ,(* (+ (length params) 1) 8)))))))

(define (compile-let ribs body)
  (define params (map car ribs))
  (define values (map cadr ribs))
  `((λ ,params ,@body) ,@values))

(define (compile-let* ribs body)
  (foldr
   (λ (rib bodyacc) 
     (if (eq? bodyacc body)
         `(let (,rib) ,@bodyacc)
         `(let (,rib) ,bodyacc)))
   body
   ribs))

(define (compile-letrec ribs body)
  (define params (map car ribs))
  (define dead-ribs (map (λ (x) `(,x 0)) params))
  (define sets (map (λ (x) `(set! ,@x)) ribs))
  `(let ,dead-ribs (begin ,@sets ,@body)))

(define (compile-begin list bindings)
  (apply append (map (λ (x)
                       (let ([compiled (compile-code list bindings)])
                         (if (eq? x (car list))
                             compiled
                             (cons 'til_drop compiled))))
                     list)))
                           
(define (compile-set! v value bindings)                      
  (define b (get-binding v bindings))
  (append (compile-code value bindings) 
          `( (til_set ,(car b) ,(cdr b)) )))
  
(define (compile-if cond true false)
  `(? ,cond (λ () ,true) (λ () ,false)))

(define (compile-code code bindings)
  (match code
    [(cons 'let (cons ribs body))    (compile-code (compile-let ribs body) bindings)]
    [(cons 'let* (cons ribs body))   (compile-code (compile-let* ribs body) bindings)]
    [(cons 'letrec (cons ribs body)) (compile-code (compile-letrec ribs body) bindings)]
    [(list 'if cond true false)      (compile-code (compile-if cond true false) bindings)]
    
    [(cons 'begin rest)       (compile-begin rest bindings)]
    [(list 'λ params body)    (compile-λ params body bindings)]
    [(list 'set! param value) (compile-set! param value bindings)]
    [(cons f params)          (compile-apply f params bindings)]
    [simple                   (compile-simple simple bindings)]
    ))

(define (flatten-code code)
  (define (flatten-single code lambdas)
    (mapfoldl (λ (x lambdas)
                (if (til-anon-lambda? x)
                    (let ([lamsym (gensym)])
                      (cons (cons (til-anon-lambda->til-lambda lamsym x) lambdas) lamsym))
                    (cons lambdas x)))
              lambdas
              code))
  (define (flatten-lambdas lambdas)
    (define flattened-once
      (mapfoldl (λ (x newlambdas)
                  (let ([flattened (flatten-single (til-lambda-body x) newlambdas)])
                    (cons (car flattened) (struct-copy til-lambda x [body (cdr flattened)]))))
                '()
                lambdas))
    (define newlambdas (car flattened-once))
    (define oldlambdas (cdr flattened-once))
    (if (empty? newlambdas)
        oldlambdas
        (flatten-lambdas (append newlambdas oldlambdas))))
  (flatten-lambdas (list (til-lambda 'main 0 (append code (list 'til_exit))))))

(define (maybe-cons x xs)
  (and xs (cons x xs)))

(define (maybe-car x)
  (and x (not (empty? x)) (car x)))

(define (maybe-append l1 l2)
  (and l1 l2 (append l1 l2)))

(define (compare-pattern lst pattern with-rest)
  (define (chew-one-token) (compare-pattern (cdr lst) (cdr pattern) with-rest))
  (cond
    [(empty? pattern)                 (if with-rest (list lst) '())]
    [(empty? lst)                      #f]
    [(and (list? (car pattern)) 
          (eq? (caar pattern) 'quote)) (and (eq? (cadar pattern) (car lst))
                                            (chew-one-token))]
    [(list? (car pattern))             (and (list? (car lst))
                                            (maybe-append (compare-pattern (car lst) (car pattern) with-rest)
                                                          (chew-one-token)))]
    [(eq? '_ (car pattern))            (chew-one-token)]
    [(symbol? (car pattern))           (maybe-cons (map car (list pattern lst)) 
                                                   (chew-one-token))]
    [else (error "Bad pattern")]))

(define (replace-list-pattern lst pattern newpattern)
  (define compare (λ (x y) (compare-pattern x y #t)))
  (define (assign pattern matches)
    matches
    (match pattern
      [(list )                         '()]
      [(cons (list 'quote value) rest) (cons value                   (assign rest matches))]
      [(cons x rest)                   (cons (cadr (assq x matches)) (assign rest matches))]))
  (if (empty? lst)
      '()
      (match (compare lst pattern)
        [#f (cons   (car lst)             (replace-list-pattern (cdr lst) pattern newpattern))]
        [m  (append (assign newpattern m) (replace-list-pattern (last m)  pattern newpattern))])))

(define (match-list-pattern lst pattern)
  (define compare (λ (x y) (compare-pattern x y #f)))
  (if (empty? lst)
      '()
      (match (compare lst pattern)
        [#f (match-list-pattern (cdr lst) pattern)]
        [m  (cons m (match-list-pattern (cdr lst) pattern))])))

(define (optimize-calls lambdas)
  ; TODO: this should only be done on code begins, and not data
  (til-lambda-map-body
   (λ (code)
     (foldl (λ (transform code)
              (replace-list-pattern code (car transform) (cadr transform)))
            code
            '( 
              (('til_lambdify x 'til_call) (x))
              )))
   lambdas))

(define (output lambdas)
  (and (file-exists? "compiled.s")
       (delete-file "compiled.s"))
  (call-with-output-file "compiled.s"
    (λ (out)
      (call-with-input-file "prologue.s"
        (λ (in)
          (write-string (port->string in)           out)
          (lambdas-to-out lambdas out))))))
          
(define (lambdas-to-out lambdas out)
  (let ([display (λ (x) (display x out))])
    (map (λ (lam)
           (display (string-append (symbol->string (car lam)) ":\n"))
           (if (not (eq? 'main (car lam))) (display "        jmp til_enter\n") #f)
           (for-each (λ (elem)
                       (display
                        (if (symbol? elem)
                            (string-join 
                             (cons
                              (string-append "        .int "  (symbol->string elem))
                              (if (eq? elem 'til_lambdify) '() (list "        .zero 4")))
                             "\n")
                            (string-append   "        .quad " (number->string elem))))
                       (display "\n"))
                     (cadr lam))
           (display "\n"))
         lambdas)))
  
(define (compile-dumb code)
  (let* ([code (optimize-calls (flatten-code (compile-code code '())))]
         [to-inline (called-once-lambdas code)])
    code))

(define (compile code)
  (define lambdas (compile-dumb code))
  (inline-lambdas! lambdas (tdisplay "called-once" (called-once-lambdas lambdas)))
  lambdas)

; should do called-once-lambdas

(define (hash-inc! h k)
  (if (hash-has-key? h k)
      (hash-set! h k (+ 1 (hash-ref h k)))
      (hash-set! h k 1)))

(define (called-once-lambdas lambdas)
  (define h (make-hash))
  (define called-once (set))
  (for-each
   (λ (code)
     (for-each (λ (result) (hash-inc! h (cadar result)))
               (match-list-pattern code '(('til_push_frame _) x) )))
   (map til-lambda-body lambdas))
  (hash-for-each h
                 (λ (k v)
                   (and (= v 1) 
                        (set! called-once (set-add called-once k)))))
  called-once)

(define (is-til-func x)
  (if (pair? x)
      (is-til-func (car x))
      (string=? (substring (symbol->string x) 0 3) "til")))
      

(define (til-func-param-count x)
  (if (is-til-func x)
      (begin
        (match x
          [(list 'til_prm _ _) 0]
          [(list 'til_push_frame _) 0]
          ['til_push_fake_frame 0]
          ['til_mult 0]
          ['til_popret 1]
          ['til_push 1]
          ['til_add 0]
          ['til_exit 0]))
      0))

(define (find-lambda name lambdas) 
  (maybe-car (memf (λ (x) (eq? (til-lambda-name x) name)) lambdas)))

(define (update-lambda-code! name code lambdas)
  (let ([c (maybe-car (memf (λ (x) (eq? (til-lambda-name x) name)) lambdas))])
    (and c (set-til-lambda-body! c code))))

(define (inline-lambdas-in-code code lambdas to-inline)
  (tdisplay "inline-lambdas-in-code" (list code lambdas to-inline))
  (define (find name) 
    (and (set-member? to-inline name)
         (find-lambda name lambdas)))
  (define (self code) (inline-lambdas-in-code code lambdas to-inline))
  (let ([call (maybe-car code)])
    (cond 
      [(not call) '()]
      [(til-inline? call) (til-inline (self (til-inline-body call)))]
      [(is-til-func call) (call-with-values 
                           (λ () 
                             (split-at code (+ 1 (til-func-param-count call))))
                           (λ (unchanged rest)
                             (append unchanged
                                     (self rest))))]
      [else (let ([body (find call)])
              (cons (if body (til-inline body) call)
                    (self (cdr code))))])))

(define (for-each-sublist f l)
  (f l)
  (if (empty? l)
      (void)
      (for-each-sublist f (cdr l))))

(define (inline-lambdas! lambdas to-inline)
  (define order (tdisplay "order" (inline-order lambdas)))
  (define (filter-scc scc) (filter (λ (x) (set-member? to-inline x)) scc))
  (define (find name) (find-lambda name lambdas))
  (define (update! name code) (update-lambda-code! name code lambdas))
  (for-each-sublist (λ (sccs-sublist) 
                      (tdisplay "sccs-sublist" sccs-sublist)
                      (if (or (empty? sccs-sublist)
                              (empty? (cdr sccs-sublist)))
                          (void)
                          (let ([scc (filter-scc (car sccs-sublist))]
                                [calling-sccs (tdisplay "calling-sccs" (cdr sccs-sublist))])
                            (for-each (λ (calling-scc) 
                                        (tdisplay "calling-scc" calling-scc)
                                        (for-each (λ (name)
                                                    (tdisplay "calling-lambda" name)
                                                    (update! name 
                                                             (inline-lambdas-in-code 
                                                              (til-lambda-body 
                                                               (find name)) 
                                                              lambdas
                                                              to-inline)))
                                                  calling-scc))
                                      calling-sccs))))
                    order))
                    
(define (map-calls f body)
  (if (empty? body)
      '()
      (let ([call (car body)])
        (call-with-values
         (λ ()
           (split-at body (+ 1 (til-func-param-count call))))
         (λ (taken rest)
           (append (f taken) (map-calls f rest)))))))

(define (inline-order lambdas)
  (define index-table (make-hash))
  (define lowlink-table (make-hash))
  (define vertices (map til-lambda-name lambdas))
  (define (neighbors v)
    (map-calls (λ (x) (if (is-til-func x) '() `(,(car x)))) 
               (til-lambda-body (find-lambda v lambdas))))
  (define (index v) (and (hash-has-key? index-table v)
                         (hash-ref index-table v)))
  (define (lowlink v) (and (hash-has-key? lowlink-table v)
                           (hash-ref lowlink-table v)))
  (define (set-index! v i) (hash-set! index-table v i))
  (define (set-lowlink! v i) (hash-set! lowlink-table v i))
  (tarjan vertices neighbors index set-index! lowlink set-lowlink!))

(define (tarjan vertices neighbors get-index set-index! get-lowlink set-lowlink!)
  (define index 0)
  (define stack '())
  (define stackset (set))
  (define sccs '())
  (define (strong-connect v)
    (define (pop-scc) 
      (define (pop-scc-worker)
        (define w (car stack))
        (set! stack (cdr stack))
        (set! stackset (set-remove stackset w))
        (set-mcar! sccs (cons w (mcar sccs)))
        (or (eq? w v)
            (pop-scc-worker)))
      (set! sccs (mcons '() sccs))
      (pop-scc-worker))
    (set-index! v index)
    (set-lowlink! v index)
    (set! index (add1 index))
    (set! stack (cons v stack))
    (set! stackset (set-add stackset v))
    (for-each (λ (w)
                (cond 
                  [(set-member? stackset w)
                   (set-lowlink! v (min (get-lowlink v) (get-index w)))]
                  [(not (get-index w))
                   (begin
                     (strong-connect w)
                     (set-lowlink! v (min (get-lowlink v) (get-lowlink w))))]))
              (neighbors v))
    (and (= (get-lowlink v) (get-index v))
         (pop-scc)))
  (for-each strong-connect vertices)
  (reverse (mlist->list sccs)))

(define code1 '(display-int ((λ (a b c) (* (+ a ((λ (x) (* x x)) b)) c)) 2 3 4))) ; should output 44
(define code2
  '(letrec 
       ([other (λ (src dst)
                 (- 6 (+ src dst)))]
        [hanoi (λ (src dst count)
                 (if (= 1 count) 
                     (display-int (+ (* 10 src) dst))
                     (begin
                       (hanoi src (other src dst) (- count 1))
                       (hanoi src dst             1          )
                       (hanoi (other src dst)     dst        ))))])
     (hanoi 1 3 3)))

(define code3 '(+ ((λ (x) (* x x)) 4) 3))

; (inline-order 