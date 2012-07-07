#lang racket

(require racket/match)

(define (range x)
  (define (range-helper h l)
    (if (= h l) 
        '()
        (cons l (range-helper h (+ l 1)))))
  (range-helper x 0))
 
(define (tdisplay x)
  (display x) (newline) x)

(define (assmap f x)
  (map (λ (x) (list (car x) (f (cadr x)))) x))

(define (mapfoldl f acc l)
  (define (mapfoldl-helper f acc l l2)
    (if (empty? l) 
        (cons acc (reverse l2))
        (let ([result (f (car l) acc)])
          (mapfoldl-helper f (car result) (cdr l) (cons (cdr result) l2)))))
  (mapfoldl-helper f acc l '()))
        

(define (compile-primitive p bindings)
  (match p
    ['+              '(til_push_fake_frame til_push_half til_add)]
    ['-              '(til_push_fake_frame til_push_half til_sub)]
    ['*              '(til_push_fake_frame til_push_half til_mult)]
    ['/              '(til_push_fake_frame til_push_half til_div)]
    ['=              '(til_push_fake_frame til_push_half til_eq)]
    ['sqrt           '(til_push_fake_frame til_push_half til_sqrt)]
    ['display-int    '(til_push_fake_frame til_push_half til_display_int)]
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
  (and b (list (string->symbol (string-append "var_" (n2s (car b)) "_" (n2s (cdr b)))))))

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
    (map display (list 
                  "params is: "       params       "\n"
                  "new-bindings is: " new-bindings "\n")) 
    (list 'til_push_frame 'til_push_half (append (compile-code body new-bindings) `(popret ,(* (+ (length params) 1) 8))))))
    
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
          (list (string->symbol (string-append "set_" (n2s (car b)) "_" (n2s (cdr b)))))))

(define (compile-if cond true false)
  `(? ,cond (λ () ,true) (λ () ,false)))

(define (compile-code code bindings)
  (tdisplay bindings)
  (match (tdisplay code)
    [(cons 'let (cons ribs body))    (compile-code (compile-let ribs body) bindings)]
    [(cons 'let* (cons ribs body))   (compile-code (compile-let* ribs body) bindings)]
    [(cons 'letrec (cons ribs body)) (compile-code (compile-letrec ribs body) bindings)]
    [(list 'if cond true false)      (compile-code (compile-if cond true false) bindings)]
    
    [(cons 'begin rest)       (compile-begin rest bindings)]
    [(list 'λ params body)    (compile-λ params body bindings)]
    [(cons f params)          (compile-apply f params bindings)]
    [(list 'set! param value) (compile-set! param value bindings)]
    [simple                   (compile-simple simple bindings)]
    ))

(define (flatten-code code)
  (define (flatten-single code lambdas)
    (mapfoldl (λ (x lambdas)
                (if (list? x)
                    (let ([lamsym (gensym)])
                      (cons (cons (list lamsym x) lambdas) lamsym))
                    (cons lambdas x)))
              lambdas
              code))
  (define (flatten-lambdas lambdas)
    (define flattened-once
      (mapfoldl (λ (x newlambdas)
                  (let ([flattened (flatten-single (cadr x) newlambdas)])
                    (cons (car flattened) (list (car x) (cdr flattened)))))
                '()
                lambdas))
    (define newlambdas (car flattened-once))
    (define oldlambdas (cdr flattened-once))
    (if (empty? newlambdas)
        oldlambdas
        (flatten-lambdas (append newlambdas oldlambdas))))
  (flatten-lambdas (list (list 'main (append code (list 'til_exit))))))

(define (replace-list-pattern lst pattern newpattern)
  (display "(replace-list-pattern ") (display lst) (display " ") (display pattern) (display " ") (display newpattern) (display ")\n")
  (define (maybe-cons x xs)
    (and xs (cons x xs)))
  (define (compare lst pattern)
    (cond
      [(empty? pattern)                 (list lst)]
      [(empty? lst)                      #f]
      [(and (list? (car pattern)) 
            (eq? (caar pattern) 'quote)) (and (eq? (cadar pattern) (car lst))
                                              (compare (cdr lst) (cdr pattern)))]
      [(symbol? (car pattern))           (maybe-cons (map car (list pattern lst)) 
                                                     (compare (cdr lst) (cdr pattern)))]))
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

(define (optimize-calls lambdas)
  (assmap (λ (code)
            (foldl (λ (transform code)
                     (replace-list-pattern code (car transform) (cadr transform)))
                   code
                   '( 
                     (('til_push_half x 'til_call) (x))
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
                              (if (eq? elem 'til_push_half) '() (list "        .zero 4")))
                             "\n")
                            (string-append   "        .quad " (number->string elem))))
                       (display "\n"))
                     (cadr lam))
           (display "\n"))
         lambdas)))
  
(define (compile code)
  (output (optimize-calls (flatten-code (compile-code code '())))))



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
                         