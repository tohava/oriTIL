#lang racket

(require racket/match)

(define (range x)
  (define (range-helper h l)
    (if (= h l) 
        '()
        (cons l (range-helper h (+ l 1)))))
  (range-helper x 0))
 
(define (tdisplay x)
  (display x) x)

(define (mapfoldl f acc l)
  (define (mapfoldl-helper f acc l l2)
    (if (empty? l) 
        (cons acc (reverse l2))
        (let ([result (f (car l) acc)])
          (mapfoldl-helper f (car result) (cdr l) (cons (cdr result) l2)))))
  (mapfoldl-helper f acc l '()))
        

(define (compile-primitive p bindings)
  (match p
    ['+ '(til_push til_add)]
    ['- '(til_push til_sub)]
    ['* '(til_push til_mult)]
    ['/ '(til_push til_div)]
    [_ #f]))

(define (compile-binding b bindings)
  (define (compile-binding-helper b bindings l)
    (define n2s number->string)
    (display (list 'compile-binding-helper b bindings l)) (newline)
    (if (empty? bindings)
        #f
        (match (assq b (car bindings))
          [#f               (compile-binding-helper b (cdr bindings) l)]
          [(list b address) (list 'til_push (string->symbol (string-append "var_" (n2s l) "_" (n2s address))))])))
  (compile-binding-helper b bindings 0))

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
    (list 'til_push_λ (compile-code body new-bindings))))
    
(define (compile-code code bindings)
  (match code 
    [(list 'λ params body) (compile-λ params body bindings)]
    [(cons f params)      (compile-apply f params bindings)]
    [simple               (compile-simple simple bindings)]
    ))

;(define (flatten-lambdas code)
;  (define (flatten-lambdas code lambdas)
;    (foldl (λ (x lambdas)
;             (if (list? x)
;                 (let ([lamsym (gensym)])
;                   
;                 (cons 