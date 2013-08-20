#lang racket
;(*
; * =====================================================================================
; *
; *       Filename:  normalize.scm
; *
; *    Description:  normalize scheme to ANF 
; *
; *         Author:  Adriaan Larmuseau, ajhl
; *        Company:  Distrinet, Kuleuven
; *
; * =====================================================================================
; *)

(require "helper.scm")

(provide normalize-program)


(define (combine vs exps) 
    (cond 
        ((null? vs)  '())
        (else (cons `(,(car vs) ,(car exps)) (combine (rest vs) (rest exps)))) 
))


(define (combine* vs) (match vs
    [ '()  '()]
    [else (cons `(,(car vs)) (combine* (cdr vs)))]
))


(define (beginize vs body) (match vs
    [`(,args) `(begin ,args ,body)]
))


(define (sets vs exps) (match vs
    [ '()  '()]
    [else (cons `(set! ,(car vs) ,(car exps)) (sets (cdr vs) (cdr exps)))]
))


(define (mnormalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize (car exp*) (λ (t) 
       (mnormalize-name* (cdr exp*) (λ (t*) 
        (k `(,t . ,t*))))))))


(define (normalize-name exp k)
  (normalize exp (λ (aexp) 
    (if (requireslet? aexp) (k aexp) 
        (let ([t (gensym)]) 
         `(let ([,t ,aexp]) ,(k t)))))))


(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (λ (t) 
       (normalize-name* (cdr exp*) (λ (t*) 
        (k `(,t . ,t*))))))))



; Atomic should not be normalized !!
(define (atomic? exp)
  (match exp
    [(quote '())         #t]
    [(? number?)         #t]
    [(? boolean?)        #t]
    [(? string?)         #t]
    [(? char?)           #t]
    [(? symbol?)         #t]
   ; [`(list . ,args) (andmap (λ (x) (atomic? x)) args)]
    [`(quote . ,args)    #t]
    ;[`(IS ,arg)         #t]
    ;[`(SI ,arg)         #t]
    [else                #f]))


(define (requireslet? exp) (match exp 
    [(? atomic?)                        #t]
    [(cons (and oper (? prim?)) args)   #t]
    [`(λ ,_ ,_)                         #t]
    [`(IS ,_)                           #t]
    [`(SI ,_)                           #t]
    [else                               #f]
))


(define (evatom? exp) 
    (if (null? exp) 
        #t 
        (if (requireslet? (car exp))
            (evatom? (cdr exp))
             #f)))


;; Expression normalization:
(define (normalize-term exp) (normalize exp (λ (x) x)))

(define (normalize exp k)
  (match exp
    ; atomic
    [(? atomic?)            
     (k exp)]

    ; lambda
    [`(λ ,params ,body)   
      (k `(λ ,params ,(normalize-term body)))]

    ;IS
    [`(IS ,lst) (k `(IS ,(normalize-term lst)))]

    ;SI
    [`(SI ,lst) (k `(SI ,(normalize-term lst)))]
    
    ; let
    [`(let () ,exp)
      (normalize exp k)]

    ; let with vars
    [`(let ([,x ,exp1] . ,clause) ,exp2) 
        `(let ([,x ,exp1])
         ,(normalize `(let (,@clause) ,exp2) k))]

    ; undef
    [`(let ([,x] . ,clause) ,body) 
        `(let ([,x]) ,(normalize `(let (,@clause) ,body) k))]

    ; if expr
    [`(if ,exp1 ,exp2 ,exp3)    
      (normalize-name exp1 (λ (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3)))))]
    ; set 
    [`(set! ,v ,exp)
      (normalize-name exp (λ (t)
       `(let ([,(gensym '_) (set! ,v ,t)])
          ,(k '(_undef)))))]

    ; letrec
    [`(letrec () ,exp)
      (normalize exp k)]

    ; letrec
    [`(letrec ([,vars ,aexps] ...) ,body) 
        (if (evatom? aexps) 
        (mnormalize-name* aexps 
         (λ (aexp1s) 
          `(letrec ,(combine vars aexp1s )
            ,body)))
        (let ((vls (map gensym vars)))
        (normalize `(let ,(combine* vars) (let ,(combine vls aexps) ,(beginize (sets vars vls) body))) k)))     
    ]
   
    ; call/cc
    [`(call/cc ,f)
        (normalize-name f (λ (t)
            (k `(call/cc ,t))))]

   
    ; begin
    [`(begin . ,args)
        (normalize-name* args (λ (t*)
            (k `(begin . ,t*))))] 

    ; car
    [`(car ,lst) (normalize-name lst (λ (t) (k `(car ,t))))]

    ; cdr
    [`(cdr ,lst) (normalize-name lst (λ (t) (k `(cdr ,t))))]

    ; cons
    [`(cons ,lst ,lst2) (normalize-name lst (λ (t) (
        normalize-name lst2 (λ (y) (k `(cons ,t ,y))))))]

    ; pair?
    [`(pair? ,lst) (normalize-name lst (λ (t) (k `(pair? ,t))))]

    ; list?
    [`(list? ,lst) (normalize-name lst (λ (t) (k `(list? ,t))))]

    ; null?
    [`(null? ,lst) (normalize-name lst (λ (t) (k `(null? ,t))))]

        ; primitive
    [(cons (and oper (? prim?)) args)
        (normalize-name* args (λ (y) (k `(,oper . ,y))))] 

    ;definition
    [`(define (,var . ,args) ,body) `(define ,var ,(normalize-term `(λ ,args ,body)))]

    ;definition 
    [`(define ,v ,exp)  (k `(define ,v ,(normalize-term exp)))]
    
    ; application
    [`(,f . ,e*) 
      (normalize-name f (λ (t) 
       (normalize-name* e* (λ (t*)
        (k `(,t . ,t*))))))]
))


;; Top-level normalization:
; normalize the top level
(define (normalize-program decs)
  (match decs
    ['() '()]
    [(cons exp rest) (cons (normalize-term exp) (normalize-program rest))]))
