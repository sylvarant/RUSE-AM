#lang racket
;(*
; * =====================================================================================
; *
; *       Filename:  byte.scm
; *
; *    Description:  convert scheme to bytecode 
; *
; *         Author:  Adriaan Larmuseau, ajhl
; *        Company:  Distrinet, Kuleuven
; *
; * =====================================================================================
; *)

(require "helper.scm")

(provide create-result)


; Global variables TODO :: remove
(define xnr 0)
(define xxpr '())
(define outl '())


;code
(define (code in elem) (match elem
    ['Nop           1]
    ['Void          2]
    ['Int           3]
    ['Boolean       4]
    ['Closure       5]
    ['List          6]
    ['Quote         7]
    ['Continuation  8]
    ['Prim          9]
    ['Lam           10]
    ['If            11]
    ['Symbol        12]
    ['Application   13]
    ['Callcc        14]
    ['Set           15]
    ['Let           16]
    ['Letrec        17]
    ['Begin         18]
    ['Car           19]
    ['Cdr           20]
    ['Cons          21]
    ['Listq         22]
    ['Define        23]
    ['Nullq         24]
    ['Pairq         25]
    ['SI            26]
    ['IS            26]
))

;prefix
(define (prefix in elem)
    (string-append (number->string (code in elem)) "\n")
)


;prim to string
(define (prim->string prim in)
  (match prim
    ['+    "+"]
    ['-    "-"]
    ['*    "*"]
    ['=    "="]))


; how to parse single syntax units
; produce a string
(define (c-singlemem exp in) (match exp
    ; integer
    [(? integer?) 
        (string-append (prefix in 'Int)  (number->string exp)) ]

    ; boolean
    [(? boolean?) 
        (string-append (prefix in 'Boolean) (if exp "1" "0") ) ]   

    ; symbol
    [(? symbol?)    
          (string-append (prefix in 'Symbol) (number->string (string-length(symbol->string exp))) "\n" (symbol->string exp))]

))

; Racket had a map bug
(define (wut a b) (match a
    [ '() '()]
    [else  (cons (lexeme (car a) b) (wut (cdr a) b))]))

(define (lexeme exp in) (match exp

    ; datums
    [(or (? integer?) (? boolean?) (? symbol?)) (c-singlemem exp in)]

    [(and (? pair?) (not (? list?))) (string-append (prefix in 'List) "-1\n"
        (lexeme (car exp) in) "\n" (lexeme (cdr exp) in) )]
    

    ;lists
    [`(,ex ...) (if (equal? (length ex) 0) (string-append (prefix in 'List) (number->string (length ex)))
        (string-append (prefix in 'List) (number->string (length ex)) "\n" (string-join (wut ex in) "\n")))]
))


; recursive parser
; produce string
(define (c-gener exp in)
    (match exp

    ; nop
        ['_undef (prefix in 'Nop)]

    ; void
        ['void (prefix in 'Void)]

    ; nil
        [(quote '()) (string-append (prefix in 'List) (number->string 0))]

    ; return datum
        [(or (? integer?) (? boolean?)  (? symbol?)) (c-singlemem exp in)]

    ; lambda
        [`(λ ,args ,body) 
            (string-append (prefix in 'Lam) (number->string (length args)) "\n" (c-gener body in) "\n" 
                (string-join (map (lambda x (c-gener x in)) args) "\n")  )]

    ; primitive
        [(cons (and oper (? prim?)) args)
            (string-append (prefix in 'Prim) (number->string (length args)) "\n"
            (prim->string oper in) "\n" (string-join (map (lambda x (c-gener x in)) args) "\n"))] 

    ; if
        [`(if ,conde ,conse ,alte) 
            (string-append (prefix in 'If) (c-gener conde in) "\n" (c-gener conse in) "\n" (c-gener alte in))]

    ; call/cc
        [`(call/cc ,f)
          (string-append (prefix in 'Callcc) (c-gener  f in))]

    ; set!
        [`(set! ,v ,aexp) (string-append (prefix in 'Set) (c-gener v in) "\n" (c-gener aexp in))]

	; letrec
        [`(letrec ([,vars ,aexps] ...) ,body) 
          (string-append (prefix in 'Letrec) (number->string (length vars)) "\n" (c-gener body in) "\n"
            (string-join (map (lambda x (c-gener x in)) vars) "\n") "\n"  
                (string-join (map (lambda x (c-gener x in)) aexps) "\n"))]

    ; let
        [`(let ([,v ,expr]) ,body) 
          (string-append (prefix in 'Let) (c-gener v in) "\n" (c-gener expr in) "\n" (c-gener body in))]

    ;let special ; TODO removed Undef !!
        [`(let ([,v]) ,body) 
          (string-append (prefix in 'Let) (c-gener v in) "\n" (prefix in 'Nop) "\n" (c-gener body in))]

    ; definition
        [`(define ,v ,exp) 
           (string-append (prefix in 'Define) (c-gener v in) "\n" (c-gener exp in))] 

    ; begin statement
        [`(begin . ,args)
            (string-append (prefix in 'Begin) (number->string (length args)) "\n"   
                           (string-join (map (lambda x (c-gener x in)) args) "\n"))]
    
    ; car
        [`(car ,lst) (string-append (prefix in 'Car) (c-gener lst in))]

    ; cdr
        [`(cdr ,lst) (string-append (prefix in 'Cdr) (c-gener lst in))]

    ; cons
        [`(cons ,lst ,lst2) (string-append (prefix in 'Cons) (c-gener lst in) "\n" (c-gener lst2 in))]

    ; pair?
        [`(pair? ,arg) (string-append (prefix in 'Pairq) (c-gener arg in))]

    ; list?
        [`(list? ,arg) (string-append (prefix in 'Listq) (c-gener arg in))]

    ; null?
        [`(null? ,arg) (string-append (prefix in 'Nullq) (c-gener arg in))]

    ;quote
        [`(quote ,args) (string-append (prefix in 'Quote) (lexeme args in))]

    ; SI
        [`(SI ,arg)  (string-append (prefix in 'SI) (number->string 0) "\n" (c-gener arg #f))] ;(error "Cannot cross to Insecure from Secure")  ]
    ; IS
        [`(IS ,arg) (begin  
                (let ((c xnr))
                (set! xnr (+ xnr 1))
                (set! outl  (cons (c-gener arg #t) outl)) 
                (string-append (prefix in 'IS) (number->string 0) "\n" (number->string c))))] ;(error "Cannot cross to Secure from InSecure") )]

    ; function appl
        [`(,f . ,args)
            (if (equal? (length args) 0) (c-gener f in)
            (string-append (prefix in 'Application) (number->string (+ (length args) 1)) "\n" 
                (c-gener f in) "\n" (string-join (map (lambda x (c-gener x in)) args) "\n")))]

    ; mistake ?
        [else (error "unhandled exp: " exp)]
))


;; build the program
(define (buildi expr in) (match expr
    ['() '()]
    [else   (begin
        (define head (c-gener (car expr) in))   
        (set! xxpr (append (reverse outl) xxpr))
        (set! outl '())
        (define tail (buildi (cdr expr) in))
        (cons head tail))]
))


;; create-result :: the fineal result
(define (create-result emit expr)
    ; parse
    (emit "==@@==PARSER==@@==")
    (emit "0") ; Language
    (emit (number->string (length expr)))
    (when (> (length expr) 0)
        (emit (string-join (buildi expr #t) "\n")))
    (emit "==@@==PARSER==@@==")
    (emit "0") ; Language
    (emit (number->string (length xxpr)))
    (when (> (length xxpr) 0)
        (emit (string-join (reverse xxpr)  "\n")))
)















