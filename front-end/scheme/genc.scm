#lang racket

;; Adriaan does SCHEME

;;; create binding for error
(define error #f)

;; call the outside
(define outside #f)
(define xnr 0)
(define xxpr '())
(define outl '())


;; Normal functions


;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display ">>>> ERROR ")
                  (newline)
                  (k error-arguments)))
              )) 

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


; prim? : symbol? -> boolean?
(define (prim? exp)
  (case exp
    [(+ - * =) #t]
    [else      #f]))


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
                (string-join (map (lambda x (c-gener x in)) args) "\n"))]

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
        [`(SI ,arg)  (string-append (prefix in 'SI) (c-gener arg #f))] ;(error "Cannot cross to Insecure from Secure")  ]
    ; IS
        [`(IS ,arg) (begin  
               (when (not outside) 
                (set! outside #t)) 
                (let ((c xnr))
                (set! xnr (+ xnr 1))
                (set! outl  (cons (c-gener arg #t) outl)) 
                (string-append (prefix in 'IS) (number->string c))))] ;(error "Cannot cross to Secure from InSecure") )]

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
    [else  (cons (c-gener (car expr) in) (buildi (cdr expr) in))]
))


;; build the secure
(define (buildr expr c) (match expr
    ['() '()]
    [else (cons (string-append "DEBUG_PRINT(\"%d\",mystate->free_adr); mystate->storage[mystate->free_adr] = "(car expr) ";\n insertLabel(&(mystate->label),mystate->free_adr);\n mystate->free_adr++;\n") (buildr (cdr expr) (+ 1 c)))]))


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
        (emit (string-join (buildr (reverse xxpr) 0)  "\n")))
)


;; Emit
(define (emit line)

  (display line)
  (newline))


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
    [`(SI ,arg)         #t]
    [else                #f]))

(define (evatom? exp) (if (null? exp) #t (match (car exp)
    [(? atomic?) (evatom? (cdr exp))]
    [(cons (and oper (? prim?)) args) (evatom? (cdr exp))]
    [`(λ ,_ ,_) (evatom? (cdr exp))]
    [`(IS ,_) (evatom? (cdr exp))]
    [`(SI ,_) (evatom? (cdr exp))]
    [else #f]
)))


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
    (if (atomic? aexp) (k aexp) 
        (let ([t (gensym)]) 
         `(let ([,t ,aexp]) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (λ (t) 
       (normalize-name* (cdr exp*) (λ (t*) 
        (k `(,t . ,t*))))))))

;; Top-level normalization:
; normalize the top level
(define (normalize-program decs)
  (match decs
    ['() 
     '()]

    [(cons exp rest)
     (cons (normalize-term exp)
           (normalize-program rest))]))

; read program as list
(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))
;
; START POINT
(define the-program (read-all))  ; read expr., pass to eval, write result
(define normalized (normalize-program the-program))
;(display "/*\n")
;(display the-program)
;(display "\n\n")
;(display normalized) TODO output to something loggable
;(display "\n*/\n")
(create-result emit normalized)



