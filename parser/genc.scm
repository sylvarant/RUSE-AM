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

; prim? : symbol? -> boolean?
(define (prim? exp)
  (case exp
    [(+ - * =) #t]
    [else      #f]))

;prim to string
(define (prim->string prim in)
  (match prim
    ['+    (if in "sumPrim" "N(sumPrim)")]
    ['-    (if in "differencePrim" "N(differencePrim)")]
    ['*    (if in "productPrim" "N(productPrim)")]
    ['=    (if in "numequalPrim" "N(numequalPrim)")]))


; how to parse single syntax units
; produce a string
(define (c-singlemem exp in) (match exp
    ; integer
    [(? integer?) 
        (string-append (if in "makeInt(" "N(makeInt)(") (number->string exp) ")") ]

    ; boolean
    [(? boolean?) 
        (string-append (if in "makeBoolean(" "N(makeBoolean)(") (if exp "1" "0") ")") ]   

    ; symbol
    [(? symbol?)    
          (string-append  (if in "makeSymbol( \"" "N(makeSymbol)( \"") (symbol->string exp) "\" )")]

))

; Racket had a map bug
(define (wut a b) (match a
    [ '() '()]
    [else  (cons (lexeme (car a) b) (wut (cdr a) b))]))

(define (lexeme exp in) (match exp

    ; datums
    [(or (? integer?) (? boolean?) (? symbol?)) (c-singlemem exp in)]

    [(and (? pair?) (not (? list?))) (string-append (if in "makePair(" "N(makePair)(")
        (lexeme (car exp) in) "," (lexeme (cdr exp) in) ")" )]
    

    ;lists
    [`(,ex ...) (if (equal? (length ex) 0) (if in "makeNIL()" "N(makeNIL)()")
        (string-append (if in "makeList(" "N(makeList)(") 
            (number->string (length ex)) "," (string-join (wut ex in) ",") ")"))]
))

; recursive parser
; produce string
(define (c-gener exp in)
    (match exp

    ; void
        ['void (if in "makeVoid()" "N(makeVoid)()")]
    ; nil
        [(quote '()) (if in "makeNIL()" "N(makeNIL)()")]

    ; return datum
        [(or (? integer?) (? boolean?)  (? symbol?)) (c-singlemem exp in)]

    ; lambda
        [`(λ ,args ,body) 
            (string-append 
                (if in "makeLambda(" "N(makeLambda)(") (number->string (length args)) "," 
                    (c-gener body in) "," (string-join (map (lambda x (c-gener x in)) args) ",") ")")]
    ; primitive
        [(cons (and oper (? prim?)) args)
            (string-append  (if in "makePrim(" "N(makePrim)(") (number->string (length args)) ","
            (prim->string oper in)  
            "," (string-join (map (lambda x (c-gener x in)) args) ",") ")"
        )] 
    ; if
        [`(if ,conde ,conse ,alte) 
            (string-append 
              (if in "makeIf(" "N(makeIf)(") (c-gener conde in) "," (c-gener conse in) "," (c-gener alte in) ")" )]
    ; call/cc
        [`(call/cc ,f)
          (string-append (if in "makeCallcc(" "N(makeCallcc)(") (c-gener  f in) ")")]
    ; set!
        [`(set! ,v ,aexp) 
          (string-append (if in "makeSet(" "N(makeSet)(") (c-gener v in) "," (c-gener aexp in) ")" )]
	; letrec
        [`(letrec ([,vars ,aexps] ...) ,body) 
          (string-append (if in "makeLetrec(" "N(makeLetrec)(") (number->string (length vars)) "," (c-gener body in) ","
            (string-join (map (lambda x (c-gener x in)) vars) ",") ","  
                (string-join (map (lambda x (c-gener x in)) aexps) ",") ")")]
    ; let
        [`(let ([,v ,expr]) ,body) 
          (string-append (if in "makeLet(" "N(makeLet)(") (c-gener v in) "," (c-gener expr in) "," (c-gener body in) ")" )]

    ;let special ; TODO removed Undef !!
        [`(let ([,v]) ,body) 
          (string-append (if in "makeLet(" "N(makeLet)(") (c-gener v in) "," (if in "makeVoid()" "makeSVoid()") 
                "," (c-gener body in) ")" )]

    ; definition
        [`(define ,v ,exp) 
           (string-append (if in "makeDefine(" "N(makeDefine)(") (c-gener v in) "," (c-gener exp in) ")")] 

    ; begin statement
        [`(begin . ,args)
            (string-append (if in "makeBegin(" "N(makeBegin)(") (number->string (length args)) ","   
                           (string-join (map (lambda x (c-gener x in)) args) ",") ")")]
    
    ; car
        [`(car ,lst) (string-append (if in "makeCar(" "N(makeCar)(") (c-gener lst in) ")")]

    ; cdr
        [`(cdr ,lst) (string-append (if in "makeCdr(" "N(makeCdr)(") (c-gener lst in) ")")]

    ; cons
        [`(cons ,lst ,lst2) (string-append (if in "makeCons(" "N(makeCons)(")(c-gener lst in) "," (c-gener lst2 in) ")")]

    ; pair?
        [`(pair? ,arg) (string-append (if in "makePairQ(" "N(makePairQ)(") (c-gener arg in) ")") ]

    ; list?
        [`(list? ,arg) (string-append (if in "makeListQ(" "N(makeListQ)(") (c-gener arg in) ")")]

    ; null?
        [`(null? ,arg) (string-append (if in "makeNullQ(" "N(makeNullQ)") (c-gener arg in) ")")]

    ;quote
        [`(quote ,args) (string-append (if in "makeQuote(" "N(makeQuote)(") (lexeme args in)")")]

    ; SI
        [`(SI ,arg)  (if (not in) (string-append "makeSI(" (c-gener arg #t) ")" ) (error "Cannot cross to Insecure from Secure")  )]
    ; IS
        [`(IS ,arg) (if in (begin  
               (when (not outside) 
                (set! outside #t)) 
                (let ((c xnr))
                (set! xnr (+ xnr 1))
                (set! outl  (cons (c-gener arg #f) outl)) 
                (string-append "makeIS(" (number->string c) ")"))) (error "Cannot cross to Secure from InSecure") )]

    ; function appl
        [`(,f . ,args)
            (if (equal? (length args) 0) (c-gener f in)
            (string-append  (if in "makeApplication(" "N(makeApplication)(") (number->string (+ (length args) 1)) "," 
                (c-gener f in) "," (string-join (map (lambda x (c-gener x in)) args) ",") ")"))]

    ; mistake ?
        [else (error "unhandled exp: " exp)]
))


;; build the program

(define (buildi expr c in) (match expr
    ['() '()]
    [else   (begin
        (define head (string-append "ret["(number->string c)"] = "(c-gener (car expr) in) ";\n"))   
        (set! xxpr (append (reverse outl) xxpr))
        (define tail (buildi (cdr expr) (+ 1 c) in))
        (cons head tail))]
))

(define (buildr expr c) (match expr
    ['() '()]
    [else (cons (string-append "DEBUG_PRINT((\"%d\",mystate->free_adr)) mystate->storage[mystate->free_adr] = "(car expr) ";\n insertLabel(&(mystate->label),mystate->free_adr);\n mystate->free_adr++;\n") (buildr (cdr expr) (+ 1 c)))]))


(define (create-result emit expr)
    ; parse
    (define parseresult  (buildi expr 0 #t))
    (emit "#ifndef SECURE")
    (emit "#include <stdlib.h>")
    (emit "#include <stdio.h>")
    (emit "#include \"scheme.h\"")
    (emit (string-append 
     "void ** getinput () {\n"
     " int c = "(number->string (length expr))";\n"
     " VALUE * ret = malloc(c * (sizeof(VALUE)));\n" 
     (string-join parseresult "\n")
    (if outside "sload();\n" "")
     " return ret;\n"
     "}\n"
     "int getinput_n(){ return " (number->string (length expr)) ";}\n"
     ))
    (emit "#endif")
     
    (when outside 
        (with-output-to-file "temp.gen"
            (lambda ()
                (display (string-append  
            "#ifdef SECURE\n"
            "ENTRYPOINT void sload(void){\n"
            " inject();\n"
            ;" int c = "(number->string (length xxpr))";\n"
            ;" VALUE * ret = MALLOC(c * (sizeof(VALUE)));\n" 
            (string-join (buildr (reverse xxpr) 0)  "\n")
            "}\n"
            "#endif\n")              
    )) #:exists 'replace )))


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
          ,(k '(void)))))]

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
    [`(define ,v ,exp)  (normalize-name exp (λ (t)
                            `(let ([,(gensym '_) (define ,v ,t)])
                                ,(k '(void)))))]
    
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
(display "/*\n")
(display the-program)
(display "\n\n")
(display normalized)
(display "\n*/\n")
(create-result emit normalized)



