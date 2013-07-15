;4146

; Secure map function
(IS 
    (define (map f a)
        (if (null? a) 
            '()
            (cons (f (car a)) (map f (cdr a))))))

; secure addition :: todo upgrade
(IS 
    (define (secadd a)
        (+ 2 a)))

; The big secret
(IS 
    (define secret 42))

; The compression
(IS
    (define (compr a b)
        (+ a b)   
))

; secure hash
(IS 
    (define (fold x ls)
       (if (null? ls)
            x
            (fold (compr x (car ls)) (cdr ls)))))

; The only function that should be outward facing fix in encryption 2
(IS 
    (define (process ls)
       (fold secret (map secadd ls))))


((IS process) '(1 2 3 4 5 6 7 8 9 10 11 12 4000))


