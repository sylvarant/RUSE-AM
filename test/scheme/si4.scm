;'(3 4 5)

(define (map f a)
   (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))

(IS (define (secadd a)
    (+ (SI 2) a)
))

(map (IS secadd) '(1 2 3))

