;'(1 2 3)
(define (map f a)
   (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))

(map (IS (λ (x) x)) '(1 2 3))
