;'(2 3 4)
(define (map f a)
   (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))

(map (λ (x) (+ 1 x)) '(1 2 3))
