;'(3 4 5)
(define (map f a)
   (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))

(define y 1)
(map (λ (x) (begin (+ 1 x y))) '(1 2 3))
