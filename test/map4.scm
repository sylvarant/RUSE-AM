;'(5 6 7)
(define (map f a)
   (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))

(define y 1)
(map (λ (x) (begin (set! y 3) (+ 1 x y))) '(1 2 3))
