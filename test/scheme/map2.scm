;'(1 2 3)
(letrec 
  ([map  (λ (f a)
    (if (null? a) '()
    (cons (f (car a)) (map f (cdr a)))
))]) (map (λ (x) x) (quote (1 2 3))))

