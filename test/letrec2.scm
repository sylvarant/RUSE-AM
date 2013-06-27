;120
(letrec ([f (let ((b (λ (n)
                 (if (= n 0)
                     1
                     (let ([n-1! (f (- n 1))])
                       (* n n-1!)))))) b)])
     (f 5))
