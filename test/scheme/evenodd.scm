;#t
(letrec
    ([even? (λ (n)
              (if (= 0 n)
                  #t
                  (odd? (- n 1))))]
     [odd? (λ (n)
             (if (= 0 n)
                 #f
                 (even? (- n 1))))])
  (even? 6))
