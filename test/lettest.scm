;3
(let ((id (λ (x) x)))
    (let ((apply (λ (f x) (f x))))
      ((id apply) id 3)))

