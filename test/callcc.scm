;3
(+ 1
  (call/cc (λ (cc)
            ; do not use 7
            (* 7 (cc 2)))))

