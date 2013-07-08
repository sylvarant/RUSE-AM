in
let f = fun x ->
  let _ = (fix x _ -> 0) 0 in
    x
in f 3
