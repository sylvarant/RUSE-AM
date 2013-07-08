let nil = fun _ n -> n
let cons = fun x l -> fun c _ -> c x l

let hd = fun l -> l (fun x _ -> x) 0
let tl = fun l -> l (fun _ l -> l) nil

let map = fix map f l ->
  l (fun x l -> cons (f x) (map f l)) nil

let fold = fix fold f a l ->
  l (fun x l -> fold f (f x a) l) a

let somme = fun x y -> x + y
let incr = somme 1

let l = cons 2 (cons 5 nil)
let l' = map incr l
let s = fold somme 0 l'

in s
