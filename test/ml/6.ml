let nil = [Nil]
let cons = fun x l ->
  [Cons, x, l]

let hd = fun l ->
  match l.[1] with
  | Nil -> 0
  | Cons -> l.[2]
  end

let tl = fun l ->
  match l.[1] with
  | Nil -> nil
  | Cons -> l.[3]
  end

let somme = fix somme l ->
  match l.[1] with
  | Nil -> 0
  | Cons -> l.[2] + somme l.[3]
  end

let l = cons 3 (cons 6 (cons 2 (cons 9 (cons 3 nil))))

in hd l + somme (tl l)
