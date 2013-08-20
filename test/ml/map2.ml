(* map that needs normalizing + boundaries *)
let map = (fun f -> (fun a -> f a)) in
let add = (fun x -> ( x + (is int : (1 + 1)))) in
let addmap = (map add) in
(addmap 99)
