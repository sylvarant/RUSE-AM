(* simple map function *)
let map = (fun f -> (fun ls -> (f ls))) in
let add = (fun x -> x) in
let addmap = (map add) in
(addmap 2)
