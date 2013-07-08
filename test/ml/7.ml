let make_err = fun _ -> raise 3

let add_err = fun f n ->
  try f 0 with x -> raise (x + n)

in add_err make_err 12
