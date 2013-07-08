let p = [2, 4, 6, 1, 3]
let pp = [p, [p.[2], p.[1]]]

let fst = fun p -> p.[1]
let snd = fun p -> p.[2]

in fst (snd pp)
