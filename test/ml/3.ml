in (fix rec l ->
    l (fun x l2 -> (fun x1 l1 -> fun c _ -> c x1 l1) x (rec l2)) 0)
    (fun c1 _ -> c1 12 (fun _ n -> n))
   (fun x _ -> x) 3
