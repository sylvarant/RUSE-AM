in (fix rec n -> n rec) (fun f -> f (fun f -> f)) (fun _ -> 12)
