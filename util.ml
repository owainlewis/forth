(* Custom range function *)
module Range = struct
  let make n =
    let rec aux xs i =
      if i <> n then
        aux (xs @ [i]) (i+1)
      else xs
    in
    if n < 0 then [] (* Negative range *)
             else aux [] 0

  let map  f n = List.map f (make n)
end
