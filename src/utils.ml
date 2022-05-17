open OUnit2

let cmp_float_lists l1 l2 =
  let rec cmp a b =
    match (a, b) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> cmp_float h1 h2 && cmp t1 t2
    | _ -> failwith "lists must have equal length"
  in
  cmp l1 l2

let cmp_float_matrices l1 l2 =
  let rec cmp a b =
    match (a, b) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> cmp_float_lists h1 h2 && cmp t1 t2
    | _ -> failwith "matrices must have equal sizes"
  in
  cmp l1 l2
