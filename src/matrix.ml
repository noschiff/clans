type t = float array array

let create n m = Array.make_matrix n m 0.

let of_list l = l |> List.map Array.of_list |> Array.of_list

let to_list m = m |> Array.map Array.to_list |> Array.to_list

let rows a = Array.length a

let cols a =
  match rows a with
  | 0 -> 0
  | _ -> Array.length a.(0)

let dims a = (rows a, cols a)

let row i a = a.(i) |> Array.to_list

let col i a = a |> Array.map (fun x -> x.(i)) |> Array.to_list

let get i j a = a.(i).(j)

let transpose a =
  let colc = cols a in
  let rec f i =
    match i with
    | 0 -> []
    | _ -> col (colc - i) a :: f (i - 1)
  in
  f colc |> of_list
(* |> (fun x -> assert (rows x = cols a); assert (cols x = rows a);
   x) *)

let dot a b =
  (* Printf.printf "Dotting %dx%d and %dx%d" (rows a) (cols a) (rows b)
     (cols b); *)
  assert (cols a = rows b);
  let bt = transpose b in
  a
  |> Array.map (fun r ->
         bt
         |> Array.map (fun c ->
                Array.map2 ( *. ) r c |> Array.fold_left ( +. ) 0.))
  |> fun x ->
  assert (rows x = rows a);
  assert (cols x = cols b);
  x

let map2 f a b =
  assert (dims a = dims b);
  Array.map2 (fun ra rb -> Array.map2 f ra rb) a b |> fun x ->
  assert (dims x = dims a);
  x

let map f a =
  Array.map (fun ra -> Array.map f ra) a |> fun x ->
  assert (dims x = dims a);
  x