type t = float array array

let create m n = Array.make_matrix m n 0.

let of_list l = l
	|> List.map Array.of_list
	|> Array.of_list

let to_list m = m
	|> Array.map Array.to_list
	|> Array.to_list

let rows a = Array.length a

let cols a = match rows a with
	| 0 -> 0
	| _ -> Array.length a.(0)

let dims a = (rows a, cols a)

let row i a = a.(i)
	|> Array.to_list

let col i a = a
	|> Array.map (fun x -> x.(i))
	|> Array.to_list

let get i j a = a.(i).(j)

let transpose a =
	let rec f i = match i with
		| 0 -> []
		| _ -> (col i a |> Array.of_list) :: f (i-1)
	in f @@ cols a
	|> Array.of_list
	|> (fun x ->
		assert (rows x = cols a);
		assert (cols x = rows a); x)

let dot a b =
	assert (cols a = rows b);
	let bt = transpose b in
	a |> Array.map (fun r -> 
		bt |> Array.map (fun c ->
			Array.map2 ( *. ) r c
			|> Array.fold_left ( +. ) 0.
		)
	)
	|> (fun x ->
		assert (rows x = rows a);
		assert (cols x = cols b); x)

let plus a b = 
	assert (dims a = dims b);
	Array.map2 (fun ra rb ->
		Array.map2 ( +. ) ra rb
	) a b
	|> (fun x ->
		assert (dims x = dims a); x)