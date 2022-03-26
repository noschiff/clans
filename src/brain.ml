type t = {
  weights : Matrix.t array;
  biases : Matrix.t array;
  mem : float array;
  out : float array;
}

(** Uses box-muller transform to generate normal distributed random
    numbers *)
let normal () =
  let x, y = (Random.float 1., Random.float 1.) in
  sqrt (-2. *. log x) *. cos (2. *. Float.pi *. y)

(** Creates an mxn matrix with normal random dostributed numbers *)
let normal_matrix m n =
  let rec g = function
    | 0 -> []
    | a -> normal () :: g (a - 1)
  in
  let rec f a = function
    | 0 -> []
    | b -> g a :: f a (b - 1)
  in
  f m n |> Matrix.of_list

let create i o m l =
  let ls = ((i + m) :: l) @ [ o + m ] in
  {
    weights =
      (let rec f a b l =
         normal_matrix a b
         ::
         (match l with
         | [] -> []
         | c :: l2 -> f b c l2)
       in
       ls
       |> (function
            | a :: b :: l -> f a b l
            | _ -> failwith "impossible")
       |> Array.of_list);
    biases =
      ls
      |> (function
           | a :: b -> b
           | _ -> failwith "impossible")
      |> List.map (fun x -> normal_matrix 1 x)
      |> Array.of_list;
    mem = Array.make m 0.;
    out = Array.make o 0.;
  }

let eval brain inp = 
	let rec e i r = match (Array.length brain.weights - i) with
		| 0 -> r
		| _ -> Matrix.dot r brain.weights.(i)
			|> Matrix.plus brain.biases.(i)
			|> e (i+1)
	in Array.append (Array.of_list inp) brain.mem
		|> Array.to_list
		|> (fun x -> Matrix.of_list [x])
		|> e 0
		|> Matrix.row 0
		|> Array.of_list
		|> (fun x -> {
			brain with
			out = Array.sub x 0 (Array.length brain.out);
			mem =  Array.sub x (Array.length brain.out) (Array.length brain.mem)
		})
	

let out brain = Array.to_list brain.out
let mem brain = Array.to_list brain.mem