type t = {
  weights : Matrix.t array;
  biases : Matrix.t array;
  mem : float array;
  out : float array;
}

type mut_params = {
  swap_chance : float;
      (** chance to completely change a given weight into a new value
          (default 0.005)

          Requires: [0 <= swap_chance < 1] **)
  mutate_chance : float;
      (** chance to mutate a given weight by adding a random normal
          variable to it (default 0.02)

          Requires: [0 <= mutate_chance < 1] **)
  mutate_stdev : float;
      (** standard deviation of the normal variable to add to the weight
          (default 1)

          Requires: [0 < mutate_stdev] **)
}

let default_params =
  { swap_chance = 0.005; mutate_chance = 0.02; mutate_stdev = 1. }

let params_of_json j =
  let open Yojson.Safe.Util in
  let json = j |> to_assoc in
  {
    swap_chance = json |> List.assoc "swap_chance" |> to_number;
    mutate_chance = json |> List.assoc "mutate_chance" |> to_number;
    mutate_stdev = json |> List.assoc "mutate_stdev" |> to_number;
  }

let params_to_json p =
  `Assoc
    [
      ("swap_chance", `Float p.swap_chance);
      ("mutate_chance", `Float p.mutate_chance);
      ("mutate_stdev", `Float p.mutate_stdev);
    ]

(** Uses box-muller transform to generate normal distributed random
    numbers *)
let gaussian () =
  let x, y = (Random.float 1., Random.float 1.) in
  sqrt (-2. *. log x) *. cos (2. *. Float.pi *. y)

let normal m s = (gaussian () *. s) +. m

(** Creates an mxn matrix with normal random distributed numbers *)
let normal_matrix n m =
  let rec g = function
    | 0 -> []
    | a -> gaussian () :: g (a - 1)
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
            | _ -> raise (Failure "impossible"))
       |> Array.of_list);
    biases =
      ls
      |> (function
           | a :: b -> b
           | _ -> raise (Failure "impossible"))
      |> List.map (fun x -> normal_matrix 1 x)
      |> Array.of_list;
    mem = Array.make m 0.;
    out = Array.make o 0.;
  }

let combine p a b =
  let wav i j = (i *. p) +. (j *. (1. -. p)) in
  {
    weights = Array.map2 (Matrix.map2 wav) a.weights b.weights;
    biases = Array.map2 (Matrix.map2 wav) a.biases b.biases;
    mem = Array.make (Array.length b.mem) 0.;
    out = Array.make (Array.length b.out) 0.;
  }

let deterministic_mutate r p b =
  let mutv v =
    match r with
    | c when c < 0. -> failwith "Invalid float"
    | c when 0. <= c && c < p.swap_chance -> gaussian ()
    | c when p.swap_chance <= c && c < p.swap_chance +. p.mutate_chance
      ->
        v +. normal 0. p.mutate_stdev
    | _ -> v
  in
  let mutm a = a |> Matrix.map mutv in
  {
    weights = b.weights |> Array.map mutm;
    biases = b.biases |> Array.map mutm;
    mem = Array.make (Array.length b.mem) 0.;
    out = Array.make (Array.length b.out) 0.;
  }

let mutate p b = deterministic_mutate (Random.float 1.) p b

let matrix_to_json m =
  `List
    (m |> Matrix.to_list
    |> List.map (fun x -> `List (x |> List.map (fun x -> `Float x))))

let matrix_from_json j =
  let open Yojson.Safe.Util in
  j |> to_list
  |> List.map (fun x -> x |> to_list |> List.map to_float)
  |> Matrix.of_list

let to_json b =
  `Assoc
    [
      ( "weights",
        `List (b.weights |> Array.to_list |> List.map matrix_to_json) );
      ( "biases",
        `List (b.biases |> Array.to_list |> List.map matrix_to_json) );
      ( "mem",
        `List (b.mem |> Array.to_list |> List.map (fun x -> `Float x))
      );
      ( "out",
        `List (b.out |> Array.to_list |> List.map (fun x -> `Float x))
      );
    ]

let from_json j =
  let open Yojson.Safe.Util in
  j |> to_assoc |> fun x ->
  {
    weights =
      List.assoc "weights" x |> to_list
      |> List.map matrix_from_json
      |> Array.of_list;
    biases =
      List.assoc "biases" x |> to_list
      |> List.map matrix_from_json
      |> Array.of_list;
    mem =
      List.assoc "mem" x |> to_list |> List.map to_number
      |> Array.of_list;
    out =
      List.assoc "out" x |> to_list |> List.map to_number
      |> Array.of_list;
  }

let eval brain inp =
  let rec e i r =
    match Array.length brain.weights - i with
    | 0 -> r
    | _ ->
        Matrix.dot r brain.weights.(i)
        |> Matrix.map2 ( +. ) brain.biases.(i)
        |> Matrix.map Float.tanh
        |> e (i + 1)
  in
  inp @ Array.to_list brain.mem
  |> (fun x -> Matrix.of_list [ x ]) (* 1xn matrix *)
  |> e 0 |> Matrix.row 0 |> Array.of_list
  |> fun x ->
  {
    brain with
    out = Array.sub x 0 (Array.length brain.out);
    mem = Array.sub x (Array.length brain.out) (Array.length brain.mem);
  }

let out brain = Array.to_list brain.out
let mem brain = Array.to_list brain.mem