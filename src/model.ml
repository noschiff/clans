exception InvalidWorldOperation of int * int

type life = {
  mutable x : int;
  mutable y : int;
  mutable nation : float;
  (** Float from 0 to 1, representing the [rad / 2pi] along a circle of 
      all possible nations. **)
  mutable energy : int;
  mutable brain : Brain.t;
}
(** RI: 0 ≤ x ≤ world width, 0 ≤ y ≤ world height, energy ≥ 0*)

type params = {
  action_threshold: float;
  (** Threshold for hostility/docility/move action 
      (Default 0.3)
  
      Requires: [0 <= action_threshold <= 1]**)
  attack_damage: float;
  (** Proportion of total energy that will be
      used to attack.
      (default 1)

      Requires: [0 <= max_energy_prop]**)
  attack_energy_retained: float;
  (** Proportion of the attacked energy returned to
      the attacker. Extra energy will be given to the 
      energy bank.
      (Default 0.5)

      Requires: [0 <= attack_energy_retained <= 1]**)
  energy_per_cell: int;
  (** amount of energy given to the energy bank
      as a linear function to the number of cells 
      (default 5)

      Requires: [0 <= energy_per_cell] **)
  move_energy_consumption: int;
  (** Amount of energy (constant) consumed on a move
      action.
      (default 1)

    Requires: [0 <= move_energy_consumption] **)
  reproduction_max_energy_use: float;
  (** Maximum proportion of energy given to offspring
      after reproduction. 
      (default 0.5)

      Requires: [0 <= reproduction_max_energy_use <= 1] **)
  reproduction_energy_retention: float;
  (** Amount of energy proportion retained and passed
      to child upon reproduction. Rest of energy is
      given to bank.
      (default 0.9) 

      Requires: [0 <= reproduction_energy_retention <= 1]**)
  life_initial_energy: int;
  (** Initial energy given to each life form.
      This and energy_per_cell dictate how common
      cells will be initially.
      (default: 100)

      Requires: [0 < life_initial_energy] **)
}

type world = {
  params : params;
  cells : (int, life ref) Hashtbl.t;
  mutable lifes : life ref list;
  dim_x : int;
  dim_y : int;
}
(** Mutable. RI: all elements in cells are in lifes and vice versa*)

let pos_mod n divisor =
  let x = n mod divisor in
  if x < 0 then x + divisor else x

let to_index world x y =
  pos_mod x world.dim_x + (pos_mod y world.dim_y * world.dim_x)

let default_params = {
  action_threshold = 0.3;
  attack_damage = 1.;
  attack_energy_retained = 0.5;
  energy_per_cell = 1;
  move_energy_consumption = 1;
  reproduction_max_energy_use = 0.5;
  reproduction_energy_retention = 0.9;
  life_initial_energy = 100;
}

let new_world dim_x dim_y : world =
  { cells = Hashtbl.create (dim_x * dim_y); lifes = []; dim_x; dim_y ; params = default_params}

let load_world cells = failwith "unimplemented"

(** TODO: make sure isn't reversed lol*)
let get_world world =
  let rec by_col hashtbl counter max : life option list =
    if counter = max then []
    else
      begin
        (try Some !(Hashtbl.find hashtbl counter) with
        | Not_found -> None)
        :: by_col hashtbl (counter + 1) max
      end
  in
  let rec by_row curr_row =
    if curr_row >= world.dim_y then [ [] ]
    else
      let this =
        let offset = curr_row * world.dim_x in
        by_col world.cells offset (world.dim_x + offset)
      in
      let next = by_row (curr_row + 1) in
      match next with
      | [ [] ] -> [ this ]
      | _ -> this :: next
  in
  by_row 0

let generate_random_life (world : world) x y =
  if x < 0 || x >= world.dim_x || y < 0 || y >= world.dim_y then
    raise (InvalidWorldOperation (x, y))
  else
    let index = to_index world x y in
    match Hashtbl.find world.cells index with
    | exception Not_found ->
        let life =
          ref
            {
              x;
              y;
              brain = Brain.create 18 3 5 [ 10; 10; 5 ];
              nation = Random.float 1.;
              energy = world.params.life_initial_energy;
            }
        in
        Hashtbl.add world.cells index life;
        world.lifes <- world.lifes @ [ life ]
    | _ -> raise (InvalidWorldOperation (x, y))

let get_cell world x y = None
let get_size world = (world.dim_x, world.dim_y)

let get_nation = function
  | Some life -> 100. *. life.nation
  | None -> -1.

let get_coordinate cell = (cell.x, cell.y)

let doAction world lref = 
  let cutoff = function
    | x when x > world.params.action_threshold -> Float.min 1. x
    | x when x < -.world.params.action_threshold -> Float.max (-1.) x
    | _ -> 0.
  in let extremify x = x /. Float.abs x
  in let life = !lref in
  Brain.out life.brain
  |> (function
    | [dx; dy; h] -> (
      cutoff dx
      |> extremify, 
      cutoff dy
      |> extremify,
      cutoff h
      |> function
        | x when x < 0. -> extremify x
        | x -> x
    )
    | _ -> failwith "Brain output did not match.")
  |> ignore

let property_of_offsets world x y property =
  let offsets =
    [
      (0, 1);
      (0, -1);
      (1, 1);
      (1, 0);
      (1, -1);
      (-1, 1);
      (-1, 0);
      (-1, -1);
    ]
  in
  List.map
    (fun (xoff, yoff) ->
      try
        property
          !(Hashtbl.find world.cells
              (to_index world (x + xoff) (y + yoff)))
      with
      | Not_found -> -1.) (* Default value per Brain *)
    offsets

let simulate world =
  let ringdist a b = 
    a -. b +. 1.
    |> (fun x -> Float.rem x 1.)
    |> (fun x -> if x < 0.5 then x else 1. -. x)
  in match world.lifes with
  | [] -> ()
  | lref :: t ->
      let life = !lref in
      let x = life.x in
      let y = life.y in
      life.brain <-
        Brain.eval life.brain
          (property_of_offsets world x y (fun l -> float_of_int l.energy /. float_of_int life.energy)
          @ property_of_offsets world x y (fun l -> ringdist l.nation life.nation));
      doAction world lref;
      world.lifes <- t @ [ lref ]

let clear_cell world x y =
  Hashtbl.remove world.cells (to_index world x y);
  world.lifes <-
    List.filter (fun l -> !l.x <> x && !l.y <> y) world.lifes

let inject_cell world x y nation =
  try
    !(Hashtbl.find world.cells (to_index world x y)).nation <- float_of_int nation /. 100.
  with
  | Not_found -> raise (InvalidWorldOperation (x, y))

let set_cell world x y life =
  try Hashtbl.find world.cells (to_index world x y) := life with
  | Not_found -> raise (InvalidWorldOperation (x, y))

let get_queue_nations world = List.map (fun x -> Some !x |> get_nation) world.lifes

let cell_to_json l =
  match l with
  | None -> `Assoc [ ("type", `String "empty") ]
  | Some x ->
    `Assoc
      [
        ("type", `String "life");
        ("nation", `Float (100. *. x.nation));
        ("energy", `Int x.energy);
        ("x", `Int x.x);
        ("y", `Int x.y);
        ("brain", Brain.to_json x.brain);
      ]

let cell_from_json json =
  let open Yojson.Safe.Util in
  json
  |> to_assoc
  |> (fun x ->
    match List.assoc "type" x |> to_string with
    | "empty" -> None
    | "life" -> Some {
      nation = List.assoc "nation" x |> to_float;
      energy = List.assoc "energy" x |> to_int;
      x = List.assoc "x" x |> to_int;
      y = List.assoc "y" x |> to_int;
      brain = List.assoc "brain" x |> Brain.from_json;
    }
    | _ -> raise (Invalid_argument "Invalid world JSON.")
  )