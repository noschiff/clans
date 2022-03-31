exception InvalidWorldOperation of int * int

type life = {
  id : int;
      (** Unique identifier representing the cell's id. Can be used to
          search for cells. **)
  nation : float;
      (** Float from 0 to 1, representing the [rad / 2pi] along a circle
          of all possible nations. **)
  mutable brain : Brain.t;
  mutable energy : int;
}
(** RI: 0 ≤ x ≤ world width, 0 ≤ y ≤ world height, energy ≥ 0*)

type params = {
  energy_per_cell : int;
      (** amount of energy given to the energy bank as a linear function
          to the number of cells (default 5)

          Requires: [0 <= energy_per_cell] **)
  step_distributed_energy : float;
      (** Proportion of global energy bank distributed to
          each cell at the onset of its step. This will affect
          the equilibrium population.
          (default: 0.001)

          Requires: [0 < step_distributed_energy <= 1] **)
  initial_steps : int;
      (** Number of steps worth of energy to distribute to a 
          randomly generated cell at the beginning of its life.
          Only used during population.
          (default: 100)

          Requires: [0 < initial_steps] **)
  action_threshold : float;
      (** Threshold for hostility/docility/move action (Default 0.3)

          Requires: [0 <= action_threshold <= 1]**)
  attack_damage : float;
      (** Proportion of total energy that will be used to attack.
          (default 1)

          Requires: [0 <= max_energy_prop]**)
  attack_energy_retained : float;
      (** Proportion of the attacked energy returned to the attacker.
          Extra energy will be given to the energy bank. (Default 0.5)

          Requires: [0 <= attack_energy_retained <= 1]**)
  move_energy_consumption : int;
      (** Amount of energy (constant) consumed on a move action.
          (default 1)

          Requires: [0 <= move_energy_consumption] **)
  reproduction_max_energy_use : float;
      (** Maximum proportion of energy given to offspring after
          reproduction. (default 0.5)

          Requires: [0 <= reproduction_max_energy_use <= 1] **)
  reproduction_energy_retention : float;
      (** Amount of energy proportion retained and passed to child upon
          reproduction. Rest of energy is given to bank. (default 0.9)

          Requires: [0 <= reproduction_energy_retention <= 1]**)
}

type world = {
  params : params;
  cells : life option array array;
  mutable steps : int;  (* Global number of steps taken *)
  mutable counter : int;  (* Represents the id count *)
  mutable bank : int; (* Global energy bank *)
  mutable queue : (int * int * int * int) list;
      (** An element in this list is of the form (x, y, id, s) where (x,
          y) is the position of the cell and [id] is the id, and [s] is
          the last step number on which this cell acted.

          If the cell at the position does not match the id, the element
          is equivalent to none. **)
  dim_x : int;
  dim_y : int;
}
(** Mutable. RI: all elements in cells are in lifes and vice versa*)

let pos_mod n divisor =
  let x = n mod divisor in
  if x < 0 then x + divisor else x

let default_params =
  {
    energy_per_cell = 5;
    initial_steps = 100;
    step_distributed_energy = 0.001;
    action_threshold = 0.3;
    attack_damage = 1.;
    attack_energy_retained = 0.5;
    move_energy_consumption = 1;
    reproduction_max_energy_use = 0.5;
    reproduction_energy_retention = 0.9;
  }

let new_world dim_x dim_y : world =
  let p = default_params in
  {
    params = p;
    cells = Array.make_matrix dim_x dim_y None;
    steps = 0;
    counter = 0;
    bank = dim_x * dim_y * p.energy_per_cell;
    queue = [];
    dim_x;
    dim_y;
  }

let cell_to_json l =
  match l with
  | None -> `Assoc [ ("type", `String "empty") ]
  | Some x ->
      `Assoc
        [
          ("type", `String "life");
          ("id", `Int x.id);
          ("nation", `Float x.nation);
          ("energy", `Int x.energy);
          ("brain", Brain.to_json x.brain);
        ]

let cell_from_json json =
  let open Yojson.Safe.Util in
  ( json |> to_assoc |> fun x ->
    match List.assoc "type" x with
    | exception Not_found -> [ ("type", json) ]
    | _correct_format -> x )
  |> fun x ->
  match List.assoc "type" x |> to_string with
  | "empty" -> None
  | "life" ->
      Some
        {
          id = List.assoc "id" x |> to_int;
          nation = List.assoc "nation" x |> to_float;
          energy = List.assoc "energy" x |> to_int;
          brain = List.assoc "brain" x |> Brain.from_json;
        }
  | _ -> raise (Invalid_argument "Invalid world JSON.")


let to_json world = `Assoc [
  ("cells", `List (
    world.cells
    |> Array.map (fun x ->
      `List (x |> Array.map cell_to_json |> Array.to_list))
    |> Array.to_list));
  ("params", `Assoc [
    ("energy_per_cell", `Int world.params.energy_per_cell);
    ("step_distributed_energy", `Float world.params.step_distributed_energy);
    ("initial_steps", `Int world.params.initial_steps);
    ("action_threshold", `Float world.params.action_threshold);
    ("attack_damage", `Float world.params.attack_damage);
    ("attack_energy_retained", `Float world.params.attack_energy_retained);
    ("move_energy_consumption", `Int world.params.move_energy_consumption);
    ("reproduction_max_energy_use", `Float world.params.reproduction_max_energy_use);
    ("reproduction_energy_retention", `Float world.params.reproduction_energy_retention);
  ]);
  ("steps", `Int world.steps);
  ("counter", `Int world.counter);
  ("bank", `Int world.bank);
  ("queue", `List (
    world.queue
    |> List.map (fun (x, y, i, s) -> `List [
      `Int x;
      `Int y;
      `Int i;
      `Int s;
    ] )
  ));
  ("dim_x", `Int world.dim_x);
  ("dim_y", `Int world.dim_y);
]

let of_json json = 
  let open Yojson.Safe.Util in
  let json = json |> to_assoc in
  {
    cells = json |> List.assoc "cells" |> to_list
    |> List.map (fun x -> x |> to_list |> List.map cell_from_json |> Array.of_list)
    |> Array.of_list;
    params = (let p = json |> List.assoc "params" |> to_assoc in
    {
      energy_per_cell = p |> List.assoc "energy_per_cell" |> to_int;
      step_distributed_energy = p |> List.assoc "step_distributed_energy" |> to_float;
      initial_steps = p |> List.assoc "initial_steps" |> to_int;
      action_threshold = p |> List.assoc "action_threshold" |> to_float;
      attack_damage = p |> List.assoc "attack_damage" |> to_float;
      attack_energy_retained = p |> List.assoc "attack_energy_retained" |> to_float;
      move_energy_consumption = p |> List.assoc "move_energy_consumption" |> to_int;
      reproduction_max_energy_use = p |> List.assoc "reproduction_max_energy_use" |> to_float;
      reproduction_energy_retention = p |> List.assoc "reproduction_energy_retention" |> to_float;
    });
    steps = json |> List.assoc "steps" |> to_int;
    counter = json |> List.assoc "counter" |> to_int;
    bank = json |> List.assoc "bank" |> to_int;
    queue = json 
      |> List.assoc "queue" 
      |> to_list 
      |> List.map to_list 
      |> List.map (function
      | [x; y; i; s] -> (to_int x, to_int y, to_int i, to_int s)
      | _ -> raise (Invalid_argument "Invalid world json"));
    dim_x = json |> List.assoc "dim_x" |> to_int;
    dim_y = json |> List.assoc "dim_y" |> to_int;
  }

let normalize world x y =
  let modulo a b = ((a mod b) + b) mod b in
  (modulo x world.dim_x, modulo y world.dim_y)

let denergy world life amt =
  life.energy <- life.energy + amt;
  world.bank <- world.bank - amt

let get_cell world x y =
  let x, y = normalize world x y in
  world.cells.(x).(y)

let clear_cell world x y =
  let x, y = normalize world x y in
  match world.cells.(x).(y) with
  | None -> ()
  | Some l ->
    denergy world l @@ -l.energy;
    world.cells.(x).(y) <- None

let set_cell world x y life =
  let x, y = normalize world x y in
  (match world.cells.(x).(y) with
  | None -> ()
  | Some l ->
    denergy world l @@ -l.energy);
  world.bank <- world.bank - life.energy;
  world.cells.(x).(y) <- Some life

let random_life world x y =
  let x, y = normalize world x y in
  match get_cell world x y with
  | None ->
      world.counter <- world.counter + 1;
      (* Increment cell id count *)
      world.cells.(x).(y) <-
        Some
          {
            id = world.counter;
            brain = Brain.create 18 3 5 [ 10; 10; 5 ];
            nation = Random.float 1.;
            energy = 0;
          };
      world.queue <-
        world.queue @ [ (x, y, world.counter, world.steps) ]
  | Some _ -> raise (InvalidWorldOperation (x, y))

let get_size world = (world.dim_x, world.dim_y)

let calculate_brain_output world life =
  let cutoff = function
    | x when x > world.params.action_threshold -> Float.min 1. x
    | x when x < -.world.params.action_threshold -> Float.max (-1.) x
    | _ -> 0.
  in
  let sign = function
    | x when x > 0. -> 1.
    | x when x < 0. -> -1.
    | _ -> 0.
  in
  Brain.out life.brain |> function
  | [ dx; dy; h ] -> (
      ( cutoff dx |> sign |> int_of_float,
        cutoff dy |> sign |> int_of_float,
        cutoff h |> function
        | x when x < 0. -> -1.
        | x -> x ))
  | _ -> raise (Invalid_argument "Brain output did not match.")

let property_of_offsets world x y property =
  let x, y = normalize world x y in
  let offsets =
    [
      (0, 0);
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
  offsets
  |> List.map (fun (xoff, yoff) ->
         match get_cell world (x + xoff) (y + yoff) with
         | Some x -> property x
         | None -> -1.)

let mate life adj_life action_bias adj_action_bias = ()

let rec step world =
  let act life x y =
    let ringdist a b =
      (a -. b +. 1. |> fun x -> Float.rem x 1.) |> fun x ->
      if x < 0.5 then x else 1. -. x
    in
    life.brain <-
      Brain.eval life.brain
        (property_of_offsets world x y (fun l ->
             float_of_int l.energy /. float_of_int life.energy)
        @ property_of_offsets world x y (fun l ->
              ringdist l.nation life.nation));
    let dx, dy, h = calculate_brain_output world life in
    let nx, ny = (x + dx, y + dy) in
    match get_cell world nx ny with
    | None ->
        (* No cell here, just move *)
        - world.params.move_energy_consumption |> denergy world life;
        (* only move if we have the energy to, otherwise die *)
        if life.energy > 0 then set_cell world nx nx life;
        clear_cell world x y
    | Some lifeo -> (
        (* Cell here, interact *)
        let _, _, ho = calculate_brain_output world lifeo in
        match (h, ho) with
        | _ when h > 0. && ho > 0. -> () (* Mate *)
        | -1., _ ->
            (* attacking *)
            let e, eo = (life.energy, lifeo.energy) in
            let de =
              (* calculate attack value *)
              float_of_int e *. world.params.attack_damage
              |> Float.ceil
              |> Float.min @@ float_of_int eo
            in
            let gde = de *. world.params.attack_energy_retained in
            life.energy <- e + int_of_float gde;
            lifeo.energy <- e - int_of_float de;
            if lifeo.energy <= 0 then clear_cell world nx ny;
            (* Remove the cost of acting *)
            life.energy <-
              life.energy - world.params.move_energy_consumption
        | _ -> ())
    (* No interaction *)
  in
  let rec pn = function
    (* Loop through queue to find the first valid cell *)
    | [] -> []
    | (x, y, id, _) :: t -> (
        match get_cell world x y with
        | None -> pn t
        | Some life ->
            if life.id = id then (
              act life x y;
              t
              @
              if life.energy > 0 then [ (x, y, id, world.steps) ]
              else [])
            else pn t)
  in
  world.queue <- pn world.queue

let simulate world =
  world.steps <- world.steps + 1;
  let rec rstep _ =
    match world.queue with
    | [] -> ()
    | (_, _, id, s) :: _ when s < world.steps -> ()
    | (_, _, oid, _) :: _ ->
        step world;
        rstep ()
  in
  rstep ()