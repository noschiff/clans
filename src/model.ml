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
  energy_per_cell : int;
      (** amount of energy given to the energy bank as a linear function
          to the number of cells (default 5)

          Requires: [0 <= energy_per_cell] **)
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
  life_initial_energy : int;
      (** Initial energy given to each life form. This and
          energy_per_cell dictate how common cells will be initially.
          (default: 100)

          Requires: [0 < life_initial_energy] **)
}

type world = {
  params : params;
  cells : life option array array;
  mutable steps : int;  (** Global number of steps taken **)
  mutable counter : int;  (** Represents the id count **)
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
  {
    cells = Array.make_matrix dim_x dim_y None;
    steps = 0;
    counter = 0;
    queue = [];
    dim_x;
    dim_y;
    params = default_params;
  }

let load_world cells = failwith "unimplemented"

let get_world world =
  world.cells |> Array.map Array.to_list |> Array.to_list

let normalize world x y =
  let modulo a b = ((a mod b) + b) mod b in
  (modulo x world.dim_x, modulo y world.dim_y)

let get_cell world x y =
  let x, y = normalize world x y in
  world.cells.(x).(y)

let clear_cell world x y =
  let x, y = normalize world x y in
  world.cells.(x).(y) <- None

let set_cell world x y life =
  let x, y = normalize world x y in
  world.cells.(x).(y) <- Some life

let generate_random_life world x y =
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
            energy = world.params.life_initial_energy;
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
  | _ -> failwith "Brain output did not match."

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
        life.energy <-
          life.energy - world.params.move_energy_consumption;
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

let cell_to_json l =
  match l with
  | None -> `Assoc [ ("type", `String "empty") ]
  | Some x ->
      `Assoc
        [
          ("type", `String "life");
          ("nation", `Float (100. *. x.nation));
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
          brain =
            (match List.assoc "brain" x with
            | exception Not_found -> Brain.create 18 3 5 [ 10; 10; 5 ]
            | json -> Brain.from_json json);
        }
  | _ -> raise (Invalid_argument "Invalid world JSON.")
