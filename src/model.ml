exception InvalidWorldOperation of int * int

type life = {
  x : int;
  y : int;
  nation : int;
  energy : float;
  brain : Brain.t;
}
(** RI: 0 ≤ x ≤ world width, 0 ≤ y ≤ world height, energy ≥ 0*)

type world = {
  cells : (int, life ref) Hashtbl.t;
  lifes : life ref list ref;
  dim_x : int;
  dim_y : int;
}
(** Mutable. RI: all elements in cells are in lifes and vice versa*)

let to_index world x y = x + (y * world.dim_x)

let new_world dim_x dim_y : world =
  {
    cells = Hashtbl.create (dim_x * dim_y);
    lifes = ref [];
    dim_x;
    dim_y;
  }

let load_world cells = failwith "unimplemented"

let rec helper hashtbl counter max : life option list =
  if counter = max then []
  else
    begin
      (try Some !(Hashtbl.find hashtbl counter) with
      | Not_found -> None)
      :: helper hashtbl (counter + 1) max
    end

(** TODO: make sure isn't reversed lol*)
let get_world world =
  let rec halp curr_row =
    if curr_row = world.dim_y then
      helper world.cells
        (curr_row * world.dim_x)
        (world.dim_y * world.dim_x)
      :: halp (curr_row + 1)
    else []
  in
  halp 0

let generate_random_life (world : world) x y =
  let idx = to_index world x y in
  match Hashtbl.find world.cells idx with
  | exception Not_found ->
      let life =
        ref
          {
            x;
            y;
            brain = Brain.create 18 2 5 [ 10; 10; 5 ];
            nation = 0;
            energy = 100.;
          }
      in
      Hashtbl.add world.cells idx life;
      world.lifes := life :: !(world.lifes)
  | _ -> raise (InvalidWorldOperation (x, y))

let get_cell world x y = None
let get_size world = (world.dim_x, world.dim_y)

let get_nation = function
  | Some life -> life.nation
  | None -> -1

let get_coordinate cell = (cell.x, cell.y)
let simulate world = ()
let clear_cell world x y = ()
let inject_cell world x y nation = ()
let set_cell world x y life = failwith "TODO"

let cell_to_json (l : life option) =
  match l with
  | None -> `Assoc [ ("type", `String "empty") ]
  | Some x ->
      `Assoc
        [
          ("type", `String "life");
          ("nation", `Int x.nation);
          ("energy", `Float x.energy);
        ]

let cell_from_json = failwith "TODO"