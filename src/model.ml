exception InvalidWorldOperation of int * int

type life = {
  mutable x : int;
  mutable y : int;
  mutable nation : int;
  mutable energy : float;
  mutable brain : Brain.t;
}
(** RI: 0 ≤ x ≤ world width, 0 ≤ y ≤ world height, energy ≥ 0*)

type world = {
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

let new_world dim_x dim_y : world =
  { cells = Hashtbl.create (dim_x * dim_y); lifes = []; dim_x; dim_y }

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
              brain = Brain.create 18 2 5 [ 10; 10; 5 ];
              nation = index;
              energy = 100.;
            }
        in
        Hashtbl.add world.cells index life;
        world.lifes <- world.lifes @ [ life ]
    | _ -> raise (InvalidWorldOperation (x, y))

let get_cell world x y = None
let get_size world = (world.dim_x, world.dim_y)

let get_nation = function
  | Some life -> life.nation
  | None -> -1

let get_coordinate cell = (cell.x, cell.y)
let doAction lref = ()

let simulate world =
  match world.lifes with
  | [] -> raise (InvalidWorldOperation (-1, -1))
  | lref :: t -> begin
      let life = !lref in
      try
        let x = life.x in
        let y = life.y in
        !lref.brain <-
          Brain.eval life.brain
            (List.map
               (fun (xoff, yoff) ->
                 !(Hashtbl.find world.cells
                     (to_index world (x + xoff) (y + yoff)))
                   .energy)
               [
                 (0, 1);
                 (0, -1);
                 (1, 1);
                 (1, 0);
                 (1, -1);
                 (-1, 1);
                 (-1, 0);
                 (-1, -1);
               ]);

        doAction lref;
        world.lifes <- t @ [ lref ]
      with
      | Not_found -> raise (InvalidWorldOperation (life.x, life.y))
    end

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
