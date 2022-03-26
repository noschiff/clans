type life = { nation : string }

type cell =
  | Cell of life
  | Wall
  | Empty

type world = cell array array

let new_world dimx dimy : world = Array.make_matrix dimx dimy Empty

let load_world (cells : cell list list) =
  Array.of_list (List.map Array.of_list cells)

let get_world world = Array.to_list (Array.map Array.to_list world)
let make_life nation = { nation }

let get_cell (world : world) (x : int) (y : int) =
  Array.get (Array.get world 0) 0
(* world |> Array.get y |> Array.get x

   Commented it out because it doesn't compile >:( *)

let get_size world = (Array.length world, Array.length world.(0))
let get_nation life = life.nation
(* function | Cell life -> life.nation | _ -> ""

   More non-compiling code >:( *)

let get_coordinate cell = (0, 0)
let simulate world = world
let clear_cell world x y = world
let inject_cell world x y (nation : cell) = world
(*world.(y).(x) <- { nation }

  For the love of god please make sure your code at least compiles
  before pushing it.*)

let to_json = failwith "EDMUND"