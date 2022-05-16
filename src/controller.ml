type state = Model.world ref

let init w = ref w
let set_world state world = state := world
let get_world state = !state

let get_json b state =
  `Assoc
    [
      ( "world", Model.to_json !state );
    ]

let save_to_file filename state =
  state |> get_json true |> Yojson.Safe.to_file filename

let load_from_file state filename =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file filename
  |> to_assoc |> List.assoc "world" |> Model.of_json |> set_world state

let load_from_string state str =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_string str
  |> to_assoc |> List.assoc "world" |> Model.of_json |> set_world state

let display_cell state x y = ()
(*Model.get_cell state.world x y |> Server.respond (* draw cell*) x y
  state.world state.view |> set_view state

  To be implimented properly, but temprarily commented out so it
  compiles*)

let update_cell state x y cell =
  Model.set_cell (get_world state) x y cell

let random_cell state x y =
  Model.random_life (get_world state) x y

let populate_world state d =
  Model.populate_random (get_world state) d

let step state = state |> get_world |> Model.simulate