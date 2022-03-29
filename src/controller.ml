type state = Model.world ref

let init w = ref w
let set_world state world = state := world
let get_world state = !state

let get_json b state =
  `Assoc
    [
      ( "world",
        `List
          (state |> get_world |> Model.get_world
          |> List.map (fun x ->
                 `List (x |> List.map Model.cell_to_json))) );
    ]

let save_to_file filename state =
  state |> get_json true |> Yojson.Safe.to_file filename

let load_from_file state filename =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file filename
  |> to_assoc |> List.assoc "world" |> to_list
  |> List.map (fun x -> x |> to_list |> List.map Model.cell_from_json)
  |> Model.load_world |> set_world state

let display_cell state x y = ()
(*Model.get_cell state.world x y |> Server.respond (* draw cell*) x y
  state.world state.view |> set_view state

  To be implimented properly, but temprarily commented out so it
  compiles*)

let update_cell state x y cell =
  Model.set_cell (get_world state) x y cell

let random_cell state x y =
  Model.generate_random_life (get_world state) x y

let step state = state |> get_world |> Model.simulate