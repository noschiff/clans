type state = Model.world ref

let init w = ref w
let set_world state world = state := world

let save_to_file filename state =
  `Assoc
    [
      ( "world",
        `List
          (!state |> Model.get_world
          |> List.map (fun x ->
                 `List (x |> List.map Model.cell_to_json))) );
    ]
  |> Yojson.Safe.to_file filename

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

(** EDMUND: now that world is mutable, do we need to make this return
    state?*)
let update_cell state x y cell = Model.set_cell !state x y cell

(*let step state = set_world state @@ Model.simulate state.world |> fun
  x -> set_view x @@ Server.render x.view

  Improper call to server.render so I'm commenting it out for
  compilation reasons rn*)

(** Edmund please make this be a function that returns the current state
    in a json, without taking the world as a parameter because I won't
    have access to it in main/server *)
let get_json state = failwith "unimplemented"

(** Another function that I need and don't have access to the current
    world*)
let step = failwith "Unimplimented"