type state = Model.world

let init event_handler = Server.init event_handler; Model.new_world 100 100

let set_world state world = world

let cell_to_json (l:Model.life option) =
  match l with
  | None -> `Assoc [ ("type", `String "empty") ]
  | Some _ ->
      `Assoc
        [
          ("type", `String "life");
          ("data", `String (Model.get_nation l |> string_of_int));
        ]

let cell_from_json json =
  (* let open Yojson.Basic.Util in
  json |> to_assoc |> fun x ->
  match List.assoc "type" x |> to_string with
  | "empty" -> Model.Empty
  | "wall" -> Model.Wall
  | "life" ->
      Model.Cell (List.assoc "data" x |> to_string |> Model.make_life)
  | _ -> raise (Invalid_argument "Invalid world JSON.") *)
  failwith "will fix later"

let save_to_file filename state =
  `Assoc
    [
      ( "world",
        `List
          (state |> Model.get_world
          |> List.map (fun x -> `List (x |> List.map cell_to_json))) );
    ]
  |> Yojson.Basic.to_file filename

let load_from_file state filename =
  let open Yojson.Basic.Util in
  Yojson.Basic.from_file filename
  |> to_assoc |> List.assoc "world" |> to_list
  |> List.map (fun x -> x |> to_list |> List.map cell_from_json)
  |> Model.load_world |> set_world state

let display_cell state x y = state
(*Model.get_cell state.world x y |> Server.respond (* draw cell*) x y
  state.world state.view |> set_view state

  To be implimented properly, but temprarily commented out so it
  compiles*)

let update_cell state x y cell : state=
  Model.set_cell state x y cell; state
  (** EDMUND: now that world is mutable, do we need to make this return state?*)

(*let step state = set_world state @@ Model.simulate state.world |> fun
  x -> set_view x @@ Server.render x.view

  Improper call to server.render so I'm commenting it out for
  compilation reasons rn*)
