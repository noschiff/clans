let init event_handler = failwith "Unimplemented"

let cell_to_json = function
	| Model.Empty -> `Assoc ([("type", `String "empty")])
	| Model.Wall -> `Assoc ([("type", `String "wall")])
	| Model.Cell l -> `Assoc ([
		("type", `String "life");
		("data", `String (Model.get_nation l))
	])

let cell_from_json json = 
	let open Yojson.Basic.Util in
	json
	|> to_assoc
	|> (fun x -> match List.assoc "type" x |> to_string with
		| "empty" -> Model.Empty
		| "wall" -> Model.Wall
		| "life" -> Model.Cell (List.assoc "data" x |> to_string |> Model.make_life)
		| _ -> raise (Invalid_argument "Invalid world JSON.")
	)

let save_to_file filename world = 
	`Assoc ([("world",
		`List (
			world |> Model.get_world
			|> List.map (fun x -> 
				`List (
					x
					|> List.map cell_to_json
				)
			)
		)
	)])
	|> Yojson.Basic.to_file filename

let load_from_file filename = 
	let open Yojson.Basic.Util in
	Yojson.Basic.from_file filename 
	|> to_assoc 
	|> List.assoc "world"
	|> to_list
	|> List.map (fun x-> x 
		|> to_list 
		|> List.map cell_from_json
	) |> Model.load_world

let display_cell x y = ()

let update_zoom scale dx dy = ()

let step () = ()