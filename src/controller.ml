type state = {
	world: Model.world;
	view: View.t
}

let init event_handler = {
	world = Model.new_world 100 100;
	view = View.init ()
}

let set_world state world = {
	world = world;
	view = state.view
}

let set_view state view = {
	world = state.world;
	view = view
}

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

let save_to_file filename state = 
	`Assoc ([("world",
		`List (
			state.world |> Model.get_world
			|> List.map (fun x -> 
				`List (
					x
					|> List.map cell_to_json
				)
			)
		)
	)])
	|> Yojson.Basic.to_file filename

let load_from_file state filename = 
	let open Yojson.Basic.Util in Yojson.Basic.from_file filename 
		|> to_assoc 
		|> List.assoc "world"
		|> to_list
		|> List.map (fun x-> x 
			|> to_list 
			|> List.map cell_from_json
		) |> Model.load_world
		|> set_world state

let display_cell state x y = Model.get_cell state.world x y
	|> View.draw_cell state.view x y
	|> set_view state

let step state = failwith "Unimplemented"