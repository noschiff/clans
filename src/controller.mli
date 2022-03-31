(** Handles commands to be sent to the model and renderer. *)

type state
(** state representing the state of a current program, containing
    information on a view and a model. *)

val init : Model.world -> state
(** [init w] creates a controller with a world [w]. *)

val save_to_file : string -> state -> unit
(** [save_to_file file s] Saves [s] to [file]. *)

val load_from_file : state -> string -> unit
(** [load_from_file s f] Loads a model from file [f] to [s]*)

val display_cell : state -> int -> int -> unit
(** [display_cell s x y] display a cell at coordinates (x, y) to the
    siebar *)

val update_cell : state -> int -> int -> Model.life -> unit
(** [update_cell s x y data] updates the cell at coordinates ([x], [y])
    with new cell [data]. *)

val random_cell : state -> int -> int -> unit
(** [random_cell s x y] inserts a random cell at ([x], [y]) *)

val populate_world : state -> float -> unit
(** [populate_world s d] populates the world in [s]
    with [d] representing the prop of energy in the bank
    to use **)

val get_json : bool -> state -> Yojson.Safe.t
(** [get_json full_world state] returns the json of [state], or just the
    changes to it depending on [full_world].

    NOTE: [full_world] is currently unused in this implimentation. *)

val step : state -> unit
(** [step s] Steps the simulation in [s] once. *)
