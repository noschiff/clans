(** Handles commands to be sent to the model and renderer. *)

type state
(** state representing the state of a current program, containing
    information on a view and a model. *)

val init : Model.world -> state
(** [init] creates the view and an empty model with an event handler. *)

val save_to_file : string -> state -> unit
(** [save_to_file file] Saves the currently open model to [file]. *)

val load_from_file : state -> string -> unit
(** Load a model from file *)

val display_cell : state -> int -> int -> unit
(** [display_cell x y] display a cell at coordinates (x, y) to the
    siebar *)

val update_cell : state -> int -> int -> Model.life -> unit
(** [update_cell x y data] updates the cell at coordinates ([x], [y])
    with new cell [data]. *)

val random_cell : state -> int -> int -> unit
(** [random_cell x y] inserts a random cell at ([x], [y]) *)

val get_json : bool -> state -> Yojson.Safe.t
(** [get_json full_world state] returns the json of [state], or just the
    changes to it depending on [full_world].

    NOTE: [full_world] is currently unused in this implimentation. *)

val step : state -> unit
(** Steps the simulation once. *)
