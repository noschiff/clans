(** Handles commands to be sent to the model and renderer. *)

type state
(** Type representing the state of a current program, containing
    information on a view and a model. *)

val init : Model.world -> state
(** [init w] creates a controller with a world [w]. *)

val save_to_file : string -> state -> unit
(** [save_to_file file s] saves [s] to file path [file]. *)

val load_from_file : state -> string -> unit
(** [load_from_file s f] stores a model from file path [f] in [s]. *)

val load_from_string : state -> string -> unit
(** [load_from_file s f] stores a model from file [f] in [s]. *)

val display_cell : state -> int -> int -> unit
(** [display_cell s x y] displays a cell at coordinates ([x], [y]) in
    state [s] to the siebar. *)

val update_cell : state -> int -> int -> Model.life -> unit
(** [update_cell s x y data] updates the cell at coordinates ([x], [y])
    in state [s] with new cell [data]. *)

val random_cell : state -> int -> int -> unit
(** [random_cell s x y] inserts a random cell at ([x], [y]) in state
    [s]. *)

val populate_world : state -> float -> unit
(** [populate_world s d] populates the world [s] with [d] proportion of
    energy in the bank to use. *)

val get_json : bool -> state -> Yojson.Safe.t
(** [get_json full_world state] returns the json of [state], or just the
    changes to it depending on [full_world].

    NOTE: [full_world] is currently unused in this implimentation. *)

val step : state -> unit
(** [step s] steps the simulation in world [s] once. *)
