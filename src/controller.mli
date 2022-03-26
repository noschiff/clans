(** Handles commands to be sent to the model and renderer. *)

(** NOTE: TEMPORARILY EXPOSING THIS SO I CAN TEST!!!!!*)
type state = Model.world
(** state representing the state of a current program, containing
    information on a view and a model. *)

val init : (Yojson.Safe.t option -> unit) -> state
(** [init] creates the view and an empty model with an event handler. *)

val save_to_file : string -> state -> unit
(** [save_to_file file] Saves the currently open model to [file]. *)

val load_from_file : state -> string -> state
(** Load a model from file *)

val display_cell : state -> int -> int -> state
(** [display_cell x y] display a cell at coordinates (x, y) to the
    siebar *)

val update_cell : state -> int -> int -> Model.life -> state
(** [update_cell x y data] updates the cell at coordinates (x, y) with
    new cell data. *)