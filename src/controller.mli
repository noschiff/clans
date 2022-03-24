(** Handles commands to be sent to the model and renderer. *)

type state
(** state representing the state of a current program, containing
    information on a view and a model. *)

val cell_to_json : Model.cell -> Yojson.Basic.t
(** [cell_to_json cell] converts a cell [cell] into its json
    representation *)

val cell_from_json : Yojson.Basic.t -> Model.cell
(** [cell_from_json json] creates a cell from its json representation
    [json] *)

(*val init : unit -> state*)
(** [init] creates the view and an empty model with an event handler. *)

val save_to_file : string -> state -> unit
(** [save_to_file file] Saves the currently open model to [file]. *)

val load_from_file : state -> string -> state
(** Load a model from file *)

val display_cell : state -> int -> int -> state
(** [display_cell x y] display a cell at coordinates (x, y) to the
    siebar *)

val update_cell : state -> int -> int -> Model.cell -> state
(** [update_cell x y data] updates the cell at coordinates (x, y) with
    new cell data. *)

(*val step : state -> state*)
(** [step] steps the game simulation and updates the view as necessary *)