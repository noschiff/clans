(** Handles commands to be sent to the model and renderer. *)

val init : (Bogue.Widget.t -> unit) -> int
(** [init event_handler] creates the view and an empty model with an event handler. *)

val save_to_file : string -> unit
(** [save_to_file file] Saves the currently open model to [file]. *)

val load_from_file : string -> unit
(** Load a model from file *)

val display_cell : int -> int -> unit
(** [display_cell x y] display a cell at coordinates (x, y) to the siebar *)

val update_zoom : float -> float -> float -> unit
(** [update_zoom scale dx dy] sends pan and zoom information to the controller *)

val step : unit -> unit
(** [step] steps the game simulation and updates the view as necessary *)