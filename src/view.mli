(** The view module for the simulation, controlling what appears on the
    screen by relaying information from the controller *)

type t
(** The abstract type of values representing the view and its current
    state *)

val init : unit -> t
(** [init] Initializes the view with preset settings. *)

val render : t -> t
(** [render] Re-renders the view with the new data. Pre-condition: The
    view must already be initialized. *)

val set_zoom : t -> float -> float -> float -> t
(** [set_zoom state x y zoom] Sets the zoom of the main window to [zoom]
    and centers it at ([x], [y]).*)

val draw_cell : t -> int -> int -> Model.cell -> t
(** [draw_cell] state x y cell Draw a cell at a position *)