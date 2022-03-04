(** The view module for the simulation, 
controlling what appears on the screen by relaying information from the controller *)

type t
(** The abstract type of values representing the view and its current state *)

val init : t
(** [init] Initializes the view with preset settings. *)

val is_initialized : t -> bool
(** [is_initialized] Checks to see whether or not the view has been initialized.
    [render] requires that the view is initialized, or else it will not work. *)

val render : t -> t
(** [render] Re-renders the view with the new data.
    Pre-condition: The view must already be initialized. *)

val get_cell : t -> int -> int -> unit
(** [get_cell] Gets the cell at the specified [x] and [y]
    coordinates, and returns the data about that cell.
    Pre-conditions: The view must already be initialized, and the coordinates must be a valid range *)

val set_cell : t -> int -> int -> string -> t
(** [set_cell] Sets the cell at the specified [x] and [y]
    coordinates with the specified data.
    Pre-conditions: The view must already be initialized, and the coordinates must be a valid range *)

val zoom_in : t -> int -> t
(** [zoom_in] Zooms into the view with the specified magnitude.
    Pre-conditions: The view must already be initialized *)

val zoom_out : t-> int -> t
(** [zoom_in] Zooms out from the view with the specified magnitude.
    Pre-conditions: The view must already be initialized *)