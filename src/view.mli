(** The view module for the simulation, 
controlling what appears on the screen by relaying information from the controller *)

type t
(** The abstract type of values representing the view and its current state *)

type event
(** An abstract type representing an event. Passed into an event handler *)

type event_handler = event -> unit

type interaction_manager = {
    getcell : event_handler;
    setcell : event_handler;
    play : event_handler;
    pause : event_handler;
    save : event_handler;
    load : event_handler
}

val init : interaction_manager -> t

(** [init] Initializes the view with preset settings. *)

val is_initialized : t -> bool
(** [is_initialized] Checks to see whether or not the view has been initialized.
    [render] requires that the view is initialized, or else it will not work. *)

val render : t -> t
(** [render] Re-renders the view with the new data.
    Pre-condition: The view must already be initialized. *)

val set_zoom : t -> float -> float -> float -> t
(** [set_zoom state x y zoom] Sets the zoom of the main window to [zoom] and centers it at ([x], [y]).*)

