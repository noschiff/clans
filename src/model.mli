(** Executes simulation, handles state changes, and provides
    representation of world state and components*)

exception InvalidWorldOperation of int * int
(** Exception for an invalid operation on a world at coordinate (a,b)*)

type world
(** Mutable abstract type that represents the entire world*)

type life
(** Abstract type that represents a single cell life form in the world*)

val new_world : int -> int -> world
(** [new_world dimx dimy] instantiates and returns a new world with no
    cells*)

val load_world : life list list -> world
(** [load_world cells] returns a world from a list of cells*)

val get_world : world -> life option list list
(** [get_world world] returns a list of list of cells where the [i],
    [j]'th element of the list is equivalent [to get_cell i j]. *)

val generate_random_life : world -> int -> int -> unit
(** [generate_random_life nation] returns a new life form with specific
    attributes*)

val get_cell : world -> int -> int -> life option
(** [get_cell x y] Returns the cell at the specified [x] and [y].
    Pre-condition: the coordinates must be valid *)

val get_size : world -> int * int
(** [get_size world] returns the size (length,width) of [world] *)

val get_nation : life option -> int
(** [get_nation cell] returns the nation of [cell] *)

val simulate : world -> unit
(** [simulate world] executes a simulation step, namely the action of a
    single cell *)

val clear_cell : world -> int -> int -> unit
(** [set_cell x y] Clears the cell at the specified [x] and [y]
    coordinates. Pre-conditions: the coordinates must be a valid range *)

val inject_cell : world -> int -> int -> string -> unit
(** [inject_cell x y] Sets the cell at the specified [x] and [y]
    coordinates with the specified data. Pre-conditions: the coordinates
    must be a valid range *)

val set_cell : world -> int -> int -> life -> unit
(** [inject_cell x y] Sets the cell at the specified [x] and [y]
    coordinates with the specified data. Pre-conditions: the coordinates
    must be a valid range *)

val cell_to_json : life option -> Yojson.Basic.t
(** [cell_to_json cell] converts a cell [cell] into its json
    representation *)

val cell_from_json : Yojson.Basic.t -> life
(** [cell_from_json json] creates a cell from its json representation
    [json] *)