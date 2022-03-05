(** Executes simulation, handles state changes, and provides
    representation of world state and components*)

type world
(** Abstract type that represents the entire world*)

type cell
(** Abstract type that represents a single cell in the world*)

val get_cell : world -> int -> int -> cell
(** [get_cell x y] Returns the cell at the specified [x] and [y].
    Pre-condition: the coordinates must be valid *)

val get_size : world -> int * int
(** [get_size world] returns the size (length,width) of [world] *)

val get_nation : cell -> string
(** [get_nation cell] returns the nation of [cell] *)

val get_coordinate : cell -> int * int
(** [get_coordinate cell] returns the zero-indexed coordinates (x,y) of
    [cell] *)

val simulate : world -> world
(** [simulate world] executes a simulation step *)

val clear_cell : world -> int -> int -> world
(** [set_cell x y] Clears the cell at the specified [x] and [y]
    coordinates. Pre-conditions: the coordinates must be a valid range *)

val inject_cell : world -> int -> int -> string -> world
(** [inject_cell x y] Sets the cell at the specified [x] and [y]
    coordinates with the specified data Pre-conditions: the coordinates
    must be a valid range *)
