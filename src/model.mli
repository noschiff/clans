(** Executes simulation, handles state changes, and provides
    representation of world state and components*)

exception InvalidWorldOperation of int * int
(** Exception for an invalid operation on a world at coordinate (a,b)*)

type world
(** Mutable abstract type that represents the entire world*)

type life
(** Mutable abstract type that represents a single cell life form in the
    world*)

type params = {
  energy_per_cell : int;
      (** amount of energy given to the energy bank as a linear function
          to the number of cells (default 5)

          Requires: [0 <= energy_per_cell] **)
  step_distributed_energy : float;
      (** Proportion of global energy bank distributed to
          each cell at the onset of its step. This will affect
          the equilibrium population.
          (default: 0.001)

          Requires: [0 < step_distributed_energy <= 1] **)
  initial_energy : int;
      (** Amount of initial energy to give to randomly generated
          cells (only used during population).
          (default: 100)

          Requires: [0 < initial_energy] **)
  action_threshold : float;
      (** Threshold for hostility/docility/move action (Default 0.3)

          Requires: [0 <= action_threshold <= 1]**)
  attack_damage : float;
      (** Proportion of total energy that will be used to attack.
          (default 1)

          Requires: [0 <= max_energy_prop]**)
  attack_energy_retained : float;
      (** Proportion of the attacked energy returned to the attacker.
          Extra energy will be given to the energy bank. (Default 0.5)

          Requires: [0 <= attack_energy_retained <= 1]**)
  move_energy_consumption : int;
      (** Amount of energy (constant) consumed on a move action.
          (default 1)

          Requires: [0 <= move_energy_consumption] **)
  reproduction_max_energy_use : float;
      (** Maximum proportion of energy given to offspring after
          reproduction. (default 0.5)

          Requires: [0 <= reproduction_max_energy_use <= 1] **)
  reproduction_energy_retention : float;
      (** Amount of energy proportion retained and passed to child upon
          reproduction. Rest of energy is given to bank. (default 0.9)

          Requires: [0 <= reproduction_energy_retention <= 1]**)
}
(** Type representing all parameters. **)

val default_params : params
(** The default set of parameters. **)

val new_world : int -> int -> world
(** [new_world dimx dimy] instantiates and returns a new world with no
    cells*)

val of_json : Yojson.Safe.t -> world
(** [of_json json] returns a world from its json representation. *)

val to_json : world -> Yojson.Safe.t
(** [to_json world] returns the json representation of a world. *)

val random_life : world -> int -> int -> unit
(** [random_life world x y] inserts a new life form with random
    attributes at ([x], [y])*)

val populate_random : world -> float -> unit
(** [populate_random world d] uses [d *. world.bank] energy to populate
    the world with as many random cells as it can. **)

val get_cell : world -> int -> int -> life option
(** [get_cell x y] Returns the cell at the specified [x] and [y].
    Pre-condition: the coordinates must be valid *)

val get_size : world -> int * int
(** [get_size world] returns the size (length,width) of [world] *)

val simulate : world -> unit
(** [simulate world] executes a simulation step, namely the action of a
    single cell. Does nothing if there are no cells in the world.*)

val clear_cell : world -> int -> int -> unit
(** [set_cell x y] Clears the cell at the specified [x] and [y]
    coordinates. Does nothing if there is no cell at ([x],[y]) *)

val set_cell : world -> int -> int -> life -> unit
(** [inject_cell x y] Sets the cell at the specified [x] and [y]
    coordinates with the specified data. Pre-conditions: the coordinates
    must be a valid range. Raises InvalidWorldOperation (x,y) if there
    is no life at coordinates (x,y). *)

val cell_to_json : life option -> Yojson.Safe.t
(** [cell_to_json cell] converts a cell [cell] into its json
    representation *)

val cell_from_json : Yojson.Safe.t -> life option
(** [cell_from_json json] converts a json representation of a cell into
    a cell. **)
