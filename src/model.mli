(** Executes simulation, handles state changes, and provides
    representation of world state and components. *)

exception InvalidWorldOperation of int * int
(** [InvalidWorldOperation (a, b)] represents an invalid operation on a
    world at coordinate ([a], [b]). *)

type world
(** Mutable abstract type that represents the entire world. *)

type life
(** Mutable abstract type that represents a single cell life form in the
    world. *)

type params = {
  energy_per_cell : int;
      (** Amount of energy given to the energy bank as times the number
          of cells.

          Default: 5.

          Requires: [0 <= energy_per_cell]. *)
  step_distributed_energy : float;
      (** Proportion of global energy bank distributed to each cell at
          the onset of its step. This will affect the equilibrium
          population.

          Default: 0.001.

          Requires: [0 < step_distributed_energy <= 1]. *)
  initial_energy : int;
      (** Amount of initial energy given to randomly generated cells
          during population.

          Default: 100.

          Requires: [0 < initial_energy]. *)
  action_threshold : float;
      (** Threshold for hostility/docility/move action

          Default: 0.3.

          Requires: [0 <= action_threshold <= 1]. *)
  attack_damage : float;
      (** Proportion of total energy that will be used to attack.

          Default: 1.

          Requires: [0 <= max_energy_prop]. *)
  attack_energy_retained : float;
      (** Proportion of the attacked energy returned to the attacker.
          Extra energy will be given to the energy bank. (Default 0.5)

          Requires: [0 <= attack_energy_retained <= 1]. *)
  move_energy_consumption : int;
      (** Amount of energy consumed on a move action.

          Default: 1.

          Requires: [0 <= move_energy_consumption]. *)
  reproduction_max_energy_use : float;
      (** Maximum proportion of energy given to offspring after
          reproduction.

          Default: 0.5.

          Requires: [0 <= reproduction_max_energy_use <= 1]. *)
  reproduction_energy_retention : float;
      (** Proportion of energy retained and passed to child upon
          reproduction. Rest of energy is given to bank.

          Default: 0.9.

          Requires: [0 <= reproduction_energy_retention <= 1]. *)
  nation_mutation_proportion : float;
      (** Maximum amount to modify the nation by. The nation of an
          offspring will have a uniform random value from
          ([-nation_mutation_proportion], [nation_mutation_proportion])
          added to it.

          Default: 0.1.

          Requires: [0 <= nation_mutation_proportion <= 0.5]. *)
  mutation : Brain.mut_params;  (** Brain's parameters for mutation. *)
}
(** Type representing all parameters. *)

val default_params : params
(** The default set of parameters. *)

val new_world : int -> int -> world
(** [new_world dimx dimy] instantiates and returns a new world with x
    size [dimx] and y size [dimy] with no cells. *)

val of_json : Yojson.Safe.t -> world
(** [of_json json] returns a world from its json representation [json]. *)

val to_json : world -> Yojson.Safe.t
(** [to_json world] returns the json representation of a world [world]. *)

val random_life : world -> int -> int -> unit
(** [random_life world x y] inserts a new life form with random
    attributes at ([x], [y]) in world [world]. *)

val populate_random : world -> float -> unit
(** [populate_random world d] uses [d *. world.bank] energy to populate
    the world with as many random cells as it can. *)

val get_cell : world -> int -> int -> life option
(** [get_cell x y] Returns the cell at the specified [x] and [y] in
    world [world]. Pre-condition: the coordinates must be valid. *)

val get_size : world -> int * int
(** [get_size world] returns the size (length, width) of [world]. *)

val simulate : world -> unit
(** [simulate world] executes a simulation step of world [world]. Does
    nothing if there are no cells in the world. *)

val clear_cell : world -> int -> int -> unit
(** [set_cell world x y] clears the cell at the specified [x] and [y]
    coordinates in world [world]. Does nothing if there is no cell at
    ([x], [y]). *)

val set_cell : world -> int -> int -> life -> unit
(** [inject_cell world x y life] sets the cell at the specified [x] and
    [y] coordinates in world [world] with the specified data [life].

    Pre-conditions: the coordinates must be a valid range. Raises
    [InvalidWorldOperation (x, y)] if there is no life at coordinates
    ([x], [y]). *)

val cell_to_json : life option -> Yojson.Safe.t
(** [cell_to_json cell] converts a cell [cell] into its json
    representation. *)

val cell_from_json : Yojson.Safe.t -> life option
(** [cell_from_json json] converts a json representation [json] of a
    cell into a cell type. *)
