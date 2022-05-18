(** Module represeting a neural network *)

type t
(** Type representing a brain *)

type mut_params = {
  swap_chance : float;
      (** Chance to completely change a given weight into a new value
          (default 0.005)

          Requires: [0 <= swap_chance < 1] *)
  mutate_chance : float;
      (** Chance to mutate a given weight by adding a random normal
          variable to it (default 0.02)

          Requires: [0 <= mutate_chance < 1] *)
  mutate_stdev : float;
      (** standard deviation of the normal variable to add to the weight
          (default 1)

          Requires: [0 < mutate_stdev] *)
}

val default_params : mut_params
(** Default mutation parameters *)

val params_of_json : Yojson.Safe.t -> mut_params
(** [params_of_json j] converts json [j] into mutation parameters *)

val params_to_json : mut_params -> Yojson.Safe.t
(** [params_of_json p] converts mutation parameters [p] into json *)

val create : int -> int -> int -> int list -> t
(** [create i o m ls] creates a neural network with an input size [i],
    output [o], and input layer sizes [ls]. Also creates [m] memory
    nodes (not included in the lengths of [i] and [o]. These will be
    saved after propogation and passed as input in the next call.

    Values in neural network will be randomly initialized normal
    distrbution with stdev 1, mean 0. *)

val combine : float -> t -> t -> t
(** [combine p a b] combines the networks [a] and [b] with a weight of
    [p] on [a] and [1-p] on [b]. *)

val mutate : ?r:float -> ?g:float -> mut_params -> t -> t
(** [mutate r g p b] creates and returns a new brain after mutating the
    weights where [r] is an optional parameter that determines which
    form of mutation will occur and [g] is an optional argument for a
    normally distrubuted random variable. If left blank, [r] and [g]
    will be generated randomly. Preconditions: [r] is in (0, 1), [g] is
    a legal float for a normal distribution. *)

val to_json : t -> Yojson.Safe.t
(** [from_json brain] converts [brain] into json. *)

val from_json : Yojson.Safe.t -> t
(** [from_json json] converts [json] into a brain. *)

val eval : t -> float list -> t
(** [eval b l] propogates a new input set [l] through the neural network
    [b] and returns the new state. *)

val out : t -> float list
(** [out b] returns the output of neural network [b] from the last
    evaluation. Does not include memory nodes.

    If the network has never been evaluated before, returns 0. *)

val mem : t -> float list
(** [mem b] returns the current memory nodes of [b]. *)

val cmp_brain : t -> t -> bool
(** [cmp_brain b1 b2] compares brains [b1] and [b2] and returns if they
    are extremely similar. "Extremely similiar" means that the 2 brains
    are similar enough for very minor floating-point differences in
    their structure to not matter. *)
