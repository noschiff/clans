(** Module represeting a neural network *)

type t
(** Type representing a neural net *)

val create : int -> int -> int -> int list -> t
(** [create i o m ls] creates a neural network with an input size [i],
    output [o], and input layer sizes [ls]. Also creates [m] memory
    nodes (not included in the lengths of [i] and [o]. These will be
    saved after propogation and passed as input in the next call.

    Values in neural network will be randomly initialized normal
    distrbution with stdev 1, mean 0. *)

val to_json : t -> Yojson.Safe.t
(** [from_json brain] converts [brain] into json. **)

val from_json : Yojson.Safe.t -> t
(** [from_json json] converts [json] into a brain. **)

val eval : t -> float list -> t
(** [eval b l] propogates a new input set [l] through the neural network
    and returns the new state. *)

val out : t -> float list
(** [out b] returns the output of neural network [b] from the last
    evaluation. Does not include memory nodes.

    If the network has never been evaluated before, this starts at 0. *)

val mem : t -> float list
(** [mem b] returns the current memory nodes of [b]. *)