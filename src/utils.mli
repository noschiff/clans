(** Additional utilities for the codebase. *)

val cmp_float_lists : float list -> float list -> bool
(** [cmp_float_lists l1 l2] compares 2 float lists and returns true if
    they are extremely similar. "Extremely similiar" means that the 2
    brains are similar enough for very minor floating-point differences
    in their structure to not matter. *)

val cmp_float_matrices : float list list -> float list list -> bool
(** [cmp_float_matrices l1 l2] compares 2 float matrices and returns
    true if they are extremely similar. "Extremely similiar" means that
    the 2 brains are similar enough for very minor floating-point
    differences in their structure to not matter. *)
