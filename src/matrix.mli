type t

val create : int -> int -> t
(** [create n m] creates an [n] by [m] matrix of 0s. *)

val of_list : float list list -> t
(** [from_list l] creates a matrix from [l] list of lists.

    Requires that the list of lists represents a matrix. *)

val to_list : t -> float list list
(** [to_list m] converts [m] to a list of lists. *)

val dims : t -> int * int
(** [dims m] is the dimensions of [m] in (rows, columns) form. *)

val rows : t -> int
(** [rows m] returns the number of rows of [m]. *)

val cols : t -> int
(** [cols m] returns the number of columns of [m]. *)

val row : int -> t -> float list
(** [row i m] is the [i]th row of matrix [m].

    Requires: [0 <= i < rows m]. *)

val col : int -> t -> float list
(** [col i m] is the [i]th column of matrix [m].

    Requires: [0 <= i < cols m]. *)

val get : int -> int -> t -> float
(** [get i j m] gets the ([i], [j])th element of the matrix [m]. *)

val transpose : t -> t
(** [transpose m] is the matrix transposition of [m]. *)

val dot : t -> t -> t
(** [dot a b] returns the matrix product of [a] and [b].

    Requires: [a] has the same number of columns as [b] has rows. *)

val map2 : (float -> float -> float) -> t -> t -> t
(** [map f a b] applies [f] on each element of [a] and [b], resulting in
    a new matrix with the same dimensions.

    Requires: [a] and [b] have the same dimensions. *)

val map : (float -> float) -> t -> t
(** [map a f] applies [f] to every element of [a]. *)