type t

val create : int -> int -> t
(** [create m n] creates an mxn matrix of 0s. *)

val of_list : float list list -> t
(** [from_list l] creates a matrix from a 
		list of lists. 

		Requires that the list of lists represents
		a matrix. *)

val rows : t -> int
(** [rows m] returns the number of rows of [m]. *)

val cols : t -> int
(** [cols m] returns the number of columns of [m]. *)

val row : int -> t -> float list
(** [row i m] is the i'th row of matrix [m]. 
		
		Requires: [0 <= i < rows m]. *)

val col : int -> t -> float list
(** [col i m] is the i'th column of matrix [m]. 
		
		Requires: [0 <= i < cols m]. *)

val get : int -> int -> t -> float
(** [get i j m] gets the (i, j)th element of the matrix m. *)

val transpose : t -> t
(** [transpose m] is the matrix transposition of [m]. *)

val dot : t -> t -> t
(** [dot a b] returns the matrix product of a and b. 

		Requires: a has the same number of columns as b
		has rows. *)