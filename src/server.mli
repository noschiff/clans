type t
(** A type representing the server*)

val init : ((string * Yojson.Safe.t) option -> unit) -> unit
(** [init p] creates a server with a stream's push function p. *)