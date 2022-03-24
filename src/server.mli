type t
(** A type representing the server*)

val init : (Yojson.Safe.t option -> unit) -> t
(* [init p] creates a server with a stream's push function p. *)