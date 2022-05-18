(** Handles the server initialization and management. *)

type t = (string * Yojson.Safe.t * Yojson.Safe.t Lwt.u) option -> unit
(** Represents a stream's push function, a [Lwt_stream] that takes a
    [string] request type, [Yojson.Safe.t] data, and has a return
    promise [Yojson.Safe.t Lwt.u] to be fulfilled. *)

val init : t -> unit
(** [init p] creates a server with a stream's push function [p]. *)
